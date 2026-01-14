open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let num_bits_pos = 32
let ram_depth = 1000
let num_bits_ram_represent = num_bits_to_represent ram_depth
let num_bits_size_product = 64

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    start : 'a;
    finish : 'a; 
    finish_input : 'a;
    input_pos : 'a [@bits num_bits_pos];
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    circuit_size_product : 'a With_valid.t [@bits num_bits_size_product];
  }
  [@@deriving hardcaml]
end

module States = struct
  type t = Idle | InputX | InputY | InputZ | CalculateOrder | Done
  [@@deriving sexp_of, compare ~localize, enumerate] 
end

let make_ram ~clock ~we ~w_addr ~w_data ~r_addr_a ~r_addr_b =
  let output = 
    Ram.create 
      ~collision_mode:Read_before_write 
      ~size:ram_depth
      ~write_ports:[|{write_clock=clock; write_enable=we; write_address=w_addr; write_data=w_data}|]
      ~read_ports:[|
        {read_clock=clock; read_enable=vdd; read_address=r_addr_a};
        {read_clock=clock; read_enable=vdd; read_address=r_addr_b}
      |]
      ()
  in
  (output.(0), output.(1))

let create scope ({clock; clear; start; finish; finish_input; input_pos}: _ I.t): _ O.t =
  let spec = Reg_spec.create ~clock:clock ~clear:clear () in
  let open Always in

  let sm = State_machine.create (module States) spec in

  let%hw_var idx_i = Variable.reg spec ~width:num_bits_ram_represent in
  let%hw_var idx_j = Variable.reg spec ~width:num_bits_ram_represent in
  let%hw_var total_points = Variable.reg spec ~width:num_bits_ram_represent in

  let%hw_var ram_w_addr = Variable.reg spec ~width:num_bits_ram_represent in

  (* We need two read addresses for the Pair Generator *)
  let ram_r_addr_a = if_ (sm.is_state States.CalculateOrder) idx_i.value ram_w_addr.value in
  let ram_r_addr_b = if_ (sm.is_state States.CalculateOrder) idx_j.value ram_w_addr.value in

  let write_enable_x = sm.is_state States.InputX in
  let write_enable_y = sm.is_state States.InputY in
  let write_enable_z = sm.is_state States.InputZ in

  let (ram_x_a, ram_x_b) = make_ram ~clock ~we:write_enable_x ~w_addr:ram_w_addr.value 
      ~w_data:(uresize input_pos ~width:num_bits_pos) ~r_addr_a:ram_r_addr_a ~r_addr_b:ram_r_addr_b in

  let (ram_y_a, ram_y_b) = make_ram ~clock ~we:write_enable_y ~w_addr:ram_w_addr.value 
      ~w_data:(uresize input_pos ~width:num_bits_pos) ~r_addr_a:ram_r_addr_a ~r_addr_b:ram_r_addr_b in

  let (ram_z_a, ram_z_b) = make_ram ~clock ~we:write_enable_z ~w_addr:ram_w_addr.value 
      ~w_data:(uresize input_pos ~width:num_bits_pos) ~r_addr_a:ram_r_addr_a ~r_addr_b:ram_r_addr_b in

  (* Pipelined Math Variables *)
  let%hw_var pipe_valid_1 = Variable.reg spec ~width:1 in
  let%hw_var pipe_valid_2 = Variable.reg spec ~width:1 in
  let%hw_var pipe_valid_3 = Variable.reg spec ~width:1 in

  let%hw_var diff_x = Variable.reg spec ~width:num_bits_pos in
  let%hw_var diff_y = Variable.reg spec ~width:num_bits_pos in
  let%hw_var diff_z = Variable.reg spec ~width:num_bits_pos in

  let%hw_var sq_x = Variable.reg spec ~width:num_bits_size_product in
  let%hw_var sq_y = Variable.reg spec ~width:num_bits_size_product in
  let%hw_var sq_z = Variable.reg spec ~width:num_bits_size_product in

  let%hw_var dist_sum = Variable.reg spec ~width:num_bits_size_product in
  let%hw_var dist_valid = Variable.reg spec ~width:1 in

  compile [
    diff_x <-- (ram_x_a -: ram_x_b);
    diff_y <-- (ram_y_a -: ram_y_b);
    diff_z <-- (ram_z_a -: ram_z_b);
    pipe_valid_1 <-- (if_ (sm.is_state States.CalculateOrder) vdd gnd);

    sq_x <-- (diff_x.value *+ diff_x.value);
    sq_y <-- (diff_y.value *+ diff_y.value);
    sq_z <-- (diff_z.value *+ diff_z.value);
    pipe_valid_2 <-- pipe_valid_1.value;

    dist_sum <-- (sq_x.value +: sq_y.value +: sq_z.value);
    dist_valid <-- pipe_valid_2.value;

    sm.switch [
      States.Idle, [
        when_ start [
          ram_w_addr <-- zero num_bits_ram_represent;
          sm.set_next States.InputX;
        ]
      ];
      States.InputX, [
        sm.set_next States.InputY;
      ];
      States.InputY, [
        sm.set_next States.InputZ;
      ];
      States.InputZ, [  
        sm.set_next States.InputX;
        ram_w_addr <-- ram_w_addr.value +:. 1;
        when_ finish_input [
          total_points <-- ram_w_addr.value +:. 1;
          idx_i <-- zero num_bits_ram_represent;
          idx_j <-- one num_bits_ram_represent; 
          sm.set_next States.CalculateOrder
        ]
      ];
      States.CalculateOrder, [
        idx_j <-- idx_j.value +:. 1;

        when_ (idx_j.value ==: total_points.value) [
          idx_i <-- idx_i.value +:. 1;
          idx_j <-- idx_i.value +:. 2; 

          when_ (idx_i.value ==: (total_points.value -:. 2)) [
            sm.set_next States.Done
          ]
        ];

      ];
      States.Done, [
        dist_valid <-- gnd; 
      ];
    ]
  ];

  {circuit_size_product = {value = dist_sum.value; valid = dist_valid.value}}
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"junction_box" create
;;