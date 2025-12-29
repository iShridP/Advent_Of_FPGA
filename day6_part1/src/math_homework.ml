open! Core 
open! Hardcaml
open! Signal
open! Hardcaml.Always
let ram_depth = 1000      
let num_bits_ram_represent = num_bits_to_represent ram_depth
let num_input_bits = 16       
let num_internal_bits = 32     
let num_result_bits = 64       

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    start : 'a;
    finish : 'a;
    data_in : 'a [@bits num_input_bits]; 
    write_en : 'a;                    
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    grand_total : 'a With_valid.t [@bits num_result_bits];
  }
  [@@deriving hardcaml]
end

let make_ram ~clock ~we ~w_addr ~w_data ~r_addr =
  let output = 
    Ram.create 
      ~collision_mode:Read_before_write 
      ~size:ram_depth
      ~write_ports:[| { write_clock=clock; write_enable=we; write_address=w_addr; write_data=w_data } |]
      ~read_ports:[|  { read_clock=clock; read_enable=vdd; read_address=r_addr } |]
      ()
  in
  output.(0)

module States = struct 
  type t = Loading | Calculate | Done 
  [@@deriving sexp_of, compare ~localize, enumerate] 
end

let create scope ({clock; clear; start; finish; data_in; write_en}: _ I.t): _ O.t = 
  let spec = Reg_spec.create ~clock:clock ~clear:clear () in
  let open Always in

  let sm = State_machine.create (module States) spec in

  (* Pointers *)
  let%hw_var write_col_idx = Variable.reg spec ~width:(num_bits_ram_represent) in
  let%hw_var write_row_idx = Variable.reg spec ~width:3 in (*which operand ka row*)
  let%hw_var read_col_idx = Variable.reg spec ~width:(num_bits_ram_represent) in
  let%hw_var max_col_count = Variable.reg spec ~width:(num_bits_ram_represent) in

  let%hw_var grand_total = Variable.reg spec ~width:num_result_bits in
  let%hw_var grand_total_valid = Variable.wire ~default:gnd () in

  (*decode which RAM is being written to*)
  let wr_enable_1 = write_en &: (write_row_idx.value ==:. 0) in
  let wr_enable_2 = write_en &: (write_row_idx.value ==:. 1) in
  let wr_enable_3 = write_en &: (write_row_idx.value ==:. 2) in
  let wr_enable_4 = write_en &: (write_row_idx.value ==:. 3) in
  let wr_enable_op = write_en &: (write_row_idx.value ==:. 4) in

  let w_data_32 = uresize data_in ~width:num_internal_bits in

  (*5 RAM blocks *)
  (*share the same read/write addresses, but we control which one writes via enable*)
  let r_data_1 = uresize (make_ram ~clock:clock ~we:wr_enable_1 ~w_addr:write_col_idx.value ~w_data:w_data_32 ~r_addr:read_col_idx.value) ~width:num_result_bits in
  let r_data_2 = uresize (make_ram ~clock:clock ~we:wr_enable_2 ~w_addr:write_col_idx.value ~w_data:w_data_32 ~r_addr:read_col_idx.value) ~width:num_result_bits in
  let r_data_3 = uresize (make_ram ~clock:clock ~we:wr_enable_3 ~w_addr:write_col_idx.value ~w_data:w_data_32 ~r_addr:read_col_idx.value) ~width:num_result_bits in
  let r_data_4 = uresize (make_ram ~clock:clock ~we:wr_enable_4 ~w_addr:write_col_idx.value ~w_data:w_data_32 ~r_addr:read_col_idx.value) ~width:num_result_bits in
  let r_op = uresize (make_ram ~clock:clock ~we:wr_enable_op~w_addr:write_col_idx.value ~w_data:w_data_32 ~r_addr:read_col_idx.value) ~width:num_result_bits in

  (*calculate both the possibilities at once*)
  let sum_res = uresize (r_data_1 +: r_data_2 +: r_data_3 +: r_data_4) ~width:num_result_bits in
  let mul_res = uresize (r_data_1 *: r_data_2 *: r_data_3 *: r_data_4) ~width:num_result_bits in

  let is_mult = lsb r_op in (*mult or add flag is just lsb*)
  let column_result = mux2 is_mult mul_res sum_res in

  compile [
    sm.switch [
      (Loading, [
          when_ finish [
            write_row_idx <-- write_row_idx.value +:. 1;
            when_ (write_row_idx.value ==:. 0) [
              max_col_count <-- write_col_idx.value;
            ];

            write_col_idx <-- zero num_bits_ram_represent;
          ];

          when_ (write_en &: ~:(finish)) [
            write_col_idx <-- write_col_idx.value +:. 1;
          ];

          when_ start [
            read_col_idx <-- zero num_bits_ram_represent;
            grand_total  <-- zero num_result_bits;
            grand_total_valid <-- gnd;
            sm.set_next Calculate;
          ]
        ]);

      (Calculate, [
          read_col_idx <-- read_col_idx.value +:. 1;

          when_ (read_col_idx.value >:. 0) [
            grand_total <-- grand_total.value +: column_result;
          ];

          when_ (read_col_idx.value ==: max_col_count.value) [
            sm.set_next Done;
          ]
        ]);

      (Done, [
          grand_total_valid <-- vdd;
        ])
    ]
  ];

  {grand_total = {value = grand_total.value; valid = grand_total_valid.value}}
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"math_homework" create
;;