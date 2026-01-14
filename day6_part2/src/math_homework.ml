open! Core 
open! Hardcaml
open! Signal
open! Hardcaml.Always
let ram_depth = 1000      
let num_bits_ram_represent = num_bits_to_represent ram_depth
let num_input_bits = 16       
let num_internal_bits = 32     
let num_result_bits = 64
let num_bits_shift = 4 

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    start : 'a;
    finish : 'a;
    data_in : 'a [@bits num_input_bits]; 
    shift : 'a [@bits num_bits_shift];
    write_en : 'a
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    grand_total : 'a With_valid.t [@bits num_result_bits];
  }
  [@@deriving hardcaml]
end

let div_mod_10 input =
  let width = width input in
  let magic_num = of_int_trunc ~width:17 52429 in 
  let product = (uresize input ~width:width) *: magic_num in
  let quotient = uresize (log_shift ~f:srl product ~by:(of_int_trunc ~width:5 19)) ~width:width in
  let remainder = input -: (uresize (quotient *: (of_int_trunc ~width:4 10)) ~width:width) in
  (quotient, uresize remainder ~width:4) 

let get_places (number : Signal.t) =
  let remaining, ones = div_mod_10 number in
  let remaining, tens = div_mod_10 remaining in
  let remaining, hundreds = div_mod_10 remaining in
  let _, thousands = div_mod_10 remaining in
  (ones, tens, hundreds, thousands)

let num_digits (num_1: Signal.t) (num_2: Signal.t) (num_3: Signal.t) =
  mux2 (num_3 <>:. 0) (of_int_trunc ~width:3 4)
    (mux2 (num_2 <>:. 0) (of_int_trunc ~width:3 3)
       (mux2 (num_1 <>:. 0) (of_int_trunc ~width:3 2) (of_int_trunc ~width:3 1))) 

let mux4 selector d0 d1 d2 d3 =
  mux selector [ d0; d1; d2; d3 ]

let get_new_num (digits: Signal.t) (num_0: Signal.t) (num_1: Signal.t) (num_2: Signal.t) (num_3: Signal.t) =
  mux4
    (uresize (digits -: (of_int_trunc ~width:3 1)) ~width:2)
    (uresize (num_0 *: of_int_trunc ~width:16 1) ~width:num_internal_bits)
    (uresize (num_0 *: of_int_trunc ~width:16 10 +: num_1 *: of_int_trunc ~width:16 1) ~width:num_internal_bits)
    (uresize (num_0 *: of_int_trunc ~width:16 100 +: num_1 *: of_int_trunc ~width:16 10 +: num_2 *: of_int_trunc ~width:16 1) ~width:num_internal_bits)
    (uresize (num_0 *: of_int_trunc ~width:16 1000 +: num_1 *: of_int_trunc ~width:16 100 +: num_2 *: of_int_trunc ~width:16 10 +: num_3 *: of_int_trunc ~width:16 1) ~width:num_internal_bits)

let weird_ahh_number_making (num1 : Signal.t) (num2 : Signal.t) (num3 : Signal.t) (num4 : Signal.t) =
  let num1_0, num1_1, num1_2, num1_3 = get_places num1 in
  let num2_0, num2_1, num2_2, num2_3 = get_places num2 in
  let num3_0, num3_1, num3_2, num3_3 = get_places num3 in
  let num4_0, num4_1, num4_2, num4_3 = get_places num4 in

  let digits0 = num_digits num2_0 num3_0 num4_0 in
  let digits1 = num_digits num2_1 num3_1 num4_1 in
  let digits2 = num_digits num2_2 num3_2 num4_2 in
  let digits3 = num_digits num2_3 num3_3 num4_3 in

  let new_num1 = get_new_num digits0 num1_0 num2_0 num3_0 num4_0 in
  let new_num2 = get_new_num digits1 num1_1 num2_1 num3_1 num4_1 in
  let new_num3 = get_new_num digits2 num1_2 num2_2 num3_2 num4_2 in
  let new_num4 = get_new_num digits3 num1_3 num2_3 num3_3 num4_3 in
  (new_num1, new_num2, new_num3, new_num4)

let convert_to_one_if_zero (num: Signal.t) =
  mux2 (num ==:. 0) (of_int_trunc ~width:(width num) 1) num

let shift_by_factor (data: Signal.t) (shift: Signal.t) =
  let width = width data in
  uresize (
    mux4 
      (uresize shift ~width:2)
      (data *: (of_int_trunc ~width:16 1))
      (data *: (of_int_trunc ~width:16 10))
      (data *: (of_int_trunc ~width:16 100))
      (data *: (of_int_trunc ~width:16 1000))
  ) ~width:width

let make_ram ~clock ~we ~w_addr ~w_data ~r_addr =
  let output = 
    Ram.create 
      ~collision_mode:Read_before_write 
      ~size:ram_depth
      ~write_ports:[|{write_clock=clock; write_enable=we; write_address=w_addr; write_data=w_data}|]
      ~read_ports:[|{read_clock=clock; read_enable=vdd; read_address=r_addr}|]
      ()
  in
  output.(0)

module States = struct 
  type t = Loading | Calculate | Done 
  [@@deriving sexp_of, compare ~localize, enumerate] 
end

let create scope ({clock; clear; start; finish; data_in; write_en; shift}: _ I.t): _ O.t = 
  let spec = Reg_spec.create ~clock:clock ~clear:clear () in
  let open Always in

  let sm = State_machine.create (module States) spec in

  let%hw_var write_col_idx = Variable.reg spec ~width:(num_bits_ram_represent) in
  let%hw_var write_row_idx = Variable.reg spec ~width:3 in 
  let%hw_var read_col_idx = Variable.reg spec ~width:(num_bits_ram_represent) in
  let%hw_var max_col_count = Variable.reg spec ~width:(num_bits_ram_represent) in

  let%hw_var grand_total = Variable.reg spec ~width:num_result_bits in
  let%hw_var grand_total_valid = Variable.wire ~default:gnd () in

  let wr_enable_1 = write_en &: (write_row_idx.value ==:. 0) in
  let wr_enable_2 = write_en &: (write_row_idx.value ==:. 1) in
  let wr_enable_3 = write_en &: (write_row_idx.value ==:. 2) in
  let wr_enable_4 = write_en &: (write_row_idx.value ==:. 3) in
  let wr_enable_op = write_en &: (write_row_idx.value ==:. 4) in

  let w_data_32 = uresize data_in ~width:num_internal_bits in

  let r_data_1 = uresize (make_ram ~clock:clock ~we:wr_enable_1 ~w_addr:write_col_idx.value ~w_data:w_data_32 ~r_addr:read_col_idx.value) ~width:num_result_bits in
  let r_data_2 = uresize (make_ram ~clock:clock ~we:wr_enable_2 ~w_addr:write_col_idx.value ~w_data:w_data_32 ~r_addr:read_col_idx.value) ~width:num_result_bits in
  let r_data_3 = uresize (make_ram ~clock:clock ~we:wr_enable_3 ~w_addr:write_col_idx.value ~w_data:w_data_32 ~r_addr:read_col_idx.value) ~width:num_result_bits in
  let r_data_4 = uresize (make_ram ~clock:clock ~we:wr_enable_4 ~w_addr:write_col_idx.value ~w_data:w_data_32 ~r_addr:read_col_idx.value) ~width:num_result_bits in
  let r_op = uresize (make_ram ~clock:clock ~we:wr_enable_op~w_addr:write_col_idx.value ~w_data:w_data_32 ~r_addr:read_col_idx.value) ~width:num_result_bits in

  let shift_data_1 = make_ram ~clock:clock ~we:wr_enable_1 ~w_addr:write_col_idx.value ~w_data:shift ~r_addr:read_col_idx.value in
  let shift_data_2 = make_ram ~clock:clock ~we:wr_enable_2 ~w_addr:write_col_idx.value ~w_data:shift ~r_addr:read_col_idx.value in
  let shift_data_3 = make_ram ~clock:clock ~we:wr_enable_3 ~w_addr:write_col_idx.value ~w_data:shift ~r_addr:read_col_idx.value in
  let shift_data_4 = make_ram ~clock:clock ~we:wr_enable_4 ~w_addr:write_col_idx.value ~w_data:shift ~r_addr:read_col_idx.value in


  let new_num1, new_num2, new_num3, new_num4 = weird_ahh_number_making (shift_by_factor r_data_1 shift_data_1) (shift_by_factor r_data_2 shift_data_2) (shift_by_factor r_data_3 shift_data_3) (shift_by_factor r_data_4 shift_data_4) in

  let sum_res = uresize (new_num1 +: new_num2 +: new_num3 +: new_num4) ~width:num_result_bits in
  let mul_res = uresize ((convert_to_one_if_zero new_num1) *: (convert_to_one_if_zero new_num2) *: (convert_to_one_if_zero new_num3) *: (convert_to_one_if_zero new_num4)) ~width:num_result_bits in

  let is_mult = lsb r_op in 
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