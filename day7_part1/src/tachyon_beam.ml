open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let num_bits_width = 32
let num_bits_width_is_beam_flag_number = 200 
let num_bits_times_split = 32

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    start : 'a;
    finish : 'a;
    finish_row : 'a;
    manifold_item : 'a [@bits 2];
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    tachyon_times_split : 'a With_valid.t [@bits num_bits_times_split];
  }
  [@@deriving hardcaml]
end

module States = struct
  type t = Idle | InputSource | InputRow | Done
  [@@deriving sexp_of, compare ~localize, enumerate] 
end

let create scope ({clock; clear; start; finish; finish_row; manifold_item}: _ I.t): _ O.t =
  let spec = Reg_spec.create ~clock:clock ~clear:clear () in
  let open Always in

  let sm = State_machine.create (module States) spec in

  let%hw_var tachyon_times_split = Variable.reg spec ~width:num_bits_times_split in
  let%hw_var tachyon_times_split_valid = Variable.wire ~default:gnd () in

  let is_beam_current = Variable.reg spec ~width:num_bits_width_is_beam_flag_number in
  let is_beam_next = Variable.reg spec ~width:num_bits_width_is_beam_flag_number in

  let width_row = Variable.reg spec ~width:num_bits_width in
  let current_row_pos = Variable.reg spec ~width:num_bits_width in

  (* FIX: Added labeled arguments ~f and ~by *)
  let dynamic_sll target shift_amount = Signal.log_shift ~f:Signal.sll target ~by:shift_amount in
  let dynamic_srl target shift_amount = Signal.log_shift ~f:Signal.srl target ~by:shift_amount in

  let beam_active = lsb (dynamic_srl is_beam_current.value current_row_pos.value) in

  let mask_pos   = dynamic_sll (one num_bits_width_is_beam_flag_number) current_row_pos.value in
  let mask_left  = dynamic_sll (one num_bits_width_is_beam_flag_number) (current_row_pos.value -:. 1) in
  let mask_right = dynamic_sll (one num_bits_width_is_beam_flag_number) (current_row_pos.value +:. 1) in

  compile [
    sm.switch [
      (Idle, [
          when_ start [
            tachyon_times_split <-- zero num_bits_times_split;
            tachyon_times_split_valid <-- gnd;
            is_beam_current <-- zero num_bits_width_is_beam_flag_number;
            is_beam_next <-- zero num_bits_width_is_beam_flag_number;
            width_row <-- zero num_bits_width;
            current_row_pos <-- zero num_bits_width;
            sm.set_next InputSource
          ]
        ]
      );

      (InputSource, [
          width_row <-- width_row.value +: (of_int_trunc ~width:num_bits_width 1);

          when_ (manifold_item ==:. 2)[
            (* FIX: 'one' is a function, must provide width to create a Signal *)
            is_beam_current <-- (is_beam_current.value |: dynamic_sll (one num_bits_width_is_beam_flag_number) width_row.value);
          ];

          when_ finish_row [
            current_row_pos <-- zero num_bits_width;
            sm.set_next InputRow;
          ]
        ]
      );

      (InputRow, [
          when_ (~: finish_row) [
            current_row_pos <-- current_row_pos.value +: (of_int_trunc ~width:num_bits_width 1);

            when_ beam_active [
              when_ (manifold_item ==:. 1) [
                tachyon_times_split <-- tachyon_times_split.value +: (of_int_trunc ~width:num_bits_times_split 1);
                is_beam_next <-- (is_beam_next.value |: mask_left |: mask_right);
              ];

              when_ (manifold_item <>:. 1) [
                is_beam_next <-- (is_beam_next.value |: mask_pos);
              ];
            ];
          ];

          when_ (finish_row) [
            current_row_pos <-- zero num_bits_width;
            is_beam_current <-- is_beam_next.value;
            is_beam_next <-- zero num_bits_width_is_beam_flag_number;
          ];

          when_ (finish) [
            sm.set_next Done
          ]
        ]
      );

      (Done, [
          tachyon_times_split_valid <-- vdd
        ]
      );
    ]
  ];

  {tachyon_times_split = {value = tachyon_times_split.value; valid = tachyon_times_split_valid.value}}
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"tachyon_beam" create
;;