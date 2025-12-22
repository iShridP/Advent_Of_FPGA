open! Core 
open! Hardcaml
open! Signal
open! Hardcaml.Always

let num_bits_row = 140
let num_bits_total = 32

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    start : 'a;
    finish : 'a;
    row : 'a [@bits num_bits_row]
  }
  [@@ deriving hardcaml]
end

module O = struct
  type 'a t = {
    total_rolls : 'a With_valid.t [@bits num_bits_total]
  }
  [@@deriving hardcaml]
end

module States = struct
  type t = Idle | Accept | Calculate | EndCalculate | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let get_current_row_output (curr_row: Signal.t) (p_row: Signal.t) (n_row : Signal.t) : Signal.t = 
  let width = 140 in

  let is_position_accessible_list = 
    List.init width ~f:(fun i ->
        let is_paper = bit curr_row ~pos:i in

        (*conditions different on edges and middle*)
        let neighbors = 
          if(i = 0) then
            [
              bit p_row ~pos:i; bit p_row ~pos:(i+1);
              bit curr_row ~pos:(i+1);
              bit n_row ~pos:i; bit n_row ~pos:(i+1)
            ]

          else if(i = 139) then
            [
              bit p_row ~pos:(i-1); bit p_row ~pos:i; 
              bit curr_row ~pos:(i-1);               
              bit n_row ~pos:(i-1); bit n_row ~pos:i; 
            ]
          else
            [
              bit p_row ~pos:(i-1); bit p_row ~pos:i; bit p_row ~pos:(i+1);
              bit curr_row ~pos:(i-1); bit curr_row ~pos:(i+1);
              bit n_row ~pos:(i-1); bit n_row ~pos:i; bit n_row ~pos:(i+1)
            ]
        in


        let neighbor_sum = 
          List.map neighbors ~f:(fun n -> uresize n ~width:4) 
          |> List.reduce_exn ~f:(+:) 
        in

        let accessible = is_paper &: (neighbor_sum <: (of_int_trunc ~width:4 4)) in

        uresize accessible ~width:num_bits_total
      )
  in

  tree ~arity:2 
    ~f:(fun inputs -> List.reduce_exn inputs ~f:(+:)) 
    is_position_accessible_list

let create scope ({clock; clear; start; finish; row}: _ I.t): _ O.t = 
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in

  let sm = State_machine.create(module States) spec in
  let%hw_var total_rolls = Variable.reg spec ~width:num_bits_total in

  let total_rolls_valid = Variable.wire ~default:gnd () in

  let past_row = Variable.reg spec ~width:num_bits_row in
  let current_row = Variable.reg spec ~width:num_bits_row in
  let next_row = Variable.reg spec ~width:num_bits_row in

  let current_row_output = get_current_row_output current_row.value past_row.value next_row.value in

  compile
    [
      sm.switch
        [
          (Idle,
           [
             when_ start [
               total_rolls_valid <-- gnd;
               past_row <-- zero num_bits_row;
               current_row <-- zero num_bits_row;
               next_row <-- zero num_bits_row;
               sm.set_next Accept
             ]
           ]
          );
          (Accept,
           [
             past_row <-- current_row.value;
             current_row <-- next_row.value;
             if_ finish [next_row <-- zero num_bits_row; sm.set_next EndCalculate] [next_row <-- row; sm.set_next Calculate]
           ]
          );
          (Calculate,
           [
             total_rolls <-- (total_rolls.value +: (uresize current_row_output ~width:num_bits_total));
             sm.set_next Accept
           ]
          );
          (EndCalculate,
           [
             total_rolls <-- (total_rolls.value +: (uresize current_row_output ~width:num_bits_total));
             sm.set_next Done
           ]
          );
          (Done,
           [
             total_rolls_valid <-- vdd
           ]
          );
        ]
    ];

  {total_rolls = {value = total_rolls.value; valid = total_rolls_valid.value}}

;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"paper_rolls" create
;;