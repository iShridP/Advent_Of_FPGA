open! Core 
open! Hardcaml
open! Signal
open! Hardcaml.Always

let num_bits_row = 140
let num_bits_total = 32
let num_bits_index = 16 

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    start : 'a;
    finish : 'a;
    row : 'a [@bits num_bits_row]
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    total_rolls : 'a With_valid.t [@bits num_bits_total]
  }
  [@@deriving hardcaml]
end

module States = struct
  type t = Idle | LoadInput | IterationStart | ProcessRow | IterationCheck | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end


let get_row_logic (curr_row: Signal.t) (p_row: Signal.t) (n_row: Signal.t) = 
  let width = 140 in

  let removal_data = 
    List.init width ~f:(fun i ->
        let is_paper = bit curr_row ~pos:i in

        let neighbors = 
          if i = 0 then
            [ bit p_row ~pos:i; bit p_row ~pos:(i+1); 
              bit curr_row ~pos:(i+1); 
              bit n_row ~pos:i; bit n_row ~pos:(i+1) ]
          else if i = 139 then
            [ bit p_row ~pos:(i-1); bit p_row ~pos:i; 
              bit curr_row ~pos:(i-1); 
              bit n_row ~pos:(i-1); bit n_row ~pos:i ]
          else
            [ bit p_row ~pos:(i-1); bit p_row ~pos:i; bit p_row ~pos:(i+1);
              bit curr_row ~pos:(i-1); bit curr_row ~pos:(i+1);
              bit n_row ~pos:(i-1); bit n_row ~pos:i; bit n_row ~pos:(i+1) ]
        in

        let neighbor_sum = 
          List.map neighbors ~f:(fun n -> uresize n ~width:4) 
          |> List.reduce_exn ~f:(+:) 
        in

        let is_removable = is_paper &: (neighbor_sum <: (of_int_trunc ~width:4 4)) in
        (is_removable, uresize is_removable ~width:num_bits_total)
      )
  in

  let removal_mask = Signal.concat_msb (List.rev (List.map removal_data ~f:fst)) in
  let count = tree ~arity:2 ~f:(fun inputs -> List.reduce_exn inputs ~f:(+:)) (List.map removal_data ~f:snd) in
  let new_row = curr_row &: (~: removal_mask) in

  (new_row, count)

let create scope ({clock; clear; start; finish; row}: _ I.t): _ O.t = 
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in

  let sm = State_machine.create(module States) spec in

  let%hw_var total_rolls = Variable.reg spec ~width:num_bits_total in
  let%hw_var past_total_rolls = Variable.reg spec ~width:num_bits_total in
  let total_rolls_valid = Variable.wire ~default:gnd () in

  let row_index = Variable.reg spec ~width:num_bits_index in
  let max_rows = Variable.reg spec ~width:num_bits_index in 
  let stored_prev_row = Variable.reg spec ~width:num_bits_row in

  let grid_storage = Array.init 140 ~f:(fun _ -> Variable.reg spec ~width:num_bits_row) in

  let read_row idx = 
    let all_rows = Array.to_list (Array.map grid_storage ~f:(fun r -> r.value)) in
    mux idx all_rows 
  in

  let p_row_read = mux2 
      (row_index.value ==: (zero num_bits_index)) 
      (zero num_bits_row) 
      stored_prev_row.value in

  let c_row_read = read_row row_index.value in

  let n_row_read = mux2 
      (row_index.value >=: (max_rows.value -: (of_int_trunc ~width:num_bits_index 1))) 
      (zero num_bits_row) 
      (read_row (row_index.value +: (of_int_trunc ~width:num_bits_index 1))) in

  let (next_row_val, removal_count) = get_row_logic c_row_read p_row_read n_row_read in

  compile
    [
      sm.switch
        [
          (Idle,
           [
             when_ start [
               total_rolls_valid <-- gnd;
               total_rolls <-- zero num_bits_total;
               past_total_rolls <-- ones num_bits_total;
               row_index <-- zero num_bits_index;
               sm.set_next LoadInput
             ]
           ]
          );
          (LoadInput, 
           [
             if_ finish 
               [
                 max_rows <-- row_index.value; 
                 sm.set_next IterationStart 
               ] 
               [
                 switch row_index.value 
                   (List.init 140 ~f:(fun i -> 
                        (of_int_trunc ~width:num_bits_index i, [grid_storage.(i) <-- row])
                      ));
                 row_index <-- row_index.value +: (of_int_trunc ~width:num_bits_index 1);
               ]
           ]
          );
          (IterationStart,
           [
             row_index <-- zero num_bits_index;
             stored_prev_row <-- zero num_bits_row;
             past_total_rolls <-- total_rolls.value; 
             sm.set_next ProcessRow
           ]
          );
          (ProcessRow, 
           [
             switch row_index.value 
               (List.init 140 ~f:(fun i -> 
                    (of_int_trunc ~width:num_bits_index i, [ grid_storage.(i) <-- next_row_val ])
                  ));
             stored_prev_row <-- c_row_read;

             total_rolls <-- total_rolls.value +: removal_count;

             row_index <-- row_index.value +: (of_int_trunc ~width:num_bits_index 1);

             if_ (row_index.value >=: (max_rows.value -: (of_int_trunc ~width:num_bits_index 1))) 
               [ sm.set_next IterationCheck ]
               [ sm.set_next ProcessRow ]
           ]
          );
          (IterationCheck,
           [
             if_ (total_rolls.value ==: past_total_rolls.value)
               [ sm.set_next Done ] 
               [ sm.set_next IterationStart ]
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