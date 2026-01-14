open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let max_intervals = 256 
let num_bits_bound = 64 
let num_bits_amount = 32
let num_bits_id = num_bits_bound

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    start : 'a;
    finish_setup : 'a;   
    finish_all : 'a;    
    id : 'a [@bits num_bits_bound]; 
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    total_fresh_count : 'a [@bits num_bits_amount];
    result_valid : 'a;
    overflow_error : 'a; (* Debug flag: Lights up if we run out of slots *)
  }
  [@@deriving hardcaml]
end

module States = struct
  type t = Idle | BoundInputLower | BoundInputUpper | IDInput | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

(* Hardware Mux Helpers *)
let min a b = mux2 (a <=: b) a b
let max a b = mux2 (a >=: b) a b

let create scope ({clock; clear; start; finish_setup; finish_all; id}: _ I.t): _ O.t =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in

  let sm = State_machine.create (module States) spec in

  let%hw_var slots_valid = Variable.reg spec ~width:max_intervals in
  let slots_lower = Array.init max_intervals ~f:(fun _ -> Variable.reg spec ~width:num_bits_bound) in
  let slots_upper = Array.init max_intervals ~f:(fun _ -> Variable.reg spec ~width:num_bits_bound) in

  let%hw_var temp_lower = Variable.reg spec ~width:num_bits_bound in
  let%hw_var fresh_count = Variable.reg spec ~width:num_bits_amount in
  let%hw_var result_ready = Variable.reg spec ~width:1 in
  let%hw_var overflow_occurred = Variable.reg spec ~width:1 in

  let is_overlap i input_L input_R = 
    let slot_L = (Array.get slots_lower i).value in
    let slot_R = (Array.get slots_upper i).value in
    let is_valid = slots_valid.value.:(i) in
    is_valid &: (slot_L <=: input_R) &: (input_L <=: slot_R)
  in

  let first_empty_index = 
    priority_select (
      List.init max_intervals ~f:(fun i -> 
          { With_valid.valid = ~: (slots_valid.value.:(i)); value = (of_int_trunc ~width:(num_bits_to_represent max_intervals) i) }
        )
    )
  in

  compile [
    sm.switch [

      States.Idle, [
        when_ start [
          slots_valid <-- zero max_intervals;
          fresh_count <-- zero num_bits_amount;
          result_ready <-- gnd;
          overflow_occurred <-- gnd;
          sm.set_next States.BoundInputLower;
        ]
      ];

      States.BoundInputLower, [
        if_ finish_setup [
          sm.set_next States.IDInput;
        ] [
          temp_lower <-- id;
          sm.set_next States.BoundInputUpper;
        ]
      ];

      States.BoundInputUpper, [
        if_ (reduce ~f:(|:) (List.init max_intervals ~f:(fun i -> is_overlap i temp_lower.value id)))
          (List.init max_intervals ~f:(fun i -> 
               when_ (is_overlap i temp_lower.value id) [
                 (Array.get slots_lower i) <-- min (Array.get slots_lower i).value temp_lower.value;
                 (Array.get slots_upper i) <-- max (Array.get slots_upper i).value id;
               ]
             ))
          [
            if_ first_empty_index.valid [
              let idx = first_empty_index.value in
              switch idx (
                List.init max_intervals ~f:(fun i -> 
                    (of_int_trunc ~width:(width idx) i), [
                      (Array.get slots_lower i) <-- temp_lower.value;
                      (Array.get slots_upper i) <-- id;
                      let raw_mask = binary_to_onehot (of_int_trunc ~width:(width idx) i) in
                      let mask = uresize raw_mask ~width:max_intervals in

                      slots_valid <-- (slots_valid.value |: mask); 
                    ]
                  )
              )
            ] [
              overflow_occurred <-- vdd;
            ]
          ];

        sm.set_next States.BoundInputLower;
      ];

      States.IDInput, [
        if_ finish_all [
          sm.set_next States.Done;
        ] [
          (*parallel stuff: Compare ID against all 256 slots at once*)
          let is_fresh = reduce ~f:(|:) (
              List.init max_intervals ~f:(fun i -> 
                  let l = (Array.get slots_lower i).value in
                  let r = (Array.get slots_upper i).value in
                  let v = slots_valid.value.:(i) in
                  v &: (id >=: l) &: (id <=: r)
                )
            ) in

          when_ is_fresh [
            fresh_count <-- fresh_count.value +:. 1
          ]
        ]
      ];

      States.Done, [
        result_ready <-- vdd;
      ]
    ]
  ];

  { 
    total_fresh_count = fresh_count.value;
    result_valid = result_ready.value;
    overflow_error = overflow_occurred.value;
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"fresh_id" create
;;