open! Core 
open! Hardcaml
open! Signal
open! Hardcaml.Always

let num_bits_bound = 50
let num_bits_amount = 32
let num_bits_count = 8
let num_bits_id = num_bits_bound

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    start : 'a;
    finish : 'a;
    id : 'a [@bits num_bits_bound] (*id or bound input*)
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    total_amount : 'a With_valid.t [@bits num_bits_amount]
  }
  [@@deriving hardcaml]
end

module States = struct
  type t = Idle | BoundInputLower | BoundInputUpper | IDInput | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({clock; clear; start; finish; id}: _ I.t): _ O.t =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in

  let sm = State_machine.create(module States) spec in

  let%hw_var inventory_id = Variable.reg spec ~width:num_bits_id in  

  let num_intervals = Variable.reg spec ~width:num_bits_count in

  compile
    [
      sm.switch
        [
          (Idle,
           when_ start  [
             inventory_id <-- zero num_bits_id;
             num_intervals <-- zero num_bits_id;
           ]
          );  
        ]
    ]