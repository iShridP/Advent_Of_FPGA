open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let num_bits_value = 64
let num_bits_sum = 128

module I = struct
  type a' t = 
    {
      clock : 'a;
      clear : 'a;
      start : 'a;
      finish : 'a;
      value : 'a [@bits num_bits_value]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    sum : 'a With_valid.t [@bits num_bits_sum]
  }
  [@@deriving hardcaml]
end

module States = struct
  type t = Idle | Check | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({clock, clear, start; finish; value} : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in

  let sm = State_machine.create (module States) spec in

  let sum = Variable.reg spec ~width:num_bits_sum in

  compile
    [
      sm.switch
        [
          (Idle,
           [when_ start
              [
                sum <-- zero num_bits_sum;
                sm.set_next Check
              ]
           ]
          );
          (Check,
           [

           ]
          );
          (Done,
           [

           ]
          );
        ]
    ]