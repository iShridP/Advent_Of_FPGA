open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let num_bits_pwd = 16
let num_bits_pos = 32
let num_bits_amount = 16
let num_bits_direction = 1

module I = struct
  type 'a t =
    { clock : 'a;
      clear : 'a;
      start : 'a;
      finish : 'a;
      direction : 'a [@bits num_bits_direction];  
      amount : 'a [@bits num_bits_amount]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { pwd : 'a With_valid.t [@bits num_bits_pwd] }
  [@@deriving hardcaml]
end

module States = struct
  type t = Idle | Accept | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; direction; amount } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in

  let sm = State_machine.create (module States) spec in

  let%hw_var pos = Variable.reg spec ~width:num_bits_pos in
  let pwd = Variable.reg spec ~width:num_bits_pwd in
  let pwd_valid = Variable.wire ~default:gnd () in

  let hundred   = of_int_trunc ~width:num_bits_pos 100 in
  let magic_100 = of_int_trunc ~width:num_bits_pos 0x51EB851F in

  let amount_32 = uresize amount ~width:num_bits_pos in
  let prod = amount_32 *: magic_100 in
  let q = select prod ~high:63 ~low:37 in
  let reduced_amount =
    amount_32 -: uresize (q *: hundred) ~width:num_bits_pos
  in

  let moved_pos =
    mux2
      direction
      (pos.value +: reduced_amount)
      (pos.value -: reduced_amount)
  in

  let wrapped_pos =
    mux2
      (moved_pos >=+ hundred)
      (moved_pos -: hundred)
      (mux2
         (moved_pos <+ zero num_bits_pos)
         (moved_pos +: hundred)
         moved_pos)
  in

  compile
    [
      sm.switch
        [
          (Idle,
           [ when_ start
               [
                 pwd_valid <-- gnd;
                 pwd <-- zero num_bits_pwd;
                 pos <-- of_int_trunc ~width:num_bits_pos 50;
                 sm.set_next Accept
               ]
           ] 
          );
          (Accept,
           [ 
             pos <-- wrapped_pos;
             when_ (wrapped_pos ==: zero num_bits_pos) [ pwd <-- pwd.value +: of_int_trunc ~width:num_bits_pwd 1 ];
             when_ finish [ sm.set_next Done ]
           ] 
          );
          (Done,
           [ 
             pwd_valid <-- vdd
           ] 
          );
        ];
    ];

  {pwd = {value = pwd.value; valid = pwd_valid.value}}
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"dial_counter" create
;;
