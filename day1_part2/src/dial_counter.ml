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
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; direction : 'a [@bits num_bits_direction]
    ; amount : 'a [@bits num_bits_amount]
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

let create scope ({ clock; clear; start; finish; direction; amount } : _ I.t)
  : _ O.t
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

  let rem = amount_32 -: uresize (q *: hundred) ~width:num_bits_pos in

  let moved_pos_add = pos.value +: rem in
  let moved_pos_sub = pos.value -: rem in

  let next_pos_add = mux2 (moved_pos_add >=: hundred) (moved_pos_add -: hundred) moved_pos_add in
  let next_pos_sub = mux2 (moved_pos_sub >=: hundred) (moved_pos_sub +: hundred) moved_pos_sub in 
  let next_pos = mux2 direction next_pos_add next_pos_sub in

  let hits_from_rotations = uresize q ~width:num_bits_pwd in
  let hit_right = (moved_pos_add >=: hundred) in
  let hit_left = (rem >=: pos.value) &: (pos.value <>: (zero num_bits_pos)) in

  let partial_hit = mux2 direction hit_right hit_left in

  let total_increment = hits_from_rotations +: uresize partial_hit ~width:num_bits_pwd in

  compile
    [
      sm.switch
        [
          ( Idle,
            [ when_ start
                [ pwd <-- zero num_bits_pwd;
                  pos <-- of_int_trunc ~width:num_bits_pos 50;
                  sm.set_next Accept
                ]
            ] );

          ( Accept,
            [ 
              pos <-- next_pos;
              pwd <-- pwd.value +: total_increment;
              when_ finish [ sm.set_next Done ]
            ] );

          ( Done,
            [ pwd_valid <-- vdd ] );
        ];
    ];

  {pwd = {value = pwd.value; valid = pwd_valid.value}}
;;


let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"dial_counter" create
;;
