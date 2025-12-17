open! Core
open! Hardcaml
open! Signal
open! Hardcaml.Always

let num_bits_value = 64
let num_bits_sum = 64

module I = struct
  type 'a t = {
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

type magic_params = { m : Signal.t; k : int }
let magic_number i : magic_params = 
  let make_m str = Signal.of_bigint ~width:65 (Bigint.of_string str) in
  match i with
  | 1 -> { m = make_m "29514790517935282586"; k = 68 } (* Div 10 *)
  | 2 -> { m = make_m "23611832414348226069"; k = 71 } (* Div 100 *)
  | 3 -> { m = make_m "18889465931478580855"; k = 74 } (* Div 1000 *)
  | 4 -> { m = make_m "30223145490365729368"; k = 78 } (* Div 10000 *)
  | 5 -> { m = make_m "24178516392292583495"; k = 81 } (* Div 100000 *)
  | 6 -> { m = make_m "19342813113834066796"; k = 84 } (* Div 1000000 *)
  | 7 -> { m = make_m "30948500982134506873"; k = 88 } (* Div 10000000 *)
  | _ -> failwith "invalid power"

let power_of_10 i =
  let make_p str = Signal.of_bigint ~width:32 (Bigint.of_string str) in
  match i with
  | 0 -> make_p "1"
  | 1 -> make_p "10"
  | 2 -> make_p "100"
  | 3 -> make_p "1000"
  | 4 -> make_p "10000"
  | 5 -> make_p "100000"
  | 6 -> make_p "1000000"
  | 7 -> make_p "10000000"
  | _ -> failwith "invalid power"

let check_invalid_id (value : Signal.t) : Signal.t =

  let checks = 
    List.map (List.range 1 8) ~f:(fun i -> 

        let magic_num = magic_number i in
        let power_num = power_of_10 i in
        let prod = magic_num.m *: value in

        let quotient = select prod ~high:(128) ~low:magic_num.k in
        let remainder = value -: uresize(quotient *: power_num) ~width:num_bits_value in

        let upper = uresize quotient ~width:num_bits_value in

        (*Basic check of upper and lower(remainder)*)
        let is_equal = (upper ==: remainder) in

        (*Check upper is > power of 10 i - 1 to avoid numbers like 101 since like 101 at i = 2 becomes 1 and 01 = 1*)
        let min_upper_val = power_of_10 (i - 1) in
        let valid_magnitude = upper >=: (uresize min_upper_val ~width:num_bits_value) in

        is_equal &: valid_magnitude
      )
  in

  reduce ~f:(|:) checks

let create scope ({clock; clear; start; finish; value} : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in

  let sm = State_machine.create (module States) spec in
  let%hw_var sum = Variable.reg spec ~width:num_bits_sum in

  let sum_valid = Variable.wire ~default:gnd () in
  let invalid_id_flag = check_invalid_id value in

  compile
    [
      sm.switch
        [
          (Idle,
           [
             when_ start
               [
                 sum_valid <-- gnd;
                 sum <-- zero num_bits_sum;
                 sm.set_next Check
               ]
           ]
          );
          (Check,
           [
             when_ invalid_id_flag
               [
                 sum <-- (sum.value +: (uresize value ~width:num_bits_sum))
               ];
             when_ finish [sm.set_next Done]
           ]
          );
          (Done,
           [
             sum_valid <-- vdd;
           ]
          );
        ]
    ];

  {sum = {valid = sum_valid.value; value = sum.value }}
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"invalid_id" create
;;
