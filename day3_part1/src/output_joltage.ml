open! Core 
open! Hardcaml
open! Signal
open! Hardcaml.Always

let num_bits_bank_value = 333 (*minimum bits to store bank input number of length 100 {100log2(10)}*)
let num_bits_digit = 8
let num_bits_output_joltage = 32

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    start : 'a;
    finish : 'a;
    bank_value : 'a [@bits num_bits_bank_value]
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    output_joltage : 'a With_valid.t [@bits num_bits_output_joltage]
  }
  [@@deriving hardcaml]
end

module States = struct
  type t = Idle | Accept | Calculate | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end 

let position_check (digit : Signal.t) : Signal.t =
  mux2 
    (digit >=: (of_int_trunc ~width:4 5))
    (digit +: (of_int_trunc ~width:4 3))
    digit

let split_into_bcd_pos (x : Signal.t) : Signal.t list =
  let num_bcd_digits = 102 in (*extra headroom*)

  let padded = uresize x ~width:(num_bcd_digits * 4) in

  (*select 4-bit positions (BCD)*)
  List.init num_bcd_digits ~f:(fun i -> 
      select padded ~high:((i * 4) + 3) ~low:(i * 4)
    )

let binary_to_bcd_list (binary_input : Signal.t) : Signal.t list =
  let input_width = width binary_input in

  let bcd_width = 408 in 

  (*initial values taken*)
  let init_bcd = zero bcd_width in
  let init_bin = binary_input in

  let final_bcd, _ = 
    List.init input_width ~f:(fun _ -> ()) 
    |> List.fold ~init:(init_bcd, init_bin) ~f:(fun (current_bcd, current_bin) _ ->

        let bcd_list = split_into_bcd_pos current_bcd in
        let corrected_bcd_val = Signal.concat_msb (List.rev (List.map bcd_list ~f:position_check)) in 

        (*shift the whole combined vector [BCD][BIN] left by 1*)
        let combined = corrected_bcd_val @: current_bin in
        let shifted = sll combined ~by:1 in

        (*split back*)
        let next_bcd = select shifted ~high:(width shifted - 1) ~low:(width current_bin) in
        let next_bin = select shifted ~high:(width current_bin - 1) ~low:0 in

        (next_bcd, next_bin)
      ) 
  in

  (*return list of the digits*)
  split_into_bcd_pos final_bcd

let compare_values a b =
  mux2
    (a >: b)
    a
    b

let max_right_of_global_max (max: Signal.t) (digits_list: Signal.t list) : Signal.t = 
  match digits_list with _ -> 
    let rec mask_elements curr_digit_list aldready_seen_max_flag =
      match curr_digit_list with 
      | [] -> []
      | x::rest ->
        let is_max_flag = x ==: max in
        (*if max not seen then replace with zero, else keep as it*)
        let masked_val = 
          mux2
            aldready_seen_max_flag
            x
            (zero 4)
        in

        let next_seen_max_flag = aldready_seen_max_flag |: is_max_flag in (*update the seen flag*)

        masked_val :: mask_elements rest next_seen_max_flag
    in

    let masked_list = mask_elements digits_list gnd in (*start with aldready seen max flag as gnd*)

    tree ~arity:2 ~f:(fun args -> List.reduce_exn args ~f:compare_values) masked_list (*find and return max value of masked list*)

let get_bank_joltage(digits : Signal.t list) : Signal.t =
  let tens_place = tree ~arity:2 ~f:(fun args -> List.reduce_exn args ~f:compare_values) (List.take digits (List.length digits - 1)) in (*ignore least significant digit from bank*) 
  let ones_place = max_right_of_global_max tens_place digits in

  uresize(tens_place*:(of_int_trunc ~width:4 10) +: uresize(ones_place) ~width:8) ~width:num_bits_output_joltage


let create scope ({clock; clear; start; finish; bank_value}: _ I.t) : _ O.t = 
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in

  let sm = State_machine.create(module States) spec in 
  let%hw_var output_joltage = Variable.reg spec ~width:num_bits_output_joltage in 

  let output_joltage_valid = Variable.wire ~default:gnd () in

  let current_bank_value = Variable.reg spec ~width:num_bits_bank_value in

  let digits = List.rev(binary_to_bcd_list current_bank_value.value) in

  let bank_joltage = get_bank_joltage digits in

  compile
    [
      sm.switch
        [
          (Idle,
           [
             when_ start [
               output_joltage_valid <-- gnd;
               output_joltage <-- zero num_bits_output_joltage;
               sm.set_next Accept
             ]
           ]
          );
          (Accept, 
           [
             current_bank_value <-- bank_value;
             if_ (finish) [sm.set_next Done] [sm.set_next Calculate]
           ]
          );
          (Calculate,
           [
             output_joltage <-- (output_joltage.value +: (uresize bank_joltage ~width:num_bits_output_joltage));
             sm.set_next Accept
           ]
          );
          (Done, 
           [
             output_joltage_valid <-- vdd;
           ]
          );
        ]
    ];

  {output_joltage = {valid = output_joltage_valid.value; value = output_joltage.value}}
;;


let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"output_joltage" create
;;