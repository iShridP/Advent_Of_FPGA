open! Core 
open! Hardcaml
open! Signal
open! Hardcaml.Always

let num_bits_bank_value = 333 (*minimum bits to store bank input number of length 100 {100log2(10)}*)
let num_bits_digit = 8
let num_bits_output_joltage = 50

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

type max_right_return_type = {value : Signal.t; masked_list: Signal.t list}

let max_right_of_global_max (max: Signal.t) (digits_list: Signal.t list): max_right_return_type = 
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

    {value = tree ~arity:2 ~f:(fun args -> List.reduce_exn args ~f:compare_values) masked_list; masked_list = masked_list} (*find and return max value of masked list*)

let get_bank_joltage(digits : Signal.t list) : Signal.t =
  let twelve_place = tree ~arity:2 ~f:(fun args -> List.reduce_exn args ~f:compare_values) (List.take digits (List.length digits - 11)) in (*ignore least significant 11 digit from bank*) 
  let eleven_place = max_right_of_global_max twelve_place (List.take digits (List.length digits - 10)) in
  let ten_place = max_right_of_global_max eleven_place.value (List.take (eleven_place.masked_list @ (List.drop digits (List.length digits - 10))) (List.length digits - 9)) in
  let nine_place = max_right_of_global_max ten_place.value (List.take (ten_place.masked_list @ (List.drop digits (List.length digits - 9))) (List.length digits - 8)) in
  let eight_place = max_right_of_global_max nine_place.value (List.take (nine_place.masked_list @ (List.drop digits (List.length digits - 8))) (List.length digits - 7)) in
  let seven_place = max_right_of_global_max eight_place.value (List.take (eight_place.masked_list @ (List.drop digits (List.length digits - 7))) (List.length digits - 6)) in
  let six_place = max_right_of_global_max seven_place.value (List.take (seven_place.masked_list @ (List.drop digits (List.length digits - 6))) (List.length digits - 5)) in
  let five_place = max_right_of_global_max six_place.value (List.take (six_place.masked_list @ (List.drop digits (List.length digits - 5))) (List.length digits - 4)) in
  let four_place = max_right_of_global_max five_place.value (List.take (five_place.masked_list @ (List.drop digits (List.length digits - 4))) (List.length digits - 3)) in
  let three_place = max_right_of_global_max four_place.value (List.take (four_place.masked_list @ (List.drop digits (List.length digits - 3))) (List.length digits - 2)) in
  let two_place = max_right_of_global_max three_place.value (List.take (three_place.masked_list @ (List.drop digits (List.length digits - 2))) (List.length digits - 1)) in
  let one_place = max_right_of_global_max two_place.value (List.take (two_place.masked_list @ (List.drop digits (List.length digits - 1))) (List.length digits)) in


  (*add all face values, width of 10^k is taken such that it is most efficent with little overhead*)
  uresize(twelve_place*:(of_int_trunc ~width:38 (Int.pow 10 11))) ~width:num_bits_output_joltage
  +: uresize(eleven_place.value*:(of_int_trunc ~width:35 (Int.pow 10 10))) ~width:num_bits_output_joltage
  +: uresize(ten_place.value*:(of_int_trunc ~width:31 (Int.pow 10 9))) ~width:num_bits_output_joltage
  +: uresize(nine_place.value*:(of_int_trunc ~width:28 (Int.pow 10 8))) ~width:num_bits_output_joltage
  +: uresize(eight_place.value*:(of_int_trunc ~width:25 (Int.pow 10 7))) ~width:num_bits_output_joltage
  +: uresize(seven_place.value*:(of_int_trunc ~width:21 (Int.pow 10 6))) ~width:num_bits_output_joltage
  +: uresize(six_place.value*:(of_int_trunc ~width:18 (Int.pow 10 5))) ~width:num_bits_output_joltage
  +: uresize(five_place.value*:(of_int_trunc ~width:15 (Int.pow 10 4))) ~width:num_bits_output_joltage
  +: uresize(four_place.value*:(of_int_trunc ~width:11 (Int.pow 10 3))) ~width:num_bits_output_joltage
  +: uresize(three_place.value*:(of_int_trunc ~width:8 (Int.pow 10 2))) ~width:num_bits_output_joltage
  +: uresize(two_place.value*:(of_int_trunc ~width:5 (Int.pow 10 1))) ~width:num_bits_output_joltage
  +: uresize(one_place.value*:(of_int_trunc ~width:2 (Int.pow 10 0))) ~width:num_bits_output_joltage


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