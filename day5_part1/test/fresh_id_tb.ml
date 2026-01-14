open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

open Fresh_id.I

module Sim = Cyclesim.With_interface (Fresh_id.I) (Fresh_id.O)

let tick sim =
  Cyclesim.cycle sim

(* Robust Parser: Prints what it finds *)
let parse_line line_idx line =
  let clean_line = String.strip line in
  if String.is_empty clean_line then begin
    Stdio.printf "Line %d: Detected BLANK -> Switching Mode\n" line_idx;
    `Switch
  end
  else if String.contains clean_line '-' then begin
    let parts = String.split ~on:'-' clean_line in
    let low = Int.of_string (List.nth_exn parts 0) in
    let high = Int.of_string (List.nth_exn parts 1) in
    (* Debug Print *)
    (* Stdio.printf "Line %d: Range [%d - %d]\n" line_idx low high; *) 
    `Range (low, high)
  end
  else begin
    let n = Int.of_string clean_line in
    `Id n
  end

let run sim filename =
  let inputs = Cyclesim.inputs sim in

  (* Reset Sequence *)
  inputs.start := Bits.vdd;
  tick sim;
  inputs.start := Bits.gnd;
  tick sim;

  let line_idx = ref 0 in
  let has_switched = ref false in

  In_channel.with_file filename ~f:(fun ic ->
      In_channel.iter_lines ic ~f:(fun line ->
          incr line_idx;
          let parsed = parse_line !line_idx line in

          match parsed with
          | `Range (l, h) ->
            inputs.id := Bits.of_int_trunc ~width:64 l;
            tick sim;
            inputs.id := Bits.of_int_trunc ~width:64 h;
            tick sim

          | `Switch ->
            has_switched := true;
            inputs.finish_setup := Bits.vdd;
            tick sim;
            inputs.finish_setup := Bits.gnd;


            tick sim;
            Stdio.printf "Hardware should now be in IDInput state.\n%!"

          | `Id n ->
            if not !has_switched then begin
              Stdio.printf "WARNING: Implicit Switch triggered at line %d (First ID found)\n%!" !line_idx;
              inputs.finish_setup := Bits.vdd;
              tick sim;
              inputs.finish_setup := Bits.gnd;
              tick sim;
              has_switched := true
            end;

            inputs.id := Bits.of_int_trunc ~width:64 n;
            tick sim;
        )
    );

  Stdio.printf "Finished file. Sending finish_all.\n%!";
  inputs.finish_all := Bits.vdd;
  tick sim;
  inputs.finish_all := Bits.gnd;
  tick sim

let () =
  let create_for_sim inputs = 
    Fresh_id.create (Scope.create ~name:"fresh_id_tb" ()) inputs 
  in
  let sim = Sim.create create_for_sim in
  let outputs = Cyclesim.outputs sim in

  let vcd_ch = Stdio.Out_channel.create "fresh_id.vcd" in
  let sim = Hardcaml.Vcd.wrap vcd_ch sim in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in

  inputs.clock := Bits.gnd;
  inputs.start := Bits.gnd;
  inputs.finish_setup := Bits.gnd;
  inputs.finish_all := Bits.gnd;
  inputs.id := Bits.zero 64;
  inputs.clear := Bits.vdd;
  tick sim;
  inputs.clear := Bits.gnd;
  tick sim;

  run sim "input.txt";

  for _ = 1 to 10 do tick sim done;  

  Stdio.Out_channel.close vcd_ch;

  let final_count = Bits.to_int_trunc !(outputs.total_fresh_count) in
  let overflow = Bits.to_int_trunc !(outputs.overflow_error) in

  Stdio.printf "\n--------------------------------\n";
  Stdio.printf "Final Fresh Count: %d\n" final_count;
  Stdio.printf "Overflow Error:    %d (1 = Bad)\n" overflow;
  Stdio.printf "--------------------------------\n";

  Waveform.print ~display_height:20 ~display_width:100 waves 
;;