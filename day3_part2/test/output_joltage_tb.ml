open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

open Output_joltage.I

module Sim = Cyclesim.With_interface (Output_joltage.I) (Output_joltage.O)

let tick sim =
  Cyclesim.cycle sim

let parse_line line =
  match String.strip line with
  | "" -> `Empty
  | "F" -> `Finish
  | s -> 
    let value = Z.of_string s in
    `Input(value)

let run sim filename =
  let inputs = Cyclesim.inputs sim in
  In_channel.with_file filename ~f:(fun ic ->
      In_channel.iter_lines ic ~f:(fun line ->
          match parse_line line with
          | `Empty -> ()
          | `Input (value) ->
            inputs.bank_value := Bits.of_decimal_string ~width:333 (Z.to_string value);
            tick sim;
            tick sim
          | `Finish ->
            inputs.finish := Bits.vdd;
            tick sim;
            inputs.finish := Bits.gnd
        )
    )

let () =
  let create_for_sim inputs = 
    Output_joltage.create (Scope.create ~name:"output_joltage_tb" ()) inputs 
  in
  let sim = Sim.create create_for_sim in

  let vcd_ch = Stdio.Out_channel.create "output_joltage.vcd" in
  let sim = Hardcaml.Vcd.wrap vcd_ch sim in

  let waves, sim = Waveform.create sim in

  let inputs = Cyclesim.inputs sim in

  inputs.clock := Bits.gnd;
  inputs.start := Bits.gnd;
  inputs.finish := Bits.gnd;

  inputs.clear := Bits.vdd;
  tick sim;
  inputs.clear := Bits.gnd;
  tick sim;

  inputs.start := Bits.vdd;
  tick sim;
  inputs.start := Bits.gnd;

  run sim "input.txt";

  for _ = 1 to 5 do
    tick sim
  done;

  Stdio.Out_channel.close vcd_ch;

  Waveform.print ~display_height:20 ~display_width:100 waves
;;