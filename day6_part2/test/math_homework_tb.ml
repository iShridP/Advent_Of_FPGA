open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

open Math_homework.I

module Sim = Cyclesim.With_interface (Math_homework.I) (Math_homework.O)

let tick sim =
  Cyclesim.cycle sim

let parse_line line =
  let clean_line = String.strip line in
  if String.is_empty clean_line then `Empty
  else 
    let numbers = 
      String.split ~on:' ' clean_line
      |> List.filter ~f:(fun s -> not (String.is_empty s)) 
      |> List.map ~f:(function
          | "+" -> 0
          | "*" -> 1
          | s -> Int.of_string s
        )
    in
    `Row numbers

let run sim filename =
  let inputs = Cyclesim.inputs sim in
  In_channel.with_file filename ~f:(fun ic ->
      In_channel.iter_lines ic ~f:(fun line ->
          match parse_line line with
          | `Empty -> ()
          | `Row numbers ->
            List.iter numbers ~f:(fun n ->
                inputs.write_en := Bits.vdd;
                inputs.data_in := Bits.of_int_trunc ~width:16 n;
                tick sim
              );
            inputs.write_en := Bits.gnd;
            inputs.finish := Bits.vdd;
            tick sim;
            inputs.finish := Bits.gnd;
            tick sim
        )
    )

let () =
  let create_for_sim inputs = 
    Math_homework.create (Scope.create ~name:"math_homework_tb" ()) inputs 
  in
  let sim = Sim.create create_for_sim in

  let vcd_ch = Stdio.Out_channel.create "math_homework.vcd" in
  let sim = Hardcaml.Vcd.wrap vcd_ch sim in

  let waves, sim = Waveform.create sim in

  let inputs = Cyclesim.inputs sim in

  inputs.clock := Bits.gnd;
  inputs.start := Bits.gnd;
  inputs.finish := Bits.gnd;
  inputs.write_en := Bits.gnd;
  inputs.data_in := Bits.zero 16;

  inputs.clear := Bits.vdd;
  tick sim;
  inputs.clear := Bits.gnd;
  tick sim;

  run sim "input.txt";

  inputs.start := Bits.vdd;
  tick sim;
  inputs.start := Bits.gnd;

  for _ = 1 to 2000 do  (*some sample length*)
    tick sim
  done;  

  Stdio.Out_channel.close vcd_ch;

  Waveform.print ~display_height:20 ~display_width:100 waves
;;