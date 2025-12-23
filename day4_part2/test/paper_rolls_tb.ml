open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

open Paper_rolls.I

module Sim = Cyclesim.With_interface (Paper_rolls.I) (Paper_rolls.O)

let tick sim =
  Cyclesim.cycle sim

let parse_line line =
  let clean_line = String.strip line in
  if String.is_empty clean_line then `Empty
  else 
    let bits_list = 
      String.to_list clean_line 
      |> List.map ~f:(function
          | '@' -> Bits.vdd
          | _   -> Bits.gnd
        )
    in
    `Input (Bits.concat_msb bits_list)

let run sim filename =
  let inputs = Cyclesim.inputs sim in
  In_channel.with_file filename ~f:(fun ic ->
      In_channel.iter_lines ic ~f:(fun line ->
          match parse_line line with
          | `Empty -> ()
          | `Input (row_bits) ->
            inputs.row := row_bits;
            tick sim; 
        )
    );

  inputs.finish := Bits.vdd;
  tick sim;
  tick sim;
  inputs.finish := Bits.gnd

let () =
  let create_for_sim inputs = 
    Paper_rolls.create (Scope.create ~name:"paper_rolls_tb" ()) inputs 
  in
  let sim = Sim.create create_for_sim in

  let vcd_ch = Stdio.Out_channel.create "paper_rolls.vcd" in
  let sim = Hardcaml.Vcd.wrap vcd_ch sim in

  let waves, sim = Waveform.create sim in

  let inputs = Cyclesim.inputs sim in

  inputs.clock := Bits.gnd;
  inputs.start := Bits.gnd;
  inputs.finish := Bits.gnd;
  inputs.row := Bits.zero 140;

  inputs.clear := Bits.vdd;
  tick sim;
  inputs.clear := Bits.gnd;
  tick sim;

  inputs.start := Bits.vdd;
  tick sim;
  inputs.start := Bits.gnd;

  run sim "input.txt";

  for _ = 1 to 40000 do (*a test length of 40000, depends on input but this high value should do*)
    tick sim
  done;  

  Stdio.Out_channel.close vcd_ch;

  Waveform.print ~display_height:20 ~display_width:100 waves
;;