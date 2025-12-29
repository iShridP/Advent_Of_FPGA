open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

open Tachyon_beam.I

module Sim = Cyclesim.With_interface (Tachyon_beam.I) (Tachyon_beam.O)

let tick sim =
  Cyclesim.cycle sim

let drive_row sim line =
  let inputs = Cyclesim.inputs sim in
  let clean_line = String.strip line in
  if not (String.is_empty clean_line) then (
    String.iter clean_line ~f:(fun char ->
        let val_to_drive = match char with
          | '.' -> 0
          | '^' -> 1
          | 'S' -> 2
          | _   -> 0
        in
        inputs.manifold_item := Bits.of_int_trunc ~width:2 val_to_drive;
        inputs.finish_row := Bits.gnd;
        tick sim
      );
    (* Pulse finish_row at end of line *)
    inputs.finish_row := Bits.vdd;
    tick sim;
    inputs.finish_row := Bits.gnd
  )

let run sim filename =
  let inputs = Cyclesim.inputs sim in
  In_channel.with_file filename ~f:(fun ic ->
      In_channel.iter_lines ic ~f:(fun line ->
          drive_row sim line
        )
    );

  inputs.finish := Bits.vdd;
  tick sim;
  tick sim;
  inputs.finish := Bits.gnd

let () =
  let create_for_sim inputs = 
    Tachyon_beam.create (Scope.create ~name:"tachyon_beam_tb" ()) inputs 
  in
  let sim = Sim.create create_for_sim in

  let vcd_ch = Stdio.Out_channel.create "tachyon_beam.vcd" in
  let sim = Hardcaml.Vcd.wrap vcd_ch sim in

  let waves, sim = Waveform.create sim in

  let inputs = Cyclesim.inputs sim in

  inputs.clock := Bits.gnd;
  inputs.start := Bits.gnd;
  inputs.finish := Bits.gnd;
  inputs.manifold_item := Bits.zero 2;
  inputs.finish_row := Bits.gnd;

  inputs.clear := Bits.vdd;
  tick sim;
  inputs.clear := Bits.gnd;
  tick sim;

  inputs.start := Bits.vdd;
  tick sim;
  inputs.start := Bits.gnd;

  run sim "input.txt";

  for _ = 1 to 40000 do (*some sample time*)
    tick sim
  done;  

  Stdio.Out_channel.close vcd_ch;

  Waveform.print ~display_height:20 ~display_width:100 waves
;;