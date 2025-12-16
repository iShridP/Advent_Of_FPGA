open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

open Dial_counter.I

module Sim = Cyclesim.With_interface (Dial_counter.I) (Dial_counter.O)

let tick sim =
  Cyclesim.cycle sim

let parse_line line =
  match String.strip line with
  | "" -> `Empty
  | "F" -> `Finish
  | s ->
    let dir =
      match s.[0] with
      | 'L' -> 0
      | 'R' -> 1
      | _ -> failwith "Invalid direction"
    in
    let amt =
      int_of_string (String.sub s ~pos:1 ~len:(String.length s - 1))
    in
    `Move (dir, amt)

let run sim filename =
  let inputs = Cyclesim.inputs sim in
  In_channel.with_file filename ~f:(fun ic ->
      In_channel.iter_lines ic ~f:(fun line ->
          match parse_line line with
          | `Empty -> ()
          | `Move (dir, amt) ->
            inputs.direction := Bits.of_int_trunc ~width:1 dir;
            inputs.amount := Bits.of_int_trunc ~width:16 amt;
            inputs.finish := Bits.gnd;
            tick sim
          | `Finish ->
            inputs.finish := Bits.vdd;
            tick sim;
            inputs.finish := Bits.gnd
        )
    )

let () =
  let create_for_sim inputs = 
    Dial_counter.create (Scope.create ~name:"dial_counter_tb" ()) inputs 
  in
  let sim = Sim.create create_for_sim in

  let vcd_ch = Stdio.Out_channel.create "dial_counter.vcd" in
  let sim = Hardcaml.Vcd.wrap vcd_ch sim in

  let waves, sim = Waveform.create sim in

  let inputs = Cyclesim.inputs sim in

  inputs.clock := Bits.gnd;
  inputs.direction := Bits.gnd;
  inputs.amount := Bits.zero 16;
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