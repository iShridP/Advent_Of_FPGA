open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

open Invalid_id.I

module Sim = Cyclesim.With_interface (Invalid_id.I) (Invalid_id.O)

let tick sim =
  Cyclesim.cycle sim

let parse_line line =
  match String.strip line with
  | "" -> `Empty
  | s ->
    let ranges = 
      String.split s ~on:',' 
      |> List.map ~f:(fun r ->
          match String.split r ~on:'-' with
          | [l; h] -> (Int64.of_string l, Int64.of_string h)
          | _ -> failwith ("Invalid range format: " ^ r)
        )
    in
    `Ranges ranges

let run sim filename =
  let inputs = Cyclesim.inputs sim in
  In_channel.with_file filename ~f:(fun ic ->
      In_channel.iter_lines ic ~f:(fun line ->
          match parse_line line with
          | `Empty -> ()
          | `Ranges ranges ->
            List.iter ranges ~f:(fun (low, high) ->
                let rec loop current =
                  if Int64.(current > high) then ()
                  else (
                    inputs.value := Bits.of_int64_trunc ~width:64 current;
                    tick sim;
                    loop (Int64.(current + 1L))
                  )
                in
                loop low
              )
        )
    )

let () =
  let create_for_sim inputs = 
    Invalid_id.create (Scope.create ~name:"invalid_id" ()) inputs 
  in
  let sim = Sim.create create_for_sim in

  let vcd_ch = Stdio.Out_channel.create "invalid_id.vcd" in
  let sim = Hardcaml.Vcd.wrap vcd_ch sim in

  let waves, sim = Waveform.create sim in

  let inputs = Cyclesim.inputs sim in

  inputs.clock := Bits.gnd;
  inputs.clear := Bits.vdd;
  inputs.start := Bits.gnd;
  inputs.finish := Bits.gnd;
  inputs.value := Bits.zero 64;

  tick sim;
  inputs.clear := Bits.gnd;
  tick sim;

  inputs.start := Bits.vdd;
  tick sim;
  inputs.start := Bits.gnd;

  run sim "input.txt";

  inputs.finish := Bits.vdd;
  inputs.value := Bits.zero 64; 
  tick sim;
  inputs.finish := Bits.gnd;

  for _ = 1 to 10 do
    tick sim
  done;

  Stdio.Out_channel.close vcd_ch;

  Waveform.print ~display_height:20 ~display_width:100 waves

;;