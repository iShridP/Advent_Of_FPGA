open! Core
open! Hardcaml
open! Tachyon_beam

let generate_dial_counter_rtl () =
  let module C = Circuit.With_interface (Tachyon_beam.I) (Tachyon_beam.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"dial_counter_top" (Tachyon_beam.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let tachyon_beam_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_dial_counter_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ "tachyon_beam", tachyon_beam_rtl_command ])
;;
