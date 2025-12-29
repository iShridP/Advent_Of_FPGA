open! Core
open! Hardcaml
open! Fresh_id

let generate_dial_counter_rtl () =
  let module C = Circuit.With_interface (Fresh_id.I) (Fresh_id.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"dial_counter_top" (Fresh_id.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let fresh_id_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_dial_counter_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ "fresh_id", fresh_id_rtl_command ])
;;
