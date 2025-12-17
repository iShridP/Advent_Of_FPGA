open! Core
open! Hardcaml
open! Invalid_id

let generate_invalid_id_rtl () =
  let module C = Circuit.With_interface (Invalid_id.I) (Invalid_id.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"dial_counter_top" (Invalid_id.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let invalid_id_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_invalid_id_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ "invalid-id", invalid_id_rtl_command ])
;;
