open! Core
open! Hardcaml
open! Math_homework

let generate_dial_counter_rtl () =
  let module C = Circuit.With_interface (Math_homework.I) (Math_homework.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"dial_counter_top" (Math_homework.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let math_homework_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_dial_counter_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ "math_homework", math_homework_rtl_command ])
;;
