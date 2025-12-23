open! Core
open! Hardcaml
open! Paper_rolls

let generate_dial_counter_rtl () =
  let module C = Circuit.With_interface (Paper_rolls.I) (Paper_rolls.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"dial_counter_top" (Paper_rolls.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let paper_rolls_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_dial_counter_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ "paper_rolls", paper_rolls_rtl_command ])
;;
