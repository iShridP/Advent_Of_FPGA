import os
import sys

def create_file(path, content):
    """Helper to create a file with specific content."""
    with open(path, 'w') as f:
        f.write(content.strip() + '\n')
    print(f"Created: {path}")

def make_directory(path):
    """Helper to create a directory if it doesn't exist."""
    if not os.path.exists(path):
        os.makedirs(path)
        print(f"Created directory: {path}")
    else:
        print(f"Directory exists: {path}")

def main():

    project_name = input("Enter project name: ").strip()
    folder_name = input("Enter folder name: ").strip()

    if not project_name or not folder_name:
        print("Error: name cannot be empty.")
        sys.exit(1)

    caps_project_name = project_name.capitalize()


    # src/dune 
    src_dune_content = f"""
(library
 (name {project_name})
 (libraries core hardcaml)
 (preprocess (pps ppx_jane ppx_hardcaml))
)
"""
    
    # test/dune 
    test_dune_content = f"""
(executable
 (name {project_name}_tb)
 (libraries 
    core 
    hardcaml 
    {project_name} 
    hardcaml_test_harness
    hardcaml_waveterm 
    re)
 (preprocess
  (pps ppx_hardcaml ppx_jane)))
    """

    # bin/dune 
    bin_dune_content = f"""
(executables
 (modes byte exe)
 (names generate)
 (libraries core.command core_unix.command_unix core core_unix hardcaml
   {project_name} jane_rope)
 (preprocess
  (pps ppx_jane))
 (promote))
"""

    # bin/generate.ml (Executable entry point)
    bin_main_content = f"""
open! Core
open! Hardcaml
open! {caps_project_name}

let generate_dial_counter_rtl () =
  let module C = Circuit.With_interface ({caps_project_name}.I) ({caps_project_name}.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"dial_counter_top" ({caps_project_name}.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let {project_name}_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_dial_counter_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ "{project_name}", {project_name}_rtl_command ])
;;

"""
    
    empty_content = f"""empty content"""

    makefile_content = """
INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

default:
	dune build

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	dune clean

.PHONY: default install uninstall reinstall clean

    """

    dune_project_content = """
(lang dune 3.17)
    """

    opam_content = """
opam-version: "2.0"
build: [
  ["dune" "build" "-p" name "-j" jobs]
]
depends: [
  "ocaml" {>= "5.1.0"}
  "dune" {>= "3.17.0"}
]
available: arch != "arm32" & arch != "x86_32"
synopsis: "Advent of FPGA"
description: "
Advent of FPGA
"
    """

    ocamlformat_content = """
profile=janestreet
    """

    bash_script_code = f"""
#!/usr/bin/env bash
set -e

dune build bin/generate.exe @runtest
dune exec test/{project_name}_tb.exe
gtkwave {project_name}.vcd

"""


    #Create Structure
    base_dir = os.path.join(os.getcwd(), folder_name)
    make_directory(base_dir)

    #create Subdirectories
    make_directory(os.path.join(base_dir, "src"))
    make_directory(os.path.join(base_dir, "bin"))
    make_directory(os.path.join(base_dir, "test"))
    make_directory(os.path.join(base_dir, "images"))

    #src/files
    create_file(os.path.join(base_dir, "src", "dune"), src_dune_content)
    create_file(os.path.join(base_dir, "src", f"{project_name}.ml"), empty_content)

    #bin/files
    create_file(os.path.join(base_dir, "bin", "dune"), bin_dune_content)
    create_file(os.path.join(base_dir, "bin", "generate.ml"), bin_main_content)

    #test/files
    create_file(os.path.join(base_dir, "test", "dune"), test_dune_content)
    create_file(os.path.join(base_dir, "test", f"{project_name}_tb.ml"), empty_content)

    #other
    create_file(os.path.join(base_dir, "Makefile"), makefile_content)
    create_file(os.path.join(base_dir, "input.txt"), empty_content)
    create_file(os.path.join(base_dir, "dune-project"), dune_project_content)
    create_file(os.path.join(base_dir, f"{project_name}.opam"), opam_content)
    create_file(os.path.join(base_dir, ".ocamlformat"), ocamlformat_content)
    create_file(os.path.join(base_dir, "runtestbench.sh"), bash_script_code)

    print(f"\nBasic project files of '{project_name}' created.")

if __name__ == "__main__":
    main()