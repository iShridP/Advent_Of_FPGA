#!/usr/bin/env bash
set -e

dune build bin/generate.exe @runtest
dune exec test/junction_box_tb.exe
gtkwave junction_box.vcd
