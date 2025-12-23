#!/usr/bin/env bash
set -e

dune build bin/generate.exe @runtest
dune exec test/paper_rolls_tb.exe
gtkwave paper_rolls.vcd