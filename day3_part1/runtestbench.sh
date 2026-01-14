#!/usr/bin/env bash
set -e

dune build bin/generate.exe @runtest
dune exec test/output_joltage_tb.exe
gtkwave output_joltage.vcd