#!/usr/bin/env bash
set -e

dune build bin/generate.exe @runtest
dune exec test/dial_counter_tb.exe
gtkwave dial_counter.vcd