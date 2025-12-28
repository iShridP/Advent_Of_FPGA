#!/usr/bin/env bash
set -e

dune build bin/generate.exe @runtest
dune exec test/math_homework_tb.exe
gtkwave math_homework.vcd
