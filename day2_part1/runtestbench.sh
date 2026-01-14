#!/usr/bin/env bash
set -e

dune build bin/generate.exe @runtest
dune exec test/invalid_id_tb.exe
gtkwave invalid_id.vcd