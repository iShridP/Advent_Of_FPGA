#!/usr/bin/env bash
set -e

dune build bin/generate.exe @runtest
dune exec test/fresh_id_tb.exe
gtkwave fresh_id.vcd
