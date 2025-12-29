#!/usr/bin/env bash
set -e

dune build bin/generate.exe @runtest
dune exec test/tachyon_beam_tb.exe
gtkwave tachyon_beam.vcd
