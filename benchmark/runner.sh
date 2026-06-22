#!/usr/bin/env sh

# core_bench timing for the GLML, verbose
dune exec --profile release -- ./bench_runner.exe -quota 5s -v "$@"
