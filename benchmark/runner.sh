#!/usr/bin/env sh

export BENCHMARKS_RUNNER=TRUE
export BENCH_LIB=./bench

dune exec --profile release -- ./bench_runner.exe -run-without-cross-library-inlining "$@" -quota 5s
