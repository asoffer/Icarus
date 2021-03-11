#!/usr/bin/env bash
set -e

bazel build //compiler:interpret

examples=$(realpath $(dirname "$BASH_SOURCE"))
root=$(dirname "$examples")
binary="$root/bazel-bin/compiler/interpret"
export ICARUS_MODULE_PATH="$root/stdlib"

# enum.ic is broken; parse error on switch
$binary "$examples/factorial.ic" >/dev/null && echo OK
$binary "$examples/fibonacci.ic" >/dev/null && echo OK
# file.ic is broken; errors on full flat_hash_map
$binary "$examples/fizzbuzz.ic" >/dev/null && echo OK
# function_calls.ic is broken; segfaults during type verification
$binary "$examples/ping_pong.ic" | head >/dev/null && echo OK
$binary "$examples/primes.ic" >/dev/null && echo OK

