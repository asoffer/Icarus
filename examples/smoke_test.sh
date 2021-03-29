#!/usr/bin/env bash
set -euo pipefail

bazel build //compiler:interpret

examples=$(realpath $(dirname "$BASH_SOURCE"))
root=$(dirname "$examples")
binary="$root/bazel-bin/compiler/interpret"
export ICARUS_MODULE_PATH="$root/stdlib"

$binary "$examples/enum.ic" >/dev/null && echo enum OK
$binary "$examples/factorial.ic" >/dev/null && echo factorial OK
$binary "$examples/fibonacci.ic" >/dev/null && echo fibonacci OK
# file.ic is broken; errors on full flat_hash_map
$binary "$examples/fizzbuzz.ic" >/dev/null && echo fizzbuzz OK
$binary "$examples/function_calls.ic" >/dev/null && echo function_calls OK
$binary "$examples/ping_pong.ic" | head >/dev/null && echo ping_pong OK
$binary "$examples/primes.ic" >/dev/null && echo primes OK
