#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

ulimit -s 16384 # debug build requires this much stack to pass tests
(cd "$SCRIPT_DIR/repo/testes" && \
    cargo run --release -- -e_U=true all.lua 2>&1 ) \
    | tee `basename "$0"`.log
