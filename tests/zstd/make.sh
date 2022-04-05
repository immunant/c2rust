#!/bin/bash
set -e; set -o pipefail

# TARGET=zstd
TARGET=fullbench

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
make -C "$SCRIPT_DIR/repo" clean && rm -f compile_commands.json
intercept-build make -C "$SCRIPT_DIR/repo/tests" -j`nproc` $TARGET 2>&1 \
    | tee `basename "$0"`.log