#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
rm -f compile_commands.json
intercept-build make -C "$SCRIPT_DIR/repo" -j`nproc` \
    CFLAGS="-std=c99 -O0" 2>&1 \
    | tee `basename "$0"`.log
