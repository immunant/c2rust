#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
make -C "$SCRIPT_DIR/repo" clean && rm -f compile_commands.json
intercept-build make -C "$SCRIPT_DIR/repo" -j`nproc` \
    MYCFLAGS="-std=c99 -DLUA_USE_LINUX -DLUA_USE_READLINE -DLUA_USE_JUMPTABLE=0" \
    | tee `basename "$0"`.log