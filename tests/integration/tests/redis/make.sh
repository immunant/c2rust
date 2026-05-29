#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
make -C "$SCRIPT_DIR/repo" distclean && rm -f compile_commands.json
bear -- make -C "$SCRIPT_DIR/repo" -j`nproc` 2>&1 \
    | tee `basename "$0"`.log
