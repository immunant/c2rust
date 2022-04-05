#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

(cd "$SCRIPT_DIR"
make -C "$SCRIPT_DIR/repo" clean && rm -f compile_commands.json
intercept-build make -C repo -j`nproc` miniruby) 2>&1 | tee `basename "$0"`.log
