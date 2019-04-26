#!/bin/bash
set -e; set -o pipefail

RUST_BACKTRACE=1 c2rust transpile \
    --output-dir repo --main ruby \
    ${EXTRA_TFLAGS:---overwrite-existing} \
    compile_commands.json \
    -- ${EXTRA_CFLAGS:--w} \
     2>&1 | tee `basename "$0"`.log

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
if [[ -f "$SCRIPT_DIR/build.rs" ]]; then
    cp "$SCRIPT_DIR/build.rs" "$SCRIPT_DIR/repo"
fi
