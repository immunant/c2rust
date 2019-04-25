#!/bin/bash
set -e; set -o pipefail

EXTRA_COMPILER_FLAGS=${EXTRA_COMPILER_FLAGS:-}
EXTRA_TRANSPILER_FLAGS=${EXTRA_TRANSPILER_FLAGS:-}

RUST_BACKTRACE=1 c2rust transpile \
    --output-dir repo --overwrite-existing --main ruby \
    ${EXTRA_TRANSPILER_FLAGS} \
    compile_commands.json \
    -- -w ${EXTRA_COMPILER_FLAGS} \
     2>&1 | tee `basename "$0"`.log

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
if [[ -f "$SCRIPT_DIR/build.rs" ]]; then
    cp "$SCRIPT_DIR/build.rs" "$SCRIPT_DIR/repo"
fi
