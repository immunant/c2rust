#!/bin/bash
set -e; set -o pipefail

c2rust transpile \
    --overwrite-existing \
    --output-dir repo \
    --emit-build-files compile_commands.json -- -w \
    | tee `basename "$0"`.log

# SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
# cp $SCRIPT_DIR/build.rs $SCRIPT_DIR/repo