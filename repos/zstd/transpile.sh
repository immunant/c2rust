#!/bin/sh

set -e

# TARGET=zstdcli
TARGET=fullbench

c2rust transpile \
    --overwrite-existing \
    --output-dir repo \
    -m $TARGET compile_commands.json \
    -- -Qunused-arguments \
    | tee `basename "$0"`.log

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
cp $SCRIPT_DIR/build.rs $SCRIPT_DIR/repo