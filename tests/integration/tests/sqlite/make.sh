#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

{
    make -C repo clean
    rm -f compile_commands.json
    # Generate the parser, opcodes and headers before recording compilation,
    # so host tools such as lemon are not included in the translated library.
    make -C repo -j"$(nproc)" .target_source
    bear -- make -C repo -j"$(nproc)" libsqlite3.a
} 2>&1 | tee "$SCRIPT_DIR/$(basename "$0").log"
