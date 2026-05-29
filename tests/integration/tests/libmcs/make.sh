#!/usr/bin/env bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

make -C "$SCRIPT_DIR/repo" clean
rm -f compile_commands.json
bear -- make -C "$SCRIPT_DIR/repo" -j$(nproc) &> $(basename "$0").log
