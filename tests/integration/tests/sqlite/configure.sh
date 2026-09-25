#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/repo"

# Compile separate translation units to exercise cross-file translation.
./configure --disable-amalgamation --disable-shared --disable-readline \
    --disable-tcl --enable-rtree 2>&1 | tee "$SCRIPT_DIR/$(basename "$0").log"
