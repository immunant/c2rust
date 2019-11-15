#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

cp "$SCRIPT_DIR/repo/target/release/libsnudown.so" "$SCRIPT_DIR/repo/snudown.so"
python2 "$SCRIPT_DIR/repo/test_snudown.py"
