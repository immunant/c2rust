#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

# TODO: run all of `test-fullbench`, compare output to native
cd "$SCRIPT_DIR/repo" && cargo run --  -i1 2>&1 | tee ../`basename "$0"`.log
