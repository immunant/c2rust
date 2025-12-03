#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
make -C "$SCRIPT_DIR/repo" clean || true
(cd "$SCRIPT_DIR/repo" && ./auto/configure 2>&1 | tee ../`basename "$0"`.log)
