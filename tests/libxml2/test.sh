#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

(cd "$SCRIPT_DIR/repo" && \
    cargo run --release 2>&1 \
    | tee `basename "$0"`.log)
