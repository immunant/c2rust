#!/usr/bin/env bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
FILE="$SCRIPT_DIR/repo/configure"
if [[ -f "$FILE" ]]; then
    echo "nothing to do; $FILE exists"
else
    (cd "$SCRIPT_DIR/repo" && ./autogen.sh 2>&1 | tee ../`basename "$0"`.log)
fi

