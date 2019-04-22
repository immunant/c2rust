#!/usr/bin/env bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
FILE="$SCRIPT_DIR/repo/configure"
if [[ -f "$FILE" ]]; then
    echo "nothing to do; $FILE exists"
else
    (cd repo && ./autogen.sh | tee ../`basename "$0"`.log)
fi

