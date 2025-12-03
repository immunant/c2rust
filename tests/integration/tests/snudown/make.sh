#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
(cd "$SCRIPT_DIR/repo" && python2 setup.py clean --all && rm -f compile_commands.json)
(cd "$SCRIPT_DIR/repo" && intercept-build python2 setup.py build 2>&1 \
         | tee ../`basename "$0"`.log)
mv "$SCRIPT_DIR/repo/compile_commands.json" "$SCRIPT_DIR/"
