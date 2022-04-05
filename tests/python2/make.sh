#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

(cd "$SCRIPT_DIR"
make -C repo clean && rm -f compile_commands.json 
intercept-build make -C repo -j`nproc` python) 2>&1 | tee `basename "$0"`.log
# make rest to properly run python
(cd "$SCRIPT_DIR"
make -C repo -j`nproc`) 2>&1 | tee -a `basename "$0"`.log
