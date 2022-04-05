#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

(cd "${SCRIPT_DIR}"
make -C repo clean && rm -f compile_commands.json)
(cd "${SCRIPT_DIR}"
intercept-build make -C repo -j`nproc`) 2>&1 | tee `basename "$0"`.log
