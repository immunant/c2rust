#!/usr/bin/env bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

export CFLAGS="-Wno-calloc-transposed-args -Wno-implicit-function-declaration"
export LDFLAGS="-pthread -lquadmath -Wl,--no-as-needed -ldl"

(cd "${SCRIPT_DIR}/repo" && ./configure --disable-shared 2>&1 \
     | tee ../`basename "$0"`.log)
