#!/usr/bin/env bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

(cd "${SCRIPT_DIR}/repo" && ./configure \
     --cross-compile="" \
     --compilation-flags="" \
     --disable-denormal-handling \
     --disable-long-double-procedures \
     --disable-complex-procedures \
     --little-endian \
     &> ../$(basename "$0").log
)
