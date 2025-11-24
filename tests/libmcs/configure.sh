#!/usr/bin/env bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Flags copied to match <https://github.com/UW-HARVEST/Hayroll/blob/8d88b7403650132521e837947667b4cd0999fd1a/prerequisites.bash#L264-L270>.
(cd "${SCRIPT_DIR}/repo" && ./configure \
     --cross-compile="" \
     --compilation-flags="" \
     --disable-denormal-handling \
     --disable-long-double-procedures \
     --disable-complex-procedures \
     --little-endian \
     &> ../$(basename "$0").log
)
