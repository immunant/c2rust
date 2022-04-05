#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

TARGET_BIN="${CARGO_TARGET_DIR:-$SCRIPT_DIR/repo/target}/release/librepo.a"
if [ ! -f ${TARGET_BIN} ]; then
    echo "Rust archive not found: $TARGET_BIN"; exit 1
fi

cp ${TARGET_BIN} "$SCRIPT_DIR/repo/.libs/libjson-c.a"

# determine libs needed to use the static lib we built
LIBS=$(cd ${SCRIPT_DIR}/repo && \
    RUSTFLAGS="--print native-static-libs" cargo build 2>&1 \
    | grep -oP '(?<=native-static-libs: ).*')

(cd "$SCRIPT_DIR/repo/tests" && \
    make clean && make check LIBS="$LIBS") 2>&1 \
    | tee `basename "$0"`.log
