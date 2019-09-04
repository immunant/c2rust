#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

TARGET_BIN="$SCRIPT_DIR/repo/target/release/librepo.a"
if [ ! -f ${TARGET_BIN} ]; then
    echo "Rust archive not found: $TARGET_BIN"; exit 1
fi

cp  ${TARGET_BIN} $SCRIPT_DIR/repo/.libs/libjson-c.a

(cd $SCRIPT_DIR/repo/tests && \
    make clean && make check \
    | tee `basename "$0"`.log)
