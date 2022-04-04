#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

TARGET_BIN="$SCRIPT_DIR/repo/target/release/librepo.a"
if [ ! -f ${TARGET_BIN} ]; then
    echo "Rust archive not found: $TARGET_BIN"; exit 1
fi

cp ${TARGET_BIN} $SCRIPT_DIR/repo/.libs/libjson-c.a

# determine ldflags needed to use the static lib we built
LDFLAGS=$(cd ${SCRIPT_DIR}/repo && RUSTFLAGS="--print native-static-libs" cargo build)

(cd $SCRIPT_DIR/repo/tests && \
    make clean && make check LDFLAGS=$LDFLAGS) 2>&1 \
    | tee `basename "$0"`.log
