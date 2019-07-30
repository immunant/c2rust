#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

cp  $SCRIPT_DIR/repo/target/debug/librepo.a $SCRIPT_DIR/repo/.libs/libjson-c.a

(cd $SCRIPT_DIR/repo/tests && \
    make clean && make check \
    | tee `basename "$0"`.log)
