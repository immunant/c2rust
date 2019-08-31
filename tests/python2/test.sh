#!/bin/bash
set -e; set -o pipefail

TARGET_BIN="repo/target/release/python"
if [ ! -f ${TARGET_BIN} ]; then
    echo "Rust binary not found: $TARGET_BIN"; exit 1
fi

rm repo/python && cp $TARGET_BIN repo/python

NPROC=`nproc`

(cd repo && make test TESTOPTS="-j$NPROC -uall,-cpu -x test_gdb test_ssl test_bsddb3 test_urllibnet test_urllib2_localnet test_httplib -G" 2>&1 \
     | tee ../`basename "$BASH_SOURCE"`.log)