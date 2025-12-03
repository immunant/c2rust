#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

TARGET_BIN="${CARGO_TARGET_DIR:-repo/target}/release/python"
if [ ! -f ${TARGET_BIN} ]; then
    echo "Rust binary not found: $TARGET_BIN"; exit 1
fi

rm -f "$SCRIPT_DIR/repo/python" && cp $TARGET_BIN "$SCRIPT_DIR/repo/python"

# https://devguide.python.org/runtests/
NPROC=`nproc`
# note: test_ftplib can fail inside of docker if ipv6 isn't enabled.
FLAKY_TESTS="test_gdb test_ssl test_bsddb3 test_urllibnet test_urllib2_localnet test_httplib test_ftplib test_imaplib"

# Don't want to allow gui tests because they flash annoying windows on screen
unset DISPLAY

# these tests pass but slow us down
SLOW_TESTS="test_xpickle test_smtpnet"
(cd "$SCRIPT_DIR/repo" && make buildbottest TESTOPTS="-j$NPROC -x $FLAKY_TESTS $SLOW_TESTS -G" 2>&1 \
     | tee ../`basename "$BASH_SOURCE"`.log)
