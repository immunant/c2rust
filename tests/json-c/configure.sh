#!/usr/bin/env bash
set -e; set -o pipefail

export LDFLAGS="-pthread -lquadmath -Wl,--no-as-needed -ldl"

(cd repo && ./configure --disable-shared 2>&1 \
     | tee ../`basename "$0"`.log)
