#!/usr/bin/env bash
set -e; set -o pipefail

(cd repo && ./configure --disable-shared 2>&1 \
     | tee ../`basename "$0"`.log)
