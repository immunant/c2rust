#!/bin/bash
set -e; set -o pipefail

(cd repo && ./autogen.sh 2>&1 \
     | tee ../`basename "$0"`.log)
