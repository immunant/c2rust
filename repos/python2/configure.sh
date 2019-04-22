#!/bin/bash
set -e; set -o pipefail

(cd repo && ./configure --disable-shared --without-computed-gotos 2>&1 \
     | tee ../`basename "$0"`.log)
