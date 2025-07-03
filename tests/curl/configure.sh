#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
# curl compiles the same files a bunch of times with different flags which runs the risk of clang picking up the wrong compile command entry
# disable shared libraries, docs and symbol hiding in an attempt to avoid this problem
(cd "$SCRIPT_DIR/repo" && ./configure --with-openssl --disable-shared --disable-docs --disable-symbol-hiding) 2>&1 > "$(basename "$0")".log
