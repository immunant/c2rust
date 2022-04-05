#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"

export CC=clang
# cflags=additional cflags; disable gcc's labeled goto's like so:
# - OPT_THREADED_CODE=2 requests call threading in interpreter dispatch loop.
# - USE_TOKEN_THREADED_VM=1 turns off token threading in regex engine.
export cflags="-DOPT_THREADED_CODE=2 -DUSE_TOKEN_THREADED_VM=0" 
(cd "${SCRIPT_DIR}/repo" && ./configure rb_cv_gcc_atomic_builtins=no rb_cv_gcc_sync_builtins=no --disable-shared --disable-install-doc --disable-install-rdoc \
     --disable-install-capi --disable-jit-support --with-setjmp-type=setjmp -with-out-ext=dbm 2>&1 \
     | tee ../`basename "$0"`.log)
