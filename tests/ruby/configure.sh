#!/bin/bash
set -e; set -o pipefail

export CC=clang
# cflags=additional cflags; disable gcc's labeled goto's
export cflags="-DOPT_THREADED_CODE=1" 
(cd repo && ./configure rb_cv_gcc_atomic_builtins=no rb_cv_gcc_sync_builtins=no --disable-shared --disable-install-doc --disable-install-rdoc \
     --disable-install-capi --disable-jit-support --with-setjmp-type=setjmp -with-out-ext=dbm 2>&1 \
     | tee ../`basename "$0"`.log)
