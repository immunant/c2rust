#!/bin/sh


(cd repo && autoconf && ./configure --disable-shared --disable-install-doc --disable-install-rdoc \
     --disable-install-capi --disable-jit-support -with-out-ext=dbm 2>&1 \
     | tee ../`basename "$0"`.log)
