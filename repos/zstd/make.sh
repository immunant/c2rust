#!/bin/sh

set -e

# TARGET=zstd
TARGET=fullbench

make -C repo clean && rm -f compile_commands.json 
intercept-build make -C repo/tests -j`nproc` $TARGET \
    | tee `basename "$0"`.log