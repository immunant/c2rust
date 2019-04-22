#!/bin/bash
set -e; set -o pipefail

make -C repo clean && rm -f compile_commands.json 
intercept-build make -C repo -j`nproc` | tee `basename "$0"`.log