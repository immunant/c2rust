#!/bin/sh

# work around System Integrity Protection on macOS
if [[ `uname` == 'Darwin' ]]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor test_f_plus_one -- old.rs $rustflags
