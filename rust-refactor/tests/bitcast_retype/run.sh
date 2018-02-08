#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` == 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    bitcast_retype i32 u32 \
    -- old.rs $rustflags
