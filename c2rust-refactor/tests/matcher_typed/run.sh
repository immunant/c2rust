#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    rewrite_expr 'typed!($i:ident, u16)' '1000u16' \; \
    -- old.rs $rustflags
