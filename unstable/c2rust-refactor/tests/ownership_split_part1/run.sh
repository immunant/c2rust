#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'crate; desc(fn && name("f2"));' \; \
    ownership_split_variants \
    -- old.rs $rustflags
