#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'crate; desc(match_ty(i16));' \; \
    select target 'crate; desc(struct && name("S"));' \; \
    select target 'crate; desc(fn && name("f"));' \; \
    generalize_items \
    -- old.rs $rustflags
