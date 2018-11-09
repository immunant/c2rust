#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select dest 'crate; desc(fn && name("f"));' \; \
    create_item 'fn new() {}' after dest \
    -- old.rs $rustflags
