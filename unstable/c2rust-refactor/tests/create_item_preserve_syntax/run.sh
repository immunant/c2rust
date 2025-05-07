#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select dest 'crate; desc(fn && name("before"));' \; \
    create_item 'fn f() { /* syntax preserved */ }' after dest \
    -- old.rs $rustflags
