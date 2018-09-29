#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'crate; desc(mod && name("foo_h"));' \; \
    select dest 'crate; desc(mod && name("foo_h"));' \; \
    reorganize_modules -- old.rs $rustflags
