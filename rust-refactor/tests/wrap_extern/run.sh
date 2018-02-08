#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'crate; desc(foreign_item && fn);' \; \
    select dest 'crate; desc(mod && name("wrap"));' \; \
    wrap_extern -- old.rs $rustflags
