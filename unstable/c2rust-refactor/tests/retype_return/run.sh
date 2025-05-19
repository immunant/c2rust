#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'crate; desc(fn && name("get_char"));' \; \
    retype_return 'char' '__old as char' '__new as u8' \
    -- old.rs $rustflags
