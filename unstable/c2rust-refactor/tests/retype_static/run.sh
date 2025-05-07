#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'item(S);' \; \
    retype_static 'char' '__old as char' '__new as u8' \
        '*(&__new as *const char as *const u8)' \
        '*(&mut __new as *mut char as *mut u8)' \
    -- old.rs $rustflags
