#!/bin/sh

# work around System Integrity Protection on macOS
if [[ `uname` == 'Darwin' ]]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'crate; child(static);' \; \
    select user 'crate; desc(fn && !name("main"));' \; \
    static_to_local_ref -- old.rs $rustflags
