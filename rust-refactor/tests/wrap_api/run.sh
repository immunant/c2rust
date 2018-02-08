#!/bin/sh

# work around System Integrity Protection on macOS
if [[ `uname` == 'Darwin' ]]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'crate; child(fn && name("api.*"));' \; \
    wrap_api -- old.rs $rustflags
