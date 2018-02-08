#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` == 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'crate; desc(static && name("A"));' \; \
    select target 'crate; desc(static && name("C"));' \; \
    static_collect_to_struct S AC -- old.rs $rustflags
