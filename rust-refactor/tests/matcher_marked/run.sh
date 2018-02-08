#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` == 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'crate; desc(match_expr(1));' \; \
    rewrite_expr 'marked!(__e) + __f' '__f + __e' \
    -- old.rs $rustflags
