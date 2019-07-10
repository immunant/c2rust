#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'crate; desc(fn && name("printf"));' \; \
    mark_arg_uses 0 target \; \
    convert_format_args \
    clear_marks \; \
    select target 'crate; child(foreign_mod); last;' \; \
    -- old.rs $rustflags
