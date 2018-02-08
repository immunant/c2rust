#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select dest 'crate; desc(impl);' \; \
    select target 'crate; desc(arg && any_child(match_pat(s)));' \; \
    select target 'crate; desc(fn && name("static_method"));' \; \
    func_to_method -- old.rs $rustflags
