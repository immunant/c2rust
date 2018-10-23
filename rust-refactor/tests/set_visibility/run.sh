#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'crate; desc(fn && name("pub1"));' \; set_visibility pub \; clear_marks \; \
    select target 'crate; desc(fn && name("crate1"));' \; set_visibility 'pub(crate)' \; clear_marks \; \
    select target 'crate; desc(fn && name("priv1"));' \; set_visibility '' \; clear_marks \; \
    select target 'crate; desc(fn && name("pub2"));' \; set_visibility pub \; clear_marks \; \
    select target 'crate; desc(fn && name("crate2"));' \; set_visibility 'pub(crate)' \; clear_marks \; \
    select target 'crate; desc(fn && name("priv2"));' \; set_visibility '' \; clear_marks \; \
    select target 'crate; desc(fn && name("pub3"));' \; set_visibility pub \; clear_marks \; \
    select target 'crate; desc(fn && name("crate3"));' \; set_visibility 'pub(crate)' \; clear_marks \; \
    select target 'crate; desc(fn && name("priv3"));' \; set_visibility '' \; clear_marks \; \
    -- old.rs $rustflags
