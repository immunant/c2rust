#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'item(alias_mod::tgt::S);' \; set_visibility 'pub' \; clear_marks \; \
    select target 'item(use_both::dual::S2);' \; set_visibility 'pub' \; clear_marks \; \
    select target 'item(tricky::shadow::U);' \; set_visibility 'pub' \; \
    -- old.rs $rustflags
