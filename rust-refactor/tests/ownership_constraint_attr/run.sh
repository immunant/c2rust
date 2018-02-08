#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` == 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select box 'crate;
        desc(foreign_item && fn && name("malloc|free|realloc"));
        desc(match_ty(*mut __t));' \; \
    select ann 'crate; desc(fn || field);' \; \
    ownership_annotate ann \
    -- old.rs $rustflags
