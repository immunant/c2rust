#!/bin/sh
$refactor \
    select box 'crate;
        desc(foreign_item && fn && name("malloc|free|realloc"));
        desc(match_ty(*mut __t));' \; \
    select ann 'crate; desc(fn || field);' \; \
    ownership_annotate ann \
    -- old.rs $rustflags
