#!/bin/sh
$refactor \
    select target 'crate; desc(match_ty(i16));' \; \
    select target 'crate; desc(struct && name("S"));' \; \
    select target 'crate; desc(fn && name("f"));' \; \
    generalize_items \
    -- old.rs $rustflags
