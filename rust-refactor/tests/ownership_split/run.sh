#!/bin/sh
$refactor \
    select target 'crate; desc(item && fn);' \; \
    ownership_split_variants \
    -- old.rs $rustflags
