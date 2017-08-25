#!/bin/sh
$refactor \
    select target 'crate; desc(fn && name("f2"));' \; \
    ownership_split_variants \
    -- old.rs $rustflags
