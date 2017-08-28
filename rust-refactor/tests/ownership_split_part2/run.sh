#!/bin/sh
$refactor \
    select target 'crate; desc(fn && (name("f1") || name("f3")));' \; \
    ownership_split_variants \
    -- old.rs $rustflags
