#!/bin/sh
$refactor \
    select target 'crate; desc(static && name("A"));' \; \
    select target 'crate; desc(static && name("C"));' \; \
    static_collect_to_struct S AC -- old.rs $rustflags
