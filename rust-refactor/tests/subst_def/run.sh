#!/bin/sh
$refactor \
    select target 'crate; desc(fn && name("f"));' \; \
    rewrite_expr 'def!(f)()' 'g(def!(f))' \
    -- old.rs $rustflags
