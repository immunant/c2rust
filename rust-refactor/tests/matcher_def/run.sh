#!/bin/sh
$refactor \
    select target 'crate; child(fn && name("f"));' \; \
    rewrite_expr 'def!(f)()' '::f2()' \
    -- old.rs $rustflags
