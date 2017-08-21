#!/bin/sh
$refactor \
    rewrite_expr 'typed!(__i, u16)' '1000u16' \; \
    -- old.rs $rustflags
