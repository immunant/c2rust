#!/bin/sh
$refactor \
    select target 'crate; desc(match_expr(1));' \; \
    rewrite_expr 'marked!(__e) + __f' '__f + __e' \
    -- old.rs $rustflags
