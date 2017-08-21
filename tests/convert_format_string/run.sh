#!/bin/sh
$refactor \
    select target 'crate; desc(fn && name("printf"));' \; \
    mark_arg_uses 0 target \; \
    convert_format_string \
    -- old.rs $rustflags
