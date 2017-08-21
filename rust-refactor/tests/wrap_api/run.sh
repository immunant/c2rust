#!/bin/sh
$refactor \
    select target 'crate; child(fn && name("api.*"));' \; \
    wrap_api -- old.rs $rustflags
