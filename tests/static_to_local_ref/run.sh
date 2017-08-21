#!/bin/sh
$refactor \
    select target 'crate; child(static);' \; \
    select user 'crate; desc(fn && !name("main"));' \; \
    static_to_local_ref -- old.rs $rustflags
