#!/bin/sh
$refactor \
    select target 'crate; desc(arg && any_child(match_pat(y)));' \; \
    retype_argument u8 '__old as u8' '__new as i32' -- old.rs $rustflags
