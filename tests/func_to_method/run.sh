#!/bin/sh
$refactor \
    select dest 'crate; desc(impl);' \; \
    select target 'crate; desc(arg && any_child(match_pat(s)));' \; \
    select target 'crate; desc(fn && name("static_method"));' \; \
    func_to_method -- old.rs $rustflags
