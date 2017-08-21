#!/bin/sh
$refactor \
    select target 'crate; desc(foreign_item && fn);' \; \
    select dest 'crate; desc(mod && name("wrap"));' \; \
    wrap_extern -- old.rs $rustflags
