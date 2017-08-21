#!/bin/sh
$refactor select target 'crate; desc(fn && name("f"));' \; func_to_macro mac -- old.rs $rustflags
