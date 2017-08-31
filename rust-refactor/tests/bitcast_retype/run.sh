#!/bin/sh
$refactor \
    bitcast_retype i32 u32 \
    -- old.rs $rustflags
