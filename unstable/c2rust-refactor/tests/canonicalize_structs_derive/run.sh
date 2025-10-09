#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select target 'item(a::TheStruct);' \; print_spans \; canonicalize_structs \
    -- old.rs $rustflags
