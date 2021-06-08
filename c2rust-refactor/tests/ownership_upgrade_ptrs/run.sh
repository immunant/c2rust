#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor script ../../scripts/test_ownership_upgrade_ptrs.lua -- old.rs $rustflags
