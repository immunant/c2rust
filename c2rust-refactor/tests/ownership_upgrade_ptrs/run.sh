#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor script ../../scripts/test_ownership_upgrade_ptrs.lua -- old.rs $rustflags

# We manually call rustfmt with a newer nightly here because the rustfmt in
# nightly-2019-06-22 does not handle param attributes. TODO: remove this when we
# update to a newer nightly
rustfmt +nightly old.new
