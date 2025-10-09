#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

# $refactor sets `-r alongside`.  This test makes sure that changes are visible
# across `commit`s, even when those changes aren't written to the original file
# (as would be done with `-r inplace`).
$refactor \
    rewrite_expr 1 2 \; commit \; \
    rewrite_expr 2 3 \; commit \; \
    -- old.rs $rustflags
