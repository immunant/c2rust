#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
  rewrite_expr '$e:Expr' 'parse!(dbg!($e))' \; \
    -- old.rs $rustflags
