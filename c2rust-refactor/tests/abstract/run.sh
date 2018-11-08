#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    abstract 'add(x: i32, y: i32) -> i32' 'x + y' \; \
    abstract 'sub<T: Sub<T, Result=T>>(x: T, y: T) -> T' \
        'typed!(x, T) - y' 'x - y' \
    -- old.rs $rustflags
