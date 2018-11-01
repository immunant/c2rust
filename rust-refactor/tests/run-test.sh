#!/bin/sh
set -e

export nightly=nightly-2017-11-20
triple=unknown
unamestr=`uname`
if [ "$unamestr" = 'Linux' ]; then
   triple='x86_64-unknown-linux-gnu'
elif [ "$unamestr" = 'Darwin' ]; then
   triple='x86_64-apple-darwin'
fi
rust_dir=$(rustc --print sysroot)
rustfmt=$rust_dir/bin/rustfmt
export LD_LIBRARY_PATH=$rust_dir/lib
# System Integrity Protection on macOS ignores previous export statement
# so export the library path under another name and set it in the child.
export not_LD_LIBRARY_PATH=$rust_dir/lib
export RUST_LOG=idiomize=info
export RUST_BACKTRACE=1
# PL: I  removed the plugin-related arguments since its not clear that
# they are necessary to correctly run the regression test suite.
# export refactor='../../target/debug/idiomize -P ../.. -p plugin_stub -r alongside'
export idiomize_bin='../../target/debug/idiomize'
export refactor="$idiomize_bin  -r alongside"
export rustflags=""

( cd $1; ./run.sh; )
if ! [ -f $1/no-rustfmt ]; then
    $rustfmt $1/old.new
fi
diff -wB $1/new.rs $1/old.new
