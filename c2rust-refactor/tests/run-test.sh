#!/bin/sh
set -e

export nightly=nightly-2022-08-08
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
export RUST_LOG=c2rust_refactor=info
export RUST_BACKTRACE=1
# PL: I  removed the plugin-related arguments since its not clear that
# they are necessary to correctly run the regression test suite.
# export refactor='../../target/debug/c2rust-refactor -P ../.. -p plugin_stub -r alongside'
export refactor_bin='../../../target/debug/c2rust-refactor'
export refactor="$refactor_bin  -r alongside"
export rustflags="--edition 2018"

( cd $1; ./run.sh; )
if ! [ -f $1/no-rustfmt ]; then
    $rustfmt $1/old.new
fi
diff -wB $1/new.rs $1/old.new
