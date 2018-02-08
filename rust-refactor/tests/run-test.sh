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
rustfmt=$HOME/.cargo/bin/rustfmt
export LD_LIBRARY_PATH=$rust_dir/lib
# System Integrity Protection on macOS ignores previous export statement
# so export the library path under another name and set it in the child.
export not_LD_LIBRARY_PATH=$rust_dir/lib
export RUST_LOG=idiomize=info
export RUST_BACKTRACE=1
export refactor='../../target/debug/idiomize -P ../.. -p plugin_stub -r alongside'
export rustflags="-L $rust_dir/lib/rustlib/$triple/lib"

( cd $1; ./run.sh; )
$rustfmt $1/old.rs.new
diff -wB $1/new.rs $1/old.rs.new
