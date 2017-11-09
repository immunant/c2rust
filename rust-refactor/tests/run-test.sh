#!/bin/sh
set -e

nightly=nightly-2017-09-18
triple=x86_64-unknown-linux-gnu
rust_dir=$HOME/.rustup/toolchains/$nightly-$triple
rustfmt=$HOME/.cargo/bin/rustfmt
export LD_LIBRARY_PATH=$rust_dir/lib
export RUST_LOG=idiomize=info
export RUST_BACKTRACE=1
export refactor='../../target/debug/idiomize -P ../.. -p plugin_stub -r alongside'
export rustflags="-L $rust_dir/lib/rustlib/$triple/lib"

( cd $1; ./run.sh; )
$rustfmt $1/old.rs.new
diff -wB $1/new.rs $1/old.rs.new
