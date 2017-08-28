#!/bin/sh
set -e

triple=x86_64-unknown-linux-gnu
export LD_LIBRARY_PATH=$HOME/install/rust/build/$triple/stage2/lib
export RUST_LOG=idiomize=info
export RUST_BACKTRACE=1
export refactor='../../target/debug/idiomize -P ../.. -p plugin_stub -r alongside'
export rustflags="-L $HOME/install/rust/build/$triple/stage2/lib/rustlib/$triple/lib"

( cd $1; ./run.sh; )
~/install/rustfmt/target/debug/rustfmt $1/old.rs.new
diff -wB $1/new.rs $1/old.rs.new
