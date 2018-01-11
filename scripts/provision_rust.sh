#!/bin/sh
# FIXME: rustup is the most convenient way to install rust 
# but it doesn't check signatures of downloaded packages :/
# https://github.com/rust-lang-nursery/rustup.rs#security
# NOTE: must run as regular user, not root.
RUST_VER=nightly-2017-11-20
curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain $RUST_VER

# make rust environment available on next login 
echo "source ~/.cargo/env" >> ~/.bashrc

# must rustup with full path since we didn't source the env script yet
RUSTUP=$HOME/.cargo/bin/rustup
$RUSTUP run $RUST_VER cargo install --force rustfmt
