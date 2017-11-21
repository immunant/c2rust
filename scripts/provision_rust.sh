#!/bin/sh
# FIXME: rustup is the most convenient way to install rust 
# but it doesn't check signatures of downloaded packages :/
# https://github.com/rust-lang-nursery/rustup.rs#security
# NOTE: must run as regular user, not root.
curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain nightly-2017-11-20
echo "source ~/.cargo/env" >> ~/.bashrc
# this takes some time... :(
rustup run nightly-2017-11-20 cargo install rustfmt
