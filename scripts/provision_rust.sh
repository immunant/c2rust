#!/bin/bash
# FIXME: rustup is the most convenient way to install rust 
# but it doesn't check signatures of downloaded packages :/
# https://github.com/rust-lang-nursery/rustup.rs#security

# must run as regular user, not root.
if [[ "$EUID" -eq 0 ]]
  then echo "Please don't run as root"
  exit
fi

RUST_VER=nightly-2017-11-20
curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain $RUST_VER

# make rust environment available on next login 
echo "source ~/.cargo/env" >> ~/.bashrc
# make rust environment available for commands below 
source ~/.cargo/env

rustup run $RUST_VER cargo install --force rustfmt
