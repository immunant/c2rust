#!/bin/bash

# Make debconf use a frontend that expects no interactive input
export DEBIAN_FRONTEND=noninteractive
SCRIPT_DIR="$(dirname "$0")"

# latest cmake directly from cmake.org -> /opt
bash $SCRIPT_DIR/provision_cmake.sh

apt-get update
apt-get install -y htop unzip tmux gdb
apt-get install -y software-properties-common build-essential
apt-get install -y git clang gcc g++ ninja-build

# install python3.6 and packages
apt-get install -y python3-pip
pip3 install --upgrade pip
pip3 install plumbum colorlog typing

# FIXME: rustup is the most convenient way to install rust 
# but it doesn't check signatures of downloaded packages :/
# https://github.com/rust-lang-nursery/rustup.rs#security
curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain nightly