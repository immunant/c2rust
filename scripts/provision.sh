#!/bin/bash

# Make debconf use a frontend that expects no interactive input
export DEBIAN_FRONTEND=noninteractive
SCRIPT_DIR="$(dirname "$0")"

# Latest cmake directly from cmake.org -> /opt
bash $SCRIPT_DIR/provision_cmake.sh

apt-get update
apt-get install -y htop unzip tmux gdb
apt-get install -y software-properties-common build-essential
apt-get install -y git clang gcc g++ ninja-build

# Install python3.6 and packages
apt-get install -y python3-pip
pip3 install --upgrade pip
pip3 install plumbum colorlog typing cbor

# FIXME: rustup is the most convenient way to install rust 
# but it doesn't check signatures of downloaded packages :/
# https://github.com/rust-lang-nursery/rustup.rs#security
curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain nightly

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
# Dependencies for test programs #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

# lua dependencies
apt-get install -y libreadline-dev

# lighttpd dependencies
apt-get install -y libbz2-dev

# python dependencies
apt-get install -y python-setuptools tcl-dev liblzma-dev libgdbm-dev
apt-get -y --no-install-recommends install tk-dev

# redis and sqlite dependencies
apt-get install -y tcl tcl-dev

# varnish dependencies
apt-get install -y python-docutils graphviz