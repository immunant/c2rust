#!/bin/bash

# Make debconf use a frontend that expects no interactive input
export DEBIAN_FRONTEND=noninteractive
SCRIPT_DIR="$(dirname "$0")"

# Latest cmake directly from cmake.org -> /opt
# Note: not required on 17.10 and later.
# bash $SCRIPT_DIR/provision_cmake.sh

apt-get update -qq
# Haven't found a way to upgrade open-vm-tools non-interactively
# apt-get install -y --only-upgrade open-vm-tools
apt-get install -qq htop unzip tmux lldb-5.0 vim curl gnupg2 cmake gperf
apt-get install -qq software-properties-common build-essential llvm-5.0 clang-5.0 ninja-build

update-alternatives --install /usr/bin/clang clang /usr/bin/clang-5.0 100
update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-5.0 100
update-alternatives --install /usr/bin/lldb lldb /usr/bin/lldb-5.0 100

# Install python3.6 and packages
apt-get install -qq python3-pip
pip3 install --upgrade pip
pip3 install plumbum colorlog typing cbor cbor2 pylint psutil

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
# Dependencies for test programs #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

# json-c dependencies
apt-get install -qq automake autoconf

# lua dependencies
apt-get install -qq libreadline-dev

# lighttpd dependencies
apt-get install -qq libbz2-dev

# python dependencies
apt-get install -qq python-setuptools tcl-dev liblzma-dev libgdbm-dev
apt-get -qq --no-install-recommends install tk-dev


# redis and sqlite dependencies
apt-get install -qq tcl tcl-dev

# varnish dependencies
apt-get install -qq python-docutils graphviz
