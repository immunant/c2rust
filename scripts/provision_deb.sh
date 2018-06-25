#!/bin/bash

# Are we on a supported distro? Note: We can't use dpkg-vendor 
# because it is installed via `build-essential`.
grep -Ei 'debian|buntu|mint' /etc/*release > /dev/null || {
    echo >&2 "Run this script on a Debian-based host."; exit 1; 
}

# Make debconf use a frontend that expects no interactive input
export DEBIAN_FRONTEND=noninteractive
SCRIPT_DIR="$(dirname "$0")"

apt-get update -qq
apt-get install -qq htop unzip tmux vim curl gnupg2 cmake gperf
apt-get install -qq software-properties-common build-essential  ninja-build

# update-alternatives --install /usr/bin/clang clang /usr/bin/clang-5.0 100
# update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-5.0 100
# update-alternatives --install /usr/bin/lldb lldb /usr/bin/lldb-5.0 100

# Install python3.6 and packages
apt-get install -qq python3-pip
pip3 install --upgrade pip
pip3 install -r /tmp/requirements.txt

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
