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
# dirmngr is required for gnupg2 key retrieval
apt-get install -qq --install-recommends dirmngr
# libclang-6.0-dev allows builds against pre-built clang/llvm
apt-get install -qq build-essential clang-6.0 cmake curl \
    git gnupg2 gperf htop ninja-build python-dev \
    software-properties-common unzip libssl-dev \
    libclang-6.0-dev pkg-config

update-alternatives --install /usr/bin/clang clang /usr/bin/clang-6.0 100
update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-6.0 100
# update-alternatives --install /usr/bin/lldb lldb /usr/bin/lldb-6.0 100

# Install python3 and packages
apt-get install -qq python3-pip
pip3 install --no-cache-dir --disable-pip-version-check -r $SCRIPT_DIR/requirements.txt

