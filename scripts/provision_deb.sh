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
# gnupg2: required for gnupg2 key retrieval
# libclang-6.0-dev: for fast builds against host libclang
apt-get install -qq \
    build-essential \
    clang-6.0 \
    cmake \
    curl \
    dirmngr \
    git \
    gnupg2 \
    gperf \
    htop \
    libclang-6.0-dev \
    libssl-dev \
    ninja-build \
    pkg-config \
    python-dev \
    python3-pip \
    python3-setuptools \
    software-properties-common \
    unzip

update-alternatives --install /usr/bin/clang clang /usr/bin/clang-6.0 100
update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-6.0 100
# update-alternatives --install /usr/bin/lldb lldb /usr/bin/lldb-6.0 100

# Install python3 and packages
pip3 install --no-cache-dir --disable-pip-version-check -r $SCRIPT_DIR/requirements.txt

