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
apt-get install -qq \
    cmake \
    curl \
    dirmngr \
    git \
    gnupg2 \
    gperf \
    htop \
    libssl-dev \
    ninja-build \
    pkg-config \
    python-dev \
    python3-pip \
    python3-setuptools \
    software-properties-common \
    unzip \
    libncurses5-dev

# Older releases do not include clang 6 and later so we grab 
# the latest versions of those packages from the LLVM project. 
export APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=DontWarn
curl -s https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
apt-add-repository "deb http://apt.llvm.org/$(lsb_release -cs)/ llvm-toolchain-$(lsb_release -cs)-6.0 main"

apt-get update -qq
# libclang-6.0-dev: for fast builds against host libclang
apt-get install -qq clang-6.0 libclang-6.0-dev 

source /etc/os-release
# Debian jessie ships with a version of cmake that is too old
if [ "$VERSION" == "8 (jessie)" ]; then
    echo "deb [check-valid-until=no] http://archive.debian.org/debian jessie-backports main" >> /etc/apt/sources.list
    apt-get update -qq
    apt-get -t jessie-backports install -y --no-install-recommends cmake
fi

apt-get clean # clear apt-caches to reduce image size

update-alternatives --install /usr/bin/clang clang /usr/bin/clang-6.0 100
update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-6.0 100
# update-alternatives --install /usr/bin/lldb lldb /usr/bin/lldb-6.0 100

# Install python3 and packages
pip3 install -r $SCRIPT_DIR/requirements.txt

