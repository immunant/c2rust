#!/bin/bash

set -e

# Are we on a supported distro? Note: We can't use dpkg-vendor 
# because it is installed via `build-essential`.
grep -Ei 'debian|buntu|mint' /etc/*release > /dev/null || {
    echo >&2 "Run this script on a Debian-based host."; exit 1; 
}

# Make debconf use a frontend that expects no interactive input
export DEBIAN_FRONTEND=noninteractive
SCRIPT_DIR="$(dirname "$0")"

# Configure apt to avoid to avoid provisioning slowdowns
echo "Acquire::ForceIPv4 \"true\";" > /etc/apt/apt.conf.d/99force-ipv4

# Retry the `apt-get update` command a few times upon failure
# to work around transient network problems in CI.
n=0
tries=5
until [ $n -ge $tries ]
do
    apt-get update -qq && break
    n=$[$n+1]
    sleep 30
done

apt-get install -y --no-install-recommends \
    apt-utils \
    apt-transport-https \
    ca-certificates
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
    libncurses5-dev \
    luarocks

# Older releases do not include clang 6 and later so we grab 
# the latest versions of those packages from the LLVM project. 
export APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=DontWarn
curl -s https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
apt-add-repository "deb http://apt.llvm.org/$(lsb_release -cs)/ llvm-toolchain-$(lsb_release -cs)-7 main"

apt-get update -qq
# libclang-7.0-dev: for fast builds against host libclang
apt-get install -qq clang-7 libclang-7-dev 

apt-get clean # clear apt-caches to reduce image size

update-alternatives --install /usr/bin/clang clang /usr/bin/clang-7 100
update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-7 100
# update-alternatives --install /usr/bin/lldb lldb /usr/bin/lldb-7 100

pip3 install --upgrade pip
# Current version of scan-build requires setuptools 20.5 or newer to parse
# environment markers in install_requires
pip3 install "setuptools >= 20.5" --disable-pip-version-check --quiet
# Install python3 packages
pip3 install -r $SCRIPT_DIR/requirements.txt --disable-pip-version-check --quiet

# Set the system-wide Lua path to include luarocks directories
luarocks path > /etc/profile.d/luarocks-path.sh

# Install penlight lua package with luarocks
luarocks install penlight
