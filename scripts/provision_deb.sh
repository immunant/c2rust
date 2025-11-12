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

apt-get update -qq

apt-get install -y --no-install-recommends \
    apt-utils \
    apt-transport-https \
    ca-certificates

packages=(
    clang
    cmake
    curl
    dirmngr
    git
    gnupg2 # required for gnupg2 key retrieval
    gperf
    htop
    libclang-dev
    libssl-dev
    llvm # required for llvm-config
    ninja-build
    pkg-config
    software-properties-common
    strace
    unzip
    libncurses5-dev
    luarocks
    zlib1g-dev
)

apt-get install -qq "${packages[@]}"

# needs `llvm-config` from `llvm` package,
# so the main packages need to be installed first
if ! [[ -x "$(llvm-config --bindir)/FileCheck" ]]; then
	IFS="." read -r major minor patch <<< "$(llvm-config --version)"
    if [[ ${major} -gt 6 ]]; then
        tools="llvm-${major}-tools"
    else
        tools="llvm-${major}.${minor}-tools"
    fi
    apt-get install -qq "${tools}"
fi

apt-get clean # clear apt-caches to reduce image size

curl -LsSf https://astral.sh/uv/install.sh | sh
uv venv
uv pip install -r "$SCRIPT_DIR/requirements.txt"

# Set the system-wide Lua path to include luarocks directories
luarocks path > /etc/profile.d/luarocks-path.sh

# Install penlight lua package with luarocks
luarocks install penlight
