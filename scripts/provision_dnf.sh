#!/bin/bash

set -e

SCRIPT_DIR="$(dirname "$0")"

. /etc/os-release

# TODO: might have to do something similar to support RHEL
if [ "$ID" != "fedora" ]; then
    echo >&2 "Run this script on a Fedora host."; exit 1;
fi

# redhat-rpm-config avoids problem when pip3-installing psutils
dnf install --quiet --assumeyes \
    clang-devel \
    cmake \
    diffutils \
    git \
    libquadmath-devel \
    llvm-devel \
    make \
    ninja-build \
    openssl-devel \
    redhat-rpm-config \
    strace \
    xz \
    zlib-devel

curl -LsSf https://astral.sh/uv/install.sh | sh
uv venv
uv pip install -r "$SCRIPT_DIR/requirements.txt"
