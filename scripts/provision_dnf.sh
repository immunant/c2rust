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
    python3-devel \
    python3-pip \
    redhat-rpm-config \
    strace \
    xz \
    zlib-devel

pip3 install --upgrade pip
# Current version of scan-build requires setuptools 20.5 or newer to parse
# environment markers in install_requires
pip3 install "setuptools >= 20.5" --disable-pip-version-check --quiet
pip3 install -r $SCRIPT_DIR/requirements.txt --disable-pip-version-check --quiet

