#!/bin/bash

set -e

SCRIPT_DIR="$(dirname "$0")"

. /etc/os-release

if [ "$NAME" != "Fedora Linux" ]; then
    echo >&2 "Run this script on a Fedora host."; exit 1;
fi

# required to install ninja-build
yum install --quiet --assumeyes epel-release
# NOTE: CentOS version of cmake is too old
yum install --quiet --assumeyes which git ninja-build make cmake libquadmath-devel strace

yum install --quiet --assumeyes luarocks

# Set the system-wide Lua path to include luarocks directories
luarocks path > /etc/profile.d/luarocks-path.sh

# Install penlight lua package with luarocks
luarocks install penlight

# TODO: provision remaining packages and test
echo >&2 "Provisioning incomplete."; exit 1;
