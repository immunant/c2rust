#!/bin/bash

SCRIPT_DIR="$(dirname "$0")"

. /etc/os-release

if [ "$NAME" != "CentOS Linux" ]; then
    echo >&2 "Run this script on a CentOS host."; exit 1; 
fi

# required to install ninja-build
yum install --quiet --assumeyes epel-release
# NOTE: CentOS version of cmake is too old 
yum install --quiet --assumeyes which ninja-build make cmake

# TODO: provision remaining packages and test
echo >&2 "Provisioning incomplete."; exit 1; 
