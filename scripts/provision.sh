#!/bin/bash

if [[ "$EUID" -ne 0 ]]; then 
    echo "Please run as root"; exit 1
fi

SCRIPT_DIR="$(dirname "$0")"

function provisionMac() {
  ${SCRIPT_DIR}/provision_mac.sh
}

function provisionLinux() {
    if [ -f /etc/os-release ]; then
        # freedesktop.org and systemd
        . /etc/os-release
        OS=$NAME
        VER=$VERSION_ID
    elif type lsb_release >/dev/null 2>&1; then
        # linuxbase.org
        OS=$(lsb_release -si)
        VER=$(lsb_release -sr)
    elif [ -f /etc/lsb-release ]; then
        # For some versions of Debian/Ubuntu without lsb_release command
        . /etc/lsb-release
        OS=$DISTRIB_ID
        VER=$DISTRIB_RELEASE
    elif [ -f /etc/debian_version ]; then
        # Older Debian/Ubuntu/etc.
        OS=Debian
        VER=$(cat /etc/debian_version)
    elif [ -f /etc/SuSe-release ]; then
        # Older SuSE/etc.
        ...
    elif [ -f /etc/redhat-release ]; then
        # Older Red Hat, CentOS, etc.
        ...
    else
        # Fall back to uname, e.g. "Linux <version>", also works for BSD, etc.
        OS=$(uname -s)
        VER=$(uname -r)
    fi

    case "${OS}" in
        "Arch Linux"*)            ${SCRIPT_DIR}/provision_arch.sh;;
        "Debian GNU/Linux"*)      ${SCRIPT_DIR}/provision_deb.sh;;
        "Ubuntu"*)                ${SCRIPT_DIR}/provision_deb.sh;;
        "CentOS Linux"*)          ${SCRIPT_DIR}/provision_rpm.sh;;
        "Fedora"*)                ${SCRIPT_DIR}/provision_rpm.sh;;
        "Red Hat Enterprise Linux Server"*)  ${SCRIPT_DIR}/provision_rpm.sh;;
        *)                        unsupportedOS "UNKNOWN-DISTRO:${OS}";;
    esac
}

function unsupportedOS() {
  echo "Provisioning on $1 is not supported"; exit 1
}

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     provisionLinux;;
    Darwin*)    provisionMac;;
    CYGWIN*)    unsupportedOS "Cygwin";;
    MINGW*)     unsupportedOS "MinGw";;
    *)          unsupportedOS "UNKNOWN:${unameOut}";;
esac