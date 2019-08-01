#!/bin/bash

SCRIPT_DIR="$(dirname "$0")"

pacman -Sy
pacman -S --quiet --noconfirm pkgconf make cmake ninja llvm clang python python-pip ncurses git diffutils luarocks

pip3 install --no-cache-dir --disable-pip-version-check -r $SCRIPT_DIR/requirements.txt

# Set the system-wide Lua path to include luarocks directories
luarocks path > /etc/profile.d/luarocks-path.sh

# Install penlight lua package with luarocks
luarocks install penlight
