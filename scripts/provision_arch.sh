#!/bin/bash

SCRIPT_DIR="$(dirname "$0")"

pacman -Sy
pacman -S --quiet --noconfirm pkgconf make cmake ninja llvm clang python python-pip ncurses

pip3 install --no-cache-dir --disable-pip-version-check -r $SCRIPT_DIR/requirements.txt
