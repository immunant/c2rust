#!/bin/bash

set -e

SCRIPT_DIR="$(dirname "$0")"

pacman -Sy
pacman -S --quiet --noconfirm pkgconf make cmake ninja llvm clang ncurses git diffutils luarocks libffi strace

curl -LsSf https://astral.sh/uv/install.sh | sh
uv venv
uv pip install -r "$SCRIPT_DIR/requirements.txt"

# Set the system-wide Lua path to include luarocks directories
luarocks path > /etc/profile.d/luarocks-path.sh

# Install penlight lua package with luarocks
luarocks install penlight
