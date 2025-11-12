#!/bin/bash

# complain if we're not on macOS
UNAME=$(uname -s)
if [ "$UNAME" != "Darwin" ]; then
  echo >&2 "Run this script on a macOS host."; exit 1;
fi

# make sure we have all prerequisites
prereqs=(brew clang)
for prereq in "${prereqs[@]}"; do
    type -P "$prereq" >/dev/null || {
        echo >&2 "$prereq not in path."; exit 1;
    }
done

SCRIPT_DIR="$(dirname "$0")"
export HOMEBREW_NO_AUTO_UPDATE=1

# NOTE: Pin LLVM to a known good version since new releases
# tend not to be backwards compatible
# `bash` needed b/c macOS ships with bash 3, which doesn't support arrays properly
brew install -q cmake ninja gpg llvm@17 bash

curl -LsSf https://astral.sh/uv/install.sh | sh
uv venv
uv pip install -r "$SCRIPT_DIR/requirements.txt"

# Rust and dependencies
RUST_TOOLCHAIN_FILE="$SCRIPT_DIR/../rust-toolchain.toml"
export RUST_VER=$($SCRIPT_DIR/query_toml.py toolchain.channel $RUST_TOOLCHAIN_FILE)
"$SCRIPT_DIR/provision_rust.sh"
