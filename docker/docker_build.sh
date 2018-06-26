#!/bin/bash

set -e

IMG_NAME=c2rust
SCRIPT_DIR="$(dirname "$0")"

# pull the rust version out of ../rust-toolchain to keep things synched
RUST_TOOLCHAIN_FILE="$SCRIPT_DIR/../rust-toolchain"
RUST_VER=$(cat $RUST_TOOLCHAIN_FILE | tr -d '\n')
# read python packages from ../scripts/requirements.txt
PY_REQUIREMENTS_FILE="$SCRIPT_DIR/../scripts/requirements.txt"
PY_REQUIREMENTS=$(cat $PY_REQUIREMENTS_FILE)

docker build \
    --build-arg UID=$(id -u $(logname)) \
    --build-arg GID=$(id -g $(logname)) \
    --build-arg RUST_VER=$RUST_VER \
    --build-arg PY_REQUIREMENTS="$PY_REQUIREMENTS" \
    --tag $IMG_NAME $SCRIPT_DIR
