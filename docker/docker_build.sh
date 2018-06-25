#!/bin/bash

set -e

IMG_NAME=c2rust
SCRIPT_DIR="$(dirname "$0")"

# pull the rust version out of ../rust-toolchain to keep things synched
RUST_TOOLCHAIN_FILE="$SCRIPT_DIR/../rust-toolchain"
RUST_VER=$(cat $RUST_TOOLCHAIN_FILE | tr -d '\n')

docker build \
    --build-arg UID=$(id -u $(logname)) \
    --build-arg GID=$(id -g $(logname)) \
    --build-arg RUST_VER=$RUST_VER \
    --tag $IMG_NAME $SCRIPT_DIR
