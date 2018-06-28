#!/bin/bash

set -e

# do we have docker?
type -P docker >/dev/null || { 
    echo >&2 "docker not in path."; exit 1; 
}

# ... and the right version?
docker --version | grep -q "Docker version 17" && { 
    echo "Docker version too old. Please upgrade."; exit 1; }

IMG_NAME=c2rust
SCRIPT_DIR="$(dirname "$0")"

# pull the rust version out of ../rust-toolchain to keep things synched
RUST_TOOLCHAIN_FILE="$SCRIPT_DIR/../rust-toolchain"
RUST_VER=$(cat $RUST_TOOLCHAIN_FILE | tr -d '\n')

docker build -f $SCRIPT_DIR/../docker/Dockerfile \
    --build-arg UID=$(id -u $(logname)) \
    --build-arg GID=$(id -g $(logname)) \
    --build-arg RUST_VER=$RUST_VER \
    --tag $IMG_NAME $SCRIPT_DIR
