#!/bin/bash

set -e

IMG_NAME=c2rust
SCRIPT_DIR="$(dirname "$0")"

# pull the rust version out of ../scripts/common.py to keep things synched
PYTHON_DIR="$SCRIPT_DIR/../scripts"
PYTHON_CMD="from common import Config; print(Config.CUSTOM_RUST_NAME)"
RUST_VER=$(cd $PYTHON_DIR && python3 -B -c "$PYTHON_CMD")

docker build \
    --build-arg UID=$(id -u $(logname)) \
    --build-arg GID=$(id -g $(logname)) \
    --build-arg RUST_VER=$RUST_VER \
    --tag $IMG_NAME $SCRIPT_DIR
