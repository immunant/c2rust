#!/bin/bash

IMG_NAME=c2rust
SCRIPT_DIR="$(dirname "$0")"
docker build \
    --build-arg UID=$(id -u $(logname)) \
    --build-arg GID=$(id -g $(logname)) \
    --tag $IMG_NAME $SCRIPT_DIR
