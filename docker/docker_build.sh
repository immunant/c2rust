#!/bin/bash

IMG_NAME=c2rust
SCRIPT_DIR="$(dirname "$0")"
docker build \
    --build-arg UID=$(id -u) \
    --build-arg GID=$(id -g) \
    --tag $IMG_NAME $SCRIPT_DIR 