#!/bin/bash

IMG_NAME=c2rust
SCRIPT_DIR="$(dirname "$0")"
docker build --tag $IMG_NAME $SCRIPT_DIR 