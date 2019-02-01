#!/bin/bash

IMAGE_NAME=${1:-immunant/c2rust}
CONTAINER_NAME=c2rust
SCRIPT_DIR=$( cd "$( dirname $0 )" && pwd )
C2RUST_HOME="$(dirname "$SCRIPT_DIR")"

# args
# 1st: names container
# 2nd: sets hostname
# 3rd: keeps STDIN open, allocates a pseudo-TTY
# 4-5th: maps pardir on host into docker guest
# NOTE: ssh forwarding does not work with Docker for Mac ATM.
# More info here https://github.com/docker/for-mac/issues/483
docker run \
    --name $CONTAINER_NAME \
    --hostname docker \
    -it \
    --volume $C2RUST_HOME:/home/docker/C2Rust \
    --volume $(dirname $SSH_AUTH_SOCK):$(dirname $SSH_AUTH_SOCK) \
    --env SSH_AUTH_SOCK=$SSH_AUTH_SOCK \
    --user docker \
    $IMAGE_NAME

