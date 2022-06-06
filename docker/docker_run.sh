#!/bin/bash

IMAGE_NAME=${1:-immunant/c2rust}
USER=${2:-docker}
CONTAINER_NAME=c2rust
SCRIPT_DIR=$( cd "$( dirname $0 )" && pwd )
C2RUST_HOME="$(dirname "$SCRIPT_DIR")"

docker run \
    `#--name $CONTAINER_NAME` `# names container` \
    --hostname docker `# sets hostname` \
    --interactive \
    --tty \
    `# maps pardir on host into docker guest` \
    --volume $C2RUST_HOME:/home/docker/C2Rust \
    `# NOTE: ssh forwarding does not work with Docker for Mac ATM.` \
    `# More info here https://github.com/docker/for-mac/issues/483` \
    --volume $(dirname $SSH_AUTH_SOCK):$(dirname $SSH_AUTH_SOCK) \
    --env SSH_AUTH_SOCK=$SSH_AUTH_SOCK \
    --user $USER \
    $IMAGE_NAME

