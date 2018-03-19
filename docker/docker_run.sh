#!/bin/bash

CONTAINER_NAME=c2rust
SCRIPT_DIR=$( cd "$( dirname $0 )" && pwd )
C2RUST_HOME="$(dirname "$SCRIPT_DIR")"

# args
# 1st: names container
# 2nd: sets hostname
# 3rd: keeps STDIN open, allocates a pseudo-TTY
# 4th: maps pardir on host into docker guest
docker run \
    --name $CONTAINER_NAME \
    --hostname docker \
    -it \
    --volume $C2RUST_HOME:/home/docker/C2Rust c2rust