#!/bin/bash

set -e

# do we have docker?
type -P docker >/dev/null || {
    echo >&2 "docker not in path."; exit 1;
}

# ... and the right version?
docker --version | grep -q "Docker version 17" && {
    echo "Docker version too old. Please upgrade."; exit 1; }

REPO_NAME=immunant/c2rust
DATE_TAG=$(date +'%Y%m%d')
SCRIPT_DIR="$(dirname "$0")"

declare -A IMAGES  # associative arrays are only supported in bash 4 and higher
IMAGES["ubuntu:focal"]="1"
IMAGES["ubuntu:bionic"]="1" # any non-empty string will do
IMAGES["debian:bullseye"]="1"
IMAGES["debian:buster"]="1"
IMAGES["archlinux:base"]="1"
IMAGES["fedora:34"]="1"

build_image() {
    BASE_IMAGE=${1}
    IMAGE_TAG=$(echo ${BASE_IMAGE} | tr -s :/ - ) # replace colons and slashes with hyphens

    # pull the nightly rust version out of the rust-toolchain.toml file to keep things synced
    RUST_TOOLCHAIN_FILE="$SCRIPT_DIR/../unstable/rust-toolchain.toml"
    NIGHTLY_RUST_VER=$($SCRIPT_DIR/query_toml.py toolchain.channel $RUST_TOOLCHAIN_FILE)

    docker pull "$BASE_IMAGE"
    docker build -f $SCRIPT_DIR/../docker/Dockerfile \
           --build-arg BASE_IMAGE=$BASE_IMAGE \
           --build-arg UID=$(id -u $(logname)) \
           --build-arg GID=$(id -g $(logname)) \
           --build-arg NIGHTLY_RUST_VER=$NIGHTLY_RUST_VER \
           --tag "$REPO_NAME:$IMAGE_TAG-$DATE_TAG" \
           --tag "$REPO_NAME:$IMAGE_TAG-latest" \
           $SCRIPT_DIR
}

if [ "$1" == "" ]; then
        options=$(echo "${!IMAGES[@]}" | tr -s '[:blank:]' '|')

        echo $"Usage: $0 {${options}|build-all|push-all}"
        exit 1
fi

if [ "${IMAGES[$1]}" != "" ]; then
        build_image "$1"
else
    case "$1" in
        build-all)
            for image in "${!IMAGES[@]}"; do
                build_image "${image}" ${IMAGES["${image}"]}
            done
            ;;

        push-all)
            for image in "${!IMAGES[@]}"; do
                IMAGE_TAG=$(echo ${image} | tr -s :/ - ) # replace colons and slashes with hyphens
                docker push "$REPO_NAME:$IMAGE_TAG-$DATE_TAG"
                docker push "$REPO_NAME:$IMAGE_TAG-latest"
            done
            ;;
    esac
fi
