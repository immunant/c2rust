ARG BASE_IMAGE=ubuntu:bionic
FROM ${BASE_IMAGE}
LABEL maintainer="c2rust@immunant.com"

ARG USER=docker
ARG UID=1000
ARG GID=1000
ENV HOME=/home/$USER
ENV DEBIAN_FRONTEND=noninteractive

USER root
RUN groupadd -f -g $GID $USER
RUN useradd --create-home -u $UID -g $GID --create-home --shell /bin/bash $USER

# /home/docker needs to be accessible by other users because we always run the
# cargo binary from /home/docker.cargo/bin
RUN chmod 755 /home/docker

# Provision debian and python packages 
COPY provision*.sh /tmp/
COPY requirements.txt /tmp/requirements.txt
RUN chmod +x /tmp/provision*.sh && /tmp/provision.sh

USER $USER
# Provision rust language and packages (should not run as root)
ARG RUST_VER=nightly-2019-06-22
RUN RUST_VER=$RUST_VER /tmp/provision_rust.sh

ENV PATH="/home/${USER}/.cargo/bin:${PATH}"
ENV CARGO_HOME="/tmp/.cargo"
ENV RUSTUP_HOME="/home/${USER}/.rustup"

# Need to switch back to root for Azure DevOps compatibility
USER root
