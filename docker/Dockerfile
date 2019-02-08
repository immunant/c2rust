ARG BASE_IMAGE=ubuntu:bionic
FROM ${BASE_IMAGE}
LABEL maintainer="c2rust@immunant.com"

ARG USER=docker
ARG UID=1000
ARG GID=1000
ARG PROVISION_SCRIPT=provision_deb.sh
ENV HOME=/home/$USER
ENV DEBIAN_FRONTEND=noninteractive

USER root
RUN groupadd -f -g $GID $USER
RUN useradd --create-home -u $UID -g $GID --create-home --shell /bin/bash $USER 

# Provision debian and python packages 
COPY provision*.sh /tmp/
COPY requirements.txt /tmp/requirements.txt
RUN chmod +x /tmp/provision*.sh && /tmp/provision.sh

USER $USER
# Provision rust language and packages (should not run as root)
ARG RUST_VER=nightly-2018-12-03
RUN RUST_VER=$RUST_VER /tmp/provision_rust.sh

# Need to switch back to root for Azure DevOps compatibility
USER root