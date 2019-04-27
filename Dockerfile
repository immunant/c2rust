ARG BASE_IMAGE=immunant/c2rust:ubuntu-bionic-latest
FROM ${BASE_IMAGE}
LABEL maintainer="c2rust@immunant.com"

USER root

COPY requirements.txt /tmp/requirements.txt
RUN pip3 install -r /tmp/requirements.txt

RUN apt-get update -qq && \
    apt-get install -qq \
    autoconf \
    bison \
    build-essential \
    libgdbm-dev \
    liblzma-dev \
    libreadline-dev \
    mdm \
    rcs \
    ruby \
    tcl-dev \
    tk-dev
