FROM ubuntu:xenial
MAINTAINER perl@immunant.com

ENV USER=docker
ENV HOME=/home/$USER
ENV DEBIAN_FRONTEND=noninteractive
ENV CMAKE_URL=https://cmake.org/files/v3.7/cmake-3.7.2-Linux-x86_64.sh 
ENV CMAKE_SCRIPT=cmake-3.7.2-Linux-x86_64.sh
ENV CMAKE_PREFIX=/opt/$CMAKE_SCRIPT

USER root
RUN useradd --user-group --create-home --shell /bin/bash $USER 
WORKDIR $HOME

RUN apt-get update && apt-get install -y \
    git \
    clang-3.9 \ 
    lldb-3.9 \ 
    ninja-build \
    python3-pip \
    curl \
    wget

# cmake setup
RUN wget --quiet $CMAKE_URL
RUN chmod u+x $CMAKE_SCRIPT
RUN mkdir -p $CMAKE_PREFIX
RUN $HOME/$CMAKE_SCRIPT --prefix=$CMAKE_PREFIX --skip-license
RUN update-alternatives --install /usr/bin/cmake cmake \
    "$CMAKE_PREFIX/bin/cmake" 1 --slave /usr/bin/ccmake ccmake  \
    "$CMAKE_PREFIX/bin/ccmake" --slave /usr/bin/cpack cpack  \
    "$CMAKE_PREFIX/bin/cpack" --slave /usr/bin/ctest ctest  \
    "$CMAKE_PREFIX/bin/ctest" --slave /usr/bin/cmake-gui cmake-gui  \
    "$CMAKE_PREFIX/bin/cmake-gui"
RUN rm -f ${CMAKE_SCRIPT}    

# clang/LLVM setup
RUN update-alternatives --install /usr/bin/clang clang /usr/bin/clang-3.9 100
RUN update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-3.9 100
RUN update-alternatives --install /usr/bin/lldb lldb /usr/bin/lldb-3.9 100   

# lua dependencies
RUN apt-get install -y  libreadline-dev 
# python dependencies
RUN apt-get install -y python-setuptools tcl-dev liblzma-dev libgdbm-dev
RUN apt-get install -y --no-install-recommends tk-dev

# Python3 setup (use --no-cache-dir to avoid warnings)
RUN pip3 install --no-cache-dir --upgrade pip
RUN pip3 install --no-cache-dir plumbum colorlog typing cbor
    

# VOLUME ["$PWD"]


