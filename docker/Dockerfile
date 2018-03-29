FROM ubuntu:artful
MAINTAINER perl@immunant.com

ARG USER=docker
ARG UID=1000
ARG GID=1000
ARG RUST_VER=nightly-2018-01-06
ENV HOME=/home/$USER
ENV DEBIAN_FRONTEND=noninteractive

USER root
RUN groupadd -f -g $GID $USER
RUN useradd --create-home -u $UID -g $GID --create-home --shell /bin/bash $USER 
WORKDIR $HOME

# Provision debian packages (dirmngr is required for gnupg2 key retrieval)
RUN apt-get update -qq && \
    apt-get install -y --install-recommends dirmngr  && \
    apt-get install -y autoconf \
                       automake \
                       clang-5.0 \
                       cmake \
                       curl \
                       git \
                       gnupg2 \
                       htop \
                       ipython3 \
                       libreadline-dev \
                       lldb-5.0 \
                       llvm-5.0 \    
                       make \
                       ninja-build \
                       python3 \
                       python3-pip \
                       tmux \
                       unzip \
                       vim \
                       wget

RUN update-alternatives --install /usr/bin/clang clang /usr/bin/clang-5.0 100 && \
    update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-5.0 100 && \
    update-alternatives --install /usr/bin/lldb lldb /usr/bin/lldb-5.0 100 

# Provision Python3 packages (using --no-cache-dir to avoid warnings)
RUN pip3 install --no-cache-dir --upgrade cbor \
                                          colorlog \
                                          pip \
                                          plumbum \
                                          psutil \
                                          typing 

USER $USER
# Provision rust language and packages (should not run as root)
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain $RUST_VER
RUN echo "source ~/.cargo/env" >> ~/.bashrc
RUN . $HOME/.cargo/env && \
    rustup run $RUST_VER cargo install --force rustfmt



