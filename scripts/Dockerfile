FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=America/Los_Angeles

RUN apt-get update
RUN apt install -y build-essential libbrotli-dev libbz2-dev libdb-dev libgcrypt20 libgdbm-dev libreadline-dev libidn2-dev libldap2-dev liblzma-dev libnghttp2-dev libpcre3-dev libpsl-dev librtmp-dev libtool libzstd-dev rcs tcl-dev tk-dev zlib1g-dev libsqlite3-dev clang cmake curl dirmngr git gnupg2 gperf htop libclang-dev libssl-dev llvm ninja-build pkg-config software-properties-common strace unzip libncurses5-dev

RUN useradd -m -u 1000 c2rust
USER c2rust
WORKDIR /home/c2rust

ADD requirements.txt /home/c2rust/
RUN curl -LsSf https://astral.sh/uv/install.sh | bash
RUN echo 'source $HOME/.local/bin/env' >> $HOME/.bashrc
RUN $HOME/.local/bin/uv venv
RUN $HOME/.local/bin/uv pip install -r "$HOME/requirements.txt"

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | bash -s -- -y
RUN echo 'source $HOME/.cargo/env' >> $HOME/.bashrc
RUN $HOME/.cargo/bin/rustup toolchain install -c rustfmt,rustc-dev,rust-src,miri,rust-analyzer nightly-2022-08-08
RUN $HOME/.cargo/bin/rustup toolchain install -c rustfmt,rustc-dev,rust-src,miri,rust-analyzer nightly-2023-04-15

VOLUME /c2rust
ENV PATH=/c2rust/target/release:$PATH
ENV C2RUST_DIR=/c2rust
