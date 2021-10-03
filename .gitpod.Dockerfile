FROM gitpod/workspace-full

# More information: https://www.gitpod.io/docs/config-docker/


SCRIPT_DIR="$(dirname "$0")/scripts"

USER root

# Also see scripts/provision_deb.sh

RUN apt-get update && \
    apt-get install -y \
       libncurses5-dev \
       luarocks \
       build-essential \
       llvm-6.0 \
       clang-6.0 \
       libclang-6.0-dev \
       libssl-dev \
       pkg-config

USER gitpod

RUN pip3 install -r $SCRIPT_DIR/requirements.txt --disable-pip-version-check --quiet

RUN luarocks path > /etc/profile.d/luarocks-path.sh
RUN luarocks install penlight
