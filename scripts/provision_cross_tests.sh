#!/bin/bash

set -e

if command -v sudo > /dev/null; then
    sudo=sudo
else
    sudo=""
fi

cargo install ripgrep
cargo install fd-find
cargo install cargo-zigbuild

${sudo} apt install -y qemu-user qemu-user-static binfmt-support
native_target="$(rustc -vV | rg '^host: (.*)$' --replace '$1')"
fd '^target-tuple$' "$(dirname "${0}")/.." --exec-batch cat | while read -r rust_target; do
    if [[ "${rust_target}" != "${native_target}" ]]; then
        c_target="${rust_target/-unknown/}"
        arch="${rust_target/-*/}"
		rustup target add "${rust_target}"
		${sudo} update-binfmts --enable "qemu-${arch}"
		${sudo} apt install -y "gcc-${c_target}"
	fi
done
