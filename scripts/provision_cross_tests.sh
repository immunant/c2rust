#!/bin/bash

set -ex

is-command() {
    command -v "${1}" > /dev/null;
}

is-root() {
    [[ ${EUID} -eq 0 ]]
}

install-from-cargo() {
    cargo install ripgrep 
    cargo install fd-find 
    cargo install cargo-zigbuild
}

install-from-apt() {
    apt install -y qemu-user qemu-user-static binfmt-support
}

install-target() {
    local rust_target="${1}"
    c_target="${rust_target/-unknown/}"
    arch="${rust_target/-*/}"
    if is-root; then
        update-binfmts --enable "qemu-${arch}"
        apt install -y "gcc-${c_target}"
    else
        rustup target add "${rust_target}"
    fi
}

install-targets() {
    native_target="$(rustc -vV | rg '^host: (.*)$' --replace '$1')"
    fd '^target-tuple$' "$(dirname "${0}")/.." --exec-batch cat | while read -r rust_target; do
        if [[ "${rust_target}" != "${native_target}" ]]; then
            install-target "${rust_target}"
    	fi
    done
}

# Should be run as non-root first (after rustup installed), and then as root.
main() {
    is-command apt || return
    if is-root; then
        install-from-apt
        install-targets
        apt clean
    else
        install-from-cargo
        install-targets
    fi
}
main
