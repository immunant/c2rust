#!/usr/bin/env bash

set -euox pipefail

# Run the same checks that are run in CI in `azure-pipelines.yml`.

# Deny all warnings, including in documentation.
if [[ "${ALLOW_WARNINGS:-0}" != "1" ]]; then
    export RUSTFLAGS="-D warnings"
    export RUSTDOCFLAGS="-D warnings"
fi

fmt() {
    cargo fmt --check
}

# Soon to be superceded by the commented out `cargo clippy` below.
# This is different from `cargo build`
# as this uses `--all-features` to check everything.
check() {
    cargo check --tests --all-features
    # cargo clippy --tests --all-features
}

doc() {
    cargo doc --all-features --document-private-items --no-deps
}

# At this point, we could unset `RUSTFLAGS` and `RUSTDOCFLAGS`,
# as we've already checked all the code,
# but doing so and then re-compiling would flush the caches,
# so we leave them until we're done compiling.

# Don't build with `--all-features` as `--all-features` includes `--features llvm-static`,
# which we don't want to test here (it doesn't work out of the box on Arch and Fedora;
# see https://github.com/immunant/c2rust/issues/500).
build() {
    cargo build --release
}

test() {
    cargo test --release --exclude c2rust-analyze --workspace
}

# `test_translatory.py` compiles translated code,
# which has tons of warnings.
# `RUSTFLAGS="-D warnings"` would be inherited by that,
# causing tons of errors, so unset that.
test-translator() {
    unset RUSTFLAGS
    ./scripts/test_translator.py tests/
}

all() {
    fmt
    check
    doc
    build
    test
    test-translator
}

"${1:-all}"
