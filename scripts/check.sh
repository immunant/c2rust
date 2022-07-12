#!/usr/bin/env bash

set -euox pipefail

export RUSTFLAGS="-D warnings"
export RUSTDOCFLAGS="-D warnings"

cargo fmt --check
cargo check --tests --all-features
# cargo clippy --tests --all-features
cargo doc --all-features --document-private-items
cargo build --features dynamic-instrumentation --release
cargo test --features dynamic-instrumentation --release
unset RUSTFLAGS # transpiled code has tons of warnings; don't allow `-D warnings`
./scripts/test_translator.py tests/
