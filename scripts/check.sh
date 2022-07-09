#!/usr/bin/env bash

set -euox pipefail

export RUSTFLAGS="-D warnings"
export RUSTDOCFLAGS="-D warnings"

cargo fmt --check
cargo build --all-features
cargo doc --all-features --document-private-items
cargo test --all-features
cargo clippy --tests --all --all-features
./scripts/test_translator.py tests/
