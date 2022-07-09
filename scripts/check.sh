#!/usr/bin/env bash

set -euox pipefail

export RUSTFLAGS="-D warnings"
export RUSTDOCFLAGS="-D warnings"

cargo fmt --check
cargo check --all-features
cargo build --features dynamic-instrumentation
cargo doc --all-features --document-private-items
cargo test --features dynamic-instrumentation
cargo clippy --tests --all --all-features
./scripts/test_translator.py tests/
