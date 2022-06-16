#!/usr/bin/env bash

set -euox pipefail

cargo() {
    command cargo "${1}" --profile "${CARGO_PROFILE}" "${@:2}"
}

main() {
    local profile_dir_name="${1:-debug}"
    if [[ "${profile_dir_name}" != "debug" ]]; then
        echo >&2 "only debug works right now, see https://github.com/immunant/c2rust/issues/448"
        return 1
    fi

    local profile_dir="target/${profile_dir_name}"
    local profile="${profile_dir_name}"
    if [[ "${profile}" == "debug" ]]; then
        profile=dev
    fi
    export CARGO_PROFILE="${profile}"

    test_dir=analysis/test

    cargo build --features dynamic-instrumentation

    export RUST_BACKTRACE=1
    unset RUSTC_WRAPPER

    rustc_path="$(rustup which rustc)"
    toolchain_dir="$(dirname "$(dirname "${rustc_path}")")"

    (cd "${test_dir}"
        cargo clean
        LD_LIBRARY_PATH="${toolchain_dir}/lib" ../../${profile_dir}/c2rust instrument metadata.bc ../runtime/ -- --profile "${CARGO_PROFILE}"
        INSTRUMENT_BACKEND=log INSTRUMENT_OUTPUT=log.bc METADATA_FILE=metadata.bc ${profile_dir}/c2rust-analysis-test
    )
    return

    (cd pdg
        RUST_LOG=info METADATA_FILE="../${test_dir}/metadata.bc" cargo run -- "../${test_dir}/log.bc"
    )
}

main "${@}"
