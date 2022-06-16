#!/usr/bin/env bash

set -euox pipefail

cargo() {
    command cargo "${1}" --profile "${CARGO_PROFILE}" "${@:2}"
}

main() {
    local test_dir="${1}"
    local args="${@:2}"

    local profile_dir_name="${PROFILE:-debug}"
    local cwd="${PWD}"

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

    cargo build --features dynamic-instrumentation

    export RUST_BACKTRACE=1
    unset RUSTC_WRAPPER

    local rustc_path="$(rustup which rustc)"
    local toolchain_dir="$(dirname "$(dirname "${rustc_path}")")"

    local c2rust="${cwd}/${profile_dir}/c2rust"
    local c2rust_instrument="${cwd}/${profile_dir}/c2rust-instrument"
    local runtime="${cwd}/analysis/runtime/"
    local metadata="${cwd}/${test_dir}/metadata.bc"

    (cd "${test_dir}"
        if [[ "${c2rust_instrument}" -nt "${metadata}" ]]; then
            cargo clean

            LD_LIBRARY_PATH="${toolchain_dir}/lib" \
            "${c2rust}" instrument \
                "${metadata}" "${runtime}" \
                -- --profile "${CARGO_PROFILE}" \
            1> instrument.out.log \
            2> instrument.err.log
        fi
        
        RUSTFLAGS=" ${RUSTFLAGS:-} -Awarnings " \
        INSTRUMENT_BACKEND=log \
        INSTRUMENT_OUTPUT=log.bc \
        METADATA_FILE="${metadata}" \
        cargo run -- "${args[@]}"
    )

    (cd pdg
        RUST_LOG=info METADATA_FILE="${metadata}" cargo run -- "../${test_dir}/log.bc" &> "../${test_dir}/pdg.log"
    )
}

main "${@}"
