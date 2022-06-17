#!/usr/bin/env bash

set -euox pipefail

main() {
    local script_path="${0}"
    local test_dir="${1}"
    local args=("${@:2}")

    local profile_dir_name="${PROFILE:-release}"
    local cwd="${PWD}"

    local script_dir="${cwd}/$(dirname "${script_path}")"

    local profile_dir="target/${profile_dir_name}"
    local profile="${profile_dir_name}"
    if [[ "${profile}" == "debug" ]]; then
        profile=dev
    fi
    local profile_args=(--profile "${profile}")

    cargo build "${profile_args[@]}" --features dynamic-instrumentation

    export RUST_BACKTRACE=1
    unset RUSTC_WRAPPER

    local rustc_path="$(rustup which rustc)"
    local toolchain_dir="$(dirname "$(dirname "${rustc_path}")")"

    local c2rust="${cwd}/${profile_dir}/c2rust"
    local c2rust_instrument="${cwd}/${profile_dir}/c2rust-instrument"
    local runtime="${cwd}/analysis/runtime/"
    local metadata="${cwd}/${test_dir}/metadata.bc"

    (cd "${test_dir}"
        local binary_name="$(command cargo metadata --format-version 1 \
            | "${script_dir}/get-binary-names-from-cargo-metadata.mjs" default)"
        local profile_dir="target/debug" # always dev/debug for now
        local binary_path="${profile_dir}/${binary_name}"
        [[ -x "${binary_path}" ]]
        
        if [[ "${c2rust_instrument}" -nt "${metadata}" ]]; then
            cargo clean --profile dev # always dev/debug for now

            LD_LIBRARY_PATH="${toolchain_dir}/lib" \
            "${c2rust}" instrument \
                "${metadata}" "${runtime}" \
                -- "${profile_args[@]}"  \
            1> instrument.out.log \
            2> instrument.err.log
        fi
        
        RUSTFLAGS=" ${RUSTFLAGS:-} -Awarnings " \
        INSTRUMENT_BACKEND=log \
        INSTRUMENT_OUTPUT=log.bc \
        METADATA_FILE="${metadata}" \
        "${binary_path}" "${args[@]}"
    )

    (cd pdg
        RUST_LOG=info \
        METADATA_FILE="${metadata}" \
        cargo run \
            "${profile_args[@]}" \
            -- "../${test_dir}/log.bc" \
        &> "../${test_dir}/pdg.log"
    )
}

main "${@}"
