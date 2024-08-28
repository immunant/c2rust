#!/usr/bin/env bash
#
# This script can be source'd to provide a few useful
# shell functions for analysis using the PDG.
#
# Environment Variables:
# * `RUST_PROFILE` (default `release`):
#       a `cargo` profile as in `target/$RUST_PROFILE`
# * `NO_USE_PDG` (default empty):
#       if non-empty, do not use the PDG as a starting point for analysis
# * `CARGO_INSTRUMENT_COMMAND` (default `build`):
#       the `cargo` command to run for instrumentation, e.g., `run` or `build`
# * `INSTRUMENT_OUTPUT_APPEND` (default `false`):
#       set to `true` to append instrumentation events to the log instead
#       of replacing the existing log on every invocation of the instrumented
#       binary

# Variables that are computed when the script is sourced
SCRIPT_PATH="$(realpath ${BASH_SOURCE[0]})"
SCRIPT_DIR="$(dirname "${SCRIPT_PATH}")"

# Variables that are exported to the functions below
export C2RUST_DIR="$(dirname "${SCRIPT_DIR}")"

_c2rust_prepare_vars() {
    test_dir="$(realpath ${1})"
    args=("${@:2}")

    profile_dir_name="${RUST_PROFILE:-release}"

    profile_dir="target/${profile_dir_name}"
    profile="${profile_dir_name}"
    if [[ "${profile}" == "debug" ]]; then
        profile=dev
    fi
    profile_args=(--profile "${profile}")

    metadata="${test_dir}/metadata.bc"
    pdg="${test_dir}/pdg.bc"
    event_log="${test_dir}/log.bc"
    runtime="${C2RUST_DIR}/analysis/runtime"
}

c2rust-set-instrument-vars() {
    _c2rust_prepare_vars "${@}"

    # We need these variables to have exactly these values
    export INSTRUMENT_RUNTIME=bg
    export INSTRUMENT_BACKEND=log
    export INSTRUMENT_OUTPUT="${event_log}"
    export METADATA_FILE="${metadata}"

    # These variables can be overridden by the user
    export INSTRUMENT_OUTPUT_APPEND=${INSTRUMENT_OUTPUT_APPEND:-false}
}

c2rust-instrument() (
    _c2rust_prepare_vars "${@}"

    unset RUSTFLAGS # transpiled code has tons of warnings; don't allow `-D warnings`
    export RUST_BACKTRACE=full

    cd "${C2RUST_DIR}"
    cargo run \
        --bin c2rust-instrument \
        "${profile_args[@]}" \
        -- \
        --metadata "${metadata}" \
        --set-runtime \
        --runtime-path "${runtime}" \
        -- "${CARGO_INSTRUMENT_COMMAND:-build}" \
        --manifest-path "${test_dir}/Cargo.toml" \
        "${profile_args[@]}" \
        -- "${args[@]}"
)

c2rust-pdg() (
    _c2rust_prepare_vars "${@}"

    export RUST_BACKTRACE=full # print sources w/ color-eyre
    export RUST_LOG=error

    cd "${C2RUST_DIR}"
    cargo run \
        --bin c2rust-pdg \
        "${profile_args[@]}" \
        -- \
        --event-log "${event_log}" \
        --metadata "${metadata}" \
        --print graphs \
        --print write-permissions \
        --print counts \
        --output "${pdg}" \
        > "${test_dir}/pdg.log"
)

c2rust-analyze-with-pdg() (
    _c2rust_prepare_vars "${@}"

    export RUST_BACKTRACE=full # print sources w/ color-eyre
    export RUST_LOG=error
    if [[ "${NO_USE_PDG:-}" == "" ]]; then
        # cargo runs this from a different pwd, so make path absolute
        export PDG_FILE="${pdg}"
    fi

    cd "${C2RUST_DIR}"
    cargo run \
        --bin c2rust-analyze \
        "${profile_args[@]}" \
        -- \
        build \
        -- \
        "${profile_args[@]}" \
        --features=c2rust-analysis-rt \
        --manifest-path "${test_dir}/Cargo.toml"
)
