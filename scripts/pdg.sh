#!/usr/bin/env bash

set -euox pipefail

CWD="${PWD}"
SCRIPT_PATH="${0}"
SCRIPT_DIR="${CWD}/$(dirname "${SCRIPT_PATH}")"

# Usage: `./pdg.sh <test crate dir> <test binary args...>`
# 
# Environment Variables:
# * `PROFILE` (default `release`):
#       a `cargo` profile as in `target/$PROFILE`
# 
# Instrument and run a test crate and create its PDG.
# 
# 1. Compile `c2rust-dynamic-instrumentation`.
# 3. Redirect `c2rust-dynamic-instrumentation` stdout to `instrument.out.log` in the test crate directory,
#    as it prints a lot of debugging info currently.
# 4. Run the instrumented binary directly.
#    The `bincode`-encoded event log is written to `log.bc`.
# 5. Using the `metadata.bc` metadata and the `log.bc` event log,
#    run `c2rust-pdg` to generate the pdg.
#    The output is saved to `pdg.log` (relative to the test crate directory).
#    A machine-readable PDG is saved to `pdg.bc` in the same directory.
main() {
    local test_dir="${1}"
    local args=("${@:2}")

    local profile_dir_name="${PROFILE:-release}"

    local profile_dir="target/${profile_dir_name}"
    local profile="${profile_dir_name}"
    if [[ "${profile}" == "debug" ]]; then
        profile=dev
    fi
    local profile_args=(--profile "${profile}")

    local metadata="${test_dir}/metadata.bc"
    local pdg="${test_dir}/pdg.bc"
    local event_log="${test_dir}/log.bc"
    local runtime="analysis/runtime"

    (
        unset RUSTFLAGS # transpiled code has tons of warnings; don't allow `-D warnings`
        export RUST_BACKTRACE=1
        export INSTRUMENT_RUNTIME=bg
        export INSTRUMENT_BACKEND=log
        export INSTRUMENT_OUTPUT="${event_log}"
        export INSTRUMENT_OUTPUT_APPEND=false
        export METADATA_FILE="${metadata}"

        cargo run \
            --bin c2rust-instrument \
            "${profile_args[@]}" \
            -- \
            --metadata "${metadata}" \
            --set-runtime \
            --runtime-path "${runtime}" \
            -- run \
            --manifest-path "${test_dir}/Cargo.toml" \
            "${profile_args[@]}" \
            -- "${args[@]}"
    )
    (
        export RUST_BACKTRACE=full # print sources w/ color-eyre
        export RUST_LOG=error
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
}

main "${@}"
