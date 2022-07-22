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

    local instrument="c2rust-dynamic-instrumentation"
    local c2rust="${CWD}/${profile_dir}/c2rust"
    local c2rust_instrument="${CWD}/${profile_dir}/${instrument}"
    local metadata="${test_dir}/metadata.bc"
    local event_log="${CWD}/${test_dir}/log.bc"
    local pdg_log="${test_dir}/pdg.log"

    export RUST_BACKTRACE=1
    export INSTRUMENT_BACKEND=log
    export INSTRUMENT_OUTPUT="${event_log}"
    export INSTRUMENT_OUTPUT_APPEND=false
    export METADATA_FILE="${metadata}"

    time "${c2rust_instrument}" \
        --metadata "${metadata}" \
        --cwd "${test_dir}" \
        -- run "${profile_args[@]}" \
        -- "${args[@]}" \
        1> instrument.out.log
    
    export RUST_BACKTRACE=full # print sources w/ color-eyre
    export RUST_LOG=error
    cargo run -p c2rust-pdg \
        "${profile_args[@]}" \
        -- \
        --event-log "${event_log}" \
        --metadata "${metadata}" \
        --print graphs \
        --print write-permissions \
        --print counts \
    > "${pdg_log}"
}

main "${@}"
