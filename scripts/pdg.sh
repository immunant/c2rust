#!/usr/bin/env bash

set -euox pipefail

SCRIPT_PATH="$(realpath ${0})"
SCRIPT_DIR="$(dirname "${SCRIPT_PATH}")"
C2RUST_DIR="$(dirname "${SCRIPT_DIR}")"

source ${SCRIPT_DIR}/pdg_setup.sh

# Usage: `./pdg.sh <test crate dir> <test binary args...>`
# 
# Environment Variables: see `scripts/pdg_functions.sh` for a full list.
# 
# Instrument and run a test crate, create its PDG, and then run analysis on it.
# 
# 1. Compile `c2rust-dynamic-instrumentation`.
# 2. Redirect `c2rust-dynamic-instrumentation` stdout to `instrument.out.log` in the test crate directory,
#    as it prints a lot of debugging info currently.
# 3. Run the instrumented binary directly.
#    The `bincode`-encoded event log is written to `log.bc`.
# 4. Using the `metadata.bc` metadata and the `log.bc` event log,
#    run `c2rust-pdg` to generate the pdg.
#    The output is saved to `pdg.log` (relative to the test crate directory).
#    A machine-readable PDG is saved to `pdg.bc` in the same directory.
# 5. Using the `pdg.bc` file as an initial state for analysis, run static
#    analysis using `c2rust-analyze`.

main() {
    CARGO_INSTRUMENT_COMMAND=run

    # set the environment variables for instrumentation
    c2rust-set-instrument-vars "${@}"
    # build and run a test with instrumentation
    c2rust-instrument "${@}"
    # construct pdg from log events
    c2rust-pdg "${@}"
    # use pdg in analysis
    c2rust-analyze-with-pdg "${@}"
}

main "${@}"
