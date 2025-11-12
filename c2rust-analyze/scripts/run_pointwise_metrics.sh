#!/bin/bash
set -euo pipefail

# Run pointwise metrics on lighttpd_rust_amalgamated.

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <path/to/lighttpd_rust_amalgamated/>"
  exit 1
fi

SCRIPT_DIR="$(dirname "$0")"

# Get the path to lighttpd_rust_amalgamated
MODULE_DIR="$1"
shift 1

# Find the sysroot directory of rustc
SYSROOT="$(rustc --print sysroot)"

# Find the necessary rlibs
extern() {
  local name=$1
  local rlib=$(find "$MODULE_DIR/target/debug/deps" -name "lib${name}*.rlib" -print -quit)
  echo >&2 "found rlib for $name: $rlib"
  echo --extern $name=$rlib
}

now=$(date +%Y%m%d-%H%M%S)


# Set $rustc_flags and run the analysis as appropriate for the target project.
# $rustc_flags is also used below for `pointwise_try_build.sh`.
project="$(basename "$MODULE_DIR")"
case "$project" in
    lighttpd_*)
        # Make sure the project has been built first.  This ensures that the
        # `extern` function can find the libraries it needs.
        cargo build --manifest-path "$MODULE_DIR/Cargo.toml"

        rustc_flags=(
            --edition 2021
            --crate-type rlib
            #--sysroot "$SYSROOT"
            -L "dependency=$MODULE_DIR/target/debug/deps"
            $(extern c2rust_bitfields)
            $(extern libc)
            -A warnings
        )

        C2RUST_ANALYZE_NO_CARGO=1 \
        C2RUST_ANALYZE_REWRITE_MODE=pointwise \
        C2RUST_ANALYZE_USE_MANUAL_SHIMS=1 \
        cargo run --bin c2rust-analyze --release -- "$MODULE_DIR/src/main.rs" \
            --crate-name "$(basename "$MODULE_DIR")" \
            "${rustc_flags[@]}" \
            |& tee pointwise-lighttpd-analyze-$now.log \
            || true

        ;;

    cfs_*)
        cargo run --bin c2rust-analyze --release -- \
            --rewrite-mode pointwise --use-manual-shims -- \
            build --manifest-path "$MODULE_DIR/Cargo.toml" \
            |& tee pointwise-cfs-analyze-$now.log \
            || true

        rustc_flags=(
            --edition 2021
            --crate-type rlib
            #--sysroot "$SYSROOT"
            -L "dependency=$MODULE_DIR/target/debug/deps"
            $(extern c2rust_bitfields)
            $(extern f128)
            $(extern libc)
            $(extern memoffset)
            -A warnings
        )

        ;;

    *)
        echo "unsupported project $project" 1>&2
        exit 1
esac


# Try to compile each function separately.

pointwise_log_file=pointwise-lighttpd-pointwise-$now.log
for f in "$MODULE_DIR"/src/main.*.rs; do
    "$SCRIPT_DIR/pointwise_try_build.sh" "$f" pointwise "${rustc_flags[@]}" || true
done |& tee "$pointwise_log_file"

unmodified_log_file=pointwise-lighttpd-unmodified-$now.log
for f in "$MODULE_DIR"/src/main.*.rs; do
    "$SCRIPT_DIR/pointwise_try_build.sh" "$f" unmodified "${rustc_flags[@]}" || true
done |& tee "$unmodified_log_file"

echo
echo 

"$SCRIPT_DIR/pointwise_metrics.py" "$pointwise_log_file" "$unmodified_log_file"
