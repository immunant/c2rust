#!/bin/bash
set -euo pipefail

# Run pointwise metrics on cfs_rust_amalgamated.

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <path/to/cfs_rust_amalgamated/>"
  exit 1
fi

SCRIPT_DIR="$(dirname "$0")"

# Get the path to cfs_rust_amalgamated
MODULE_DIR="$1"
shift 1


now=$(date +%Y%m%d-%H%M%S)

# Run c2rust-analyze in pointwise mode
: cargo run --bin c2rust-analyze --release -- \
    --rewrite-mode pointwise --use-manual-shims -- \
    build --manifest-path "$MODULE_DIR/Cargo.toml" \
    |& tee pointwise-cfs-analyze-$now.log \
    || true

# Try to compile each function separately.

# Find the sysroot directory of rustc
SYSROOT="$(rustc --print sysroot)"

# Find the necessary rlibs
extern() {
  local name=$1
  local rlib=$(find "$MODULE_DIR/target/debug/deps" -name "lib${name}*.rlib" -print -quit)
  echo >&2 "found rlib for $name: $rlib"
  echo --extern $name=$rlib
}

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

pointwise_log_file=pointwise-cfs-pointwise-$now.log
for f in "$MODULE_DIR"/src/main.*.rs; do
    "$SCRIPT_DIR/pointwise_try_build.sh" "$f" "${rustc_flags[@]}" || true
done |& tee "$pointwise_log_file"

unmodified_log_file=pointwise-cfs-unmodified-$now.log
for f in "$MODULE_DIR"/src/main.*.rs; do
    "$SCRIPT_DIR/pointwise_try_build_unmodified.sh" "$f" "${rustc_flags[@]}" || true
done |& tee "$unmodified_log_file"

echo
echo 

python3 "$SCRIPT_DIR/pointwise_metrics.py" "$pointwise_log_file" "$unmodified_log_file"
