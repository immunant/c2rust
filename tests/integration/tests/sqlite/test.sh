#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/repo"

{
    TARGET_DIR=$(cargo ${TOOLCHAIN:+"$TOOLCHAIN"} metadata --no-deps --format-version 1 \
        | jq -er .target_directory)
    RUST_ARCHIVE="$TARGET_DIR/release/librepo.a"
    if [[ ! -f "$RUST_ARCHIVE" ]]; then
        echo "Rust archive not found: $RUST_ARCHIVE" >&2
        exit 1
    fi
    WORK_DIR=$(mktemp -d)
    trap 'rm -rf "$WORK_DIR"' EXIT

    # Ask rustc for the native dependencies of this static archive.
    RUSTFLAGS="${RUSTFLAGS:--Awarnings}" cargo ${TOOLCHAIN:+"$TOOLCHAIN"} rustc \
        --release --lib --color never -- --print native-static-libs \
        2>&1 | tee "$WORK_DIR/native-libs.log"
    LIBS=$(sed -n 's/^note: native-static-libs: //p' "$WORK_DIR/native-libs.log")
    if [[ -z "$LIBS" ]]; then
        echo "rustc did not report native static libraries" >&2
        exit 1
    fi
    read -r -a RUST_LIBS <<< "$LIBS"

    # Explicit archives keep both C callers from picking up a system SQLite.
    for implementation in native rust; do
        if [[ "$implementation" == native ]]; then
            archive="$SCRIPT_DIR/repo/libsqlite3.a"
            libs=(-lm -ldl -lpthread)
        else
            archive="$RUST_ARCHIVE"
            libs=("${RUST_LIBS[@]}")
        fi
        cc -O2 -I. "$SCRIPT_DIR/smoke.c" "$archive" "${libs[@]}" \
            -o "$WORK_DIR/smoke-$implementation"
        cc -O2 -DSQLITE_ENABLE_RTREE -I. test/speedtest1.c "$archive" "${libs[@]}" \
            -o "$WORK_DIR/$implementation"
        "$WORK_DIR/smoke-$implementation" "$WORK_DIR/smoke-$implementation.db"
    done

    # Only these workloads hash result rows. JSON and numeric parsing have
    # explicit assertions in smoke.c instead of comparisons of empty hashes.
    for testset in main cte orm fp rtree; do
        for implementation in native rust; do
            "$WORK_DIR/$implementation" --size 1 --verify --testset "$testset" \
                "$WORK_DIR/$implementation.db" \
                | tee "$WORK_DIR/$implementation.out"
            # Timings differ; the result count and verification hash must match.
            grep '^Verification Hash: ' "$WORK_DIR/$implementation.out" \
                > "$WORK_DIR/$implementation.hash"
        done
        diff -u "$WORK_DIR/native.hash" "$WORK_DIR/rust.hash"
    done
} 2>&1 | tee "$SCRIPT_DIR/$(basename "$0").log"
