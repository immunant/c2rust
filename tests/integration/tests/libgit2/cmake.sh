#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
LOG_FILE="$(basename "$0")".log
BUILD_DIR="$SCRIPT_DIR/repo/build"

rm -f compile_commands.json

cmake -S "$SCRIPT_DIR/repo" -B "$BUILD_DIR" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DBUILD_SHARED_LIBS=OFF \
    -DBUILD_TESTS=OFF \
    -DBUILD_CLI=OFF \
    -DBUILD_EXAMPLES=OFF \
    -DBUILD_FUZZERS=OFF \
    -DUSE_HTTPS=OFF \
    -DUSE_SSH=OFF \
    -DUSE_SHA256=builtin \
    -DUSE_HTTP_PARSER=builtin \
    -DREGEX_BACKEND=builtin \
    -DUSE_BUNDLED_ZLIB=ON \
    -DUSE_NTLMCLIENT=OFF \
    -DUSE_ICONV=OFF \
    2>&1 | tee "$LOG_FILE"

# Configuring is enough: it generates compile_commands.json and every header
# that transpilation needs (e.g. gen_headers/{experimental,git2_features}.h).
# Building the library is not required to transpile, so we skip it.

# The test runner expects compile_commands.json in the fixture directory.
ln -sf repo/build/compile_commands.json compile_commands.json
