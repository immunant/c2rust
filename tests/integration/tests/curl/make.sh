#!/bin/bash
set -e; set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0" )" && pwd)"
make -C "$SCRIPT_DIR/repo" clean && rm -f compile_commands.json
bear -- make -C "$SCRIPT_DIR/repo" -j`nproc` 2>&1 \
    | tee `basename "$0"`.log

# remove compile_commands entries where `arguments` contains `UNITTEST` 
# this is necessary because tool_main.c only defines main if `UNITTESTS` is not defined
# I did not find a good way to disable unittests via the configure script which would have been cleaner
tmp=$(mktemp)
jq 'map(select((.arguments // []) | any(.[]; contains("DUNITTESTS")) | not))' $SCRIPT_DIR/compile_commands.json > "$tmp"
cp "$tmp" $SCRIPT_DIR/compile_commands.json

# work around https://github.com/immunant/c2rust/issues/1319
#
# each lib/curlx source is compiled both for libcurl (from `lib/`) and again for the curl tool (from `src/`),
# so the same `file` appears in multiple compile_commands.json entries. clang-15 may pick the wrong entry, which
# causes the transpiler to fail because it doesn't get the right include paths. clang-18 does not seem to have
# this problem which indicates the problem is not in the transpiler. keep the `lib/`-side entries so the curlx
# functions are still transpiled; dropping them all leaves `curlx_*` undefined when linking the tool binary.
# (`bear` records `file` as an absolute path, so match on `directory` rather than a relative `file` prefix.)
jq 'map(select(((.file | contains("lib/curlx")) and (.directory | endswith("/src"))) | not))' $SCRIPT_DIR/compile_commands.json > "$tmp"
mv "$tmp" $SCRIPT_DIR/compile_commands.json