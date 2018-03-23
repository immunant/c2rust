#!/bin/bash
#
# This file builds a C2Rust translated version of snudown,
# either with or without cross-checks
#
# Usage:
# $ ./translate.sh translate
# or
# $ ./translate.sh rustcheck

MACHINE_NAME=`uname -n`
MACHINE_TYPE=`uname -s`

LIB_PATH=""
if [ $MACHINE_TYPE == "Darwin" ]; then
  LIB_PATH=$HOME/.rustup/toolchains/nightly-2018-01-06-x86_64-apple-darwin/lib
else 
  LIB_PATH=$HOME/.rustup/toolchains/nightly-2018-01-06-x86_64-unknown-linux-gnu/lib
fi

C2RUST=$(readlink -f $(dirname $0)/../..)
SNUDOWN=$(readlink -f $(dirname $0)/repo)
AST_EXTRACTOR=$C2RUST/dependencies/llvm-6.0.0/build.$MACHINE_NAME/bin/ast-extractor
AST_IMPORTER=$C2RUST/ast-importer/target/debug/ast_importer
RUSTFMT=rustfmt

XCHECK_TOPDIR=$C2RUST/cross-checks/rust-checks
XCHECK_PLUGIN=$XCHECK_TOPDIR/rustc-plugin/target/debug/libcross_check_plugin.so
XCHECK_DERIVE=$XCHECK_TOPDIR/derive-macros/target/debug/libcross_check_derive.so
XCHECK_RUNTIME=$XCHECK_TOPDIR/runtime/target/debug/libcross_check_runtime.rlib

# FIXME: this should be an absolute path, but rustc-plugin cannot handle
# absolute paths for the external configuration
#OUTPUT_DIR=$SNUDOWN/translator-build
OUTPUT_DIR=translator-build

# Stop on first error
set -e

translate() {
  $AST_EXTRACTOR $SNUDOWN/src/$1.c
  env RUST_BACKTRACE=1 LD_LIBRARY_PATH=$LIB_PATH $AST_IMPORTER --reloop-cfgs $SNUDOWN/src/$1.c.cbor > $OUTPUT_DIR/$1.rs
  #$RUSTFMT $OUTPUT_DIR/$1.rs --force
  rustc --crate-type=rlib --crate-name=$1 $OUTPUT_DIR/$1.rs -o $OUTPUT_DIR/lib$1.rlib
}

translate_xcheck() {
  $AST_EXTRACTOR $SNUDOWN/src/$1.c
  env RUST_BACKTRACE=1 LD_LIBRARY_PATH=$LIB_PATH \
      $AST_IMPORTER --reloop-cfgs --cross-checks \
      --cross-check-config $SNUDOWN/../snudown_rust.c2r \
      -- $SNUDOWN/src/$1.c.cbor > $OUTPUT_DIR/$1.rs
  #$RUSTFMT $OUTPUT_DIR/$1.rs --force
  rustc -g --crate-type=rlib --crate-name=$1 \
      --extern cross_check_plugin=$XCHECK_PLUGIN \
      --extern cross_check_derive=$XCHECK_DERIVE \
      --extern cross_check_runtime=$XCHECK_RUNTIME \
      $OUTPUT_DIR/$1.rs -o $OUTPUT_DIR/lib$1.rlib \
      #--Z unstable-options --pretty=expanded \
}

compile_commands_entry() {

        cat >> compile_commands.json <<END
{
  "directory": "${SNUDOWN}",
  "command": "cc -o ${OUTPUT_DIR}/${1}.c.o -c ${SNUDOWN}/src/${1}.c -Wwrite-strings -D_FORTIFY_SOURCE=0 -DNDEBUG=1",
  "file": "${SNUDOWN}/src/${1}.c"
},
END

}

# Generate html_entities.h from html_entities.gpers (setup.py used to do this)
`cd $SNUDOWN/src && gperf html_entities.gperf --output-file=html_entities.h`

if [ "${1}" == "translate" ]; then
  echo "[" > compile_commands.json
  compile_commands_entry "autolink"
  compile_commands_entry "buffer"
  compile_commands_entry "stack"
  compile_commands_entry "markdown"
  echo "]" >> compile_commands.json

  mkdir -p $OUTPUT_DIR
 
  translate "autolink"
  translate "buffer"
  translate "stack"
  translate "markdown"
 
  rustc --crate-name=snudownrust --crate-type=staticlib -L $OUTPUT_DIR \
      $C2RUST/examples/snudown/snudownrust.rs -o $OUTPUT_DIR/libsnudownrust.a

elif [ "$1" == "rustcheck" ]; then

  echo "[" > compile_commands.json
  compile_commands_entry "autolink"
  compile_commands_entry "buffer"
  compile_commands_entry "stack"
  compile_commands_entry "markdown"
  echo "]" >> compile_commands.json

  mkdir -p $OUTPUT_DIR

  translate_xcheck "autolink"
  translate_xcheck "buffer"
  translate_xcheck "stack"
  translate_xcheck "markdown"

  rustc --crate-name=snudownrust --crate-type=staticlib -L $OUTPUT_DIR \
      --extern cross_check_derive=$XCHECK_DERIVE \
      --extern cross_check_runtime=$XCHECK_RUNTIME \
      --cfg "feature=\"cross-check\"" \
      $C2RUST/examples/snudown/snudownrust.rs -o $OUTPUT_DIR/libsnudownrustxcheck.a

fi
