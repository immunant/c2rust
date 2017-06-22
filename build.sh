#!/bin/bash
set -e

cflags='-Isrc -DHAVE_CONFIG_H'
rustc=~/install/rust/build/x86_64-unknown-linux-gnu/stage2/bin/rustc
libs='-ldl -lrt -lpthread -lgcc_s -lc -lm -lutil -lncurses'
ldflags="-Wl,--no-as-needed -z now $libs"

gcc -o rfk-gcc src/robotfindskitten.c $cflags $ldflags
$rustc -Z print-link-args -o rfk-rust src/robotfindskitten.rs -C link-args="$ldflags"
