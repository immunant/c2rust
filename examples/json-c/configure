#!/bin/bash

# Custom configure script for json-c.  Use this in place of ./configure to
# build json-c for translation.

cd "$(dirname "$0")"/repo

# Prevent library from using compare-and-swap intrinsics, which c2rust doesn't
# support.
CFLAGS="$CFLAGS -U__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2"
CFLAGS="$CFLAGS -U__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4"
CFLAGS="$CFLAGS -U__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8"

config_args=(
    "CFLAGS=$CFLAGS"
    # Always behave as if __thread (thread-local variables) is unavailable.
    ac_cv___thread=no
    )

./configure "${config_args[@]}" "$@"
