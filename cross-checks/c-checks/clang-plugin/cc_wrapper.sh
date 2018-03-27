#!/bin/sh
#
# This wrapper script wraps a call to clang with all the arguments that
# the plugin needs to run

if [ $# -lt 2 ]; then
    echo "Usage: $0 <compiler> <plugin> <arguments...>"
    exit 1
fi

PLUGIN_CC=$1
shift

PLUGIN=$1
shift

RUNTIME=$(readlink -f $(dirname $PLUGIN)/../runtime/libruntime.a)
if [ ! -e $RUNTIME ]; then
   unset RUNTIME
fi

exec "$PLUGIN_CC" -Xclang -load -Xclang "$PLUGIN" \
    -Xclang -add-plugin -Xclang crosschecks \
    -Wno-unknown-attributes "$@" "$RUNTIME"
