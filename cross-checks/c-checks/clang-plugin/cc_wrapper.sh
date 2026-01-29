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
if [ ! -e $PLUGIN_CC ]; then
    echo "Compiler not found at '$PLUGIN_CC'" >&2
    exit 1
fi

PLUGIN=$1
shift
if [ ! -e $PLUGIN ]; then
    echo "Compiler plugin not found at '$PLUGIN'" >&2
    exit 1
fi

RUNTIME=$(readlink -f $(dirname $PLUGIN)/../runtime/libruntime.a)
if [ ! -e $RUNTIME ]; then
   unset RUNTIME
fi

exec "$PLUGIN_CC" -Xclang -load -Xclang "$PLUGIN" \
    -Xclang -add-plugin -Xclang crosschecks \
    -Wno-unknown-attributes "$@" "$RUNTIME"
