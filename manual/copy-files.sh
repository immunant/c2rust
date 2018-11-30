#!/bin/bash
dest="$1"
shift
tar -c "$@" | (cd "$dest"; tar -xv)
