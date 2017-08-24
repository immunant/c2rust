#!/bin/sh
for f in */; do
    [ -f "$f/run.sh" ] || continue
    ./run-test.sh $f >$f/log 2>&1
    ret=$?
    if [ $ret -eq 0 ]; then
        echo "[ OK ] ${f%/}"
    else
        echo "[FAIL] ${f%/}"
    fi
done
