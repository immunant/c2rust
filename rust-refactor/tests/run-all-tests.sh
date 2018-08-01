#!/bin/sh
exit_code=0
for f in */; do
    [ -f "$f/run.sh" ] || continue
    ./run-test.sh $f >$f/log 2>&1
    ret=$?
    if [ $ret -eq 0 ]; then
        echo "[ OK ] ${f%/}"
    else
        echo "[FAIL] ${f%/}"
	exit_code=$ret
    fi
done
exit $exit_code
