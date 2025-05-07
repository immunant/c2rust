#!/bin/bash
set -euo pipefail

echo

f=$1
mode=$2
shift 2
flags=( "$@" )
echo "f=$f"
echo "mode=$mode"

name=${f%%.*.rs}
name=${name##**/}
echo "name=$name"

func=${f%.rs}
func=${func##*.}
echo "func=$func"

filter_errors() {
    jq 'select(.level == "error") | .message' -r |
        { grep -v -e '^aborting due to ' -e '^call to unsafe function is unsafe ' || true; }
}

case "$mode" in
    pointwise)
        sed -i -e "/fn $func\\>/s/\\<unsafe //" $f
        ;;
    unmodified)
        d="$(dirname "$f")"
        f="$d/${name}_safe_${func}.rs"
        cp "$d/$name.rs" "$f"
        sed -i -e "/fn $func\\>/s/\\<unsafe //" $f
        ;;
    *)
        echo "unsupported mode $mode" 1>&2
        exit 1
        ;;
esac

rustc --error-format json --emit metadata --crate-name $name "$f" "${flags[@]}" 2>rustc-$func.json || true
num_lines="$(cat rustc-$func.json | filter_errors | wc -l)"
echo "got $num_lines errors for $func"
if [[ "$num_lines" -eq 0 ]]; then
    exit 0
else
    cat rustc-$func.json | filter_errors
    exit 1
fi
