#!/bin/sh
set -e

# An unchanged preexisting error must not prevent an otherwise successful
# autoretype command.
$refactor autoretype -- old.rs $rustflags

# A preexisting error in a function must not hide errors introduced by
# retyping that same function.
if $refactor_bin -r print \
    select target 'crate; desc(arg); child(ty && match_ty(i32));' \; \
    autoretype 'target: String' \
    -- new_error.rs $rustflags >new_error.log 2>&1
then
    echo "autoretype accepted newly introduced type errors"
    exit 1
fi

grep -q 'Could not retype crate!: "Typechecking failed"' new_error.log
