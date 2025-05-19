#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

$refactor \
    select str 'crate; desc(arg || field || fn); child(ty && match_ty(*const libc::c_char));' \; \
    select mut_str 'crate; desc(arg || field || fn); child(ty && match_ty(*mut libc::c_char));' \; \
    canonicalize_refs \; \
    autoretype 'str: *const u8' 'mut_str: *mut u8' \; \
    remove_unnecessary_refs \
    -- old.rs $rustflags
