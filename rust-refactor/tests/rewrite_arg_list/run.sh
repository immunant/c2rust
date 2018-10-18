#!/bin/bash

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

cp old.rs old_new.rs

cmds=()

insert_remove() {
    local fn=$1
    local ins=$2
    local rem=$3
    cmds+=(
        select target "crate; desc(name(\"$fn\"));" \;
        test_insert_remove_args "$ins" "$rem" \;
        clear_marks \;
    )
}

insert_remove f_ins_end '1' ''
insert_remove f_ins_end_trail '1' ''
insert_remove f_ins_begin '0' ''
insert_remove f_ins_mid '1' ''
insert_remove f_ins_empty '0' ''

insert_remove f_ins2_end '1,1' ''
insert_remove f_ins2_end_trail '1,1' ''
insert_remove f_ins2_begin '0,0' ''
insert_remove f_ins2_mid '1,1' ''
insert_remove f_ins2_empty '0,0' ''

insert_remove f_del_first '' '0'
insert_remove f_del_last '' '1'
insert_remove f_del_both '' '0,1'
insert_remove f_del_only '' '0'
insert_remove f_del_only_trail '' '0'
insert_remove f_del_mid '' '1'

insert_remove f_insdel_last '1' '1'
insert_remove f_insdel_last_trail '1' '1'
insert_remove f_insdel2_last '1,2' '1,2'
insert_remove f_insdel2_last_trail '1,2' '1,2'

$idiomize_bin -r inplace \
    "${cmds[@]}" \
    -- old_new.rs $rustflags

cp old_new.rs old.new
