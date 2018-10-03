#!/bin/sh

# work around System Integrity Protection on macOS
if [ `uname` = 'Darwin' ]; then
    export LD_LIBRARY_PATH=$not_LD_LIBRARY_PATH
fi

cp old.rs old_new.rs

$idiomize_bin -r inplace \
    select target 'crate; desc(name("f_ins_end"));' \; \
    test_insert_remove_args '1' '' \; \
    select target 'crate; desc(name("f_ins_end_trail"));' \; \
    test_insert_remove_args '1' '' \; \
    select target 'crate; desc(name("f_ins_begin"));' \; \
    test_insert_remove_args '0' '' \; \
    select target 'crate; desc(name("f_ins_mid"));' \; \
    test_insert_remove_args '1' '' \; \
    select target 'crate; desc(name("f_ins_empty"));' \; \
    test_insert_remove_args '0' '' \; \
    select target 'crate; desc(name("f_ins2_end"));' \; \
    test_insert_remove_args '1,1' '' \; \
    select target 'crate; desc(name("f_ins2_end_trail"));' \; \
    test_insert_remove_args '1,1' '' \; \
    select target 'crate; desc(name("f_ins2_begin"));' \; \
    test_insert_remove_args '0,0' '' \; \
    select target 'crate; desc(name("f_ins2_mid"));' \; \
    test_insert_remove_args '1,1' '' \; \
    select target 'crate; desc(name("f_ins2_empty"));' \; \
    test_insert_remove_args '0,0' '' \; \
    select target 'crate; desc(name("f_del_first"));' \; \
    test_insert_remove_args '' '0' \; \
    select target 'crate; desc(name("f_del_last"));' \; \
    test_insert_remove_args '' '1' \; \
    select target 'crate; desc(name("f_del_both"));' \; \
    test_insert_remove_args '' '0,1' \; \
    select target 'crate; desc(name("f_del_only"));' \; \
    test_insert_remove_args '' '0' \; \
    select target 'crate; desc(name("f_del_only_trail"));' \; \
    test_insert_remove_args '' '0' \; \
    select target 'crate; desc(name("f_del_mid"));' \; \
    test_insert_remove_args '' '1' \; \
    select target 'crate; desc(name("f_insdel_last"));' \; \
    test_insert_remove_args '1' '1' \; \
    select target 'crate; desc(name("f_insdel_last_trail"));' \; \
    test_insert_remove_args '1' '1' \; \
    select target 'crate; desc(name("f_insdel2_last"));' \; \
    test_insert_remove_args '1,2' '1,2' \; \
    select target 'crate; desc(name("f_insdel2_last_trail"));' \; \
    test_insert_remove_args '1,2' '1,2' \; \
    -- old_new.rs $rustflags

cp old_new.rs old.new
