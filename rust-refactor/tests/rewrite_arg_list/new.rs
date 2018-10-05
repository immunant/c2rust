// The empty comments before and after each function's argument list are meant to detect (and fail
// the test) if rewriting resorts to reprinting the entire item.

// Insert arg at end, without trailing comma
fn f_ins_end /**/ (x: (), new_arg0: ()) /**/ {}

// Insert arg at end, with trailing comma
fn f_ins_end_trail /**/ (x: (),new_arg0: ()) /**/ {}

// Insert arg at beginning
fn f_ins_begin /**/ (new_arg0: (), x: ()) /**/ {}

// Insert arg in the middle
fn f_ins_mid /**/ (x: (),new_arg0: (),  y: ()) /**/ {}

// Insert arg into empty list
fn f_ins_empty /**/ (new_arg0: ()) /**/ {}


// Insert 2 args at end, without trailing comma
fn f_ins2_end /**/ (x: (), new_arg0: (), new_arg1: ()) /**/ {}

// Insert 2 args at end, with trailing comma
fn f_ins2_end_trail /**/ (x: (),new_arg0: (), new_arg1: ()) /**/ {}

// Insert 2 args at beginning
fn f_ins2_begin /**/ (new_arg0: (), new_arg1: (), x: ()) /**/ {}

// Insert 2 args in the middle
fn f_ins2_mid /**/ (x: (),new_arg0: (), new_arg1: (),  y: ()) /**/ {}

// Insert 2 args into empty list
fn f_ins2_empty /**/ (new_arg0: (), new_arg1: ()) /**/ {}


// Delete the first arg in the list
fn f_del_first /**/ ( y: ()) /**/ {}

// Delete the last arg in the list
fn f_del_last /**/ (x: (), ) /**/ {}

// Delete both args in the list
fn f_del_both /**/ ( ) /**/ {}

// Delete the only arg in the list
fn f_del_only /**/ () /**/ {}

// Delete the only arg in the list, with trailing comma
fn f_del_only_trail /**/ () /**/ {}

// Delete an arg from the middle of the list
fn f_del_mid /**/ (x: (),  z: ()) /**/ {}


// Replace the last argument in the list
fn f_insdel_last /**/ (x: (), new_arg0: ()) /**/ {}

// Replace the last argument in the list, with trailing comma
fn f_insdel_last_trail /**/ (x: (), new_arg0: (),) /**/ {}

// Replace the last 2 arguments in the list
fn f_insdel2_last /**/ (x: (), new_arg0: (), new_arg1: ()) /**/ {}

// Replace the last 2 arguments in the list, with trailing comma
fn f_insdel2_last_trail /**/ (x: (), new_arg0: (), new_arg1: (),) /**/ {}


fn main() {}
