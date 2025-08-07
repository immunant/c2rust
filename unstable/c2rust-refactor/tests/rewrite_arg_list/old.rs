// The empty comments before and after each function's argument list are meant to detect (and fail
// the test) if rewriting resorts to reprinting the entire item.

// Insert arg at end, without trailing comma
fn f_ins_end /**/ (x: ()) /**/ {}

// Insert arg at end, with trailing comma
fn f_ins_end_trail /**/ (x: (),) /**/ {}

// Insert arg at beginning
fn f_ins_begin /**/ (x: ()) /**/ {}

// Insert arg in the middle
fn f_ins_mid /**/ (x: (), y: ()) /**/ {}

// Insert arg into empty list
fn f_ins_empty /**/ () /**/ {}


// Insert 2 args at end, without trailing comma
fn f_ins2_end /**/ (x: ()) /**/ {}

// Insert 2 args at end, with trailing comma
fn f_ins2_end_trail /**/ (x: (),) /**/ {}

// Insert 2 args at beginning
fn f_ins2_begin /**/ (x: ()) /**/ {}

// Insert 2 args in the middle
fn f_ins2_mid /**/ (x: (), y: ()) /**/ {}

// Insert 2 args into empty list
fn f_ins2_empty /**/ () /**/ {}


// Delete the first arg in the list
fn f_del_first /**/ (x: (), y: ()) /**/ {}

// Delete the last arg in the list
fn f_del_last /**/ (x: (), y: ()) /**/ {}

// Delete both args in the list
fn f_del_both /**/ (x: (), y: ()) /**/ {}

// Delete the only arg in the list
fn f_del_only /**/ (x: ()) /**/ {}

// Delete the only arg in the list, with trailing comma
fn f_del_only_trail /**/ (x: (),) /**/ {}

// Delete an arg from the middle of the list
fn f_del_mid /**/ (x: (), y: (), z: ()) /**/ {}


// Replace the last argument in the list
fn f_insdel_last /**/ (x: (), y: ()) /**/ {}

// Replace the last argument in the list, with trailing comma
fn f_insdel_last_trail /**/ (x: (), y: (),) /**/ {}

// Replace the last 2 arguments in the list
fn f_insdel2_last /**/ (x: (), y: (), z: ()) /**/ {}

// Replace the last 2 arguments in the list, with trailing comma
fn f_insdel2_last_trail /**/ (x: (), y: (), z: (),) /**/ {}


fn main() {}
