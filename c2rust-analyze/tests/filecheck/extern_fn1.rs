use std::mem;

extern "C" {
    fn foo(_: i32) -> i32;
    fn malloc(_: u64) -> *mut u8;
    fn free(_: *mut u8);
}

/*
// TODO: calls to `malloc`/`free` library functions are not supported
unsafe fn test_malloc() -> u32 {
    let p = malloc(4) as *mut u32;
    (*p) = 1;
    let x = *p;
    free(p as *mut u8);
    x
}
*/

// CHECK-LABEL: final labeling for "fn_ptr"
fn fn_ptr() {
    // TODO: function pointer types are not fully supported yet
    //let f: unsafe extern "C" fn(i32) -> i32 = foo;
    let f = foo;
}
