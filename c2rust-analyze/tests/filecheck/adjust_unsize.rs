
// CHECK-LABEL: unsafe fn f
// CHECK-SAME: <'[[LT:[a-z0-9]+]]>(p: &'[[LT]] mut (u8)) {
unsafe fn f(p: *mut u8) {
    *p = 1;
}

// CHECK-LABEL: unsafe fn pass_arr_simple()
unsafe fn pass_arr_simple() {
    let mut arr: [u8; 3] = [0; 3];
    // CHECK: f(&mut (&mut (arr) as &mut [u8])[0]);
    f(arr.as_mut_ptr());
}

// CHECK-LABEL: unsafe fn pass_arr_explicit_ref()
unsafe fn pass_arr_explicit_ref() {
    let mut arr: [u8; 3] = [0; 3];
    // CHECK: f(&mut (&mut *((&mut arr)) as &mut [u8])[0]);
    f((&mut arr).as_mut_ptr());
}

// CHECK-LABEL: unsafe fn pass_arr_ufcs()
unsafe fn pass_arr_ufcs() {
    let mut arr: [u8; 3] = [0; 3];
    // CHECK: f(&mut (&mut *(&mut arr) as &mut [u8])[0]);
    f(<[_]>::as_mut_ptr(&mut arr));
}

