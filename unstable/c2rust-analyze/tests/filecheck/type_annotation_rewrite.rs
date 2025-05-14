// CHECK-DAG: pub unsafe fn let_decl()
pub unsafe fn let_decl() {
    let mut z = 2;
    // CHECK-DAG: let x: &(i32);
    let x: *mut i32;
    x = std::ptr::addr_of_mut!(z);
}

// CHECK-DAG: pub unsafe fn let_decl_mut()
pub unsafe fn let_decl_mut() {
    let mut z = 2;
    // CHECK-DAG: let x: &mut (i32);
    let x: *mut i32;
    x = std::ptr::addr_of_mut!(z);
    *x = *x;
}

// CHECK-DAG: pub unsafe extern "C" fn offset_mut<'h0>(p: &'h0 mut [(i32)]) {
pub unsafe extern "C" fn offset_mut(p: *mut i32) {
    // CHECK-DAG: let x: &mut (i32) = &mut (&mut (p)[((1 as isize) as usize) ..])[0];
    let x: *mut i32 = p.offset(1 as isize);
    *x = 1;
}

// CHECK-DAG: pub unsafe fn aggregate1_array<'h0>(p: &'h0 core::cell::Cell<(i32)>)
pub unsafe fn aggregate1_array(p: *mut i32) {
    // CHECK-DAG: let arr: [&core::cell::Cell<(i32)>; 3] = [p, p, p];
    let arr: [*mut i32; 3] = [p, p, p];
    *arr[0] = 1;
}

// CHECK-DAG: pub unsafe fn aggregate1_array1<'h0>(p: &'h0 mut (i32))
pub unsafe fn aggregate1_array1(p: *mut i32) {
    // CHECK-DAG: let arr: [&mut (i32); 1] = [p];
    let arr: [*mut i32; 1] = [p];
    *arr[0] = 1;
}
