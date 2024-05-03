use std::ptr;

// CHECK-LABEL: unsafe fn f{{[<(]}}
pub unsafe fn f(cond: bool, p: *mut i32) {
    let mut p = p;
    if cond {
        p = ptr::null_mut();
    }


    // CHECK: let ([[arr:.+]], [[idx:.+]], ) = ({{.*}}(p){{.*}}, (3) as usize, );
    // CHECK-NEXT: [[arr]].map(|arr| &arr[[[idx]] ..])
    let q = p.offset(3);
}


// CHECK-LABEL: unsafe fn call_use_mut(
unsafe fn call_use_mut(cond: bool) -> i32 {
    let mut x = 1;
    let p = if cond {

        ptr::addr_of_mut!(x)
    } else {
        ptr::null_mut()
    };
    use_mut(p)
}

// CHECK-LABEL: unsafe fn use_mut{{[<(]}}
unsafe fn use_mut(p: *mut i32) -> i32 {
    if !p.is_null() {
        *p = 1;
    }
    // CHECK: use_const
    // CHECK-SAME: (p).as_ref().as_deref()
    use_const(p)
}

// CHECK-LABEL: unsafe fn use_const{{[<(]}}
unsafe fn use_const(p: *const i32) -> i32 {
    *p
}
