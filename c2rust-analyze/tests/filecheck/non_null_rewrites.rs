use std::ptr;

// CHECK-LABEL: unsafe fn f{{[<(]}}
pub unsafe fn f(cond: bool, p: *mut i32) {
    let mut p = p;
    if cond {
        p = ptr::null_mut();
    }


    // CHECK: let ([[arr:.+]], [[idx:.+]], ) = ((p), (3) as usize, );
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
// CHECK-SAME: p: core::option::Option<&{{('[^ ]* )?}}mut (i32)>
unsafe fn use_mut(p: *mut i32) -> i32 {
    if !p.is_null() {
        *p = 1;
    }
    // CHECK: use_const
    // CHECK-SAME: (p).as_deref()
    use_const(p)
}

// CHECK-LABEL: unsafe fn use_const{{[<(]}}
// CHECK-SAME: p: core::option::Option<&{{('[^ ]* )?}}(i32)>
unsafe fn use_const(p: *const i32) -> i32 {
    *p
}

// CHECK-LABEL: unsafe fn call_use_slice{{[<(]}}
// CHECK-SAME: q: &{{('[^ ]* )?}}[(i32)]
unsafe fn call_use_slice(cond: bool, q: *const i32) -> i32 {
    // `q` is not nullable, so `q.is_null()` should be rewritten to `false`.
    // CHECK: if !false {
    if !q.is_null() {
        // No-op
    }
    let p = if cond {
        // CHECK: Some((q))
        q
    } else {
        // CHECK: None
        ptr::null_mut()
    };
    use_slice(p)
}

// CHECK-LABEL: unsafe fn use_slice{{[<(]}}
// CHECK-SAME: p: core::option::Option<&{{('[^ ]* )?}}[(i32)]>
unsafe fn use_slice(p: *const i32) -> i32 {
    // `p`'s new type is `Option<&[i32]>`, so `is_null()` should become `is_none()`.
    // CHECK: .is_none()
    if !p.is_null() {
        let x = *p.offset(1);
    }
    // CHECK: use_single((p).map(|__ptr| &__ptr[0]))
    use_single(p)
}

// CHECK-LABEL: unsafe fn use_single{{[<(]}}
// CHECK-SAME: p: core::option::Option<&{{('[^ ]* )?}}(i32)>
unsafe fn use_single(p: *const i32) -> i32 {
    *p
}
