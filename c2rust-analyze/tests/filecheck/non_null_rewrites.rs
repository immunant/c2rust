use std::ptr;

// CHECK-LABEL: unsafe fn f{{[<(]}}
pub unsafe fn f(cond: bool, p: *mut i32) {
    let mut p = p;
    if cond {
        // Both ways of writing null constants should be rewritten to `None`.

        // CHECK: p = None;
        p = ptr::null_mut();
        // CHECK: [[@LINE-1]]: ptr::null_mut():

        // CHECK: p = None;
        p = 0 as *mut _;
        // CHECK: [[@LINE-1]]: 0 as *mut _:
    }


    // CHECK: let ([[arr:.+]], [[idx:.+]], ) = ((p), (3) as usize, );
    // CHECK-NEXT: [[arr]].map(|arr| &arr{{\[}}[[idx]] ..])
    let q = p.offset(3);
}


// CHECK-LABEL: unsafe fn call_use_mut(
unsafe fn call_use_mut(cond: bool) -> i32 {
    let mut x = 1;
    let p = if cond {
        // CHECK: Some(&mut (x))
        ptr::addr_of_mut!(x)
    } else {
        // CHECK: None
        ptr::null_mut()
    };
    use_mut(p)
}

// CHECK-LABEL: unsafe fn use_mut{{[<(]}}
// CHECK-SAME: p: core::option::Option<&{{('[^ ]* )?}}mut (i32)>
unsafe fn use_mut(mut p: *mut i32) -> i32 {
    if !p.is_null() {
        // CHECK: *(p).as_deref_mut().unwrap() = 1;
        *p = 1;
    }
    // CHECK: use_const
    // CHECK-SAME: (p).as_deref()
    use_const(p)
}

// CHECK-LABEL: unsafe fn use_const{{[<(]}}
// CHECK-SAME: p: core::option::Option<&{{('[^ ]* )?}}(i32)>
unsafe fn use_const(p: *const i32) -> i32 {
    // CHECK: *(p).unwrap()
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


// CHECK-LABEL: unsafe fn downgrade_mut_to_imm_on_deref{{[<(]}}
// CHECK-SAME: p: core::option::Option<&{{('[^ ]* )?}}mut (i32)>
unsafe fn downgrade_mut_to_imm_on_deref(cond: bool, mut p: *mut i32) -> i32 {
    if cond {
        // Ensure `p` is wrapped in `Option`.
        p = ptr::null_mut();
    }
    // Ensure `p` is mutable.
    *p = 1;
    // This read needs a downgrade via `as_deref()` to avoid moving `p: Option<&mut _>`.
    // CHECK: let x = *(p).as_deref().unwrap();
    let x = *p;
    *p = 2;
    x
}

struct S {
    x: i32,
    y: i32,
}

// CHECK-LABEL: unsafe fn field_projection{{[<(]}}
// CHECK-SAME: p: core::option::Option<&{{('[^ ]* )?}}(S)>
unsafe fn field_projection(cond: bool, mut p: *const S) -> i32 {
    if cond {
        // Ensure `p` is wrapped in `Option`.
        p = ptr::null();
    }
    // Do a field projection.  This should become a `.map()` call.
    // TODO: Currently, we generate an incorrect rewrite for such projections
    // CHECK: let q: core::option::Option<&(i32)> =
    // CHECK-SAME: &(*(p).unwrap()).x
    let q: *const i32 = &(*p).x;
    *q
}
