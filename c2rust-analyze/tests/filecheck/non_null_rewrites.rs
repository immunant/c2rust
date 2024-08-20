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
    // The first use of `p` must borrow; the second and final use can move.
    // CHECK: use_const
    // CHECK-SAME: (p).as_deref()
    let x = use_const(p);
    // CHECK: use_const
    // CHECK-SAME: (p).map(|ptr| &*ptr)
    // CHECK-NOT: as_deref
    let y = use_const(p);
    x + y
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

    // Do a field projection.  This becomes an `unwrap()` + project + `Some(_)`.
    // CHECK: let q: core::option::Option<&(i32)> = std::option::Option::Some(&((*(p).unwrap()).x));
    let q: *const i32 = ptr::addr_of!((*p).x);

    // Same projection, but using `&` instead of `addr_of!`.  This produces an equivalent but more
    // convoluted result due to the implicit `addr_of!(*_)` adjustment.
    // CHECK: let q: core::option::Option<&(i32)> = std::option::Option::Some(&*std::option::Option::Some((&((*(p).unwrap()).x))).unwrap());
    let q: *const i32 = &((*p).x);

    *q
}

// CHECK-LABEL: unsafe fn local_field_projection{{[<(]}}
unsafe fn local_field_projection(cond: bool) -> i32 {
    let s = S { x: 1, y: 2 };

    // Obtain a pointer to a field of a local.  This should produce `Some(&s.x)`.
    // CHECK: let mut p: core::option::Option<&(i32)> = std::option::Option::Some(&*(&s.x));
    let mut p: *const i32 = &s.x;

    if cond {
        // Ensure `p` is wrapped in `Option`.
        p = ptr::null();
    }
    *p
}

// CHECK-LABEL: unsafe fn null_ptr_special_cases{{[<(]}}
unsafe fn null_ptr_special_cases(cond: bool, p: *const i32, i: isize) -> *const i32 {
    // In C, `*NULL` is usually undefined behavior, even in cases like `sizeof(*(char*)0)` that
    // don't involve an actual memory access.  However, there are two special cases in the
    // standard:
    //
    // 1. `&*p == p` for all `p`, even if `p == NULL`.
    // 2. `&p[i] == p + i` for all `p`, even if `p == NULL`.  Note that `NULL + 0 == NULL`, but
    //    `NULL + i` with nonzero `i` is undefined.
    //
    // Here we test these two cases to see if rewriting introduces a panic even in cases where the
    // C operation is valid.

    // Make `p` nullable.
    let mut p = p;
    if cond {
        p = ptr::null();
    }

    // Currently, `&*p` rewrites to `Some(&*p.unwrap())`, which panics when `p` is null.
    //
    // CHECK: Some(&(*(p).unwrap()));
    let q = ptr::addr_of!(*p);

    // `offset(i)` rewrites to `map`, which passes null/`None` through unchanged.
    //
    // CHECK: let (arr, idx, ) = ((q), (0) as usize, );
    // CHECK-NEXT: arr.map(|arr| &arr[idx ..])
    let r = q.offset(0);

    // Because we use `Option::map` for this rewrite, it returns `None` if `p` is `None`, even when
    // `i != 0`.  This is different from the concrete behavior of most C compilers, where `NULL + i
    // != NULL`.  Adding `NULL + i` (with nonzero `i`) is undefined behavior in C, so it's legal
    // for us to define it this way, though it may produce surprising results in some cases like
    // handrolled `offsetof` macros.
    //
    // CHECK: let (arr, idx, ) = ((r), (i) as usize, );
    // CHECK-NEXT: arr.map(|arr| &arr[idx ..])
    let s = r.offset(i);

    s
}
