unsafe fn cell() {
    // CHECK-DAG: let mut x = std::cell::Cell::new((1));
    let mut x = 1;
    // CHECK-DAG: let mut y = &(x);
    let mut y = std::ptr::addr_of_mut!(x);
    // CHECK-DAG: let mut z = &(x);
    let mut z = std::ptr::addr_of_mut!(x);
    // CHECK-DAG: (z).set((1));
    *z = 1;
    // CHECK-DAG: (y).set((1));
    *y = 1;
    // CHECK-DAG: (y).set(((z).get()));
    *y = *z;
}

struct R {
    i: i32,
}

struct S {
    r: R,
    i: i32,
}

unsafe extern "C" fn cell_field(mut s: *mut S) {
    (*s).i = 1;
    // CHECK-DAG: let r1: &core::cell::Cell<(R)> = &((*s).r);
    let r1: *mut R = &mut (*s).r;
    // CHECK-DAG: let r2: &core::cell::Cell<(R)> = &((*s).r);
    let r2: *mut R = &mut (*s).r;
    // CHECK-DAG: ((*r1)).set((0));
    (*r1).i = 0;
    // CHECK-DAG: ((*r2)).set((1));
    (*r2).i = 1;
    *s = S {
        r: R { i: 0 },
        i: 0,
    };
}
