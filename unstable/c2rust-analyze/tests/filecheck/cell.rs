// CHECK-LABEL: fn cell(
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

// CHECK-LABEL: fn cell_field(
unsafe extern "C" fn cell_field(mut s: *mut S) {
    (*s).i = 1;
    // FIXME: The initializers for `r1` and `r2` are rewritten incorrectly.  Neither `s` nor the
    // `r` field have `Cell` type, and an `&mut T -> &Cell<T>` rewrite is not applied.
    // XXXXX: let r1: &core::cell::Cell<(R)> = &((*s).r);
    let r1: *mut R = &mut (*s).r;
    // XXXXX: let r2: &core::cell::Cell<(R)> = &((*s).r);
    let r2: *mut R = &mut (*s).r;
    // FIXME: The assignments to `(*r1).i` and `(*r2).i` are rewritten incorrectly.  The field
    // projection is omitted, producing `(*r1).set(0)`.
    // XXXXX: ((*r1)).set((0));
    (*r1).i = 0;
    // XXXXX: ((*r2)).set((1));
    (*r2).i = 1;
    *s = S {
        r: R { i: 0 },
        i: 0,
    };
}
