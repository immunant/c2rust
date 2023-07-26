extern "C" {
    fn foo(bar: Alias) -> Baz;
}

type Alias = Bar;

// CHECK-DAG: br: ({{.*}}) perms = UNIQUE, flags = FIXED
// CHECK-DAG: bz: ({{.*}}) perms = UNIQUE, flags = FIXED
// CHECK-DAG: x: ({{.*}}) perms = UNIQUE, flags = FIXED
// CHECK-DAG: y: ({{.*}}) perms = UNIQUE, flags = FIXED
// CHECK-DAG: "s": addr_of flags = FIXED
// CHECK-DAG: "STATIC_PTR": addr_of flags = FIXED, type flags = FIXED#{{.*}}

// CHECK-LABEL: BEGIN{{.*}}foreign.rs

// CHECK-LABEL: struct Bar
#[repr(C)]
struct Bar {
    // CHECK-DAG: br: *mut i32
    br: *mut i32
}

// CHECK-LABEL: struct Baz
#[repr(C)]
struct Baz {
    // CHECK-DAG: bz: *mut i32
    bz: *mut i32
}

// we need something to get rewritten in order
// to check that `Bar` and `Baz` get output
// in the rewrite string
fn fizz(i: *const i32) {}

extern "C" {
    static mut s: S;
    static mut STATIC_PTR: *mut S;
}

#[derive(Copy, Clone)]
#[repr(C)]
// CHECK-LABEL: struct S
pub struct S {
    // CHECK-DAG: pub x: *const i32
    pub x: *const i32,
    // CHECK-DAG: pub y: *const i32
    pub y: *const i32,
}

// CHECK-LABEL: struct Nit
#[repr(C)]
struct Nit {
    // CHECK-DAG: x: *mut i32
    x: *mut i32,
    // CHECK-DAG: y: *mut i32
    y: *mut i32,
}

// CHECK-LABEL: struct Bin
#[repr(C)]
struct Bin {
    // CHECK-DAG: nit: *mut Nit
    nit: *mut Nit,
}

extern "C" {
    // CHECK-DAG: fn f(bin: *mut Bin)
    fn f(bin: *mut Bin);
}
