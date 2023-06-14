extern "C" {
    fn foo(bar: Alias) -> Baz;
}

type Alias = Bar;

// CHECK-DAG: br: ({{.*}}) perms = UNIQUE, flags = FIXED
// CHECK-DAG: bz: ({{.*}}) perms = UNIQUE, flags = FIXED
// CHECK-DAG: x: ({{.*}}) perms = UNIQUE, flags = FIXED
// CHECK-DAG: y: ({{.*}}) perms = UNIQUE, flags = FIXED

// CHECK-DAG: struct Bar
#[repr(C)]
struct Bar {
    // CHECK-DAG: br: *mut i32
    br: *mut i32
}

// CHECK-DAG: struct Baz
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
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct S {
    // CHECK-DAG: pub x: *const i32
    pub x: *const i32,
    // CHECK-DAG: pub y: *const i32
    pub y: *const i32,
}
