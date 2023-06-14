extern "Rust" {
    fn foo(bar: Alias) -> Baz;
}

type Alias = Bar;

// CHECK-DAG: p: (g1) perms = UNIQUE, flags = FIXED
// CHECK-DAG: p: (g2) perms = UNIQUE, flags = FIXED

// CHECK-DAG: struct Bar
struct Bar {
    // CHECK-DAG: p: *mut i32
    p: *mut i32
}

// CHECK-DAG: struct Baz
struct Baz {
    // CHECK-DAG: p: *mut i32
    p: *mut i32
}

// we need something to get rewritten in order
// to check that `Bar` and `Baz` get output
// in the rewrite string
fn fizz(i: *const i32) {}
