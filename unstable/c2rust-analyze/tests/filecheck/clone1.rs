// Just check that the analysis doesn't crash on types deriving `Clone`.  This derive is one of the
// few sources of `impl`s in translated code.

// CHECK-LABEL: struct Foo {
#[derive(Clone, Copy)]
struct Foo {
    x: i32,
    y: i32,
}
