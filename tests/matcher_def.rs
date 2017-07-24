/*
    rewrite_expr def!(f)() ::f2()
    -c tests/matcher_def.rs:6:1
    */

fn f() {}
fn f2() {}

fn g() {
    fn f() {}
    f();
}

fn main() {
    f();
    g();
}
