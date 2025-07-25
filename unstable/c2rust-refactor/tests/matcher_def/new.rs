fn f() {}
fn f2() {}

fn g() {
    fn f() {}
    f();
}

fn main() {
    crate::f2();
    g();
}
