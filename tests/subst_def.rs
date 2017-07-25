/*
    -c tests/subst_def.rs:9:5
    rewrite_expr def!(f)() g(def!(f))
 */

use m::f;

mod m {
    pub fn f() {}
}

fn g(_: fn()) {}

fn main() {
    f();
    m::f();
    ::m::f();
}
