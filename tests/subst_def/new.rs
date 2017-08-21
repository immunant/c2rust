use m::f;

mod m {
    pub fn f() {}
}

fn g(_: fn()) {}

fn main() {
    g(f);
    g(m::f);
    g(::m::f);
}
