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
