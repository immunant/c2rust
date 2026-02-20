#![feature(type_ascription)]

struct S<T>(T);

impl S<(i32, S<Option<i32>>)> {
    fn f(self) {}
}

fn g() {}

fn main() {
    let s = S((0, S(None)));
    let x = s.f();
    let f = S::f;
    let s = S((1, S(Some(2))));

    let mut v = Vec::new();
    v.push(123);
    let f = Vec::pop;
    f(&mut v);
    Vec::drain(&mut v, ..);

}
