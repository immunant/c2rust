struct S {
    x: u32,
    y: u32,
}

fn main() {
    let mut s = S { x: 0, y: 0 };
    s = ::S { x: 1, ..s };
    s = ::S { y: 2, ..s };
}

mod m {
    fn g() {
        let mut r = 0..10;
        r = ::std::ops::Range { start: 2, ..r };
        r = ::std::ops::Range { end: 8, ..r };
    }
}

/*
// This part of the test case doesn't work yet.  The generated code refers to "::f::T" which is not
// a valid path.
fn f() {
    struct T { x: u32 }
    let mut t = T { x: 0 };
    t.x = 1;
}
*/
