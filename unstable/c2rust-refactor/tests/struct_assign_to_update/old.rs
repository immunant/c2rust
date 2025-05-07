struct S {
    x: u32,
    y: u32,
}

fn main() {
    let mut s = S { x: 0, y: 0 };
    s.x = 1;
    s.y = 2;
}

mod m {
    fn g() {
        let mut r = 0 .. 10;
        r.start = 2;
        r.end = 8;
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
