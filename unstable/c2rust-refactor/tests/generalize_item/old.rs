struct S {
    x: i16,
    y: i32,
}

fn f(x: i16, y: i32) {
}

fn g(s: S) {
    f(s.x, s.y);
}

fn main() {
}
