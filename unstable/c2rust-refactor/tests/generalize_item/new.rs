struct S<T> {
    x: T,
    y: i32,
}

fn f<T>(x: T, y: i32) {}

fn g(s: S<i16>) {
    f::<i16>(s.x, s.y);
}

fn main() {}
