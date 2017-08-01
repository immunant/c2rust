fn f() {
    let x: i32 = 1;
    let y: i32 = 2;
    let z: i32 = x;
}

fn g(x: i32, y: i32) -> i32 {
    x
}

fn g2(x: i32, y: i32) -> i32 {
    g(x, y)
}

fn h1(x: i32, dest: &mut i32) {
    *dest = x;
}

fn h2(x: i32) -> i32 {
    let mut dest = 0;
    h1(x, &mut dest);
    dest
}

fn i(p: (i32, i32)) -> i32 {
    let (x, y) = p;
    x
}


fn main() {
}
