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

struct S {
    f: i32,
}

impl S {
}

type T = i32;

fn type_alias(x: T, y: T) -> i32 {
    x
}

fn substs() {
    //use std::slice::Iter;
    //let x = Option::<i32>::map::<i32, _>(Some(17), |x| x + 1);
    //let f = <Option<i32> as Clone>::clone;
    //let f = <str as AsRef<[u8]>>::as_ref;
    //let f = <Iter<u8> as Iterator>::collect::<Vec<_>>;
}

fn vec_push1(v: Vec<i32>, x: i32) {
    Vec::<i32>::push(&mut v, x);
}

fn vec_push2(v: Vec<i32>, x: i32) {
    v.push(x);
}


fn main() {
}
