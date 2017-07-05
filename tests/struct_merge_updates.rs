struct S {
    x: u32,
    y: u32,
    z: u32,
}

fn main() {
    let mut s = S { x: 0, y: 0, z: 0 };
    s = S { x: 1, .. s };
    s = S { y: 2, z: 3, .. s };

    let mut s2 = S { x: 0, y: 0, z: 0 };
    s2 = S { x: 1, .. s2 };
    s2 = S { y: 2, .. s2 };
    s2 = S { x: 1, .. s2 };
}

