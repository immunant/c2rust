#![feature(asm)]
fn with_params(mut unused: i32, mut used: i32, mut unused2: i32, mut used_immut: i32) {
    let mut unused3 = 1;
    let mut used2 = 2;
    used += used_immut + used2;
}

fn used_in_local(mut p1: i32, mut p2: i32) {
    let mut unused = p1;
    p2 = p1;

    let mut p2 = 1;
    let mut arr = [p2 + 1];
    let mut arr2 = [1, 2];
    let mut arr3 = [1, 2];
    let mut arr4 = [1, 2];

    unsafe {}

    arr2[0] = 2;
    arr3[1];
    arr4.as_mut_ptr();
}

unsafe fn ptrs(mut p1: *mut u32, mut p2: u32, mut p3: *mut u32, mut p4: *mut Foo, mut p5: Option<fn() -> bool>) {
    *p1.offset(0) += 1;
    asm!("cpuid" : "={ax}" (p2));
    (*p3) += 1;
    (*p4).0 = 1;
    while !(p5.unwrap()()) {}
}

struct Foo(u32);

impl Foo {
    fn foo(mut p1: i32, mut p2: i32, mut p3: i32) -> i32 {
        p3 += 1;
        p1
    }
}

trait Trait {
    fn func();
}

impl Trait for Foo {
    fn func() {}
}
