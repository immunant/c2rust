fn with_params(_unused: i32, mut used: i32, _unused2: i32, used_immut: i32) {
    let _unused3 = 1;
    let used2 = 2;
    used += used_immut + used2;
}

pub static mut foo: u32 = 1;

fn foobar(a: i32, _b: u32) -> u32 {
    if a < 2 {
        1
    } else {
        3
    }
}

fn used_in_local(p1: i32, mut p2: i32) {
    let _unused = p1;
    p2 = p1;

    let p2 = 1;
    let _arr = [p2 + 1];
    let mut arr2 = [1, 2];
    let arr3 = [1, 2];
    let mut arr4 = [1, 2];
    let _c = 5.;
    let _func = Some(foobar as fn(i32, u32) -> u32);

    unsafe {}

    arr2[0] = 2;
    arr3[1];
    arr4.as_mut_ptr();
}

unsafe fn ptrs(p1: *mut u32, mut p2: u32, p3: *mut u32, p4: *mut Foo, p5: Option<fn() -> bool>) {
    *p1.offset(0) += 1;
    (*p3) += 1;
    (*p4).0 = 1;
    while !(p5.unwrap()()) {}
    asm!("cpuid" : "={ax}" (p2))
}

struct Foo(u32);

impl Foo {
    fn foo(p1: i32, _p2: i32, mut p3: i32) -> i32 {
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
