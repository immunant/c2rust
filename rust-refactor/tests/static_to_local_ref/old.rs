static S: u32 = 123;
static mut M: u32 = 234;

unsafe fn f() {
    println!("S = {}", S);
}

unsafe fn g() {
    println!("M = {}", M);
}

unsafe fn h() {
    f();
    g();
}

fn main() {
    unsafe {
        f();
        g();
        h();
    }
}
