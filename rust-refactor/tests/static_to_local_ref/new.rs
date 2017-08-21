static S: u32 = 123;
static mut M: u32 = 234;

unsafe fn f(S_: &u32) {
    println!("S = {}", (*S_));
}

unsafe fn g(M_: &mut u32) {
    println!("M = {}", (*M_));
}

unsafe fn h(S_: &u32, M_: &mut u32) {
    f(S_);
    g(M_);
}

fn main() {
    unsafe {
        f(&S);
        g(&mut M);
        h(&S, &mut M);
    }
}
