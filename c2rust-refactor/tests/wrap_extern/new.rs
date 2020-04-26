extern "C" {
    fn f();
    fn g(x: u32);
    fn h(x: u32, y: u32) -> u32;
}

mod wrap {
    pub unsafe fn f() {
        crate::f()
    }
    pub unsafe fn g(x: u32) {
        crate::g(x)
    }
    pub unsafe fn h(x: u32, y: u32) -> u32 {
        crate::h(x, y)
    }
}

fn main() {
    unsafe {
        crate::wrap::f();
        crate::wrap::g(17);
        let x = crate::wrap::h(10, 20);
    }
}
