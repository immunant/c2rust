extern "C" {
    fn f();
    fn g(x: u32);
    fn h(x: u32, y: u32) -> u32;
}

mod wrap {
    pub unsafe fn f() {
        ::f()
    }
    pub unsafe fn g(x: u32) {
        ::g(x)
    }
    pub unsafe fn h(x: u32, y: u32) -> u32 {
        ::h(x, y)
    }
}

fn main() {
    unsafe {
        ::wrap::f();
        ::wrap::g(17);
        let x = ::wrap::h(10, 20);
    }
}
