extern "C" {
    fn f();
    fn g(x: u32);
    fn h(x: u32, y: u32) -> u32;
}

mod wrap {
}

fn main() {
    unsafe {
        f();
        g(17);
        let x = h(10, 20);
    }
}
