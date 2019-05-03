#![feature(const_transmute)]
#![feature(stdsimd)]
#![feature(nll)]
#![feature(custom_attribute)]
#![feature(libc)]
#![feature(extern_types)]
#![feature(simd_ffi)]


pub mod test_modules;
pub mod modules;


pub fn main() {
    match std::env::args().nth(1).as_ref().map(AsRef::<str>::as_ref) {
        Some("test_modules::test_modules") => test_modules::test_modules(),
        e => panic!("Tried to run unknown test: {:?}", e),
    }
}

