use libc;

#[cfg(not(source_header = "/usr/include/stdio.h"))]
pub mod stdio_h {
    use super::libc;
    extern "C" {
        #[no_mangle]
        pub fn printf(_: *const libc::c_char, ...) -> libc::c_int;
    }
}

use self::stdio_h::printf;
unsafe fn main_0() -> libc::c_int {
    printf(
        b"\n  \x1b[32m\xe2\x9c\x93 \x1b[90mok\x1b[0m\n\n\x00" as *const u8 as *const libc::c_char,
    );
    return 0i32;
}

pub fn main() {
    unsafe { ::std::process::exit(main_0() as i32) }
}
