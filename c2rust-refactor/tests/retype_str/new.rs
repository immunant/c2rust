#![allow(mutable_transmutes)]
#![feature(libc)]
extern crate libc;

use std::slice;

extern "C" {
    #[no_mangle]
    fn strlen(_: *const u8) -> libc::c_ulong;
    #[no_mangle]
    fn printf(_: *const u8, ...) -> libc::c_int;
    #[no_mangle]
    fn test_mut(_: *mut u8) -> libc::c_ulong;
}

struct S {
    s: *const u8,
    mut_s: *mut u8,
}

unsafe fn locals(s: *const u8) {
    let f_str: &[u8; 2usize] = &*b"%s";
    let mut local_str: ::std::vec::Vec<u8> = Vec::new();
    local_str.extend_from_slice(slice::from_raw_parts(s, 7));
    printf(std::mem::transmute(f_str.as_ptr()), local_str.as_ptr());

    // rustc can't infer this type once we elide the explicit type
    let x: u32 = "10".parse().unwrap();
    printf((b"%u").as_ptr(), x);
}

pub fn main() {
    unsafe {
        // Assign to str
        let s: *const i8 =
            ::std::mem::transmute::<&[u8; 7], &[libc::c_char; 7]>(b"testing").as_ptr();
        // Call API function taking str
        let len = strlen(s as *const u8);
        // Call API function with immediate expression
        printf((b"%lu").as_ptr(), len);
        // Call API function taking a mutable str. We cannot remove the
        // transmute in this case because a rust str slice is not mutable.
        test_mut(
            (::std::mem::transmute::<&[u8; 3], &mut [libc::c_char; 3]>(b"%lu")).as_mut_ptr()
                as *mut u8,
        );
        // Mutable string transmute
        printf(b"hello".as_ptr());
        // Nested expressions
        printf(b"%lu".as_ptr(), strlen(b"testing".as_ptr()));
        // Initialize a struct containing a str
        let x = S {
            s: s as *const u8,
            mut_s: (::std::mem::transmute::<&[u8; 5], &mut [libc::c_char; 5]>(b"hello"))
                .as_mut_ptr() as *mut u8,
        };
        // Use a struct containing a str
        printf(b"%s %s".as_ptr(), x.s, x.mut_s);

        locals(s as *const u8);
    }
}
