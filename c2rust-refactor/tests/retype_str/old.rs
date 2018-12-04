#![allow(mutable_transmutes)]
#![feature(libc)]
extern crate libc;

use std::slice;

extern "C" {
    #[no_mangle]
    fn strlen(_: *const libc::c_char) -> libc::c_ulong;
    #[no_mangle]
    fn printf(_: *const libc::c_char, ...) -> libc::c_int;
    #[no_mangle]
    fn test_mut(_: *mut libc::c_char) -> libc::c_ulong;
}

struct S {
    s: *const libc::c_char,
    mut_s: *mut libc::c_char,
}

unsafe fn locals(s: *const libc::c_char) {
    let f_str: &[u8] = b"%s";
    let mut local_str: Vec<i8> = Vec::new();
    local_str.extend_from_slice(slice::from_raw_parts(s, 7));
    printf(std::mem::transmute(f_str.as_ptr()), local_str.as_ptr());

    // rustc can't infer this type once we elide the explicit type
    let x: u32 = "10".parse().unwrap();
    printf((*::std::mem::transmute::<&[u8; 2], &[libc::c_char; 2]>(b"%u")).as_ptr(), x);
}

pub fn main() {
    unsafe {
        // Assign to str
        let s: *const libc::c_char = ::std::mem::transmute::<&[u8; 7], &[libc::c_char; 7]>(b"testing").as_ptr();
        // Call API function taking str
        let len = strlen(s);
        // Call API function with immediate expression
        printf((*::std::mem::transmute::<&[u8; 3], &[libc::c_char; 3]>(b"%lu")).as_ptr(), len);
        // Call API function taking a mutable str. We cannot remove the
        // transmute in this case because a rust str slice is not mutable.
        test_mut((*::std::mem::transmute::<&[u8; 3], &mut [libc::c_char; 3]>(b"%lu")).as_mut_ptr());
        // Mutable string transmute
        printf((*::std::mem::transmute::<&[u8; 5], &mut [libc::c_char; 5]>(b"hello")).as_mut_ptr());
        // Nested expressions
        printf((*::std::mem::transmute::<&[u8; 3], &mut [libc::c_char; 3]>(b"%lu")).as_mut_ptr(), strlen(::std::mem::transmute::<&[u8; 7], &[libc::c_char; 7]>(b"testing").as_ptr()));
        // Initialize a struct containing a str
        let x = S {
            s: s,
            mut_s: (*::std::mem::transmute::<&[u8; 5], &mut [libc::c_char; 5]>(b"hello")).as_mut_ptr(),
        };
        // Use a struct containing a str
        printf(::std::mem::transmute::<&[u8; 5], &[libc::c_char; 5]>(b"%s %s").as_ptr(), x.s, x.mut_s);

        locals(s);
    }
}
