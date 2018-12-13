use libc;

extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn reallocarray(__ptr: *mut libc::c_void, __nmemb: size_t, __size: size_t)
     -> *mut libc::c_void;
    #[no_mangle]
    fn free(__ptr: *mut libc::c_void);
    #[no_mangle]
    fn printf(_: *const libc::c_char, ...) -> libc::c_int;
}
pub type size_t = libc::c_ulong;
#[derive ( Copy , Clone )]
#[repr(C)]
pub struct S {
    pub field: libc::c_int,
}
pub unsafe fn main_0(mut argc: libc::c_int, mut argv: *mut *mut libc::c_char)
 -> libc::c_int {
    let mut s: *mut S =
        malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    s =
        realloc(s as *mut libc::c_void,
                (2i32 as
                     libc::c_ulong).wrapping_mul(::std::mem::size_of::<S>() as
                                                     libc::c_ulong)) as
            *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    let mut i: libc::c_int = 0i32;
    while i < 2i32 {
        printf(b"%i\n\x00" as *const u8 as *const libc::c_char,
               (*s.offset(i as isize)).field);
        i += 1
    }
    s =
        reallocarray(s as *mut libc::c_void, 3i32 as size_t,
                     ::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    (*s.offset(2isize)).field = 12i32;
    let mut i_0: libc::c_int = 0i32;
    while i_0 < 3i32 {
        printf(b"%i\n\x00" as *const u8 as *const libc::c_char,
               (*s.offset(i_0 as isize)).field);
        i_0 += 1
    }
    free(s as *mut libc::c_void);
    s =
        calloc(4i32 as libc::c_ulong,
               ::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    (*s.offset(2isize)).field = 12i32;
    (*s.offset(3isize)).field = 13i32;
    let mut i_1: libc::c_int = 0i32;
    while i_1 < 4i32 {
        printf(b"%i\n\x00" as *const u8 as *const libc::c_char,
               (*s.offset(i_1 as isize)).field);
        i_1 += 1
    }
    free(s as *mut libc::c_void);
    return 0i32;
}
