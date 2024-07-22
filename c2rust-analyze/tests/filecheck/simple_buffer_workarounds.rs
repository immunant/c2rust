#![feature(rustc_private)]
#![allow(dead_code, mutable_transmutes, non_camel_case_types, non_snake_case, non_upper_case_globals, unused_assignments, unused_mut)]
#![register_tool(c2rust)]
#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]

extern crate libc;
extern "C" {
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(__ptr: *mut libc::c_void);
}
pub type uint8_t = libc::c_uchar;
pub type size_t = libc::c_ulong;

#[derive(Clone)]
#[repr(C)]
pub struct buffer {
    pub data: *mut uint8_t,
    pub len: size_t,
    pub cap: size_t,
}

#[no_mangle]
#[c2rust_analyze_test::skip_borrowck]
// CHECK-LABEL: fn buffer_new
pub unsafe extern "C" fn buffer_new(mut cap: size_t) -> *mut buffer {
    // The string "malloc" appears in some annotation, but no longer appears outside comments.
    // CHECK-NOT: {{^[^/]*}}malloc
    // CHECK: Box::new(buffer {
    let mut buf: *mut buffer = malloc(::std::mem::size_of::<buffer>() as libc::c_ulong)
        as *mut buffer;
    if cap == 0 as libc::c_int as libc::c_ulong {
        let fresh0 = &mut (*buf).data;
        *fresh0 = 0 as *mut uint8_t;
    } else {
        let fresh1 = &mut (*buf).data;
        // CHECK-NOT: {{^[^/]*}}malloc
        // CHECK: Vec::with_capacity
        let mut p = malloc(cap) as *mut uint8_t;
        *p.offset(0) = 1;
        *fresh1 = p;
    }
    (*buf).len = 0 as libc::c_int as size_t;
    (*buf).cap = cap;
    return buf;
    // CHECK-NOT: {{^[^/]*}}malloc
}

#[no_mangle]
// CHECK-LABEL: fn buffer_delete
pub unsafe extern "C" fn buffer_delete(mut buf: *mut buffer) {
    // CHECK-NOT: {{^[^/]*}}free
    if !((*buf).data).is_null() {
        free((*buf).data as *mut libc::c_void);
    }
    free(buf as *mut libc::c_void);
}

#[no_mangle]
#[c2rust_analyze_test::skip_borrowck]
// CHECK-LABEL: fn buffer_realloc
pub unsafe extern "C" fn buffer_realloc(mut buf: *mut buffer, mut new_cap: size_t) {
    if new_cap == (*buf).cap {
        return;
    }
    if (*buf).cap == 0 as libc::c_int as libc::c_ulong {
        let fresh2 = &mut (*buf).data;
        // CHECK-NOT: {{^[^/]*}}malloc
        // CHECK: Vec::with_capacity
        let mut p = malloc(new_cap) as *mut uint8_t;
        *p.offset(0) = 1;
        *fresh2 = p;
    } else if new_cap == 0 as libc::c_int as libc::c_ulong {
        // CHECK-NOT: {{^[^/]*}}free
        free((*buf).data as *mut libc::c_void);
        let fresh3 = &mut (*buf).data;
        *fresh3 = 0 as *mut uint8_t;
    } else {
        // CHECK-NOT: {{^[^/]*}}realloc
        // CHECK: Vec::from
        let mut p = realloc((*buf).data as *mut libc::c_void, new_cap) as *mut uint8_t;
        *p.offset(0) = 1;
        let fresh4 = &mut (*buf).data;
        *fresh4 = p;
    }
    (*buf).cap = new_cap;
    if (*buf).len > new_cap {
        (*buf).len = new_cap;
    }
    // CHECK-NOT: {{^[^/]*}}{{malloc|realloc|free}}
}

#[no_mangle]
// CHECK-LABEL: fn buffer_push
// CHECK-NOT: {{^[^/]*}}{{malloc|[^_]realloc|free}}
pub unsafe extern "C" fn buffer_push(mut buf: *mut buffer, mut byte: uint8_t) {
    if (*buf).len == (*buf).cap {
        if (*buf).cap == 0 as libc::c_int as libc::c_ulong {
            buffer_realloc(buf, 4 as libc::c_int as size_t);
        } else {
            let new_cap = ((*buf).cap).wrapping_mul(2 as libc::c_int as libc::c_ulong);
            buffer_realloc(
                buf,
                new_cap,
            );
        }
    }
    *((*buf).data).offset((*buf).len as isize) = byte;
    let fresh5 = &mut (*buf).len;
    *fresh5 = (*fresh5).wrapping_add(1);
}

#[no_mangle]
// CHECK-LABEL: fn test_buffer
pub unsafe extern "C" fn test_buffer() {
    let mut buf: *mut buffer = buffer_new(3 as libc::c_int as size_t);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < 10 as libc::c_int {
        buffer_push(buf, i as uint8_t);
        i += 1;
    }
    buffer_delete(buf);
}

unsafe fn main_0() -> libc::c_int {
    test_buffer();
    return 0;
}

pub fn main() {
    unsafe { ::std::process::exit(main_0() as i32) }
}
