
extern "C" {
    fn calloc(__nmemb: u64, __size: u64) -> *mut ::std::os::raw::c_void;
    fn free(__ptr: *mut ::std::os::raw::c_void);
    fn malloc(__size: u64) -> *mut ::std::os::raw::c_void;
    fn memcpy(
        __dest: *mut ::std::os::raw::c_void,
        __src: *const ::std::os::raw::c_void,
        __n: u64,
    ) -> *mut ::std::os::raw::c_void;
    fn memset(__s: *mut ::std::os::raw::c_void, __c: i32, __n: u64) -> *mut ::std::os::raw::c_void;
    fn realloc(__ptr: *mut ::std::os::raw::c_void, __size: u64) -> *mut ::std::os::raw::c_void;
}

#[derive(Copy)]
#[repr(C)]
pub struct printbuf {
    pub buf: *mut u8,
    pub bpos: i32,
    pub size: i32,
}

impl Clone for printbuf {
    fn clone(&self) -> Self {
        *self
    }
}

pub unsafe fn printbuf_new() -> *mut printbuf {
    let mut p: *mut printbuf;
    p = calloc(1u64, ::std::mem::size_of::<printbuf>() as (u64)) as (*mut printbuf);
    if p.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut printbuf)
    } else {
        (*p).size = 32i32;
        (*p).bpos = 0i32;
        (if {
            (*p).buf = malloc((*p).size as (u64)) as (*mut u8);
            (*p).buf
        }.is_null()
        {
             free(p as (*mut ::std::os::raw::c_void));
             0i32 as (*mut ::std::os::raw::c_void) as (*mut printbuf)
         } else {
             p
         })
    }
}
#[export_name = "printbuf_new"]
pub unsafe extern "C" fn printbuf_new_wrapper() -> *mut printbuf {
    printbuf_new()
}

unsafe extern "C" fn printbuf_extend(mut p: *mut printbuf, mut min_size: i32) -> i32 {
    let mut t: *mut u8;
    let mut new_size: i32;
    if (*p).size >= min_size {
        0i32
    } else {
        new_size = if (*p).size * 2i32 > min_size + 8i32 {
            (*p).size * 2i32
        } else {
            min_size + 8i32
        };
        (if {
            t = realloc((*p).buf as (*mut ::std::os::raw::c_void), new_size as (u64)) as (*mut u8);
            t
        }.is_null()
        {
             -1i32
         } else {
             (*p).size = new_size;
             (*p).buf = t;
             0i32
         })
    }
}

pub unsafe fn printbuf_memappend(mut p: *mut printbuf, mut buf: *const u8, mut size: i32) -> i32 {
    if (*p).size <= (*p).bpos + size + 1i32 {
        if printbuf_extend(p, (*p).bpos + size + 1i32) < 0i32 {
            return -1i32;
        }
    }
    memcpy(
        (*p).buf.offset((*p).bpos as (isize)) as (*mut ::std::os::raw::c_void),
        buf as (*const ::std::os::raw::c_void),
        size as (u64),
    );
    (*p).bpos = (*p).bpos + size;
    *(*p).buf.offset((*p).bpos as (isize)) = b'\0';
    size
}
#[export_name = "printbuf_memappend"]
pub unsafe extern "C" fn printbuf_memappend_wrapper(
    p: *mut printbuf,
    buf: *const u8,
    size: i32,
) -> i32 {
    printbuf_memappend(p, buf, size)
}

pub unsafe fn printbuf_memset(
    mut pb: *mut printbuf,
    mut offset: i32,
    mut charvalue: i32,
    mut len: i32,
) -> i32 {
    let mut size_needed: i32;
    if offset == -1i32 {
        offset = (*pb).bpos;
    }
    size_needed = offset + len;
    if (*pb).size < size_needed {
        if printbuf_extend(pb, size_needed) < 0i32 {
            return -1i32;
        }
    }
    memset(
        (*pb).buf.offset(offset as (isize)) as (*mut ::std::os::raw::c_void),
        charvalue,
        len as (u64),
    );
    if (*pb).bpos < size_needed {
        (*pb).bpos = size_needed;
    }
    0i32
}
#[export_name = "printbuf_memset"]
pub unsafe extern "C" fn printbuf_memset_wrapper(
    pb: *mut printbuf,
    offset: i32,
    charvalue: i32,
    len: i32,
) -> i32 {
    printbuf_memset(pb, offset, charvalue, len)
}

pub unsafe fn printbuf_reset(mut p: *mut printbuf) {
    *(*p).buf.offset(0isize) = b'\0';
    (*p).bpos = 0i32;
}
#[export_name = "printbuf_reset"]
pub unsafe extern "C" fn printbuf_reset_wrapper(p: *mut printbuf) {
    printbuf_reset(p)
}

pub unsafe fn printbuf_free(mut p: *mut printbuf) {
    if !p.is_null() {
        free((*p).buf as (*mut ::std::os::raw::c_void));
        free(p as (*mut ::std::os::raw::c_void));
    }
}
#[export_name = "printbuf_free"]
pub unsafe extern "C" fn printbuf_free_wrapper(p: *mut printbuf) {
    printbuf_free(p)
}
