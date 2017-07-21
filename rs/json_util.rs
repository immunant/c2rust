
extern {
    fn __errno_location() -> *mut i32;
    fn close(__fd : i32) -> i32;
    fn isdigit(arg1 : i32) -> i32;
    fn isspace(arg1 : i32) -> i32;
    fn json_object_to_json_string_ext(
        obj : *mut json_object, flags : i32
    ) -> *const u8;
    fn json_tokener_parse(str : *const u8) -> *mut json_object;
    fn mc_debug(msg : *const u8, ...);
    fn mc_error(msg : *const u8, ...);
    fn open(__file : *const u8, __oflag : i32, ...) -> i32;
    fn printbuf_free(p : *mut printbuf);
    fn printbuf_memappend(
        p : *mut printbuf, buf : *const u8, size : i32
    ) -> i32;
    fn printbuf_new() -> *mut printbuf;
    fn read(
        __fd : i32, __buf : *mut ::std::os::raw::c_void, __nbytes : u64
    ) -> i64;
    fn snprintf(
        __s : *mut u8, __maxlen : u64, __format : *const u8, ...
    ) -> i32;
    fn sscanf(__s : *const u8, __format : *const u8, ...) -> i32;
    fn strerror(__errnum : i32) -> *mut u8;
    fn strlen(__s : *const u8) -> u64;
    fn strncmp(__s1 : *const u8, __s2 : *const u8, __n : u64) -> i32;
    fn write(
        __fd : i32, __buf : *const ::std::os::raw::c_void, __n : u64
    ) -> i64;
}

pub enum json_object {
}

static mut sscanf_is_broken : i32 = 0i32;

static mut sscanf_is_broken_testdone : i32 = 0i32;

#[derive(Copy)]
#[repr(C)]
pub struct printbuf {
    pub buf : *mut u8,
    pub bpos : i32,
    pub size : i32,
}

impl Clone for printbuf {
    fn clone(&self) -> Self { *self }
}

#[no_mangle]
pub unsafe extern fn json_object_from_file(
    mut filename : *const u8
) -> *mut json_object {
    let mut pb : *mut printbuf;
    let mut obj : *mut json_object;
    let mut buf : [u8; 4096] = ::std::mem::uninitialized();
    let mut fd : i32;
    let mut ret : i32;
    if {
           fd = open(filename,0o0i32);
           fd
       } < 0i32 {
        mc_error(
            (*b"json_object_from_file: error opening file %s: %s\n\0").as_ptr(
            ),
            filename,
            strerror(*__errno_location())
        );
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else if {
                  pb = printbuf_new();
                  pb
              }.is_null(
              ) {
        close(fd);
        mc_error(
            (*b"json_object_from_file: printbuf_new failed\n\0").as_ptr()
        );
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else {
        'loop2: loop {
            if !({
                     ret = read(
                               fd,
                               buf.as_mut_ptr() as (*mut ::std::os::raw::c_void),
                               4096u64
                           ) as (i32);
                     ret
                 } > 0i32) {
                break;
            }
            printbuf_memappend(pb,buf.as_mut_ptr() as (*const u8),ret);
        }
        close(fd);
        (if ret < 0i32 {
             mc_error(
                 (*b"json_object_from_file: error reading file %s: %s\n\0").as_ptr(
                 ),
                 filename,
                 strerror(*__errno_location())
             );
             printbuf_free(pb);
             0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
         } else {
             obj = json_tokener_parse((*pb).buf as (*const u8));
             printbuf_free(pb);
             obj
         })
    }
}

#[no_mangle]
pub unsafe extern fn json_object_to_file_ext(
    mut filename : *const u8,
    mut obj : *mut json_object,
    mut flags : i32
) -> i32 {
    let mut _currentBlock;
    let mut json_str : *const u8;
    let mut fd : i32;
    let mut ret : i32;
    let mut wpos : u32;
    let mut wsize : u32;
    if obj.is_null() {
        mc_error((*b"json_object_to_file: object is null\n\0").as_ptr());
        -1i32
    } else if {
                  fd = open(filename,0o1i32 | 0o1000i32 | 0o100i32,0o644i32);
                  fd
              } < 0i32 {
        mc_error(
            (*b"json_object_to_file: error opening file %s: %s\n\0").as_ptr(),
            filename,
            strerror(*__errno_location())
        );
        -1i32
    } else if {
                  json_str = json_object_to_json_string_ext(obj,flags);
                  json_str
              }.is_null(
              ) {
        close(fd);
        -1i32
    } else {
        wsize = (strlen(json_str) & 0x7fffffffu32.wrapping_mul(
                                        2u32
                                    ).wrapping_add(
                                        1u32
                                    ) as (u64)) as (u32);
        wpos = 0u32;
        'loop4: loop {
            if !(wpos < wsize) {
                _currentBlock = 5;
                break;
            }
            if {
                   ret = write(
                             fd,
                             json_str.offset(
                                 wpos as (isize)
                             ) as (*const ::std::os::raw::c_void),
                             wsize.wrapping_sub(wpos) as (u64)
                         ) as (i32);
                   ret
               } < 0i32 {
                _currentBlock = 8;
                break;
            }
            wpos = wpos.wrapping_add(ret as (u32));
        }
        (if _currentBlock == 5 {
             close(fd);
             0i32
         } else {
             close(fd);
             mc_error(
                 (*b"json_object_to_file: error writing file %s: %s\n\0").as_ptr(),
                 filename,
                 strerror(*__errno_location())
             );
             -1i32
         })
    }
}

#[no_mangle]
pub unsafe extern fn json_object_to_file(
    mut filename : *const u8, mut obj : *mut json_object
) -> i32 {
    json_object_to_file_ext(filename,obj,0i32)
}

#[no_mangle]
pub unsafe extern fn json_parse_double(
    mut buf : *const u8, mut retval : *mut f64
) -> i32 {
    if sscanf(buf,(*b"%lf\0").as_ptr(),retval) == 1i32 {
        0i32
    } else {
        1i32
    }
}

unsafe extern fn sscanf_is_broken_test() {
    let mut num64 : i64 = ::std::mem::uninitialized();
    let mut ret_errno : i32;
    let mut is_int64_min : i32;
    let mut ret_errno2 : i32;
    let mut is_int64_max : i32;
    sscanf(
        (*b" -01234567890123456789012345\0").as_ptr(),
        (*b"%ld\0").as_ptr(),
        &mut num64 as (*mut i64)
    );
    ret_errno = *__errno_location();
    is_int64_min = (num64 == -9223372036854775807i64 - 1i64) as (i32);
    sscanf(
        (*b" 01234567890123456789012345\0").as_ptr(),
        (*b"%ld\0").as_ptr(),
        &mut num64 as (*mut i64)
    );
    ret_errno2 = *__errno_location();
    is_int64_max = (num64 == 9223372036854775807i64) as (i32);
    if ret_errno != 34i32 || is_int64_min == 0 || ret_errno2 != 34i32 || is_int64_max == 0 {
        if false {
            mc_debug(
                (*b"sscanf_is_broken_test failed, enabling workaround code\n\0").as_ptr(
                )
            );
        }
        sscanf_is_broken = 1i32;
    }
}

#[no_mangle]
pub unsafe extern fn json_parse_int64(
    mut buf : *const u8, mut retval : *mut i64
) -> i32 {
    let mut num64 : i64 = ::std::mem::uninitialized();
    let mut buf_sig_digits : *const u8;
    let mut orig_has_neg : i32;
    let mut saved_errno : i32;
    if sscanf_is_broken_testdone == 0 {
        sscanf_is_broken_test();
        sscanf_is_broken_testdone = 1i32;
    }
    'loop2: loop {
        if !(isspace(*buf as (i32)) != 0 && (*buf != 0)) {
            break;
        }
        buf = buf.offset(1isize);
    }
    *__errno_location() = 0i32;
    if sscanf(
           buf,
           (*b"%ld\0").as_ptr(),
           &mut num64 as (*mut i64)
       ) != 1i32 {
        if false {
            mc_debug((*b"Failed to parse, sscanf != 1\n\0").as_ptr());
        }
        1i32
    } else {
        saved_errno = *__errno_location();
        buf_sig_digits = buf;
        orig_has_neg = 0i32;
        if *buf_sig_digits as (i32) == b'-' as (i32) {
            buf_sig_digits = buf_sig_digits.offset(1isize);
            orig_has_neg = 1i32;
        }
        if sscanf_is_broken != 0 && (saved_errno != 34i32) {
            let mut buf_cmp : [u8; 100] = ::std::mem::uninitialized();
            let mut buf_cmp_start : *mut u8 = buf_cmp.as_mut_ptr();
            let mut recheck_has_neg : i32 = 0i32;
            let mut buf_cmp_len : i32;
            'loop8: loop {
                if !(*buf_sig_digits.offset(
                          0isize
                      ) as (i32) == b'0' as (i32) && (*buf_sig_digits.offset(
                                                           1isize
                                                       ) as (i32) != b'\0' as (i32))) {
                    break;
                }
                buf_sig_digits = buf_sig_digits.offset(1isize);
            }
            if num64 == 0i64 {
                orig_has_neg = 0i32;
            }
            snprintf(
                buf_cmp_start,
                ::std::mem::size_of::<[u8; 100]>() as (u64),
                (*b"%ld\0").as_ptr(),
                num64
            );
            if *buf_cmp_start as (i32) == b'-' as (i32) {
                recheck_has_neg = 1i32;
                buf_cmp_start = buf_cmp_start.offset(1isize);
            }
            buf_cmp_len = strlen(buf_cmp_start as (*const u8)) as (i32);
            if orig_has_neg != recheck_has_neg || strncmp(
                                                      buf_sig_digits,
                                                      buf_cmp_start as (*const u8),
                                                      strlen(buf_cmp_start as (*const u8))
                                                  ) != 0i32 || strlen(
                                                                   buf_sig_digits
                                                               ) as (i32) != buf_cmp_len && (isdigit(
                                                                                                 *buf_sig_digits.offset(
                                                                                                      buf_cmp_len as (isize)
                                                                                                  ) as (i32)
                                                                                             ) != 0) {
                saved_errno = 34i32;
            }
        }
        if saved_errno == 34i32 {
            if orig_has_neg != 0 {
                num64 = -9223372036854775807i64 - 1i64;
            } else {
                num64 = 9223372036854775807i64;
            }
        }
        *retval = num64;
        0i32
    }
}

static mut json_type_name
    : [*const u8; 7] = [0 as *const _; 7];
unsafe extern "C" fn _init_json_type_name() {
    json_type_name
    = [   (*b"null\0").as_ptr(),
          (*b"boolean\0").as_ptr(),
          (*b"double\0").as_ptr(),
          (*b"int\0").as_ptr(),
          (*b"object\0").as_ptr(),
          (*b"array\0").as_ptr(),
          (*b"string\0").as_ptr()
      ];
}
#[link_section = ".ctors"]
static _INIT_JSON_TYPE_NAME: unsafe extern "C" fn() = _init_json_type_name;

#[derive(Clone, Copy)]
#[repr(i32)]
pub enum json_type {
    json_type_null,
    json_type_boolean,
    json_type_double,
    json_type_int,
    json_type_object,
    json_type_array,
    json_type_string,
}

#[no_mangle]
pub unsafe extern fn json_type_to_name(
    mut o_type : json_type
) -> *const u8 {
    let mut o_type_int : i32 = o_type as (i32);
    if o_type_int < 0i32 || o_type_int >= ::std::mem::size_of::<[*const u8; 7]>(
                                          ).wrapping_div(
                                              ::std::mem::size_of::<*const u8>()
                                          ) as (i32) {
        mc_error(
            (*b"json_type_to_name: type %d is out of range [0,%d]\n\0").as_ptr(
            ),
            o_type as (i32),
            ::std::mem::size_of::<[*const u8; 7]>().wrapping_div(
                ::std::mem::size_of::<*const u8>()
            )
        );
        0i32 as (*mut ::std::os::raw::c_void) as (*const u8)
    } else {
        json_type_name[o_type as (usize)]
    }
}
