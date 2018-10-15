use libc;

extern "C" {
    // json_pointer_f.c
    pub fn json_pointer_getf(obj: *mut ::json_object::json_object,
                             res: *mut *mut ::json_object::json_object,
                             path_fmt: *const libc::c_char,
                             ...) -> libc::c_int;
    pub fn json_pointer_setf(obj: *mut *mut ::json_object::json_object,
                             value: *mut ::json_object::json_object,
                             path_fmt: *const libc::c_char,
                             ...) -> libc::c_int;

    // last_err.c
    pub fn json_util_get_last_err() -> *const libc::c_char;
    pub fn _json_c_set_last_err(err_fmt: *const libc::c_char, ...);

    // sprintbuf.c
    pub fn sprintbuf(p: *mut ::printbuf::printbuf,
                     msg: *const libc::c_char,
                     ...) -> libc::c_int;
}
