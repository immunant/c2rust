
extern "C" {
    fn calloc(__nmemb: u64, __size: u64) -> *mut ::std::os::raw::c_void;
    fn free(__ptr: *mut ::std::os::raw::c_void);
    fn isspace(arg1: i32) -> i32;
    static mut json_hex_chars: *const u8;
    static mut json_number_chars: *const u8;
    fn json_object_array_add(
        obj: *mut ::json_object::json_object,
        val: *mut ::json_object::json_object,
    ) -> i32;
    fn json_object_get(obj: *mut ::json_object::json_object) -> *mut ::json_object::json_object;
    fn json_object_new_array() -> *mut ::json_object::json_object;
    fn json_object_new_boolean(b: i32) -> *mut ::json_object::json_object;
    fn json_object_new_double(d: f64) -> *mut ::json_object::json_object;
    fn json_object_new_double_s(d: f64, ds: *const u8) -> *mut ::json_object::json_object;
    fn json_object_new_int64(i: i64) -> *mut ::json_object::json_object;
    fn json_object_new_object() -> *mut ::json_object::json_object;
    fn json_object_new_string_len(s: *const u8, len: i32) -> *mut ::json_object::json_object;
    fn json_object_object_add(
        obj: *mut ::json_object::json_object,
        key: *const u8,
        val: *mut ::json_object::json_object,
    );
    fn json_object_put(obj: *mut ::json_object::json_object) -> i32;
    fn json_parse_double(buf: *const u8, retval: *mut f64) -> i32;
    fn json_parse_int64(buf: *const u8, retval: *mut i64) -> i32;
    fn mc_debug(msg: *const u8, ...);
    fn memcpy(
        __dest: *mut ::std::os::raw::c_void,
        __src: *const ::std::os::raw::c_void,
        __n: u64,
    ) -> *mut ::std::os::raw::c_void;
    fn printbuf_free(p: *mut ::printbuf::printbuf);
    fn printbuf_memappend(p: *mut ::printbuf::printbuf, buf: *const u8, size: i32) -> i32;
    fn printbuf_new() -> *mut ::printbuf::printbuf;
    fn printbuf_reset(p: *mut ::printbuf::printbuf);
    fn setlocale(__category: i32, __locale: *const u8) -> *mut u8;
    fn strchr(__s: *const u8, __c: i32) -> *mut u8;
    fn strdup(__s: *const u8) -> *mut u8;
    fn strlen(__s: *const u8) -> u64;
    fn strncasecmp(__s1: *const u8, __s2: *const u8, __n: u64) -> i32;
    fn strncmp(__s1: *const u8, __s2: *const u8, __n: u64) -> i32;
}

pub enum json_object {
}

static mut json_null_str: [u8; 5] = *b"null\0";
static json_null_str_len: i32 = 4;

static mut json_inf_str: [u8; 9] = *b"Infinity\0";
static json_inf_str_len: i32 = 8;

static mut json_nan_str: [u8; 4] = *b"NaN\0";
static json_nan_str_len: i32 = 3;

static mut json_true_str: [u8; 5] = *b"true\0";
static json_true_str_len: i32 = 4;

static mut json_false_str: [u8; 6] = *b"false\0";
static json_false_str_len: i32 = 5;

static mut json_tokener_errors: [*const u8; 15] = [0 as *const _; 15];
unsafe extern "C" fn _init_json_tokener_errors() {
    json_tokener_errors = [
        (*b"success\0").as_ptr(),
        (*b"continue\0").as_ptr(),
        (*b"nesting too deep\0").as_ptr(),
        (*b"unexpected end of data\0").as_ptr(),
        (*b"unexpected character\0").as_ptr(),
        (*b"null expected\0").as_ptr(),
        (*b"boolean expected\0").as_ptr(),
        (*b"number expected\0").as_ptr(),
        (*b"array value separator \',\' expected\0").as_ptr(),
        (*b"quoted object property name expected\0").as_ptr(),
        (*b"object property name separator \':\' expected\0").as_ptr(),
        (*b"object value separator \',\' expected\0").as_ptr(),
        (*b"invalid string sequence\0").as_ptr(),
        (*b"expected comment\0").as_ptr(),
        (*b"buffer size overflow\0").as_ptr(),
    ];
}
#[link_section = ".ctors"]
static _INIT_JSON_TOKENER_ERRORS: unsafe extern "C" fn() = _init_json_tokener_errors;

#[derive(Clone, Copy)]
#[repr(i32)]
pub enum json_tokener_error {
    json_tokener_success,
    json_tokener_continue,
    json_tokener_error_depth,
    json_tokener_error_parse_eof,
    json_tokener_error_parse_unexpected,
    json_tokener_error_parse_null,
    json_tokener_error_parse_boolean,
    json_tokener_error_parse_number,
    json_tokener_error_parse_array,
    json_tokener_error_parse_object_key_name,
    json_tokener_error_parse_object_key_sep,
    json_tokener_error_parse_object_value_sep,
    json_tokener_error_parse_string,
    json_tokener_error_parse_comment,
    json_tokener_error_size,
}

#[no_mangle]
pub unsafe extern "C" fn json_tokener_error_desc(mut jerr: json_tokener_error) -> *const u8 {
    let mut jerr_int: i32 = jerr as (i32);
    if jerr_int < 0i32 ||
        jerr_int >=
            ::std::mem::size_of::<[*const u8; 15]>().wrapping_div(
                ::std::mem::size_of::<*const u8>(),
            ) as (i32)
    {
        (*b"Unknown error, invalid json_tokener_error value passed to json_tokener_error_desc()\0")
            .as_ptr()
    } else {
        json_tokener_errors[jerr as (usize)]
    }
}





#[derive(Clone, Copy)]
#[repr(i32)]
pub enum json_tokener_state {
    json_tokener_state_eatws,
    json_tokener_state_start,
    json_tokener_state_finish,
    json_tokener_state_null,
    json_tokener_state_comment_start,
    json_tokener_state_comment,
    json_tokener_state_comment_eol,
    json_tokener_state_comment_end,
    json_tokener_state_string,
    json_tokener_state_string_escape,
    json_tokener_state_escape_unicode,
    json_tokener_state_boolean,
    json_tokener_state_number,
    json_tokener_state_array,
    json_tokener_state_array_add,
    json_tokener_state_array_sep,
    json_tokener_state_object_field_start,
    json_tokener_state_object_field,
    json_tokener_state_object_field_end,
    json_tokener_state_object_value,
    json_tokener_state_object_value_add,
    json_tokener_state_object_sep,
    json_tokener_state_array_after_sep,
    json_tokener_state_object_field_start_after_sep,
    json_tokener_state_inf,
}

#[derive(Copy)]
#[repr(C)]
pub struct json_tokener_srec {
    pub state: json_tokener_state,
    pub saved_state: json_tokener_state,
    pub obj: *mut ::json_object::json_object,
    pub current: *mut ::json_object::json_object,
    pub obj_field_name: *mut u8,
}

impl Clone for json_tokener_srec {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Copy)]
#[repr(C)]
pub struct json_tokener {
    pub str: *mut u8,
    pub pb: *mut ::printbuf::printbuf,
    pub max_depth: i32,
    pub depth: i32,
    pub is_double: i32,
    pub st_pos: i32,
    pub char_offset: i32,
    pub err: json_tokener_error,
    pub ucs_char: u32,
    pub quote_char: u8,
    pub stack: *mut json_tokener_srec,
    pub flags: i32,
}

impl Clone for json_tokener {
    fn clone(&self) -> Self {
        *self
    }
}

#[no_mangle]
pub unsafe extern "C" fn json_tokener_get_error(mut tok: *mut json_tokener) -> json_tokener_error {
    (*tok).err
}

static mut utf8_replacement_char: [u8; 3] = [0xefu8, 0xbfu8, 0xbdu8];

#[no_mangle]
pub unsafe extern "C" fn json_tokener_new_ex(mut depth: i32) -> *mut json_tokener {
    let mut tok: *mut json_tokener;
    tok = calloc(1u64, ::std::mem::size_of::<json_tokener>() as (u64)) as (*mut json_tokener);
    if tok.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_tokener)
    } else {
        (*tok).stack = calloc(
            depth as (u64),
            ::std::mem::size_of::<json_tokener_srec>() as (u64),
        ) as (*mut json_tokener_srec);
        (if (*tok).stack.is_null() {
             free(tok as (*mut ::std::os::raw::c_void));
             0i32 as (*mut ::std::os::raw::c_void) as (*mut json_tokener)
         } else {
             (*tok).pb = printbuf_new();
             (*tok).max_depth = depth;
             json_tokener_reset(tok);
             tok
         })
    }
}

#[no_mangle]
pub unsafe extern "C" fn json_tokener_new() -> *mut json_tokener {
    json_tokener_new_ex(32i32)
}

#[no_mangle]
pub unsafe extern "C" fn json_tokener_free(mut tok: *mut json_tokener) {
    json_tokener_reset(tok);
    if !(*tok).pb.is_null() {
        printbuf_free((*tok).pb);
    }
    if !(*tok).stack.is_null() {
        free((*tok).stack as (*mut ::std::os::raw::c_void));
    }
    free(tok as (*mut ::std::os::raw::c_void));
}

unsafe extern "C" fn json_tokener_reset_level(mut tok: *mut json_tokener, mut depth: i32) {
    (*(*tok).stack.offset(depth as (isize))).state = json_tokener_state::json_tokener_state_eatws;
    (*(*tok).stack.offset(depth as (isize))).saved_state =
        json_tokener_state::json_tokener_state_start;
    json_object_put((*(*tok).stack.offset(depth as (isize))).current);
    (*(*tok).stack.offset(depth as (isize))).current = 0i32 as (*mut ::std::os::raw::c_void) as
        (*mut ::json_object::json_object);
    free(
        (*(*tok).stack.offset(depth as (isize))).obj_field_name as (*mut ::std::os::raw::c_void),
    );
    (*(*tok).stack.offset(depth as (isize))).obj_field_name =
        0i32 as (*mut ::std::os::raw::c_void) as (*mut u8);
}

#[no_mangle]
pub unsafe extern "C" fn json_tokener_reset(mut tok: *mut json_tokener) {
    let mut i: i32;
    if tok.is_null() {
    } else {
        i = (*tok).depth;
        'loop2: loop {
            if !(i >= 0i32) {
                break;
            }
            json_tokener_reset_level(tok, i);
            i = i - 1;
        }
        (*tok).depth = 0i32;
        (*tok).err = json_tokener_error::json_tokener_success;
    }
}

#[no_mangle]
pub unsafe extern "C" fn json_tokener_parse(mut str: *const u8) -> *mut ::json_object::json_object {
    let mut jerr_ignored: json_tokener_error = ::std::mem::uninitialized();
    let mut obj: *mut ::json_object::json_object;
    obj = json_tokener_parse_verbose(str, &mut jerr_ignored as (*mut json_tokener_error));
    obj
}

#[no_mangle]
pub unsafe extern "C" fn json_tokener_parse_verbose(
    mut str: *const u8,
    mut error: *mut json_tokener_error,
) -> *mut ::json_object::json_object {
    let mut tok: *mut json_tokener;
    let mut obj: *mut ::json_object::json_object;
    tok = json_tokener_new();
    if tok.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut ::json_object::json_object)
    } else {
        obj = json_tokener_parse_ex(tok, str, -1i32);
        *error = (*tok).err;
        if (*tok).err as (i32) != json_tokener_error::json_tokener_success as (i32) {
            if obj != 0i32 as (*mut ::std::os::raw::c_void) as (*mut ::json_object::json_object) {
                json_object_put(obj);
            }
            obj = 0i32 as (*mut ::std::os::raw::c_void) as (*mut ::json_object::json_object);
        }
        json_tokener_free(tok);
        obj
    }
}

#[no_mangle]
pub unsafe extern "C" fn json_tokener_parse_ex(
    mut tok: *mut json_tokener,
    mut str: *const u8,
    mut len: i32,
) -> *mut ::json_object::json_object {
    let mut unescaped_utf: [u8; 4] = ::std::mem::uninitialized();
    let mut case_start: *const u8 = ::std::mem::uninitialized();
    let mut case_len: i32 = ::std::mem::uninitialized();
    let mut got_hi_surrogate: u32 = ::std::mem::uninitialized();
    let mut _currentBlock;
    let mut obj: *mut ::json_object::json_object = 0i32 as (*mut ::std::os::raw::c_void) as
        (*mut ::json_object::json_object);
    let mut c: u8 = b'\x01';
    let mut oldlocale: *mut u8 = 0i32 as (*mut ::std::os::raw::c_void) as (*mut u8);
    let mut tmplocale: *mut u8;
    tmplocale = setlocale(1i32, 0i32 as (*mut ::std::os::raw::c_void) as (*const u8));
    if !tmplocale.is_null() {
        oldlocale = strdup(tmplocale as (*const u8));
    }
    setlocale(1i32, (*b"C\0").as_ptr());
    (*tok).char_offset = 0i32;
    (*tok).err = json_tokener_error::json_tokener_success;
    if len < -1i32 || len == -1i32 && (strlen(str) > 2147483647u64) {
        (*tok).err = json_tokener_error::json_tokener_error_size;
        0i32 as (*mut ::std::os::raw::c_void) as (*mut ::json_object::json_object)
    } else {
        'loop3: loop {
            if if (*tok).char_offset == len {
                (if (*tok).depth == 0i32 &&
                     ((*(*tok).stack.offset((*tok).depth as (isize))).state as (i32) ==
                          json_tokener_state::json_tokener_state_eatws as (i32)) &&
                     ((*(*tok).stack.offset((*tok).depth as (isize))).saved_state as (i32) ==
                          json_tokener_state::json_tokener_state_finish as (i32))
                {
                     ({
                          (*tok).err = json_tokener_error::json_tokener_success;
                          0i32
                      })
                 } else {
                     ({
                          (*tok).err = json_tokener_error::json_tokener_continue;
                          0i32
                      })
                 })
            } else {
                ({
                     c = *str;
                     1i32
                 })
            } == 0
            {
                _currentBlock = 303;
                break;
            }
            'loop4: loop {
                let switch1 = (*(*tok).stack.offset((*tok).depth as (isize))).state;
                if switch1 as (i32) == json_tokener_state::json_tokener_state_eatws as (i32) {
                    'loop293: loop {
                        if isspace(c as (i32)) == 0 {
                            break;
                        }
                        if {
                            str = str.offset(1isize);
                            (*tok).char_offset = (*tok).char_offset + 1;
                            c
                        } == 0 ||
                            if (*tok).char_offset == len {
                                (if (*tok).depth == 0i32 &&
                                     ((*(*tok).stack.offset((*tok).depth as (isize))).state as
                                          (i32) ==
                                          json_tokener_state::json_tokener_state_eatws as (i32)) &&
                                     ((*(*tok).stack.offset((*tok).depth as (isize))).saved_state as
                                          (i32) ==
                                          json_tokener_state::json_tokener_state_finish as (i32))
                                {
                                     ({
                                          (*tok).err = json_tokener_error::json_tokener_success;
                                          0i32
                                      })
                                 } else {
                                     ({
                                          (*tok).err = json_tokener_error::json_tokener_continue;
                                          0i32
                                      })
                                 })
                            } else {
                                ({
                                     c = *str;
                                     1i32
                                 })
                            } == 0
                        {
                            _currentBlock = 303;
                            break 'loop3;
                        }
                    }
                    if c as (i32) == b'/' as (i32) && ((*tok).flags & 0x1i32 == 0) {
                        _currentBlock = 296;
                        break;
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        (*(*tok).stack.offset((*tok).depth as (isize))).saved_state;
                } else if switch1 as (i32) ==
                           json_tokener_state::json_tokener_state_start as (i32)
                {
                    if c as (i32) == b'-' as (i32) ||
                        c as (i32) >= b'0' as (i32) && (c as (i32) <= b'9' as (i32))
                    {
                        (*(*tok).stack.offset((*tok).depth as (isize))).state =
                            json_tokener_state::json_tokener_state_number;
                        printbuf_reset((*tok).pb);
                        (*tok).is_double = 0i32;
                    } else if c as (i32) == b'f' as (i32) || c as (i32) == b'F' as (i32) ||
                               c as (i32) == b't' as (i32) ||
                               c as (i32) == b'T' as (i32)
                    {
                        (*(*tok).stack.offset((*tok).depth as (isize))).state =
                            json_tokener_state::json_tokener_state_boolean;
                        printbuf_reset((*tok).pb);
                        (*tok).st_pos = 0i32;
                    } else {
                        if c as (i32) == b'\"' as (i32) {
                            _currentBlock = 290;
                            break;
                        }
                        if c as (i32) == b'\'' as (i32) {
                            _currentBlock = 288;
                            break;
                        }
                        if c as (i32) == b'n' as (i32) || c as (i32) == b'N' as (i32) {
                            (*(*tok).stack.offset((*tok).depth as (isize))).state =
                                json_tokener_state::json_tokener_state_null;
                            printbuf_reset((*tok).pb);
                            (*tok).st_pos = 0i32;
                        } else {
                            if !(c as (i32) == b'i' as (i32) || c as (i32) == b'I' as (i32)) {
                                _currentBlock = 281;
                                break;
                            }
                            (*(*tok).stack.offset((*tok).depth as (isize))).state =
                                json_tokener_state::json_tokener_state_inf;
                            printbuf_reset((*tok).pb);
                            (*tok).st_pos = 0i32;
                        }
                    }
                } else {
                    if switch1 as (i32) ==
                        json_tokener_state::json_tokener_state_comment_end as (i32)
                    {
                        _currentBlock = 267;
                        break;
                    }
                    if switch1 as (i32) ==
                        json_tokener_state::json_tokener_state_string_escape as (i32)
                    {
                        _currentBlock = 237;
                        break;
                    }
                    if switch1 as (i32) ==
                        json_tokener_state::json_tokener_state_object_sep as (i32)
                    {
                        _currentBlock = 232;
                        break;
                    }
                    if switch1 as (i32) ==
                        json_tokener_state::json_tokener_state_object_value_add as (i32)
                    {
                        json_object_object_add(
                            (*(*tok).stack.offset((*tok).depth as (isize))).current,
                            (*(*tok).stack.offset((*tok).depth as (isize))).obj_field_name as
                                (*const u8),
                            obj,
                        );
                        free(
                            (*(*tok).stack.offset((*tok).depth as (isize))).obj_field_name as
                                (*mut ::std::os::raw::c_void),
                        );
                        (*(*tok).stack.offset((*tok).depth as (isize))).obj_field_name =
                            0i32 as (*mut ::std::os::raw::c_void) as (*mut u8);
                        (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                            json_tokener_state::json_tokener_state_object_sep;
                        (*(*tok).stack.offset((*tok).depth as (isize))).state =
                            json_tokener_state::json_tokener_state_eatws;
                    } else if switch1 as (i32) ==
                               json_tokener_state::json_tokener_state_object_value as (i32)
                    {
                        if (*tok).depth >= (*tok).max_depth - 1i32 {
                            _currentBlock = 230;
                            break 'loop3;
                        }
                        (*(*tok).stack.offset((*tok).depth as (isize))).state =
                            json_tokener_state::json_tokener_state_object_value_add;
                        (*tok).depth = (*tok).depth + 1;
                        json_tokener_reset_level(tok, (*tok).depth);
                    } else {
                        if switch1 as (i32) ==
                            json_tokener_state::json_tokener_state_object_field_end as (i32)
                        {
                            _currentBlock = 225;
                            break;
                        }
                        if switch1 as (i32) ==
                            json_tokener_state::json_tokener_state_object_field as (i32)
                        {
                            _currentBlock = 210;
                            break;
                        }
                        if switch1 as (i32) ==
                            json_tokener_state::json_tokener_state_object_field_start_after_sep as
                                (i32) ||
                            switch1 as (i32) ==
                                json_tokener_state::json_tokener_state_object_field_start as (i32)
                        {
                            _currentBlock = 203;
                            break;
                        }
                        if switch1 as (i32) ==
                            json_tokener_state::json_tokener_state_array_sep as (i32)
                        {
                            _currentBlock = 198;
                            break;
                        }
                        if switch1 as (i32) ==
                            json_tokener_state::json_tokener_state_array_add as (i32)
                        {
                            json_object_array_add(
                                (*(*tok).stack.offset((*tok).depth as (isize))).current,
                                obj,
                            );
                            (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                                json_tokener_state::json_tokener_state_array_sep;
                            (*(*tok).stack.offset((*tok).depth as (isize))).state =
                                json_tokener_state::json_tokener_state_eatws;
                        } else if switch1 as (i32) ==
                                   json_tokener_state::json_tokener_state_array as (i32) ||
                                   switch1 as (i32) ==
                                       json_tokener_state::json_tokener_state_array_after_sep as
                                           (i32)
                        {
                            if c as (i32) == b']' as (i32) {
                                _currentBlock = 194;
                                break;
                            }
                            if (*tok).depth >= (*tok).max_depth - 1i32 {
                                _currentBlock = 193;
                                break 'loop3;
                            }
                            (*(*tok).stack.offset((*tok).depth as (isize))).state =
                                json_tokener_state::json_tokener_state_array_add;
                            (*tok).depth = (*tok).depth + 1;
                            json_tokener_reset_level(tok, (*tok).depth);
                        } else if switch1 as (i32) ==
                                   json_tokener_state::json_tokener_state_number as (i32)
                        {
                            case_start = str;
                            case_len = 0i32;
                            'loop169: loop {
                                if !(c != 0 && !strchr(json_number_chars, c as (i32)).is_null()) {
                                    break;
                                }
                                case_len = case_len + 1;
                                if c as (i32) == b'.' as (i32) || c as (i32) == b'e' as (i32) ||
                                    c as (i32) == b'E' as (i32)
                                {
                                    (*tok).is_double = 1i32;
                                }
                                if {
                                    str = str.offset(1isize);
                                    (*tok).char_offset = (*tok).char_offset + 1;
                                    c
                                } == 0 ||
                                    if (*tok).char_offset == len {
                                        (if (*tok).depth == 0i32 &&
                                             ((*(*tok).stack.offset((*tok).depth as (isize)))
                                                  .state as
                                                  (i32) ==
                                                  json_tokener_state::json_tokener_state_eatws as
                                                      (i32)) &&
                                             ((*(*tok).stack.offset((*tok).depth as (isize)))
                                                  .saved_state as
                                                  (i32) ==
                                                  json_tokener_state::json_tokener_state_finish as
                                                      (i32))
                                        {
                                             ({
                                                  (*tok).err =
                                                      json_tokener_error::json_tokener_success;
                                                  0i32
                                              })
                                         } else {
                                             ({
                                                  (*tok).err =
                                                      json_tokener_error::json_tokener_continue;
                                                  0i32
                                              })
                                         })
                                    } else {
                                        ({
                                             c = *str;
                                             1i32
                                         })
                                    } == 0
                                {
                                    _currentBlock = 187;
                                    break 'loop3;
                                }
                            }
                            if case_len > 0i32 {
                                if (*(*tok).pb).size - (*(*tok).pb).bpos > case_len {
                                    memcpy(
                                        (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                            (*mut ::std::os::raw::c_void),
                                        case_start as (*const ::std::os::raw::c_void),
                                        case_len as (u64),
                                    );
                                    (*(*tok).pb).bpos = (*(*tok).pb).bpos + case_len;
                                    *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                                } else {
                                    printbuf_memappend((*tok).pb, case_start, case_len);
                                }
                            }
                            if *(*(*tok).pb).buf.offset(0isize) as (i32) == b'-' as (i32) &&
                                (case_len == 1i32) &&
                                (c as (i32) == b'i' as (i32) || c as (i32) == b'I' as (i32))
                            {
                                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                                    json_tokener_state::json_tokener_state_inf;
                            } else {
                                let mut num64: i64 = ::std::mem::uninitialized();
                                let mut numd: f64 = ::std::mem::uninitialized();
                                if (*tok).is_double == 0 &&
                                    (json_parse_int64(
                                        (*(*tok).pb).buf as (*const u8),
                                        &mut num64 as (*mut i64),
                                    ) == 0i32)
                                {
                                    if num64 != 0 &&
                                        (*(*(*tok).pb).buf.offset(0isize) as (i32) ==
                                             b'0' as (i32)) &&
                                        ((*tok).flags & 0x1i32 != 0)
                                    {
                                        _currentBlock = 182;
                                        break 'loop3;
                                    }
                                    (*(*tok).stack.offset((*tok).depth as (isize))).current =
                                        json_object_new_int64(num64);
                                } else {
                                    if !((*tok).is_double != 0 &&
                                             (json_parse_double(
                                            (*(*tok).pb).buf as (*const u8),
                                            &mut numd as (*mut f64),
                                        ) == 0i32))
                                    {
                                        _currentBlock = 177;
                                        break 'loop3;
                                    }
                                    (*(*tok).stack.offset((*tok).depth as (isize))).current =
                                        json_object_new_double_s(
                                            numd,
                                            (*(*tok).pb).buf as (*const u8),
                                        );
                                }
                                (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                                    json_tokener_state::json_tokener_state_finish;
                                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                                    json_tokener_state::json_tokener_state_eatws;
                            }
                        } else if switch1 as (i32) ==
                                   json_tokener_state::json_tokener_state_boolean as (i32)
                        {
                            let mut size1: i32;
                            let mut size2: i32;
                            if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                                memcpy(
                                    (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                        (*mut ::std::os::raw::c_void),
                                    &mut c as (*mut u8) as (*const ::std::os::raw::c_void),
                                    1u64,
                                );
                                (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                                *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                            } else {
                                printbuf_memappend(
                                    (*tok).pb,
                                    &mut c as (*mut u8) as (*const u8),
                                    1i32,
                                );
                            }
                            size1 = if (*tok).st_pos + 1i32 < json_true_str_len {
                                (*tok).st_pos + 1i32
                            } else {
                                json_true_str_len
                            };
                            size2 = if (*tok).st_pos + 1i32 < json_false_str_len {
                                (*tok).st_pos + 1i32
                            } else {
                                json_false_str_len
                            };
                            if (*tok).flags & 0x1i32 == 0 &&
                                (strncasecmp(
                                    json_true_str.as_ptr(),
                                    (*(*tok).pb).buf as (*const u8),
                                    size1 as (u64),
                                ) == 0i32) ||
                                strncmp(
                                    json_true_str.as_ptr(),
                                    (*(*tok).pb).buf as (*const u8),
                                    size1 as (u64),
                                ) == 0i32
                            {
                                if !((*tok).st_pos == json_true_str_len) {
                                    _currentBlock = 166;
                                    break;
                                }
                                (*(*tok).stack.offset((*tok).depth as (isize))).current =
                                    json_object_new_boolean(1i32);
                                (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                                    json_tokener_state::json_tokener_state_finish;
                                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                                    json_tokener_state::json_tokener_state_eatws;
                            } else {
                                if !((*tok).flags & 0x1i32 == 0 &&
                                         (strncasecmp(
                                        json_false_str.as_ptr(),
                                        (*(*tok).pb).buf as (*const u8),
                                        size2 as (u64),
                                    ) == 0i32) ||
                                         strncmp(
                                        json_false_str.as_ptr(),
                                        (*(*tok).pb).buf as (*const u8),
                                        size2 as (u64),
                                    ) == 0i32)
                                {
                                    _currentBlock = 162;
                                    break 'loop3;
                                }
                                if !((*tok).st_pos == json_false_str_len) {
                                    _currentBlock = 166;
                                    break;
                                }
                                (*(*tok).stack.offset((*tok).depth as (isize))).current =
                                    json_object_new_boolean(0i32);
                                (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                                    json_tokener_state::json_tokener_state_finish;
                                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                                    json_tokener_state::json_tokener_state_eatws;
                            }
                        } else {
                            if switch1 as (i32) ==
                                json_tokener_state::json_tokener_state_escape_unicode as (i32)
                            {
                                _currentBlock = 98;
                                break;
                            }
                            if switch1 as (i32) ==
                                json_tokener_state::json_tokener_state_string as (i32)
                            {
                                _currentBlock = 83;
                                break;
                            }
                            if switch1 as (i32) ==
                                json_tokener_state::json_tokener_state_comment_eol as (i32)
                            {
                                _currentBlock = 71;
                                break;
                            }
                            if switch1 as (i32) ==
                                json_tokener_state::json_tokener_state_comment as (i32)
                            {
                                _currentBlock = 61;
                                break;
                            }
                            if switch1 as (i32) ==
                                json_tokener_state::json_tokener_state_comment_start as (i32)
                            {
                                _currentBlock = 53;
                                break;
                            }
                            if switch1 as (i32) ==
                                json_tokener_state::json_tokener_state_null as (i32)
                            {
                                let mut size: i32;
                                let mut size_nan: i32;
                                if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                                    memcpy(
                                        (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                            (*mut ::std::os::raw::c_void),
                                        &mut c as (*mut u8) as (*const ::std::os::raw::c_void),
                                        1u64,
                                    );
                                    (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                                    *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                                } else {
                                    printbuf_memappend(
                                        (*tok).pb,
                                        &mut c as (*mut u8) as (*const u8),
                                        1i32,
                                    );
                                }
                                size = if (*tok).st_pos + 1i32 < json_null_str_len {
                                    (*tok).st_pos + 1i32
                                } else {
                                    json_null_str_len
                                };
                                size_nan = if (*tok).st_pos + 1i32 < json_nan_str_len {
                                    (*tok).st_pos + 1i32
                                } else {
                                    json_nan_str_len
                                };
                                if (*tok).flags & 0x1i32 == 0 &&
                                    (strncasecmp(
                                        json_null_str.as_ptr(),
                                        (*(*tok).pb).buf as (*const u8),
                                        size as (u64),
                                    ) == 0i32) ||
                                    strncmp(
                                        json_null_str.as_ptr(),
                                        (*(*tok).pb).buf as (*const u8),
                                        size as (u64),
                                    ) == 0i32
                                {
                                    if !((*tok).st_pos == json_null_str_len) {
                                        _currentBlock = 51;
                                        break;
                                    }
                                    (*(*tok).stack.offset((*tok).depth as (isize))).current =
                                        0i32 as (*mut ::std::os::raw::c_void) as
                                            (*mut ::json_object::json_object);
                                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                                        json_tokener_state::json_tokener_state_finish;
                                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                                        json_tokener_state::json_tokener_state_eatws;
                                } else {
                                    if !((*tok).flags & 0x1i32 == 0 &&
                                             (strncasecmp(
                                            json_nan_str.as_ptr(),
                                            (*(*tok).pb).buf as (*const u8),
                                            size_nan as (u64),
                                        ) == 0i32) ||
                                             strncmp(
                                            json_nan_str.as_ptr(),
                                            (*(*tok).pb).buf as (*const u8),
                                            size_nan as (u64),
                                        ) == 0i32)
                                    {
                                        _currentBlock = 47;
                                        break 'loop3;
                                    }
                                    if !((*tok).st_pos == json_nan_str_len) {
                                        _currentBlock = 51;
                                        break;
                                    }
                                    (*(*tok).stack.offset((*tok).depth as (isize))).current =
                                        json_object_new_double((0.0f32 / 0.0f32) as (f64));
                                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                                        json_tokener_state::json_tokener_state_finish;
                                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                                        json_tokener_state::json_tokener_state_eatws;
                                }
                            } else if switch1 as (i32) ==
                                       json_tokener_state::json_tokener_state_inf as (i32)
                            {
                                let mut size_inf: i32;
                                let mut is_negative: i32 = 0i32;
                                if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                                    memcpy(
                                        (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                            (*mut ::std::os::raw::c_void),
                                        &mut c as (*mut u8) as (*const ::std::os::raw::c_void),
                                        1u64,
                                    );
                                    (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                                    *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                                } else {
                                    printbuf_memappend(
                                        (*tok).pb,
                                        &mut c as (*mut u8) as (*const u8),
                                        1i32,
                                    );
                                }
                                size_inf = if (*tok).st_pos + 1i32 < json_inf_str_len {
                                    (*tok).st_pos + 1i32
                                } else {
                                    json_inf_str_len
                                };
                                let mut infbuf: *mut u8 = (*(*tok).pb).buf;
                                if *infbuf as (i32) == b'-' as (i32) {
                                    infbuf = infbuf.offset(1isize);
                                    is_negative = 1i32;
                                }
                                if !((*tok).flags & 0x1i32 == 0 &&
                                         (strncasecmp(
                                        json_inf_str.as_ptr(),
                                        infbuf as (*const u8),
                                        size_inf as (u64),
                                    ) == 0i32) ||
                                         strncmp(
                                        json_inf_str.as_ptr(),
                                        infbuf as (*const u8),
                                        size_inf as (u64),
                                    ) == 0i32)
                                {
                                    _currentBlock = 37;
                                    break 'loop3;
                                }
                                if !((*tok).st_pos == json_inf_str_len) {
                                    _currentBlock = 39;
                                    break;
                                }
                                (*(*tok).stack.offset((*tok).depth as (isize))).current =
                                    json_object_new_double(if is_negative != 0 {
                                        -(1.0f32 / 0.0f32)
                                    } else {
                                        1.0f32 / 0.0f32
                                    } as (f64));
                                (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                                    json_tokener_state::json_tokener_state_finish;
                                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                                    json_tokener_state::json_tokener_state_eatws;
                            } else {
                                if !(switch1 as (i32) ==
                                         json_tokener_state::json_tokener_state_finish as (i32))
                                {
                                    _currentBlock = 301;
                                    break;
                                }
                                if (*tok).depth == 0i32 {
                                    _currentBlock = 303;
                                    break 'loop3;
                                }
                                obj = json_object_get(
                                    (*(*tok).stack.offset((*tok).depth as (isize))).current,
                                );
                                json_tokener_reset_level(tok, (*tok).depth);
                                (*tok).depth = (*tok).depth - 1;
                            }
                        }
                    }
                }
            }
            if _currentBlock == 39 {
                (*tok).st_pos = (*tok).st_pos + 1;
                _currentBlock = 301;
            } else if _currentBlock == 51 {
                (*tok).st_pos = (*tok).st_pos + 1;
                _currentBlock = 301;
            } else if _currentBlock == 53 {
                if c as (i32) == b'*' as (i32) {
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_comment;
                } else {
                    if !(c as (i32) == b'/' as (i32)) {
                        _currentBlock = 55;
                        break;
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_comment_eol;
                }
                if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                    memcpy(
                        (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                            (*mut ::std::os::raw::c_void),
                        &mut c as (*mut u8) as (*const ::std::os::raw::c_void),
                        1u64,
                    );
                    (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                    *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                } else {
                    printbuf_memappend((*tok).pb, &mut c as (*mut u8) as (*const u8), 1i32);
                }
                _currentBlock = 301;
            } else if _currentBlock == 61 {
                case_start = str;
                'loop62: loop {
                    if !(c as (i32) != b'*' as (i32)) {
                        break;
                    }
                    if {
                        str = str.offset(1isize);
                        (*tok).char_offset = (*tok).char_offset + 1;
                        c
                    } == 0 ||
                        if (*tok).char_offset == len {
                            (if (*tok).depth == 0i32 &&
                                 ((*(*tok).stack.offset((*tok).depth as (isize))).state as (i32) ==
                                      json_tokener_state::json_tokener_state_eatws as (i32)) &&
                                 ((*(*tok).stack.offset((*tok).depth as (isize))).saved_state as
                                      (i32) ==
                                      json_tokener_state::json_tokener_state_finish as (i32))
                            {
                                 ({
                                      (*tok).err = json_tokener_error::json_tokener_success;
                                      0i32
                                  })
                             } else {
                                 ({
                                      (*tok).err = json_tokener_error::json_tokener_continue;
                                      0i32
                                  })
                             })
                        } else {
                            ({
                                 c = *str;
                                 1i32
                             })
                        } == 0
                    {
                        _currentBlock = 68;
                        break 'loop3;
                    }
                }
                if ((*(*tok).pb).size - (*(*tok).pb).bpos) as (isize) >
                    (str.offset(1isize) as (isize)).wrapping_sub(case_start as (isize)) /
                        ::std::mem::size_of::<u8>() as (isize)
                {
                    memcpy(
                        (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                            (*mut ::std::os::raw::c_void),
                        case_start as (*const ::std::os::raw::c_void),
                        ((str.offset(1isize) as (isize)).wrapping_sub(case_start as (isize)) /
                             ::std::mem::size_of::<u8>() as (isize)) as (u64),
                    );
                    (*(*tok).pb).bpos =
                        ((*(*tok).pb).bpos as (isize) +
                             (str.offset(1isize) as (isize)).wrapping_sub(case_start as (isize)) /
                                 ::std::mem::size_of::<u8>() as (isize)) as
                            (i32);
                    *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                } else {
                    printbuf_memappend(
                        (*tok).pb,
                        case_start,
                        ((str.offset(1isize) as (isize)).wrapping_sub(case_start as (isize)) /
                             ::std::mem::size_of::<u8>() as (isize)) as (i32),
                    );
                }
                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                    json_tokener_state::json_tokener_state_comment_end;
                _currentBlock = 301;
            } else if _currentBlock == 71 {
                case_start = str;
                'loop72: loop {
                    if !(c as (i32) != b'\n' as (i32)) {
                        break;
                    }
                    if {
                        str = str.offset(1isize);
                        (*tok).char_offset = (*tok).char_offset + 1;
                        c
                    } == 0 ||
                        if (*tok).char_offset == len {
                            (if (*tok).depth == 0i32 &&
                                 ((*(*tok).stack.offset((*tok).depth as (isize))).state as (i32) ==
                                      json_tokener_state::json_tokener_state_eatws as (i32)) &&
                                 ((*(*tok).stack.offset((*tok).depth as (isize))).saved_state as
                                      (i32) ==
                                      json_tokener_state::json_tokener_state_finish as (i32))
                            {
                                 ({
                                      (*tok).err = json_tokener_error::json_tokener_success;
                                      0i32
                                  })
                             } else {
                                 ({
                                      (*tok).err = json_tokener_error::json_tokener_continue;
                                      0i32
                                  })
                             })
                        } else {
                            ({
                                 c = *str;
                                 1i32
                             })
                        } == 0
                    {
                        _currentBlock = 80;
                        break 'loop3;
                    }
                }
                if ((*(*tok).pb).size - (*(*tok).pb).bpos) as (isize) >
                    (str as (isize)).wrapping_sub(case_start as (isize)) /
                        ::std::mem::size_of::<u8>() as (isize)
                {
                    memcpy(
                        (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                            (*mut ::std::os::raw::c_void),
                        case_start as (*const ::std::os::raw::c_void),
                        ((str as (isize)).wrapping_sub(case_start as (isize)) /
                             ::std::mem::size_of::<u8>() as (isize)) as (u64),
                    );
                    (*(*tok).pb).bpos = ((*(*tok).pb).bpos as (isize) +
                                             (str as (isize)).wrapping_sub(case_start as (isize)) /
                                                 ::std::mem::size_of::<u8>() as (isize)) as
                        (i32);
                    *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                } else {
                    printbuf_memappend(
                        (*tok).pb,
                        case_start,
                        ((str as (isize)).wrapping_sub(case_start as (isize)) /
                             ::std::mem::size_of::<u8>() as (isize)) as (i32),
                    );
                }
                if false {
                    mc_debug(
                        (*b"json_tokener_comment: %s\n\0").as_ptr(),
                        (*(*tok).pb).buf,
                    );
                }
                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                    json_tokener_state::json_tokener_state_eatws;
                _currentBlock = 301;
            } else if _currentBlock == 83 {
                case_start = str;
                'loop84: loop {
                    if c as (i32) == (*tok).quote_char as (i32) {
                        _currentBlock = 94;
                        break;
                    }
                    if c as (i32) == b'\\' as (i32) {
                        _currentBlock = 90;
                        break;
                    }
                    if {
                        str = str.offset(1isize);
                        (*tok).char_offset = (*tok).char_offset + 1;
                        c
                    } == 0 ||
                        if (*tok).char_offset == len {
                            (if (*tok).depth == 0i32 &&
                                 ((*(*tok).stack.offset((*tok).depth as (isize))).state as (i32) ==
                                      json_tokener_state::json_tokener_state_eatws as (i32)) &&
                                 ((*(*tok).stack.offset((*tok).depth as (isize))).saved_state as
                                      (i32) ==
                                      json_tokener_state::json_tokener_state_finish as (i32))
                            {
                                 ({
                                      (*tok).err = json_tokener_error::json_tokener_success;
                                      0i32
                                  })
                             } else {
                                 ({
                                      (*tok).err = json_tokener_error::json_tokener_continue;
                                      0i32
                                  })
                             })
                        } else {
                            ({
                                 c = *str;
                                 1i32
                             })
                        } == 0
                    {
                        _currentBlock = 87;
                        break 'loop3;
                    }
                }
                if _currentBlock == 90 {
                    if ((*(*tok).pb).size - (*(*tok).pb).bpos) as (isize) >
                        (str as (isize)).wrapping_sub(case_start as (isize)) /
                            ::std::mem::size_of::<u8>() as (isize)
                    {
                        memcpy(
                            (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                (*mut ::std::os::raw::c_void),
                            case_start as (*const ::std::os::raw::c_void),
                            ((str as (isize)).wrapping_sub(case_start as (isize)) /
                                 ::std::mem::size_of::<u8>() as (isize)) as
                                (u64),
                        );
                        (*(*tok).pb).bpos =
                            ((*(*tok).pb).bpos as (isize) +
                                 (str as (isize)).wrapping_sub(case_start as (isize)) /
                                     ::std::mem::size_of::<u8>() as (isize)) as
                                (i32);
                        *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                    } else {
                        printbuf_memappend(
                            (*tok).pb,
                            case_start,
                            ((str as (isize)).wrapping_sub(case_start as (isize)) /
                                 ::std::mem::size_of::<u8>() as (isize)) as
                                (i32),
                        );
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                        json_tokener_state::json_tokener_state_string;
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_string_escape;
                } else {
                    if ((*(*tok).pb).size - (*(*tok).pb).bpos) as (isize) >
                        (str as (isize)).wrapping_sub(case_start as (isize)) /
                            ::std::mem::size_of::<u8>() as (isize)
                    {
                        memcpy(
                            (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                (*mut ::std::os::raw::c_void),
                            case_start as (*const ::std::os::raw::c_void),
                            ((str as (isize)).wrapping_sub(case_start as (isize)) /
                                 ::std::mem::size_of::<u8>() as (isize)) as
                                (u64),
                        );
                        (*(*tok).pb).bpos =
                            ((*(*tok).pb).bpos as (isize) +
                                 (str as (isize)).wrapping_sub(case_start as (isize)) /
                                     ::std::mem::size_of::<u8>() as (isize)) as
                                (i32);
                        *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                    } else {
                        printbuf_memappend(
                            (*tok).pb,
                            case_start,
                            ((str as (isize)).wrapping_sub(case_start as (isize)) /
                                 ::std::mem::size_of::<u8>() as (isize)) as
                                (i32),
                        );
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).current =
                        json_object_new_string_len(
                            (*(*tok).pb).buf as (*const u8),
                            (*(*tok).pb).bpos,
                        );
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                        json_tokener_state::json_tokener_state_finish;
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_eatws;
                }
                _currentBlock = 301;
            } else if _currentBlock == 98 {
                got_hi_surrogate = 0u32;
                'loop99: loop {
                    if strchr(json_hex_chars, c as (i32)).is_null() {
                        _currentBlock = 100;
                        break 'loop3;
                    }
                    (*tok).ucs_char = (*tok).ucs_char.wrapping_add(
                        if c as (i32) <= b'9' as (i32) {
                            c as (i32) - b'0' as (i32)
                        } else {
                            (c as (i32) & 7i32) + 9i32
                        } as (u32) <<
                            (3i32 -
                                 {
                                     let _old = (*tok).st_pos;
                                     (*tok).st_pos = (*tok).st_pos + 1;
                                     _old
                                 }) * 4i32,
                    );
                    if (*tok).st_pos == 4i32 {
                        if got_hi_surrogate != 0 {
                            if (*tok).ucs_char & 0xfc00u32 == 0xdc00u32 {
                                (*tok).ucs_char = ((got_hi_surrogate & 0x3ffu32) << 10i32)
                                    .wrapping_add((*tok).ucs_char & 0x3ffu32)
                                    .wrapping_add(0x10000u32);
                            } else if (*(*tok).pb).size - (*(*tok).pb).bpos > 3i32 {
                                memcpy(
                                    (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                        (*mut ::std::os::raw::c_void),
                                    utf8_replacement_char.as_mut_ptr() as
                                        (*const ::std::os::raw::c_void),
                                    3u64,
                                );
                                (*(*tok).pb).bpos = (*(*tok).pb).bpos + 3i32;
                                *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                            } else {
                                printbuf_memappend(
                                    (*tok).pb,
                                    utf8_replacement_char.as_mut_ptr() as (*const u8),
                                    3i32,
                                );
                            }
                            got_hi_surrogate = 0u32;
                        }
                        if (*tok).ucs_char < 0x80u32 {
                            _currentBlock = 151;
                            break;
                        }
                        if (*tok).ucs_char < 0x800u32 {
                            _currentBlock = 147;
                            break;
                        }
                        if !((*tok).ucs_char & 0xfc00u32 == 0xd800u32) {
                            _currentBlock = 117;
                            break;
                        }
                        got_hi_surrogate = (*tok).ucs_char;
                        if !((*tok).char_offset + 1i32 != len &&
                                 ((*tok).char_offset + 2i32 != len) &&
                                 (*str.offset(1isize) as (i32) == b'\\' as (i32)) &&
                                 (*str.offset(2isize) as (i32) == b'u' as (i32)))
                        {
                            _currentBlock = 135;
                            break;
                        }
                        if {
                            str = str.offset(1isize);
                            (*tok).char_offset = (*tok).char_offset + 1;
                            c
                        } == 0 ||
                            {
                                str = str.offset(1isize);
                                (*tok).char_offset = (*tok).char_offset + 1;
                                c
                            } == 0
                        {
                            if (*(*tok).pb).size - (*(*tok).pb).bpos > 3i32 {
                                memcpy(
                                    (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                        (*mut ::std::os::raw::c_void),
                                    utf8_replacement_char.as_mut_ptr() as
                                        (*const ::std::os::raw::c_void),
                                    3u64,
                                );
                                (*(*tok).pb).bpos = (*(*tok).pb).bpos + 3i32;
                                *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                            } else {
                                printbuf_memappend(
                                    (*tok).pb,
                                    utf8_replacement_char.as_mut_ptr() as (*const u8),
                                    3i32,
                                );
                            }
                        }
                        if {
                            str = str.offset(1isize);
                            (*tok).char_offset = (*tok).char_offset + 1;
                            c
                        } == 0 ||
                            if (*tok).char_offset == len {
                                (if (*tok).depth == 0i32 &&
                                     ((*(*tok).stack.offset((*tok).depth as (isize))).state as
                                          (i32) ==
                                          json_tokener_state::json_tokener_state_eatws as (i32)) &&
                                     ((*(*tok).stack.offset((*tok).depth as (isize))).saved_state as
                                          (i32) ==
                                          json_tokener_state::json_tokener_state_finish as (i32))
                                {
                                     ({
                                          (*tok).err = json_tokener_error::json_tokener_success;
                                          0i32
                                      })
                                 } else {
                                     ({
                                          (*tok).err = json_tokener_error::json_tokener_continue;
                                          0i32
                                      })
                                 })
                            } else {
                                ({
                                     c = *str;
                                     1i32
                                 })
                            } == 0
                        {
                            _currentBlock = 144;
                            break 'loop3;
                        }
                        (*tok).ucs_char = 0u32;
                        (*tok).st_pos = 0i32;
                    } else if {
                               str = str.offset(1isize);
                               (*tok).char_offset = (*tok).char_offset + 1;
                               c
                           } == 0 ||
                               if (*tok).char_offset == len {
                                   (if (*tok).depth == 0i32 &&
                                        ((*(*tok).stack.offset((*tok).depth as (isize))).state as
                                             (i32) ==
                                             json_tokener_state::json_tokener_state_eatws as
                                                 (i32)) &&
                                        ((*(*tok).stack.offset((*tok).depth as (isize)))
                                             .saved_state as
                                             (i32) ==
                                             json_tokener_state::json_tokener_state_finish as
                                                 (i32))
                            {
                                        ({
                                             (*tok).err = json_tokener_error::json_tokener_success;
                                             0i32
                                         })
                                    } else {
                                        ({
                                             (*tok).err = json_tokener_error::json_tokener_continue;
                                             0i32
                                         })
                                    })
                               } else {
                                   ({
                                        c = *str;
                                        1i32
                                    })
                               } == 0
                    {
                        _currentBlock = 103;
                        break 'loop3;
                    }
                }
                if _currentBlock == 117 {
                    if (*tok).ucs_char & 0xfc00u32 == 0xdc00u32 {
                        if (*(*tok).pb).size - (*(*tok).pb).bpos > 3i32 {
                            memcpy(
                                (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                    (*mut ::std::os::raw::c_void),
                                utf8_replacement_char.as_mut_ptr() as (*const ::std::os::raw::c_void),
                                3u64,
                            );
                            (*(*tok).pb).bpos = (*(*tok).pb).bpos + 3i32;
                            *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                        } else {
                            printbuf_memappend(
                                (*tok).pb,
                                utf8_replacement_char.as_mut_ptr() as (*const u8),
                                3i32,
                            );
                        }
                    } else if (*tok).ucs_char < 0x10000u32 {
                        unescaped_utf[0usize] = (0xe0u32 | (*tok).ucs_char >> 12i32) as (u8);
                        unescaped_utf[1usize] = (0x80u32 | (*tok).ucs_char >> 6i32 & 0x3fu32) as
                            (u8);
                        unescaped_utf[2usize] = (0x80u32 | (*tok).ucs_char & 0x3fu32) as (u8);
                        if (*(*tok).pb).size - (*(*tok).pb).bpos > 3i32 {
                            memcpy(
                                (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                    (*mut ::std::os::raw::c_void),
                                unescaped_utf.as_mut_ptr() as (*const ::std::os::raw::c_void),
                                3u64,
                            );
                            (*(*tok).pb).bpos = (*(*tok).pb).bpos + 3i32;
                            *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                        } else {
                            printbuf_memappend(
                                (*tok).pb,
                                unescaped_utf.as_mut_ptr() as (*const u8),
                                3i32,
                            );
                        }
                    } else if (*tok).ucs_char < 0x110000u32 {
                        unescaped_utf[0usize] = (0xf0u32 | (*tok).ucs_char >> 18i32 & 0x7u32) as
                            (u8);
                        unescaped_utf[1usize] = (0x80u32 | (*tok).ucs_char >> 12i32 & 0x3fu32) as
                            (u8);
                        unescaped_utf[2usize] = (0x80u32 | (*tok).ucs_char >> 6i32 & 0x3fu32) as
                            (u8);
                        unescaped_utf[3usize] = (0x80u32 | (*tok).ucs_char & 0x3fu32) as (u8);
                        if (*(*tok).pb).size - (*(*tok).pb).bpos > 4i32 {
                            memcpy(
                                (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                    (*mut ::std::os::raw::c_void),
                                unescaped_utf.as_mut_ptr() as (*const ::std::os::raw::c_void),
                                4u64,
                            );
                            (*(*tok).pb).bpos = (*(*tok).pb).bpos + 4i32;
                            *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                        } else {
                            printbuf_memappend(
                                (*tok).pb,
                                unescaped_utf.as_mut_ptr() as (*const u8),
                                4i32,
                            );
                        }
                    } else if (*(*tok).pb).size - (*(*tok).pb).bpos > 3i32 {
                        memcpy(
                            (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                (*mut ::std::os::raw::c_void),
                            utf8_replacement_char.as_mut_ptr() as (*const ::std::os::raw::c_void),
                            3u64,
                        );
                        (*(*tok).pb).bpos = (*(*tok).pb).bpos + 3i32;
                        *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                    } else {
                        printbuf_memappend(
                            (*tok).pb,
                            utf8_replacement_char.as_mut_ptr() as (*const u8),
                            3i32,
                        );
                    }
                } else if _currentBlock == 135 {
                    if (*(*tok).pb).size - (*(*tok).pb).bpos > 3i32 {
                        memcpy(
                            (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                (*mut ::std::os::raw::c_void),
                            utf8_replacement_char.as_mut_ptr() as (*const ::std::os::raw::c_void),
                            3u64,
                        );
                        (*(*tok).pb).bpos = (*(*tok).pb).bpos + 3i32;
                        *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                    } else {
                        printbuf_memappend(
                            (*tok).pb,
                            utf8_replacement_char.as_mut_ptr() as (*const u8),
                            3i32,
                        );
                    }
                } else if _currentBlock == 147 {
                    unescaped_utf[0usize] = (0xc0u32 | (*tok).ucs_char >> 6i32) as (u8);
                    unescaped_utf[1usize] = (0x80u32 | (*tok).ucs_char & 0x3fu32) as (u8);
                    if (*(*tok).pb).size - (*(*tok).pb).bpos > 2i32 {
                        memcpy(
                            (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                (*mut ::std::os::raw::c_void),
                            unescaped_utf.as_mut_ptr() as (*const ::std::os::raw::c_void),
                            2u64,
                        );
                        (*(*tok).pb).bpos = (*(*tok).pb).bpos + 2i32;
                        *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                    } else {
                        printbuf_memappend(
                            (*tok).pb,
                            unescaped_utf.as_mut_ptr() as (*const u8),
                            2i32,
                        );
                    }
                } else {
                    unescaped_utf[0usize] = (*tok).ucs_char as (u8);
                    if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                        memcpy(
                            (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                (*mut ::std::os::raw::c_void),
                            unescaped_utf.as_mut_ptr() as (*const ::std::os::raw::c_void),
                            1u64,
                        );
                        (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                        *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                    } else {
                        printbuf_memappend(
                            (*tok).pb,
                            unescaped_utf.as_mut_ptr() as (*const u8),
                            1i32,
                        );
                    }
                }
                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state;
                _currentBlock = 301;
            } else if _currentBlock == 166 {
                (*tok).st_pos = (*tok).st_pos + 1;
                _currentBlock = 301;
            } else if _currentBlock == 194 {
                if (*(*tok).stack.offset((*tok).depth as (isize))).state as (i32) ==
                    json_tokener_state::json_tokener_state_array_after_sep as (i32) &&
                    ((*tok).flags & 0x1i32 != 0)
                {
                    _currentBlock = 196;
                    break;
                }
                (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                    json_tokener_state::json_tokener_state_finish;
                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                    json_tokener_state::json_tokener_state_eatws;
                _currentBlock = 301;
            } else if _currentBlock == 198 {
                if c as (i32) == b']' as (i32) {
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                        json_tokener_state::json_tokener_state_finish;
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_eatws;
                } else {
                    if !(c as (i32) == b',' as (i32)) {
                        _currentBlock = 200;
                        break;
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                        json_tokener_state::json_tokener_state_array_after_sep;
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_eatws;
                }
                _currentBlock = 301;
            } else if _currentBlock == 203 {
                if c as (i32) == b'}' as (i32) {
                    if (*(*tok).stack.offset((*tok).depth as (isize))).state as (i32) ==
                        json_tokener_state::json_tokener_state_object_field_start_after_sep as
                            (i32) && ((*tok).flags & 0x1i32 != 0)
                    {
                        _currentBlock = 209;
                        break;
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                        json_tokener_state::json_tokener_state_finish;
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_eatws;
                } else {
                    if !(c as (i32) == b'\"' as (i32) || c as (i32) == b'\'' as (i32)) {
                        _currentBlock = 205;
                        break;
                    }
                    (*tok).quote_char = c;
                    printbuf_reset((*tok).pb);
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_object_field;
                }
                _currentBlock = 301;
            } else if _currentBlock == 210 {
                case_start = str;
                'loop211: loop {
                    if c as (i32) == (*tok).quote_char as (i32) {
                        _currentBlock = 221;
                        break;
                    }
                    if c as (i32) == b'\\' as (i32) {
                        _currentBlock = 217;
                        break;
                    }
                    if {
                        str = str.offset(1isize);
                        (*tok).char_offset = (*tok).char_offset + 1;
                        c
                    } == 0 ||
                        if (*tok).char_offset == len {
                            (if (*tok).depth == 0i32 &&
                                 ((*(*tok).stack.offset((*tok).depth as (isize))).state as (i32) ==
                                      json_tokener_state::json_tokener_state_eatws as (i32)) &&
                                 ((*(*tok).stack.offset((*tok).depth as (isize))).saved_state as
                                      (i32) ==
                                      json_tokener_state::json_tokener_state_finish as (i32))
                            {
                                 ({
                                      (*tok).err = json_tokener_error::json_tokener_success;
                                      0i32
                                  })
                             } else {
                                 ({
                                      (*tok).err = json_tokener_error::json_tokener_continue;
                                      0i32
                                  })
                             })
                        } else {
                            ({
                                 c = *str;
                                 1i32
                             })
                        } == 0
                    {
                        _currentBlock = 214;
                        break 'loop3;
                    }
                }
                if _currentBlock == 217 {
                    if ((*(*tok).pb).size - (*(*tok).pb).bpos) as (isize) >
                        (str as (isize)).wrapping_sub(case_start as (isize)) /
                            ::std::mem::size_of::<u8>() as (isize)
                    {
                        memcpy(
                            (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                (*mut ::std::os::raw::c_void),
                            case_start as (*const ::std::os::raw::c_void),
                            ((str as (isize)).wrapping_sub(case_start as (isize)) /
                                 ::std::mem::size_of::<u8>() as (isize)) as
                                (u64),
                        );
                        (*(*tok).pb).bpos =
                            ((*(*tok).pb).bpos as (isize) +
                                 (str as (isize)).wrapping_sub(case_start as (isize)) /
                                     ::std::mem::size_of::<u8>() as (isize)) as
                                (i32);
                        *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                    } else {
                        printbuf_memappend(
                            (*tok).pb,
                            case_start,
                            ((str as (isize)).wrapping_sub(case_start as (isize)) /
                                 ::std::mem::size_of::<u8>() as (isize)) as
                                (i32),
                        );
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                        json_tokener_state::json_tokener_state_object_field;
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_string_escape;
                } else {
                    if ((*(*tok).pb).size - (*(*tok).pb).bpos) as (isize) >
                        (str as (isize)).wrapping_sub(case_start as (isize)) /
                            ::std::mem::size_of::<u8>() as (isize)
                    {
                        memcpy(
                            (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                (*mut ::std::os::raw::c_void),
                            case_start as (*const ::std::os::raw::c_void),
                            ((str as (isize)).wrapping_sub(case_start as (isize)) /
                                 ::std::mem::size_of::<u8>() as (isize)) as
                                (u64),
                        );
                        (*(*tok).pb).bpos =
                            ((*(*tok).pb).bpos as (isize) +
                                 (str as (isize)).wrapping_sub(case_start as (isize)) /
                                     ::std::mem::size_of::<u8>() as (isize)) as
                                (i32);
                        *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                    } else {
                        printbuf_memappend(
                            (*tok).pb,
                            case_start,
                            ((str as (isize)).wrapping_sub(case_start as (isize)) /
                                 ::std::mem::size_of::<u8>() as (isize)) as
                                (i32),
                        );
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).obj_field_name =
                        strdup((*(*tok).pb).buf as (*const u8));
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                        json_tokener_state::json_tokener_state_object_field_end;
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_eatws;
                }
                _currentBlock = 301;
            } else if _currentBlock == 225 {
                if !(c as (i32) == b':' as (i32)) {
                    _currentBlock = 226;
                    break;
                }
                (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                    json_tokener_state::json_tokener_state_object_value;
                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                    json_tokener_state::json_tokener_state_eatws;
                _currentBlock = 301;
            } else if _currentBlock == 232 {
                if c as (i32) == b'}' as (i32) {
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                        json_tokener_state::json_tokener_state_finish;
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_eatws;
                } else {
                    if !(c as (i32) == b',' as (i32)) {
                        _currentBlock = 234;
                        break;
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                        json_tokener_state::json_tokener_state_object_field_start_after_sep;
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_eatws;
                }
                _currentBlock = 301;
            } else if _currentBlock == 237 {
                if c as (i32) == b'/' as (i32) || c as (i32) == b'\\' as (i32) ||
                    c as (i32) == b'\"' as (i32)
                {
                    if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                        memcpy(
                            (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                (*mut ::std::os::raw::c_void),
                            &mut c as (*mut u8) as (*const ::std::os::raw::c_void),
                            1u64,
                        );
                        (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                        *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                    } else {
                        printbuf_memappend((*tok).pb, &mut c as (*mut u8) as (*const u8), 1i32);
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        (*(*tok).stack.offset((*tok).depth as (isize))).saved_state;
                } else if c as (i32) == b'u' as (i32) {
                    (*tok).ucs_char = 0u32;
                    (*tok).st_pos = 0i32;
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_escape_unicode;
                } else {
                    if !(c as (i32) == b'f' as (i32) || c as (i32) == b't' as (i32) ||
                             c as (i32) == b'r' as (i32) ||
                             c as (i32) == b'n' as (i32) ||
                             c as (i32) == b'b' as (i32))
                    {
                        _currentBlock = 240;
                        break;
                    }
                    if c as (i32) == b'b' as (i32) {
                        if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                            memcpy(
                                (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                    (*mut ::std::os::raw::c_void),
                                (*b"\x08\0").as_ptr() as (*const ::std::os::raw::c_void),
                                1u64,
                            );
                            (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                            *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                        } else {
                            printbuf_memappend((*tok).pb, (*b"\x08\0").as_ptr(), 1i32);
                        }
                    } else if c as (i32) == b'n' as (i32) {
                        if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                            memcpy(
                                (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                    (*mut ::std::os::raw::c_void),
                                (*b"\n\0").as_ptr() as (*const ::std::os::raw::c_void),
                                1u64,
                            );
                            (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                            *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                        } else {
                            printbuf_memappend((*tok).pb, (*b"\n\0").as_ptr(), 1i32);
                        }
                    } else if c as (i32) == b'r' as (i32) {
                        if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                            memcpy(
                                (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                    (*mut ::std::os::raw::c_void),
                                (*b"\r\0").as_ptr() as (*const ::std::os::raw::c_void),
                                1u64,
                            );
                            (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                            *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                        } else {
                            printbuf_memappend((*tok).pb, (*b"\r\0").as_ptr(), 1i32);
                        }
                    } else if c as (i32) == b't' as (i32) {
                        if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                            memcpy(
                                (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                    (*mut ::std::os::raw::c_void),
                                (*b"\t\0").as_ptr() as (*const ::std::os::raw::c_void),
                                1u64,
                            );
                            (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                            *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                        } else {
                            printbuf_memappend((*tok).pb, (*b"\t\0").as_ptr(), 1i32);
                        }
                    } else if c as (i32) == b'f' as (i32) {
                        if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                            memcpy(
                                (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                                    (*mut ::std::os::raw::c_void),
                                (*b"\x0C\0").as_ptr() as (*const ::std::os::raw::c_void),
                                1u64,
                            );
                            (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                            *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                        } else {
                            printbuf_memappend((*tok).pb, (*b"\x0C\0").as_ptr(), 1i32);
                        }
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        (*(*tok).stack.offset((*tok).depth as (isize))).saved_state;
                }
                _currentBlock = 301;
            } else if _currentBlock == 267 {
                if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                    memcpy(
                        (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                            (*mut ::std::os::raw::c_void),
                        &mut c as (*mut u8) as (*const ::std::os::raw::c_void),
                        1u64,
                    );
                    (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                    *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                } else {
                    printbuf_memappend((*tok).pb, &mut c as (*mut u8) as (*const u8), 1i32);
                }
                if c as (i32) == b'/' as (i32) {
                    if false {
                        mc_debug(
                            (*b"json_tokener_comment: %s\n\0").as_ptr(),
                            (*(*tok).pb).buf,
                        );
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_eatws;
                } else {
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_comment;
                }
                _currentBlock = 301;
            } else if _currentBlock == 281 {
                if c as (i32) == b'[' as (i32) {
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_eatws;
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                        json_tokener_state::json_tokener_state_array;
                    (*(*tok).stack.offset((*tok).depth as (isize))).current =
                        json_object_new_array();
                } else {
                    if !(c as (i32) == b'{' as (i32)) {
                        _currentBlock = 283;
                        break;
                    }
                    (*(*tok).stack.offset((*tok).depth as (isize))).state =
                        json_tokener_state::json_tokener_state_eatws;
                    (*(*tok).stack.offset((*tok).depth as (isize))).saved_state =
                        json_tokener_state::json_tokener_state_object_field_start;
                    (*(*tok).stack.offset((*tok).depth as (isize))).current =
                        json_object_new_object();
                }
                _currentBlock = 301;
            } else if _currentBlock == 288 {
                if (*tok).flags & 0x1i32 != 0 {
                    _currentBlock = 289;
                    break;
                }
                _currentBlock = 290;
            } else if _currentBlock == 296 {
                printbuf_reset((*tok).pb);
                if (*(*tok).pb).size - (*(*tok).pb).bpos > 1i32 {
                    memcpy(
                        (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                            (*mut ::std::os::raw::c_void),
                        &mut c as (*mut u8) as (*const ::std::os::raw::c_void),
                        1u64,
                    );
                    (*(*tok).pb).bpos = (*(*tok).pb).bpos + 1i32;
                    *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                } else {
                    printbuf_memappend((*tok).pb, &mut c as (*mut u8) as (*const u8), 1i32);
                }
                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                    json_tokener_state::json_tokener_state_comment_start;
                _currentBlock = 301;
            }
            if _currentBlock == 290 {
                (*(*tok).stack.offset((*tok).depth as (isize))).state =
                    json_tokener_state::json_tokener_state_string;
                printbuf_reset((*tok).pb);
                (*tok).quote_char = c;
            }
            if {
                str = str.offset(1isize);
                (*tok).char_offset = (*tok).char_offset + 1;
                c
            } == 0
            {
                _currentBlock = 303;
                break;
            }
        }
        if _currentBlock == 37 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_unexpected;
        } else if _currentBlock == 47 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_null;
        } else if _currentBlock == 55 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_comment;
        } else if _currentBlock == 68 {
            if ((*(*tok).pb).size - (*(*tok).pb).bpos) as (isize) >
                (str as (isize)).wrapping_sub(case_start as (isize)) /
                    ::std::mem::size_of::<u8>() as (isize)
            {
                memcpy(
                    (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                        (*mut ::std::os::raw::c_void),
                    case_start as (*const ::std::os::raw::c_void),
                    ((str as (isize)).wrapping_sub(case_start as (isize)) /
                         ::std::mem::size_of::<u8>() as (isize)) as (u64),
                );
                (*(*tok).pb).bpos = ((*(*tok).pb).bpos as (isize) +
                                         (str as (isize)).wrapping_sub(case_start as (isize)) /
                                             ::std::mem::size_of::<u8>() as (isize)) as
                    (i32);
                *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
            } else {
                printbuf_memappend(
                    (*tok).pb,
                    case_start,
                    ((str as (isize)).wrapping_sub(case_start as (isize)) /
                         ::std::mem::size_of::<u8>() as (isize)) as (i32),
                );
            }
        } else if _currentBlock == 80 {
            if ((*(*tok).pb).size - (*(*tok).pb).bpos) as (isize) >
                (str as (isize)).wrapping_sub(case_start as (isize)) /
                    ::std::mem::size_of::<u8>() as (isize)
            {
                memcpy(
                    (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                        (*mut ::std::os::raw::c_void),
                    case_start as (*const ::std::os::raw::c_void),
                    ((str as (isize)).wrapping_sub(case_start as (isize)) /
                         ::std::mem::size_of::<u8>() as (isize)) as (u64),
                );
                (*(*tok).pb).bpos = ((*(*tok).pb).bpos as (isize) +
                                         (str as (isize)).wrapping_sub(case_start as (isize)) /
                                             ::std::mem::size_of::<u8>() as (isize)) as
                    (i32);
                *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
            } else {
                printbuf_memappend(
                    (*tok).pb,
                    case_start,
                    ((str as (isize)).wrapping_sub(case_start as (isize)) /
                         ::std::mem::size_of::<u8>() as (isize)) as (i32),
                );
            }
        } else if _currentBlock == 87 {
            if ((*(*tok).pb).size - (*(*tok).pb).bpos) as (isize) >
                (str as (isize)).wrapping_sub(case_start as (isize)) /
                    ::std::mem::size_of::<u8>() as (isize)
            {
                memcpy(
                    (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                        (*mut ::std::os::raw::c_void),
                    case_start as (*const ::std::os::raw::c_void),
                    ((str as (isize)).wrapping_sub(case_start as (isize)) /
                         ::std::mem::size_of::<u8>() as (isize)) as (u64),
                );
                (*(*tok).pb).bpos = ((*(*tok).pb).bpos as (isize) +
                                         (str as (isize)).wrapping_sub(case_start as (isize)) /
                                             ::std::mem::size_of::<u8>() as (isize)) as
                    (i32);
                *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
            } else {
                printbuf_memappend(
                    (*tok).pb,
                    case_start,
                    ((str as (isize)).wrapping_sub(case_start as (isize)) /
                         ::std::mem::size_of::<u8>() as (isize)) as (i32),
                );
            }
        } else if _currentBlock == 100 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_string;
        } else if _currentBlock == 103 {
            if got_hi_surrogate != 0 {
                if (*(*tok).pb).size - (*(*tok).pb).bpos > 3i32 {
                    memcpy(
                        (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                            (*mut ::std::os::raw::c_void),
                        utf8_replacement_char.as_mut_ptr() as (*const ::std::os::raw::c_void),
                        3u64,
                    );
                    (*(*tok).pb).bpos = (*(*tok).pb).bpos + 3i32;
                    *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
                } else {
                    printbuf_memappend(
                        (*tok).pb,
                        utf8_replacement_char.as_mut_ptr() as (*const u8),
                        3i32,
                    );
                }
            }
        } else if _currentBlock == 144 {
            if (*(*tok).pb).size - (*(*tok).pb).bpos > 3i32 {
                memcpy(
                    (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                        (*mut ::std::os::raw::c_void),
                    utf8_replacement_char.as_mut_ptr() as (*const ::std::os::raw::c_void),
                    3u64,
                );
                (*(*tok).pb).bpos = (*(*tok).pb).bpos + 3i32;
                *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
            } else {
                printbuf_memappend(
                    (*tok).pb,
                    utf8_replacement_char.as_mut_ptr() as (*const u8),
                    3i32,
                );
            }
        } else if _currentBlock == 162 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_boolean;
        } else if _currentBlock == 177 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_number;
        } else if _currentBlock == 182 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_number;
        } else if _currentBlock == 187 {
            if (*(*tok).pb).size - (*(*tok).pb).bpos > case_len {
                memcpy(
                    (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                        (*mut ::std::os::raw::c_void),
                    case_start as (*const ::std::os::raw::c_void),
                    case_len as (u64),
                );
                (*(*tok).pb).bpos = (*(*tok).pb).bpos + case_len;
                *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
            } else {
                printbuf_memappend((*tok).pb, case_start, case_len);
            }
        } else if _currentBlock == 193 {
            (*tok).err = json_tokener_error::json_tokener_error_depth;
        } else if _currentBlock == 196 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_unexpected;
        } else if _currentBlock == 200 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_array;
        } else if _currentBlock == 205 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_object_key_name;
        } else if _currentBlock == 209 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_unexpected;
        } else if _currentBlock == 214 {
            if ((*(*tok).pb).size - (*(*tok).pb).bpos) as (isize) >
                (str as (isize)).wrapping_sub(case_start as (isize)) /
                    ::std::mem::size_of::<u8>() as (isize)
            {
                memcpy(
                    (*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) as
                        (*mut ::std::os::raw::c_void),
                    case_start as (*const ::std::os::raw::c_void),
                    ((str as (isize)).wrapping_sub(case_start as (isize)) /
                         ::std::mem::size_of::<u8>() as (isize)) as (u64),
                );
                (*(*tok).pb).bpos = ((*(*tok).pb).bpos as (isize) +
                                         (str as (isize)).wrapping_sub(case_start as (isize)) /
                                             ::std::mem::size_of::<u8>() as (isize)) as
                    (i32);
                *(*(*tok).pb).buf.offset((*(*tok).pb).bpos as (isize)) = b'\0';
            } else {
                printbuf_memappend(
                    (*tok).pb,
                    case_start,
                    ((str as (isize)).wrapping_sub(case_start as (isize)) /
                         ::std::mem::size_of::<u8>() as (isize)) as (i32),
                );
            }
        } else if _currentBlock == 226 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_object_key_sep;
        } else if _currentBlock == 230 {
            (*tok).err = json_tokener_error::json_tokener_error_depth;
        } else if _currentBlock == 234 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_object_value_sep;
        } else if _currentBlock == 240 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_string;
        } else if _currentBlock == 283 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_unexpected;
        } else if _currentBlock == 289 {
            (*tok).err = json_tokener_error::json_tokener_error_parse_unexpected;
        }
        if c != 0 &&
            ((*(*tok).stack.offset((*tok).depth as (isize))).state as (i32) ==
                 json_tokener_state::json_tokener_state_finish as (i32)) &&
            ((*tok).depth == 0i32) && ((*tok).flags & 0x1i32 != 0)
        {
            (*tok).err = json_tokener_error::json_tokener_error_parse_unexpected;
        }
        if c == 0 {
            if (*(*tok).stack.offset((*tok).depth as (isize))).state as (i32) !=
                json_tokener_state::json_tokener_state_finish as (i32) &&
                ((*(*tok).stack.offset((*tok).depth as (isize))).saved_state as (i32) !=
                     json_tokener_state::json_tokener_state_finish as (i32))
            {
                (*tok).err = json_tokener_error::json_tokener_error_parse_eof;
            }
        }
        setlocale(1i32, oldlocale as (*const u8));
        if !oldlocale.is_null() {
            free(oldlocale as (*mut ::std::os::raw::c_void));
        }
        (if (*tok).err as (i32) == json_tokener_error::json_tokener_success as (i32) {
             let mut ret: *mut ::json_object::json_object =
                 json_object_get((*(*tok).stack.offset((*tok).depth as (isize))).current);
             let mut ii: i32;
             ii = (*tok).depth;
             'loop315: loop {
                 if !(ii >= 0i32) {
                     break;
                 }
                 json_tokener_reset_level(tok, ii);
                 ii = ii - 1;
             }
             ret
         } else {
             if false {
                 mc_debug(
                    (*b"json_tokener_parse_ex: error %s at offset %d\n\0").as_ptr(),
                    json_tokener_errors[(*tok).err as (usize)],
                    (*tok).char_offset,
                );
             }
             0i32 as (*mut ::std::os::raw::c_void) as (*mut ::json_object::json_object)
         })
    }
}

#[no_mangle]
pub unsafe extern "C" fn json_tokener_set_flags(mut tok: *mut json_tokener, mut flags: i32) {
    (*tok).flags = flags;
}
