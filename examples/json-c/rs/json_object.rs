extern "C" {
    fn __errno_location() -> *mut i32;
    fn calloc(__nmemb: u64, __size: u64) -> *mut ::std::os::raw::c_void;
    fn free(__ptr: *mut ::std::os::raw::c_void);
    fn malloc(__size: u64) -> *mut ::std::os::raw::c_void;
    fn memcpy(
        __dest: *mut ::std::os::raw::c_void,
        __src: *const ::std::os::raw::c_void,
        __n: u64,
    ) -> *mut ::std::os::raw::c_void;
    fn snprintf(__s: *mut u8, __maxlen: u64, __format: *const u8, ...) -> i32;
    fn sprintbuf(p: *mut ::printbuf::printbuf, msg: *const u8, ...) -> i32;
    fn strchr(__s: *const u8, __c: i32) -> *mut u8;
    fn strdup(__s: *const u8) -> *mut u8;
    fn strlen(__s: *const u8) -> u64;
    fn strtod(__nptr: *const u8, __endptr: *mut *mut u8) -> f64;
}

#[no_mangle]
pub static mut json_number_chars: *const u8 = 0 as *const _;

unsafe extern "C" fn _init_json_number_chars() {
    json_number_chars = (*b"0123456789.+-eE\0").as_ptr();
}

#[link_section = ".ctors"]
pub static INIT_JSON_NUMBER_CHARS: unsafe extern "C" fn() = _init_json_number_chars;

#[no_mangle]
pub static mut json_hex_chars: *const u8 = 0 as *const _;

unsafe extern "C" fn _init_json_hex_chars() {
    json_hex_chars = (*b"0123456789abcdefABCDEF\0").as_ptr();
}

#[link_section = ".ctors"]
pub static INIT_JSON_HEX_CHARS: unsafe extern "C" fn() = _init_json_hex_chars;

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

















#[derive(Copy)]
#[repr(C)]
pub struct Struct1 {
    pub str: *mut u8,
    pub len: i32,
}

impl Clone for Struct1 {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Copy)]
#[repr(C)]
pub union data {
    pub c_boolean: i32,
    pub c_double: f64,
    pub c_int64: i64,
    pub c_object: *mut ::linkhash::lh_table,
    pub c_array: *mut ::arraylist::array_list,
    pub c_string: Struct1,
}

impl Clone for data {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Copy)]
#[repr(C)]
pub struct json_object {
    pub o_type: json_type,
    pub _delete: unsafe extern "C" fn(*mut json_object),
    pub _to_json_string:
        unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32) -> i32,
    pub _ref_count: i32,
    pub _pb: *mut ::printbuf::printbuf,
    pub o: data,
    pub _user_delete: unsafe extern "C" fn(*mut json_object, *mut ::std::os::raw::c_void),
    pub _userdata: *mut ::std::os::raw::c_void,
}

impl Clone for json_object {
    fn clone(&self) -> Self {
        *self
    }
}

pub unsafe fn json_object_get(mut jso: *mut json_object) -> *mut json_object {
    if !jso.is_null() {
        (*jso)._ref_count = (*jso)._ref_count + 1;
    }
    jso
}
#[export_name = "json_object_get"]
pub unsafe extern "C" fn json_object_get_wrapper(jso: *mut json_object) -> *mut json_object {
    json_object_get(jso)
}

pub unsafe fn json_object_put(mut jso: *mut json_object) -> i32 {
    if !jso.is_null() {
        (*jso)._ref_count = (*jso)._ref_count - 1;
        if (*jso)._ref_count == 0 {
            if (*jso)._user_delete as usize != 0 {
                ((*jso)._user_delete)(jso as (*mut json_object), (*jso)._userdata);
            }
            ((*jso)._delete)(jso as (*mut json_object));
            return 1i32;
        }
    }
    0i32
}
#[export_name = "json_object_put"]
pub unsafe extern "C" fn json_object_put_wrapper(jso: *mut json_object) -> i32 {
    json_object_put(jso)
}

pub unsafe fn json_object_is_type(mut jso: *mut json_object, mut type_: json_type) -> i32 {
    if jso.is_null() {
        (type_ as (i32) == json_type::json_type_null as (i32)) as (i32)
    } else {
        ((*jso).o_type as (i32) == type_ as (i32)) as (i32)
    }
}
#[export_name = "json_object_is_type"]
pub unsafe extern "C" fn json_object_is_type_wrapper(
    jso: *mut json_object,
    type_: json_type,
) -> i32 {
    json_object_is_type(jso, type_)
}

pub unsafe fn json_object_get_type(mut jso: *mut json_object) -> json_type {
    if jso.is_null() {
        json_type::json_type_null
    } else {
        (*jso).o_type
    }
}
#[export_name = "json_object_get_type"]
pub unsafe extern "C" fn json_object_get_type_wrapper(jso: *mut json_object) -> json_type {
    json_object_get_type(jso)
}

pub unsafe fn json_object_set_serializer(
    mut jso: *mut json_object,
    mut to_string_func: unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
        -> i32,
    mut userdata: *mut ::std::os::raw::c_void,
    mut user_delete: unsafe extern "C" fn(*mut json_object, *mut ::std::os::raw::c_void),
) {
    if (*jso)._user_delete as usize != 0 {
        ((*jso)._user_delete)(jso, (*jso)._userdata);
    }
    (*jso)._userdata = 0i32 as (*mut ::std::os::raw::c_void);
    (*jso)._user_delete = ::std::mem::transmute(0_usize);
    if to_string_func as (*mut ::std::os::raw::c_void) == 0i32 as (*mut ::std::os::raw::c_void) {
        let switch2 = (*jso).o_type;
        if switch2 as (i32) == json_type::json_type_string as (i32) {
            (*jso)._to_json_string = json_object_string_to_json_string as
                (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                    -> i32);
        } else if switch2 as (i32) == json_type::json_type_array as (i32) {
            (*jso)._to_json_string = json_object_array_to_json_string as
                (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                    -> i32);
        } else if switch2 as (i32) == json_type::json_type_object as (i32) {
            (*jso)._to_json_string = json_object_object_to_json_string as
                (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                    -> i32);
        } else if switch2 as (i32) == json_type::json_type_int as (i32) {
            (*jso)._to_json_string = json_object_int_to_json_string as
                (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                    -> i32);
        } else if switch2 as (i32) == json_type::json_type_double as (i32) {
            (*jso)._to_json_string = json_object_double_to_json_string as
                (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                    -> i32);
        } else if switch2 as (i32) == json_type::json_type_boolean as (i32) {
            (*jso)._to_json_string = json_object_boolean_to_json_string as
                (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                    -> i32);
        } else if switch2 as (i32) == json_type::json_type_null as (i32) {
            (*jso)._to_json_string = ::std::mem::transmute(0_usize);
        }
    } else {
        (*jso)._to_json_string = to_string_func;
        (*jso)._userdata = userdata;
        (*jso)._user_delete = user_delete;
    }
}
#[export_name = "json_object_set_serializer"]
pub unsafe extern "C" fn json_object_set_serializer_wrapper(
    jso: *mut json_object,
    to_string_func: unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
        -> i32,
    userdata: *mut ::std::os::raw::c_void,
    user_delete: unsafe extern "C" fn(*mut json_object, *mut ::std::os::raw::c_void),
) {
    json_object_set_serializer(jso, to_string_func, userdata, user_delete)
}

pub unsafe fn json_object_to_json_string_ext(
    mut jso: *mut json_object,
    mut flags: i32,
) -> *const u8 {
    if jso.is_null() {
        (*b"null\0").as_ptr()
    } else if (*jso)._pb.is_null() && {
        (*jso)._pb = ::printbuf::printbuf_new();
        (*jso)._pb
    }.is_null()
    {
        0i32 as (*mut ::std::os::raw::c_void) as (*const u8)
    } else {
        ::printbuf::printbuf_reset((*jso)._pb);
        if ((*jso)._to_json_string)(jso as (*mut json_object), (*jso)._pb, 0i32, flags) < 0i32 {
            0i32 as (*mut ::std::os::raw::c_void) as (*const u8)
        } else {
            (*(*jso)._pb).buf as (*const u8)
        }
    }
}
#[export_name = "json_object_to_json_string_ext"]
pub unsafe extern "C" fn json_object_to_json_string_ext_wrapper(
    jso: *mut json_object,
    flags: i32,
) -> *const u8 {
    json_object_to_json_string_ext(jso, flags)
}

pub unsafe fn json_object_to_json_string(mut jso: *mut json_object) -> *const u8 {
    json_object_to_json_string_ext(jso, 1i32 << 0i32)
}
#[export_name = "json_object_to_json_string"]
pub unsafe extern "C" fn json_object_to_json_string_wrapper(jso: *mut json_object) -> *const u8 {
    json_object_to_json_string(jso)
}

#[derive(Copy)]
#[repr(C)]
pub struct json_object_iter {
    pub key: *mut u8,
    pub val: *mut json_object,
    pub entry: *mut ::linkhash::lh_entry,
}

impl Clone for json_object_iter {
    fn clone(&self) -> Self {
        *self
    }
}

unsafe extern "C" fn indent(mut pb: *mut ::printbuf::printbuf, mut level: i32, mut flags: i32) {
    if flags & 1i32 << 1i32 != 0 {
        ::printbuf::printbuf_memset(pb, -1i32, b' ' as (i32), level * 2i32);
    }
}

unsafe extern "C" fn json_escape_str(
    mut pb: *mut ::printbuf::printbuf,
    mut str: *mut u8,
    mut len: i32,
) -> i32 {
    let mut pos: i32 = 0i32;
    let mut start_offset: i32 = 0i32;
    let mut c: u8;
    'loop1: loop {
        if {
            let _old = len;
            len = len - 1;
            _old
        } == 0
        {
            break;
        }
        c = *str.offset(pos as (isize));
        if c as (i32) == b'/' as (i32) || c as (i32) == b'\\' as (i32) ||
            c as (i32) == b'\"' as (i32) || c as (i32) == b'\x0C' as (i32) ||
            c as (i32) == b'\t' as (i32) || c as (i32) == b'\r' as (i32) ||
            c as (i32) == b'\n' as (i32) ||
            c as (i32) == b'\x08' as (i32)
        {
            if pos - start_offset > 0i32 {
                ::printbuf::printbuf_memappend(
                    pb,
                    str.offset(start_offset as (isize)) as (*const u8),
                    pos - start_offset,
                );
            }
            if c as (i32) == b'\x08' as (i32) {
                ::printbuf::printbuf_memappend(pb, (*b"\\b\0").as_ptr(), 2i32);
            } else if c as (i32) == b'\n' as (i32) {
                ::printbuf::printbuf_memappend(pb, (*b"\\n\0").as_ptr(), 2i32);
            } else if c as (i32) == b'\r' as (i32) {
                ::printbuf::printbuf_memappend(pb, (*b"\\r\0").as_ptr(), 2i32);
            } else if c as (i32) == b'\t' as (i32) {
                ::printbuf::printbuf_memappend(pb, (*b"\\t\0").as_ptr(), 2i32);
            } else if c as (i32) == b'\x0C' as (i32) {
                ::printbuf::printbuf_memappend(pb, (*b"\\f\0").as_ptr(), 2i32);
            } else if c as (i32) == b'\"' as (i32) {
                ::printbuf::printbuf_memappend(pb, (*b"\\\"\0").as_ptr(), 2i32);
            } else if c as (i32) == b'\\' as (i32) {
                ::printbuf::printbuf_memappend(pb, (*b"\\\\\0").as_ptr(), 2i32);
            } else if c as (i32) == b'/' as (i32) {
                ::printbuf::printbuf_memappend(pb, (*b"\\/\0").as_ptr(), 2i32);
            }
            start_offset = {
                pos = pos + 1;
                pos
            };
        } else if c as (i32) < b' ' as (i32) {
            if pos - start_offset > 0i32 {
                ::printbuf::printbuf_memappend(
                    pb,
                    str.offset(start_offset as (isize)) as (*const u8),
                    pos - start_offset,
                );
            }
            sprintbuf(
                pb,
                (*b"\\u00%c%c\0").as_ptr(),
                *json_hex_chars.offset((c as (i32) >> 4i32) as (isize)) as (i32),
                *json_hex_chars.offset((c as (i32) & 0xfi32) as (isize)) as (i32),
            );
            start_offset = {
                pos = pos + 1;
                pos
            };
        } else {
            pos = pos + 1;
        }
    }
    if pos - start_offset > 0i32 {
        ::printbuf::printbuf_memappend(
            pb,
            str.offset(start_offset as (isize)) as (*const u8),
            pos - start_offset,
        );
    }
    0i32
}

unsafe extern "C" fn json_object_object_to_json_string(
    mut jso: *mut json_object,
    mut pb: *mut ::printbuf::printbuf,
    mut level: i32,
    mut flags: i32,
) -> i32 {
    let mut had_children: i32 = 0i32;
    let mut iter: json_object_iter = ::std::mem::uninitialized();
    sprintbuf(pb, (*b"{\0").as_ptr());
    if flags & 1i32 << 1i32 != 0 {
        sprintbuf(pb, (*b"\n\0").as_ptr());
    }
    iter.entry =
        (*json_object_get_object(jso as (*mut json_object))).head as (*mut ::linkhash::lh_entry);
    'loop3: loop {
        if if !iter.entry.is_null() {
            {
                iter.key = (*iter.entry).k as (*mut u8);
                iter.val = (*iter.entry).v as (*mut json_object) as (*mut json_object);
                iter.entry
            }
        } else {
            0i32 as (*mut ::linkhash::lh_entry)
        }.is_null()
        {
            break;
        }
        if had_children != 0 {
            sprintbuf(pb, (*b",\0").as_ptr());
            if flags & 1i32 << 1i32 != 0 {
                sprintbuf(pb, (*b"\n\0").as_ptr());
            }
        }
        had_children = 1i32;
        if flags & 1i32 << 0i32 != 0 {
            sprintbuf(pb, (*b" \0").as_ptr());
        }
        indent(pb, level + 1i32, flags);
        sprintbuf(pb, (*b"\"\0").as_ptr());
        json_escape_str(pb, iter.key, strlen(iter.key as (*const u8)) as (i32));
        if flags & 1i32 << 0i32 != 0 {
            sprintbuf(pb, (*b"\": \0").as_ptr());
        } else {
            sprintbuf(pb, (*b"\":\0").as_ptr());
        }
        if iter.val == 0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object) {
            sprintbuf(pb, (*b"null\0").as_ptr());
        } else {
            ((*iter.val)._to_json_string)(iter.val, pb, level + 1i32, flags);
        }
        iter.entry = (*iter.entry).next;
    }
    if flags & 1i32 << 1i32 != 0 {
        if had_children != 0 {
            sprintbuf(pb, (*b"\n\0").as_ptr());
        }
        indent(pb, level, flags);
    }
    if flags & 1i32 << 0i32 != 0 {
        sprintbuf(pb, (*b" }\0").as_ptr())
    } else {
        sprintbuf(pb, (*b"}\0").as_ptr())
    }
}

unsafe extern "C" fn json_object_generic_delete(mut jso: *mut json_object) {
    ::printbuf::printbuf_free((*jso)._pb);
    free(jso as (*mut ::std::os::raw::c_void));
}

unsafe extern "C" fn json_object_new(mut o_type: json_type) -> *mut json_object {
    let mut jso: *mut json_object;
    jso = calloc(::std::mem::size_of::<json_object>() as (u64), 1u64) as (*mut json_object);
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else {
        (*jso).o_type = o_type;
        (*jso)._ref_count = 1i32;
        (*jso)._delete = json_object_generic_delete as (unsafe extern "C" fn(*mut json_object));
        jso
    }
}

unsafe extern "C" fn json_object_object_delete(mut jso: *mut json_object) {
    ::linkhash::lh_table_free((*jso).o.c_object);
    json_object_generic_delete(jso);
}

unsafe extern "C" fn json_object_lh_entry_free(mut ent: *mut ::linkhash::lh_entry) {
    free((*ent).k);
    json_object_put((*ent).v as (*mut json_object));
}

pub unsafe fn json_object_new_object() -> *mut json_object {
    let mut jso: *mut json_object = json_object_new(json_type::json_type_object);
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else {
        (*jso)._delete = json_object_object_delete as (unsafe extern "C" fn(*mut json_object));
        (*jso)._to_json_string = json_object_object_to_json_string as
            (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                -> i32);
        (*jso).o.c_object = ::linkhash::lh_kchar_table_new(
            16i32,
            0i32 as (*mut ::std::os::raw::c_void) as (*const u8),
            json_object_lh_entry_free as (unsafe extern "C" fn(*mut ::linkhash::lh_entry)),
        );
        jso
    }
}
#[export_name = "json_object_new_object"]
pub unsafe extern "C" fn json_object_new_object_wrapper() -> *mut json_object {
    json_object_new_object()
}

pub unsafe fn json_object_get_object(mut jso: *mut json_object) -> *mut ::linkhash::lh_table {
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut ::linkhash::lh_table)
    } else {
        let switch3 = (*jso).o_type;
        if switch3 as (i32) == json_type::json_type_object as (i32) {
            (*jso).o.c_object
        } else {
            0i32 as (*mut ::std::os::raw::c_void) as (*mut ::linkhash::lh_table)
        }
    }
}
#[export_name = "json_object_get_object"]
pub unsafe extern "C" fn json_object_get_object_wrapper(
    jso: *mut json_object,
) -> *mut ::linkhash::lh_table {
    json_object_get_object(jso)
}

pub unsafe fn json_object_object_add(
    mut jso: *mut json_object,
    mut key: *const u8,
    mut val: *mut json_object,
) {
    let mut existing_value: *mut json_object =
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object);
    let mut existing_entry: *mut ::linkhash::lh_entry;
    existing_entry = ::linkhash::lh_table_lookup_entry(
        (*jso).o.c_object,
        key as (*mut ::std::os::raw::c_void) as (*const ::std::os::raw::c_void),
    );
    if existing_entry.is_null() {
        ::linkhash::lh_table_insert(
            (*jso).o.c_object,
            strdup(key) as (*mut ::std::os::raw::c_void),
            val as (*const ::std::os::raw::c_void),
        );
    } else {
        existing_value = (*existing_entry).v as (*mut ::std::os::raw::c_void) as (*mut json_object);
        if !existing_value.is_null() {
            json_object_put(existing_value as (*mut json_object));
        }
        (*existing_entry).v = val as (*const ::std::os::raw::c_void);
    }
}
#[export_name = "json_object_object_add"]
pub unsafe extern "C" fn json_object_object_add_wrapper(
    jso: *mut json_object,
    key: *const u8,
    val: *mut json_object,
) {
    json_object_object_add(jso, key, val)
}

pub unsafe fn json_object_object_length(mut jso: *mut json_object) -> i32 {
    ::linkhash::lh_table_length((*jso).o.c_object)
}
#[export_name = "json_object_object_length"]
pub unsafe extern "C" fn json_object_object_length_wrapper(jso: *mut json_object) -> i32 {
    json_object_object_length(jso)
}

pub unsafe fn json_object_object_get(
    mut jso: *mut json_object,
    mut key: *const u8,
) -> *mut json_object {
    let mut result: *mut json_object = 0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object);
    json_object_object_get_ex(
        jso as (*mut json_object),
        key,
        &mut result as (*mut *mut json_object) as (*mut *mut json_object),
    );
    result
}
#[export_name = "json_object_object_get"]
pub unsafe extern "C" fn json_object_object_get_wrapper(
    jso: *mut json_object,
    key: *const u8,
) -> *mut json_object {
    json_object_object_get(jso, key)
}

pub unsafe fn json_object_object_get_ex(
    mut jso: *mut json_object,
    mut key: *const u8,
    mut value: *mut *mut json_object,
) -> i32 {
    if value != 0i32 as (*mut ::std::os::raw::c_void) as (*mut *mut json_object) {
        *value = 0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object);
    }
    if 0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object) == jso {
        0i32
    } else {
        let switch4 = (*jso).o_type;
        if switch4 as (i32) == json_type::json_type_object as (i32) {
            ::linkhash::lh_table_lookup_ex(
                (*jso).o.c_object,
                key as (*mut ::std::os::raw::c_void) as (*const ::std::os::raw::c_void),
                value as (*mut *mut ::std::os::raw::c_void),
            )
        } else {
            if value != 0i32 as (*mut ::std::os::raw::c_void) as (*mut *mut json_object) {
                *value = 0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object);
            }
            0i32
        }
    }
}
#[export_name = "json_object_object_get_ex"]
pub unsafe extern "C" fn json_object_object_get_ex_wrapper(
    jso: *mut json_object,
    key: *const u8,
    value: *mut *mut json_object,
) -> i32 {
    json_object_object_get_ex(jso, key, value)
}

pub unsafe fn json_object_object_del(mut jso: *mut json_object, mut key: *const u8) {
    ::linkhash::lh_table_delete((*jso).o.c_object, key as (*const ::std::os::raw::c_void));
}
#[export_name = "json_object_object_del"]
pub unsafe extern "C" fn json_object_object_del_wrapper(jso: *mut json_object, key: *const u8) {
    json_object_object_del(jso, key)
}

unsafe extern "C" fn json_object_boolean_to_json_string(
    mut jso: *mut json_object,
    mut pb: *mut ::printbuf::printbuf,
    mut level: i32,
    mut flags: i32,
) -> i32 {
    if (*jso).o.c_boolean != 0 {
        sprintbuf(pb, (*b"true\0").as_ptr())
    } else {
        sprintbuf(pb, (*b"false\0").as_ptr())
    }
}

pub unsafe fn json_object_new_boolean(mut b: i32) -> *mut json_object {
    let mut jso: *mut json_object = json_object_new(json_type::json_type_boolean);
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else {
        (*jso)._to_json_string = json_object_boolean_to_json_string as
            (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                -> i32);
        (*jso).o.c_boolean = b;
        jso
    }
}
#[export_name = "json_object_new_boolean"]
pub unsafe extern "C" fn json_object_new_boolean_wrapper(b: i32) -> *mut json_object {
    json_object_new_boolean(b)
}

pub unsafe fn json_object_get_boolean(mut jso: *mut json_object) -> i32 {
    if jso.is_null() {
        0i32
    } else {
        let switch5 = (*jso).o_type;
        if switch5 as (i32) == json_type::json_type_string as (i32) {
            ((*jso).o.c_string.len != 0i32) as (i32)
        } else if switch5 as (i32) == json_type::json_type_double as (i32) {
            ((*jso).o.c_double != 0i32 as (f64)) as (i32)
        } else if switch5 as (i32) == json_type::json_type_int as (i32) {
            ((*jso).o.c_int64 != 0i64) as (i32)
        } else if switch5 as (i32) == json_type::json_type_boolean as (i32) {
            (*jso).o.c_boolean
        } else {
            0i32
        }
    }
}
#[export_name = "json_object_get_boolean"]
pub unsafe extern "C" fn json_object_get_boolean_wrapper(jso: *mut json_object) -> i32 {
    json_object_get_boolean(jso)
}

unsafe extern "C" fn json_object_int_to_json_string(
    mut jso: *mut json_object,
    mut pb: *mut ::printbuf::printbuf,
    mut level: i32,
    mut flags: i32,
) -> i32 {
    sprintbuf(pb, (*b"%ld\0").as_ptr(), (*jso).o.c_int64)
}

pub unsafe fn json_object_new_int(mut i: i32) -> *mut json_object {
    let mut jso: *mut json_object = json_object_new(json_type::json_type_int);
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else {
        (*jso)._to_json_string = json_object_int_to_json_string as
            (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                -> i32);
        (*jso).o.c_int64 = i as (i64);
        jso
    }
}
#[export_name = "json_object_new_int"]
pub unsafe extern "C" fn json_object_new_int_wrapper(i: i32) -> *mut json_object {
    json_object_new_int(i)
}

pub unsafe fn json_object_get_int(mut jso: *mut json_object) -> i32 {
    let mut cint64: i64;
    let mut o_type: json_type;
    if jso.is_null() {
        0i32
    } else {
        o_type = (*jso).o_type;
        cint64 = (*jso).o.c_int64;
        if o_type as (i32) == json_type::json_type_string as (i32) {
            if ::json_util::json_parse_int64(
                (*jso).o.c_string.str as (*const u8),
                &mut cint64 as (*mut i64),
            ) != 0i32
            {
                return 0i32;
            } else {
                o_type = json_type::json_type_int;
            }
        }
        if o_type as (i32) == json_type::json_type_boolean as (i32) {
            (*jso).o.c_boolean
        } else if o_type as (i32) == json_type::json_type_double as (i32) {
            (*jso).o.c_double as (i32)
        } else if o_type as (i32) == json_type::json_type_int as (i32) {
            if cint64 <= (-2147483647i32 - 1i32) as (i64) {
                -2147483647i32 - 1i32
            } else if cint64 >= 2147483647i64 {
                2147483647i32
            } else {
                cint64 as (i32)
            }
        } else {
            0i32
        }
    }
}
#[export_name = "json_object_get_int"]
pub unsafe extern "C" fn json_object_get_int_wrapper(jso: *mut json_object) -> i32 {
    json_object_get_int(jso)
}

pub unsafe fn json_object_new_int64(mut i: i64) -> *mut json_object {
    let mut jso: *mut json_object = json_object_new(json_type::json_type_int);
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else {
        (*jso)._to_json_string = json_object_int_to_json_string as
            (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                -> i32);
        (*jso).o.c_int64 = i;
        jso
    }
}
#[export_name = "json_object_new_int64"]
pub unsafe extern "C" fn json_object_new_int64_wrapper(i: i64) -> *mut json_object {
    json_object_new_int64(i)
}

pub unsafe fn json_object_get_int64(mut jso: *mut json_object) -> i64 {
    let mut cint: i64 = ::std::mem::uninitialized();
    if jso.is_null() {
        0i64
    } else {
        let switch6 = (*jso).o_type;
        if switch6 as (i32) == json_type::json_type_string as (i32) {
            if ::json_util::json_parse_int64(
                (*jso).o.c_string.str as (*const u8),
                &mut cint as (*mut i64),
            ) == 0i32
            {
                return cint;
            }
        } else if switch6 as (i32) == json_type::json_type_boolean as (i32) {
            return (*jso).o.c_boolean as (i64);
        } else if switch6 as (i32) == json_type::json_type_double as (i32) {
            return (*jso).o.c_double as (i64);
        } else if switch6 as (i32) == json_type::json_type_int as (i32) {
            return (*jso).o.c_int64;
        }
        0i64
    }
}
#[export_name = "json_object_get_int64"]
pub unsafe extern "C" fn json_object_get_int64_wrapper(jso: *mut json_object) -> i64 {
    json_object_get_int64(jso)
}

unsafe extern "C" fn json_object_double_to_json_string(
    mut jso: *mut json_object,
    mut pb: *mut ::printbuf::printbuf,
    mut level: i32,
    mut flags: i32,
) -> i32 {
    let mut buf: [u8; 128] = ::std::mem::uninitialized();
    let mut p: *mut u8;
    let mut q: *mut u8;
    let mut size: i32;
    if f64::is_nan((*jso).o.c_double) {
        size = snprintf(
            buf.as_mut_ptr(),
            ::std::mem::size_of::<[u8; 128]>() as (u64),
            (*b"NaN\0").as_ptr(),
        );
    } else if f64::is_infinite((*jso).o.c_double) {
        if (*jso).o.c_double > 0i32 as (f64) {
            size = snprintf(
                buf.as_mut_ptr(),
                ::std::mem::size_of::<[u8; 128]>() as (u64),
                (*b"Infinity\0").as_ptr(),
            );
        } else {
            size = snprintf(
                buf.as_mut_ptr(),
                ::std::mem::size_of::<[u8; 128]>() as (u64),
                (*b"-Infinity\0").as_ptr(),
            );
        }
    } else {
        size = snprintf(
            buf.as_mut_ptr(),
            ::std::mem::size_of::<[u8; 128]>() as (u64),
            (*b"%.17g\0").as_ptr(),
            (*jso).o.c_double,
        );
    }
    p = strchr(buf.as_mut_ptr() as (*const u8), b',' as (i32));
    if !p.is_null() {
        *p = b'.';
    } else {
        p = strchr(buf.as_mut_ptr() as (*const u8), b'.' as (i32));
    }
    if !p.is_null() && flags & 1i32 << 2i32 != 0 {
        p = p.offset(1isize);
        q = p;
        'loop12: loop {
            if *q == 0 {
                break;
            }
            if *q as (i32) != b'0' as (i32) {
                p = q;
            }
            q = q.offset(1isize);
        }
        *{
            p = p.offset(1isize);
            p
        } = 0u8;
        size = ((p as (isize)).wrapping_sub(buf.as_mut_ptr() as (isize)) /
            ::std::mem::size_of::<u8>() as (isize)) as (i32);
    }
    ::printbuf::printbuf_memappend(pb, buf.as_mut_ptr() as (*const u8), size);
    size
}

pub unsafe fn json_object_new_double(mut d: f64) -> *mut json_object {
    let mut jso: *mut json_object = json_object_new(json_type::json_type_double);
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else {
        (*jso)._to_json_string = json_object_double_to_json_string as
            (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                -> i32);
        (*jso).o.c_double = d;
        jso
    }
}
#[export_name = "json_object_new_double"]
pub unsafe extern "C" fn json_object_new_double_wrapper(d: f64) -> *mut json_object {
    json_object_new_double(d)
}

pub unsafe fn json_object_new_double_s(mut d: f64, mut ds: *const u8) -> *mut json_object {
    let mut jso: *mut json_object = json_object_new_double(d);
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else {
        json_object_set_serializer(
            jso as (*mut json_object),
            json_object_userdata_to_json_string,
            strdup(ds) as (*mut ::std::os::raw::c_void),
            json_object_free_userdata,
        );
        jso
    }
}
#[export_name = "json_object_new_double_s"]
pub unsafe extern "C" fn json_object_new_double_s_wrapper(
    d: f64,
    ds: *const u8,
) -> *mut json_object {
    json_object_new_double_s(d, ds)
}

#[no_mangle]
pub unsafe extern "C" fn json_object_userdata_to_json_string(
    mut jso: *mut json_object,
    mut pb: *mut ::printbuf::printbuf,
    mut level: i32,
    mut flags: i32,
) -> i32 {
    let mut userdata_len: i32 = strlen((*jso)._userdata as (*const u8)) as (i32);
    ::printbuf::printbuf_memappend(pb, (*jso)._userdata as (*const u8), userdata_len);
    userdata_len
}

#[no_mangle]
pub unsafe extern "C" fn json_object_free_userdata(
    mut jso: *mut json_object,
    mut userdata: *mut ::std::os::raw::c_void,
) {
    free(userdata);
}

pub unsafe fn json_object_get_double(mut jso: *mut json_object) -> f64 {
    let mut cdouble: f64;
    let mut errPtr: *mut u8 = 0i32 as (*mut ::std::os::raw::c_void) as (*mut u8);
    if jso.is_null() {
        0.0f64
    } else {
        let switch7 = (*jso).o_type;
        if switch7 as (i32) == json_type::json_type_string as (i32) {
            *__errno_location() = 0i32;
            cdouble = strtod(
                (*jso).o.c_string.str as (*const u8),
                &mut errPtr as (*mut *mut u8),
            );
            if errPtr == (*jso).o.c_string.str {
                0.0f64
            } else if *errPtr as (i32) != b'\0' as (i32) {
                0.0f64
            } else {
                if (1.0f64 / 0.0f64 == cdouble || -(1.0f64 / 0.0f64) == cdouble) &&
                    34i32 == *__errno_location()
                {
                    cdouble = 0.0f64;
                }
                cdouble
            }
        } else if switch7 as (i32) == json_type::json_type_boolean as (i32) {
            (*jso).o.c_boolean as (f64)
        } else if switch7 as (i32) == json_type::json_type_int as (i32) {
            (*jso).o.c_int64 as (f64)
        } else if switch7 as (i32) == json_type::json_type_double as (i32) {
            (*jso).o.c_double
        } else {
            0.0f64
        }
    }
}
#[export_name = "json_object_get_double"]
pub unsafe extern "C" fn json_object_get_double_wrapper(jso: *mut json_object) -> f64 {
    json_object_get_double(jso)
}

unsafe extern "C" fn json_object_string_to_json_string(
    mut jso: *mut json_object,
    mut pb: *mut ::printbuf::printbuf,
    mut level: i32,
    mut flags: i32,
) -> i32 {
    sprintbuf(pb, (*b"\"\0").as_ptr());
    json_escape_str(pb, (*jso).o.c_string.str, (*jso).o.c_string.len);
    sprintbuf(pb, (*b"\"\0").as_ptr());
    0i32
}

unsafe extern "C" fn json_object_string_delete(mut jso: *mut json_object) {
    free((*jso).o.c_string.str as (*mut ::std::os::raw::c_void));
    json_object_generic_delete(jso);
}

pub unsafe fn json_object_new_string(mut s: *const u8) -> *mut json_object {
    let mut jso: *mut json_object = json_object_new(json_type::json_type_string);
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else {
        (*jso)._delete = json_object_string_delete as (unsafe extern "C" fn(*mut json_object));
        (*jso)._to_json_string = json_object_string_to_json_string as
            (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                -> i32);
        (*jso).o.c_string.str = strdup(s);
        (*jso).o.c_string.len = strlen(s) as (i32);
        jso
    }
}
#[export_name = "json_object_new_string"]
pub unsafe extern "C" fn json_object_new_string_wrapper(s: *const u8) -> *mut json_object {
    json_object_new_string(s)
}

pub unsafe fn json_object_new_string_len(mut s: *const u8, mut len: i32) -> *mut json_object {
    let mut jso: *mut json_object = json_object_new(json_type::json_type_string);
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else {
        (*jso)._delete = json_object_string_delete as (unsafe extern "C" fn(*mut json_object));
        (*jso)._to_json_string = json_object_string_to_json_string as
            (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                -> i32);
        (*jso).o.c_string.str = malloc((len + 1i32) as (u64)) as (*mut u8);
        memcpy(
            (*jso).o.c_string.str as (*mut ::std::os::raw::c_void),
            s as (*mut ::std::os::raw::c_void) as (*const ::std::os::raw::c_void),
            len as (u64),
        );
        *(*jso).o.c_string.str.offset(len as (isize)) = b'\0';
        (*jso).o.c_string.len = len;
        jso
    }
}
#[export_name = "json_object_new_string_len"]
pub unsafe extern "C" fn json_object_new_string_len_wrapper(
    s: *const u8,
    len: i32,
) -> *mut json_object {
    json_object_new_string_len(s, len)
}

pub unsafe fn json_object_get_string(mut jso: *mut json_object) -> *const u8 {
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*const u8)
    } else {
        let switch8 = (*jso).o_type;
        if switch8 as (i32) == json_type::json_type_string as (i32) {
            (*jso).o.c_string.str as (*const u8)
        } else {
            json_object_to_json_string(jso)
        }
    }
}
#[export_name = "json_object_get_string"]
pub unsafe extern "C" fn json_object_get_string_wrapper(jso: *mut json_object) -> *const u8 {
    json_object_get_string(jso)
}

pub unsafe fn json_object_get_string_len(mut jso: *mut json_object) -> i32 {
    if jso.is_null() {
        0i32
    } else {
        let switch9 = (*jso).o_type;
        if switch9 as (i32) == json_type::json_type_string as (i32) {
            (*jso).o.c_string.len
        } else {
            0i32
        }
    }
}
#[export_name = "json_object_get_string_len"]
pub unsafe extern "C" fn json_object_get_string_len_wrapper(jso: *mut json_object) -> i32 {
    json_object_get_string_len(jso)
}

unsafe extern "C" fn json_object_array_to_json_string(
    mut jso: *mut json_object,
    mut pb: *mut ::printbuf::printbuf,
    mut level: i32,
    mut flags: i32,
) -> i32 {
    let mut had_children: i32 = 0i32;
    let mut ii: i32;
    sprintbuf(pb, (*b"[\0").as_ptr());
    if flags & 1i32 << 1i32 != 0 {
        sprintbuf(pb, (*b"\n\0").as_ptr());
    }
    ii = 0i32;
    'loop3: loop {
        if !(ii < json_object_array_length(jso as (*mut json_object))) {
            break;
        }
        let mut val: *mut json_object;
        if had_children != 0 {
            sprintbuf(pb, (*b",\0").as_ptr());
            if flags & 1i32 << 1i32 != 0 {
                sprintbuf(pb, (*b"\n\0").as_ptr());
            }
        }
        had_children = 1i32;
        if flags & 1i32 << 0i32 != 0 {
            sprintbuf(pb, (*b" \0").as_ptr());
        }
        indent(pb, level + 1i32, flags);
        val = json_object_array_get_idx(jso as (*mut json_object), ii) as (*mut json_object);
        if val == 0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object) {
            sprintbuf(pb, (*b"null\0").as_ptr());
        } else {
            ((*val)._to_json_string)(val as (*mut json_object), pb, level + 1i32, flags);
        }
        ii = ii + 1;
    }
    if flags & 1i32 << 1i32 != 0 {
        if had_children != 0 {
            sprintbuf(pb, (*b"\n\0").as_ptr());
        }
        indent(pb, level, flags);
    }
    if flags & 1i32 << 0i32 != 0 {
        sprintbuf(pb, (*b" ]\0").as_ptr())
    } else {
        sprintbuf(pb, (*b"]\0").as_ptr())
    }
}

unsafe extern "C" fn json_object_array_delete(mut jso: *mut json_object) {
    ::arraylist::array_list_free((*jso).o.c_array);
    json_object_generic_delete(jso);
}

unsafe extern "C" fn json_object_array_entry_free(mut data: *mut ::std::os::raw::c_void) {
    json_object_put(data as (*mut json_object));
}

pub unsafe fn json_object_new_array() -> *mut json_object {
    let mut jso: *mut json_object = json_object_new(json_type::json_type_array);
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut json_object)
    } else {
        (*jso)._delete = json_object_array_delete as (unsafe extern "C" fn(*mut json_object));
        (*jso)._to_json_string = json_object_array_to_json_string as
            (unsafe extern "C" fn(*mut json_object, *mut ::printbuf::printbuf, i32, i32)
                -> i32);
        (*jso).o.c_array = ::arraylist::array_list_new(json_object_array_entry_free);
        jso
    }
}
#[export_name = "json_object_new_array"]
pub unsafe extern "C" fn json_object_new_array_wrapper() -> *mut json_object {
    json_object_new_array()
}

pub unsafe fn json_object_get_array(mut jso: *mut json_object) -> *mut ::arraylist::array_list {
    if jso.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut ::arraylist::array_list)
    } else {
        let switch10 = (*jso).o_type;
        if switch10 as (i32) == json_type::json_type_array as (i32) {
            (*jso).o.c_array
        } else {
            0i32 as (*mut ::std::os::raw::c_void) as (*mut ::arraylist::array_list)
        }
    }
}
#[export_name = "json_object_get_array"]
pub unsafe extern "C" fn json_object_get_array_wrapper(
    jso: *mut json_object,
) -> *mut ::arraylist::array_list {
    json_object_get_array(jso)
}

pub unsafe fn json_object_array_sort(
    mut jso: *mut json_object,
    mut sort_fn: unsafe extern "C" fn(*const ::std::os::raw::c_void, *const ::std::os::raw::c_void)
        -> i32,
) {
    ::arraylist::array_list_sort((*jso).o.c_array, sort_fn);
}
#[export_name = "json_object_array_sort"]
pub unsafe extern "C" fn json_object_array_sort_wrapper(
    jso: *mut json_object,
    sort_fn: unsafe extern "C" fn(*const ::std::os::raw::c_void, *const ::std::os::raw::c_void)
        -> i32,
) {
    json_object_array_sort(jso, sort_fn)
}

pub unsafe fn json_object_array_length(mut jso: *mut json_object) -> i32 {
    ::arraylist::array_list_length((*jso).o.c_array)
}
#[export_name = "json_object_array_length"]
pub unsafe extern "C" fn json_object_array_length_wrapper(jso: *mut json_object) -> i32 {
    json_object_array_length(jso)
}

pub unsafe fn json_object_array_add(mut jso: *mut json_object, mut val: *mut json_object) -> i32 {
    ::arraylist::array_list_add((*jso).o.c_array, val as (*mut ::std::os::raw::c_void))
}
#[export_name = "json_object_array_add"]
pub unsafe extern "C" fn json_object_array_add_wrapper(
    jso: *mut json_object,
    val: *mut json_object,
) -> i32 {
    json_object_array_add(jso, val)
}

pub unsafe fn json_object_array_put_idx(
    mut jso: *mut json_object,
    mut idx: i32,
    mut val: *mut json_object,
) -> i32 {
    ::arraylist::array_list_put_idx((*jso).o.c_array, idx, val as (*mut ::std::os::raw::c_void))
}
#[export_name = "json_object_array_put_idx"]
pub unsafe extern "C" fn json_object_array_put_idx_wrapper(
    jso: *mut json_object,
    idx: i32,
    val: *mut json_object,
) -> i32 {
    json_object_array_put_idx(jso, idx, val)
}

pub unsafe fn json_object_array_get_idx(
    mut jso: *mut json_object,
    mut idx: i32,
) -> *mut json_object {
    ::arraylist::array_list_get_idx((*jso).o.c_array, idx) as (*mut json_object)
}
#[export_name = "json_object_array_get_idx"]
pub unsafe extern "C" fn json_object_array_get_idx_wrapper(
    jso: *mut json_object,
    idx: i32,
) -> *mut json_object {
    json_object_array_get_idx(jso, idx)
}
