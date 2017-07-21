
extern "C" {
    fn json_object_get_object(obj: *mut json_object) -> *mut lh_table;
}

pub enum printbuf {
}

static mut kObjectEndIterValue: *const ::std::os::raw::c_void =
    0i32 as (*mut ::std::os::raw::c_void) as (*const ::std::os::raw::c_void);

#[derive(Copy)]
#[repr(C)]
pub struct json_object_iterator {
    pub opaque_: *const ::std::os::raw::c_void,
}

impl Clone for json_object_iterator {
    fn clone(&self) -> Self {
        *self
    }
}

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
pub struct lh_entry {
    pub k: *mut ::std::os::raw::c_void,
    pub v: *const ::std::os::raw::c_void,
    pub next: *mut lh_entry,
    pub prev: *mut lh_entry,
}

impl Clone for lh_entry {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Copy)]
#[repr(C)]
pub struct lh_table {
    pub size: i32,
    pub count: i32,
    pub collisions: i32,
    pub resizes: i32,
    pub lookups: i32,
    pub inserts: i32,
    pub deletes: i32,
    pub name: *const u8,
    pub head: *mut lh_entry,
    pub tail: *mut lh_entry,
    pub table: *mut lh_entry,
    pub free_fn: unsafe extern "C" fn(*mut lh_entry),
    pub hash_fn: unsafe extern "C" fn(*const ::std::os::raw::c_void) -> u64,
    pub equal_fn:
        unsafe extern "C" fn(*const ::std::os::raw::c_void, *const ::std::os::raw::c_void) -> i32,
}

impl Clone for lh_table {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Copy)]
#[repr(C)]
pub struct array_list {
    pub array: *mut *mut ::std::os::raw::c_void,
    pub length: i32,
    pub size: i32,
    pub free_fn: unsafe extern "C" fn(*mut ::std::os::raw::c_void),
}

impl Clone for array_list {
    fn clone(&self) -> Self {
        *self
    }
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
    pub c_boolean : i32,
    pub c_double : f64,
    pub c_int64 : i32,
    pub c_object : *mut lh_table,
    pub c_array : *mut array_list,
    pub c_string : Struct1,
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
        unsafe extern "C" fn(*mut json_object, *mut ::json_object::printbuf, i32, i32) -> i32,
    pub _ref_count: i32,
    pub _pb: *mut ::json_object::printbuf,
    pub o: data,
    pub _user_delete: unsafe extern "C" fn(*mut json_object, *mut ::std::os::raw::c_void),
    pub _userdata: *mut ::std::os::raw::c_void,
}

impl Clone for json_object {
    fn clone(&self) -> Self {
        *self
    }
}

#[no_mangle]
pub unsafe extern "C" fn json_object_iter_begin(mut obj: *mut json_object) -> json_object_iterator {
    let mut iter: json_object_iterator = ::std::mem::uninitialized();
    let mut pTable: *mut lh_table;
    pTable = json_object_get_object(obj as (*mut json_object)) as (*mut lh_table);
    iter.opaque_ = (*pTable).head as (*const ::std::os::raw::c_void);
    iter
}

#[no_mangle]
pub unsafe extern "C" fn json_object_iter_end(mut obj: *const json_object) -> json_object_iterator {
    let mut iter: json_object_iterator = ::std::mem::uninitialized();
    iter.opaque_ = kObjectEndIterValue;
    iter
}

#[no_mangle]
pub unsafe extern "C" fn json_object_iter_next(mut iter: *mut json_object_iterator) {
    (*iter).opaque_ = (*((*iter).opaque_ as (*mut lh_entry))).next as
        (*const ::std::os::raw::c_void);
}

#[no_mangle]
pub unsafe extern "C" fn json_object_iter_peek_name(
    mut iter: *const json_object_iterator,
) -> *const u8 {
    (*((*iter).opaque_ as (*mut lh_entry))).k as (*const u8)
}

#[no_mangle]
pub unsafe extern "C" fn json_object_iter_peek_value(
    mut iter: *const json_object_iterator,
) -> *mut json_object {
    (*((*iter).opaque_ as (*mut lh_entry))).v as (*mut json_object)
}

#[no_mangle]
pub unsafe extern "C" fn json_object_iter_equal(
    mut iter1: *const json_object_iterator,
    mut iter2: *const json_object_iterator,
) -> i32 {
    ((*iter1).opaque_ == (*iter2).opaque_) as (i32)
}

#[no_mangle]
pub unsafe extern "C" fn json_object_iter_init_default() -> json_object_iterator {
    let mut iter: json_object_iterator = ::std::mem::uninitialized();
    iter.opaque_ = 0i32 as (*mut ::std::os::raw::c_void) as (*const ::std::os::raw::c_void);
    iter
}
