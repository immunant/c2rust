extern "C" {}

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
    pub c_int64: i32,
    pub c_object: *mut ::linkhash::lh_table,
    pub c_array: *mut ::arraylist::array_list,
    pub c_string: Struct1,
}

impl Clone for data {
    fn clone(&self) -> Self {
        *self
    }
}





pub unsafe fn json_object_iter_begin(
    mut obj: *mut ::json_object::json_object,
) -> json_object_iterator {
    let mut iter: json_object_iterator = ::std::mem::uninitialized();
    let mut pTable: *mut ::linkhash::lh_table;
    pTable = ::json_object::json_object_get_object(obj as (*mut ::json_object::json_object)) as
        (*mut ::linkhash::lh_table);
    iter.opaque_ = (*pTable).head as (*const ::std::os::raw::c_void);
    iter
}
#[export_name = "json_object_iter_begin"]
pub unsafe extern "C" fn json_object_iter_begin_wrapper(
    obj: *mut ::json_object::json_object,
) -> json_object_iterator {
    json_object_iter_begin(obj)
}

pub unsafe fn json_object_iter_end(
    mut obj: *const ::json_object::json_object,
) -> json_object_iterator {
    let mut iter: json_object_iterator = ::std::mem::uninitialized();
    iter.opaque_ = kObjectEndIterValue;
    iter
}
#[export_name = "json_object_iter_end"]
pub unsafe extern "C" fn json_object_iter_end_wrapper(
    obj: *const ::json_object::json_object,
) -> json_object_iterator {
    json_object_iter_end(obj)
}

pub unsafe fn json_object_iter_next(mut iter: *mut json_object_iterator) {
    (*iter).opaque_ =
        (*((*iter).opaque_ as (*mut ::linkhash::lh_entry))).next as (*const ::std::os::raw::c_void);
}
#[export_name = "json_object_iter_next"]
pub unsafe extern "C" fn json_object_iter_next_wrapper(iter: *mut json_object_iterator) {
    json_object_iter_next(iter)
}

pub unsafe fn json_object_iter_peek_name(mut iter: *const json_object_iterator) -> *const u8 {
    (*((*iter).opaque_ as (*mut ::linkhash::lh_entry))).k as (*const u8)
}
#[export_name = "json_object_iter_peek_name"]
pub unsafe extern "C" fn json_object_iter_peek_name_wrapper(
    iter: *const json_object_iterator,
) -> *const u8 {
    json_object_iter_peek_name(iter)
}

pub unsafe fn json_object_iter_peek_value(
    mut iter: *const json_object_iterator,
) -> *mut ::json_object::json_object {
    (*((*iter).opaque_ as (*mut ::linkhash::lh_entry))).v as (*mut ::json_object::json_object)
}
#[export_name = "json_object_iter_peek_value"]
pub unsafe extern "C" fn json_object_iter_peek_value_wrapper(
    iter: *const json_object_iterator,
) -> *mut ::json_object::json_object {
    json_object_iter_peek_value(iter)
}

pub unsafe fn json_object_iter_equal(
    mut iter1: *const json_object_iterator,
    mut iter2: *const json_object_iterator,
) -> i32 {
    ((*iter1).opaque_ == (*iter2).opaque_) as (i32)
}
#[export_name = "json_object_iter_equal"]
pub unsafe extern "C" fn json_object_iter_equal_wrapper(
    iter1: *const json_object_iterator,
    iter2: *const json_object_iterator,
) -> i32 {
    json_object_iter_equal(iter1, iter2)
}

pub unsafe fn json_object_iter_init_default() -> json_object_iterator {
    let mut iter: json_object_iterator = ::std::mem::uninitialized();
    iter.opaque_ = 0i32 as (*mut ::std::os::raw::c_void) as (*const ::std::os::raw::c_void);
    iter
}
#[export_name = "json_object_iter_init_default"]
pub unsafe extern "C" fn json_object_iter_init_default_wrapper() -> json_object_iterator {
    json_object_iter_init_default()
}
