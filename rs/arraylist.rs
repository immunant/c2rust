
extern "C" {
    fn calloc(__nmemb: u64, __size: u64) -> *mut ::std::os::raw::c_void;
    fn free(__ptr: *mut ::std::os::raw::c_void);
    fn memset(__s: *mut ::std::os::raw::c_void, __c: i32, __n: u64) -> *mut ::std::os::raw::c_void;
    fn qsort(
        __base: *mut ::std::os::raw::c_void,
        __nmemb: u64,
        __size: u64,
        __compar: unsafe extern "C" fn(*const ::std::os::raw::c_void, *const ::std::os::raw::c_void)
                                       -> i32,
    );
    fn realloc(__ptr: *mut ::std::os::raw::c_void, __size: u64) -> *mut ::std::os::raw::c_void;
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

pub unsafe fn array_list_new(
    mut free_fn: unsafe extern "C" fn(*mut ::std::os::raw::c_void),
) -> *mut array_list {
    let mut arr: *mut array_list;
    arr = calloc(1u64, ::std::mem::size_of::<array_list>() as (u64)) as (*mut array_list);
    if arr.is_null() {
        0i32 as (*mut ::std::os::raw::c_void) as (*mut array_list)
    } else {
        (*arr).size = 32i32;
        (*arr).length = 0i32;
        (*arr).free_fn = free_fn;
        (if {
            (*arr).array = calloc(
                ::std::mem::size_of::<*mut ::std::os::raw::c_void>() as (u64),
                (*arr).size as (u64),
            ) as (*mut *mut ::std::os::raw::c_void);
            (*arr).array
        }.is_null()
        {
             free(arr as (*mut ::std::os::raw::c_void));
             0i32 as (*mut ::std::os::raw::c_void) as (*mut array_list)
         } else {
             arr
         })
    }
}
#[export_name = "array_list_new"]
pub unsafe extern "C" fn array_list_new_wrapper(
    free_fn: unsafe extern "C" fn(*mut ::std::os::raw::c_void),
) -> *mut array_list {
    array_list_new(free_fn)
}

pub unsafe fn array_list_free(mut arr: *mut array_list) {
    let mut i: i32;
    i = 0i32;
    'loop1: loop {
        if !(i < (*arr).length) {
            break;
        }
        if !(*(*arr).array.offset(i as (isize))).is_null() {
            ((*arr).free_fn)(*(*arr).array.offset(i as (isize)));
        }
        i = i + 1;
    }
    free((*arr).array as (*mut ::std::os::raw::c_void));
    free(arr as (*mut ::std::os::raw::c_void));
}
#[export_name = "array_list_free"]
pub unsafe extern "C" fn array_list_free_wrapper(arr: *mut array_list) {
    array_list_free(arr)
}

pub unsafe fn array_list_get_idx(
    mut arr: *mut array_list,
    mut i: i32,
) -> *mut ::std::os::raw::c_void {
    if i >= (*arr).length {
        0i32 as (*mut ::std::os::raw::c_void)
    } else {
        *(*arr).array.offset(i as (isize))
    }
}
#[export_name = "array_list_get_idx"]
pub unsafe extern "C" fn array_list_get_idx_wrapper(
    arr: *mut array_list,
    i: i32,
) -> *mut ::std::os::raw::c_void {
    array_list_get_idx(arr, i)
}

unsafe extern "C" fn array_list_expand_internal(mut arr: *mut array_list, mut max: i32) -> i32 {
    let mut t: *mut ::std::os::raw::c_void;
    let mut new_size: i32;
    if max < (*arr).size {
        0i32
    } else {
        new_size = if (*arr).size << 1i32 > max {
            (*arr).size << 1i32
        } else {
            max
        };
        (if {
            t = realloc(
                (*arr).array as (*mut ::std::os::raw::c_void),
                (new_size as (usize)).wrapping_mul(
                    ::std::mem::size_of::<*mut ::std::os::raw::c_void>(),
                ) as (u64),
            );
            t
        }.is_null()
        {
             -1i32
         } else {
             (*arr).array = t as (*mut *mut ::std::os::raw::c_void);
             memset(
                (*arr).array.offset((*arr).size as (isize)) as (*mut ::std::os::raw::c_void),
                0i32,
                ((new_size - (*arr).size) as (usize)).wrapping_mul(
                    ::std::mem::size_of::<*mut ::std::os::raw::c_void>(),
                ) as (u64),
            );
             (*arr).size = new_size;
             0i32
         })
    }
}

pub unsafe fn array_list_put_idx(
    mut arr: *mut array_list,
    mut idx: i32,
    mut data: *mut ::std::os::raw::c_void,
) -> i32 {
    if array_list_expand_internal(arr, idx + 1i32) != 0 {
        -1i32
    } else {
        if !(*(*arr).array.offset(idx as (isize))).is_null() {
            ((*arr).free_fn)(*(*arr).array.offset(idx as (isize)));
        }
        *(*arr).array.offset(idx as (isize)) = data;
        if (*arr).length <= idx {
            (*arr).length = idx + 1i32;
        }
        0i32
    }
}
#[export_name = "array_list_put_idx"]
pub unsafe extern "C" fn array_list_put_idx_wrapper(
    arr: *mut array_list,
    idx: i32,
    data: *mut ::std::os::raw::c_void,
) -> i32 {
    array_list_put_idx(arr, idx, data)
}

pub unsafe fn array_list_add(
    mut arr: *mut array_list,
    mut data: *mut ::std::os::raw::c_void,
) -> i32 {
    array_list_put_idx(arr, (*arr).length, data)
}
#[export_name = "array_list_add"]
pub unsafe extern "C" fn array_list_add_wrapper(
    arr: *mut array_list,
    data: *mut ::std::os::raw::c_void,
) -> i32 {
    array_list_add(arr, data)
}

pub unsafe fn array_list_sort(
    mut arr: *mut array_list,
    mut sort_fn: unsafe extern "C" fn(*const ::std::os::raw::c_void, *const ::std::os::raw::c_void)
                                      -> i32,
) {
    qsort(
        (*arr).array as (*mut ::std::os::raw::c_void),
        (*arr).length as (u64),
        ::std::mem::size_of::<*mut ::std::os::raw::c_void>() as (u64),
        sort_fn,
    );
}
#[export_name = "array_list_sort"]
pub unsafe extern "C" fn array_list_sort_wrapper(
    arr: *mut array_list,
    sort_fn: unsafe extern "C" fn(*const ::std::os::raw::c_void, *const ::std::os::raw::c_void) -> i32,
) {
    array_list_sort(arr, sort_fn)
}

pub unsafe fn array_list_length(mut arr: *mut array_list) -> i32 {
    (*arr).length
}
#[export_name = "array_list_length"]
pub unsafe extern "C" fn array_list_length_wrapper(arr: *mut array_list) -> i32 {
    array_list_length(arr)
}
