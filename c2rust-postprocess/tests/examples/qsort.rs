#![allow(
    dead_code,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]
#[no_mangle]
pub unsafe extern "C" fn swap(mut a: *mut ::core::ffi::c_int, mut b: *mut ::core::ffi::c_int) {
    let mut t: ::core::ffi::c_int = *a;
    *a = *b;
    *b = t;
}
#[no_mangle]
pub unsafe extern "C" fn partition(
    mut arr: *mut ::core::ffi::c_int,
    mut low: ::core::ffi::c_int,
    mut high: ::core::ffi::c_int,
) -> ::core::ffi::c_int {
    let mut pivot: ::core::ffi::c_int = *arr.offset(high as isize);
    let mut i: ::core::ffi::c_int = low - 1 as ::core::ffi::c_int;
    let mut j: ::core::ffi::c_int = low;
    while j <= high - 1 as ::core::ffi::c_int {
        if *arr.offset(j as isize) <= pivot {
            i += 1;
            swap(
                arr.offset(i as isize) as *mut ::core::ffi::c_int,
                arr.offset(j as isize) as *mut ::core::ffi::c_int,
            );
        }
        j += 1;
    }
    swap(
        arr.offset((i + 1 as ::core::ffi::c_int) as isize) as *mut ::core::ffi::c_int,
        arr.offset(high as isize) as *mut ::core::ffi::c_int,
    );
    return i + 1 as ::core::ffi::c_int;
}
#[no_mangle]
pub unsafe extern "C" fn quickSort(
    mut arr: *mut ::core::ffi::c_int,
    mut low: ::core::ffi::c_int,
    mut high: ::core::ffi::c_int,
) {
    if low < high {
        let mut pi: ::core::ffi::c_int = partition(arr, low, high);
        quickSort(arr, low, pi - 1 as ::core::ffi::c_int);
        quickSort(arr, pi + 1 as ::core::ffi::c_int, high);
    }
}
