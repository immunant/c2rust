#![allow(dead_code, mutable_transmutes, non_camel_case_types, non_snake_case,
    non_upper_case_globals, unused_assignments, unused_mut)]
#![feature(rustc_private)]

extern crate libc;

#[no_mangle]
pub unsafe extern "C" fn insertion_sort(n: libc::c_int, p: *mut libc::c_int) {
let mut i: libc::c_int = 1 as libc::c_int;
while i < n {
   let tmp: libc::c_int = *p.offset(i as isize);
   let mut j: libc::c_int = i;
   while j > 0 as libc::c_int &&
             *p.offset((j - 1 as libc::c_int) as isize) > tmp {
       *p.offset(j as isize) =
           *p.offset((j - 1 as libc::c_int) as isize);
       j -= 1
   }
   *p.offset(j as isize) = tmp;
   i += 1
};
}

#[no_mangle]
pub unsafe extern "C" fn check_eq(n: libc::c_int, p: *mut libc::c_int,
                             q: *mut libc::c_int) {
let mut i: libc::c_int = 0 as libc::c_int;
while i < n {
   assert!(*p.offset(i as isize) == *q.offset(i as isize));
   i += 1
};
}

unsafe fn main_0() -> libc::c_int {
let mut arr1: [libc::c_int; 3] = [1, 3, 2];
insertion_sort(3 as libc::c_int, arr1.as_mut_ptr());
let mut expect1: [libc::c_int; 3] = [1, 2, 3];
check_eq(3 as libc::c_int, arr1.as_mut_ptr(), expect1.as_mut_ptr());

let mut arr2: [libc::c_int; 7] = [15, 31, 50, 99, 18, 98, 85];
insertion_sort(7 as libc::c_int, arr2.as_mut_ptr());
let mut expect2: [libc::c_int; 7] = [15, 18, 31, 50, 85, 98, 99];
check_eq(7 as libc::c_int, arr2.as_mut_ptr(), expect2.as_mut_ptr());

let mut arr3: [libc::c_int; 3] = [3, 2, 1];
// Memory error: `arr3` has only 3 elements, not 5
insertion_sort(5 as libc::c_int, arr3.as_mut_ptr());

return 0 as libc::c_int;
}

pub fn main() { unsafe { ::std::process::exit(main_0() as i32) } }
