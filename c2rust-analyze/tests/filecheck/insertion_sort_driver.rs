#![allow(dead_code, mutable_transmutes, non_camel_case_types, non_snake_case,
         non_upper_case_globals, unused_assignments, unused_mut)]
#![feature(rustc_private)]

extern crate libc;

// CHECK-LABEL: pub unsafe extern "C" fn insertion_sort
// CHECK-SAME: p: &'h0 mut [(libc::c_int)]
#[no_mangle]
pub unsafe extern "C" fn insertion_sort(n: libc::c_int, p: *mut libc::c_int) {
    let mut i: libc::c_int = 1 as libc::c_int;
    while i < n {
        // CHECK: let tmp: {{.*}} = *&(&(&*(p))[((i as isize) as usize) ..])[0];
        let tmp: libc::c_int = *p.offset(i as isize);
        let mut j: libc::c_int = i;
        // CHECK-NOT: p.offset
        while j > 0 as libc::c_int &&
                  *p.offset((j - 1 as libc::c_int) as isize) > tmp {
            *p.offset(j as isize) =
                *p.offset((j - 1 as libc::c_int) as isize);
            j -= 1
        }
        // CHECK: *&mut (&mut (p)[((j as isize) as usize) ..])[0] = tmp;
        *p.offset(j as isize) = tmp;
        i += 1
    };
}

// CHECK-LABEL: pub unsafe extern "C" fn check_eq
// CHECK-SAME: p: &'h0 [(libc::c_int)]
// CHECK-NEXT: q: &'h1 [(libc::c_int)]
#[no_mangle]
pub unsafe extern "C" fn check_eq(n: libc::c_int, p: *mut libc::c_int,
                                  q: *mut libc::c_int) {
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        //assert!(*p.offset(i as isize) == *q.offset(i as isize));
        // CHECK: *&(&(p)[((i as isize) as usize) ..])[0]
        // CHECK: *&(&(q)[((i as isize) as usize) ..])[0]
        if *p.offset(i as isize) != *q.offset(i as isize) {
            std::process::abort();
        }
        i += 1
    };
}

// CHECK-LABEL: unsafe fn main_0
unsafe fn main_0() -> libc::c_int {
    let mut arr1: [libc::c_int; 3] = [1, 3, 2];
    // CHECK: &mut (arr1) as &mut [i32]
    insertion_sort(3 as libc::c_int, arr1.as_mut_ptr());
    let mut expect1: [libc::c_int; 3] = [1, 2, 3];
    // CHECK: &*((&mut arr1)) as &[i32]
    // CHECK-SAME: &(expect1) as &[i32]
    check_eq(3 as libc::c_int, (&mut arr1).as_mut_ptr(), expect1.as_mut_ptr());

    let mut arr2: [libc::c_int; 7] = [15, 31, 50, 99, 18, 98, 85];
    insertion_sort(7 as libc::c_int, arr2.as_mut_ptr());
    let mut expect2: [libc::c_int; 7] = [15, 18, 31, 50, 85, 98, 99];
    check_eq(7 as libc::c_int, arr2.as_mut_ptr(), expect2.as_mut_ptr());

    let mut arr3: [libc::c_int; 3] = [3, 2, 1];
    // Memory error: `arr3` has only 3 elements, not 5
    insertion_sort(5 as libc::c_int, arr3.as_mut_ptr());

    return 0 as libc::c_int;
}

pub fn main() {
    unsafe {
        ::std::process::exit(main_0() as i32);
    }
}
