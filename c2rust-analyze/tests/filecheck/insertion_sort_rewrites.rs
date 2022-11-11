#![allow(dead_code, mutable_transmutes, non_camel_case_types, non_snake_case,
         non_upper_case_globals, unused_assignments, unused_mut)]
#![feature(rustc_private)]

extern crate libc;

#[no_mangle]
// CHECK-LABEL: generated 8 rewrites for "insertion_sort"
pub unsafe extern "C" fn insertion_sort(n: libc::c_int, p: *mut libc::c_int) {
    let mut i: libc::c_int = 1 as libc::c_int;
    while i < n {
        // CHECK-DAG: [[#@LINE+2]]: p: &*$e
        // CHECK-DAG: [[#@LINE+1]]: p.offset({{.*}}): &(&$0[($1 as usize) ..])[0]
        let tmp: libc::c_int = *p.offset(i as isize);
        let mut j: libc::c_int = i;
        // CHECK-DAG: [[#@LINE+2]]: p: &*$e
        // CHECK-DAG: [[#@LINE+1]]: p.offset({{.*}}): &(&$0[($1 as usize) ..])[0]
        while j > 0 as libc::c_int && *p.offset((j - 1 as libc::c_int) as isize) > tmp {
            // CHECK-DAG: [[#@LINE+3]]: p: &*$e
            // CHECK-DAG: [[#@LINE+2]]: p.offset({{.*}}): &(&$0[($1 as usize) ..])[0]
            // CHECK-DAG: [[#@LINE+1]]: p.offset({{.*}}): &mut (&mut $0[($1 as usize) ..])[0]
            *p.offset(j as isize) = *p.offset((j - 1 as libc::c_int) as isize);
            j -= 1
        }
        // CHECK-DAG: [[#@LINE+1]]: p.offset({{.*}}): &mut (&mut $0[($1 as usize) ..])[0]
        *p.offset(j as isize) = tmp;
        i += 1
    }
}
