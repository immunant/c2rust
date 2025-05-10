#![allow(dead_code, mutable_transmutes, non_camel_case_types, non_snake_case,
         non_upper_case_globals, unused_assignments, unused_mut)]
#![feature(rustc_private)]

extern crate libc;

#[no_mangle]
// CHECK-LABEL: final labeling for "insertion_sort"
// CHECK-DAG: ([[@LINE+1]]: p): {{.*}}type = READ | WRITE | UNIQUE | OFFSET_ADD | OFFSET_SUB | NON_NULL | HEAP | STACK#
pub unsafe extern "C" fn insertion_sort(n: libc::c_int, p: *mut libc::c_int) {
    let mut i: libc::c_int = 1 as libc::c_int;
    while i < n {
        // CHECK-DAG: ([[@LINE+2]]: p): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB | NON_NULL | HEAP | STACK#
        // CHECK-DAG: ([[@LINE+1]]: p.offset(i as isize)): {{.*}}type = READ | UNIQUE | NON_NULL | HEAP | STACK#
        let tmp: libc::c_int = *p.offset(i as isize);
        let mut j: libc::c_int = i;
        // CHECK-DAG: ([[@LINE+2]]: p): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB | NON_NULL | HEAP | STACK#
        // CHECK-DAG: ([[@LINE+1]]: p.offset{{.*}}): {{.*}}type = READ | UNIQUE | NON_NULL | HEAP | STACK#
        while j > 0 as libc::c_int && *p.offset((j - 1 as libc::c_int) as isize) > tmp {
            // CHECK-DAG: ([[@LINE+4]]: p): {{.*}}type = READ | UNIQUE | OFFSET_ADD | OFFSET_SUB | NON_NULL | HEAP | STACK#
            // CHECK-DAG: ([[@LINE+3]]: p): {{.*}}type = READ | WRITE | UNIQUE | OFFSET_ADD | OFFSET_SUB | NON_NULL | HEAP | STACK#
            // CHECK-DAG: ([[@LINE+2]]: p.offset((j {{.*}}): {{.*}}type = READ | UNIQUE | NON_NULL | HEAP | STACK#
            // CHECK-DAG: ([[@LINE+1]]: p.offset(j {{.*}}): {{.*}}type = READ | WRITE | UNIQUE | NON_NULL | HEAP | STACK#
            *p.offset(j as isize) = *p.offset((j - 1 as libc::c_int) as isize);
            j -= 1
        }
        // CHECK-DAG: ([[@LINE+2]]: p): {{.*}}type = READ | WRITE | UNIQUE | OFFSET_ADD | OFFSET_SUB | NON_NULL | HEAP | STACK#
        // CHECK-DAG: ([[@LINE+1]]: p.offset(j {{.*}}): {{.*}}type = READ | WRITE | UNIQUE | NON_NULL | HEAP | STACK#
        *p.offset(j as isize) = tmp;
        i += 1
    }
}
