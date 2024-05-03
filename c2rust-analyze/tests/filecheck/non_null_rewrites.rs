use std::ptr;

// CHECK-LABEL: unsafe fn f{{[<(]}}
pub unsafe fn f(cond: bool, p: *mut i32) {
    let mut p = p;
    if cond {
        p = ptr::null_mut();
    }


    // CHECK: let ([[arr:.+]], [[idx:.+]], ) = ({{.*}}(p){{.*}}, (3) as usize, );
    // CHECK-NEXT: [[arr]].map(|arr| &arr[[[idx]] ..])
    let q = p.offset(3);
}

