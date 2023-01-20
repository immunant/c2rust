// Test case where a field expression is evaluated into a MIR temporary, to ensure we can handle
// that case when lifting MIR rewrites to HIR.
pub struct MyList {
    data: *mut i32,
    len: usize,
}

// CHECK-LABEL: final labeling for "list_get"
pub unsafe fn list_get(l: *const MyList, i: usize) -> i32 {
    // The temporary `(*l).data` requires a MIR `MutToImm` rewrite.
    // CHECK: ([[#@LINE+2]]: (*l).data): &[i32]
    // CHECK: rewrite Ref(Deref(Identity), Not) at {{.*}}:[[#@LINE+1]]:6: [[#@LINE+1]]:15
    *(*l).data.offset(i as isize)
}

// CHECK-LABEL: final labeling for "list_set"
pub unsafe fn list_set(l: *const MyList, i: usize, x: i32) {
    // CHECK: ([[#@LINE+1]]: (*l).data): &mut [i32]
    *(*l).data.offset(i as isize) = x;
}
