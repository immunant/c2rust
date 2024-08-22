
// Test dereferencing a pointer that has OFFSET permissions.  This requires inserting a cast from
// `&mut [T]` to `&mut T`.
// CHECK-LABEL: fn offset_deref
pub unsafe fn offset_deref(x: *mut i32, off: isize) -> *mut i32 {
    // CHECK: *&mut (x)[0] = 0;
    *x = 0;
    // CHECK: *&mut (&mut (x)[((1) as usize) ..])[0] = 1;
    *x.offset(1) = 1;
    // CHECK: {{.*}}x{{.*}}
    x
}
