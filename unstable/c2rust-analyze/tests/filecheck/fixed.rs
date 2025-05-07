use std::ptr;

// CHECK-LABEL: final labeling for "func"
// Ref-typed function arguments and results should be marked `FIXED`.
// CHECK: ([[@LINE+2]]: &'a u8): {{.*}}, type flags = FIXED#&u8[{{.*}}]
// CHECK: ([[@LINE+1]]: x): {{.*}}, type flags = FIXED#&&u8[FIXED#&u8[{{.*}}]]
unsafe fn func<'a>(x: &'a &'a u8) -> &'a u8 {
    // Locals declared as ref type should be marked `FIXED`
    // CHECK: ([[@LINE+1]]: y): {{.*}}, type flags = FIXED#&u8[{{.*}}]
    let y: &u8 = *x;

    // Locals inferred as ref type should be marked `FIXED`
    // CHECK: ([[@LINE+1]]: y): {{.*}}, type flags = FIXED#&u8[{{.*}}]
    let y = *x;

    // Locals inferred as non-refs should not marked `FIXED`, and neither should temporary refs.
    // CHECK: ([[@LINE+2]]: y): {{.*}}, type flags = (empty)#*const u8[{{.*}}]
    // CHECK: ([[@LINE+1]]: &**x): {{.*}}, type flags = (empty)#&u8[{{.*}}]
    let y = &**x as *const u8;

    // CHECK: ([[@LINE+1]]: &*y): {{.*}}, type flags = (empty)#&u8[{{.*}}]
    &*y
}

// CHECK-LABEL: final labeling for static items

// Refs in statics should be marked `FIXED`.
// CHECK: "REF_STATIC": {{.*}}, type flags = FIXED#&'static u8[{{.*}}]
static REF_STATIC: &'static u8 = &1;

// CHECK: "NON_REF_STATIC": {{.*}}, type flags = (empty)#*mut u8[{{.*}}]
static mut NON_REF_STATIC: *mut u8 = ptr::null_mut();

