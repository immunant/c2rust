#![feature(explicit_tail_calls)]
#![allow(incomplete_features)]

// CHECK: mark sig of {{.*}}tail) fixed: EXPLICIT_TAIL_CALL
// CHECK: pub unsafe fn tail(p: *const i32) -> i32
// CHECK: become leaf_shim(p)
// CHECK: unsafe fn leaf_shim(arg0: *const i32) -> i32
pub unsafe fn tail(p: *const i32) -> i32 {
    become leaf(p)
}

pub unsafe fn leaf(p: *const i32) -> i32 {
    *p
}
