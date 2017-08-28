//! Testing part 2 of two-part splitting.  The middle function (`f2`) was already split.  Now we
//! split `f1` and `f3` and make sure the calls still line up right.

#![feature(custom_attribute, attr_literals)]

unsafe fn f1(p: *mut u8) -> *mut u8 {
    f2_take(p)
}

#[ownership_mono("take", MOVE, MOVE)]
#[ownership_variant_of("old/8cd878b::f2[0]")]
unsafe fn f2_take(p: *mut u8) -> *mut u8 {
    f3(p)
}
#[ownership_mono("mut", WRITE, WRITE)]
#[ownership_variant_of("old/8cd878b::f2[0]")]
unsafe fn f2_mut(p: *mut u8) -> *mut u8 {
    f3(p)
}
#[ownership_mono("", READ, READ)]
#[ownership_variant_of("old/8cd878b::f2[0]")]
unsafe fn f2(p: *mut u8) -> *mut u8 {
    f3(p)
}

unsafe fn f3(p: *mut u8) -> *mut u8 {
    g(p)
}

unsafe fn g(p: *mut u8) -> *mut u8 {
    p
}

fn main() {}
