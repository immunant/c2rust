//! Testing part 1 of two-part splitting.  We split the middle function (`f2`) first, and ensure
//! that the `f1 -> f2` and `f2 -> f3` calls continue to line up.

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
