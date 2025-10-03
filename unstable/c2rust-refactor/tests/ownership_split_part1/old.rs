//! Testing part 1 of two-part splitting.  We split the middle function (`f2`) first, and ensure
//! that the `f1 -> f2` and `f2 -> f3` calls continue to line up.

unsafe fn f1(p: *mut u8) -> *mut u8 {
    f2(p)
}

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
