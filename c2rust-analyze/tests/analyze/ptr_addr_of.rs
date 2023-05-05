pub fn std_ptr_addr_of(x: ()) {
    std::ptr::addr_of!(x);
}

pub fn abs_std_ptr_addr_of(x: ()) {
    ::std::ptr::addr_of!(x);
}

pub fn core_ptr_addr_of(x: ()) {
    core::ptr::addr_of!(x);
}

#[cfg(any())]
pub fn abs_core_ptr_addr_of(x: ()) {
    ::core::ptr::addr_of!(x);
}

pub fn use_std_ptr_addr_of(x: ()) {
    use std::ptr::addr_of;
    addr_of!(x);
}

#[cfg(any())]
pub fn use_core_ptr_addr_of(x: ()) {
    use core::ptr::addr_of;
    addr_of!(x);
}
