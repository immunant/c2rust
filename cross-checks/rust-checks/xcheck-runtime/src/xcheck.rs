
extern {
    #[no_mangle]
    fn rb_xcheck(tag: u8, val: u64);
}

#[inline]
pub fn xcheck(tag: u8, val: u64) {
    unsafe { rb_xcheck(tag, val) }
}
