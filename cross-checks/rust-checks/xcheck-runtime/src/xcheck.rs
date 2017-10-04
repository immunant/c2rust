
pub const FUNCTION_CALL_TAG: u8 = 0;
pub const FUNCTION_ARG_TAG: u8 = 1;
pub const FUNCTION_RETURN_TAG: u8 = 2;

extern {
    #[no_mangle]
    fn rb_xcheck(tag: u8, val: u64);
}

#[inline]
pub fn xcheck(tag: u8, val: u64) {
    unsafe { rb_xcheck(tag, val) }
}
