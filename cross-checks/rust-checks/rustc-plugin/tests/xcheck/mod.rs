use std::collections::VecDeque;
use std::cell::RefCell;

#[derive(Debug, PartialEq, Eq)]
struct XCheck(u8, u64);

thread_local! {
    static XCHECKS: RefCell<VecDeque<XCheck>> = RefCell::new(VecDeque::new())
}

#[no_mangle]
pub extern fn rb_xcheck(tag: u8, val: u64) {
    XCHECKS.with(|xc| xc.borrow_mut().push_back(XCheck(tag, val)));
}

pub fn expect_xcheck(tag: u8, val: u64) {
    let xc = XCHECKS.with(|xc| xc.borrow_mut().pop_front().unwrap());
    assert_eq!(xc, XCheck(tag, val));
}

pub fn expect_no_xchecks() {
    assert!(XCHECKS.with(|xc| xc.borrow_mut().is_empty()),
            "found more cross-checks than expected");
}
