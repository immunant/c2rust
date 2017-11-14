#![feature(plugin, custom_attribute)]
#![plugin(cross_check_plugin)]
#![cross_check(none)]

#[macro_use]
extern crate cross_check_derive;
#[macro_use]
extern crate cross_check_runtime;

use std::collections::VecDeque;
use std::cell::RefCell;

use cross_check_runtime::xcheck;

#[derive(Debug, PartialEq, Eq)]
struct XCheck(u8, u64);

thread_local! {
    static XCHECKS: RefCell<VecDeque<XCheck>> = RefCell::new(VecDeque::new())
}

#[no_mangle]
pub extern fn rb_xcheck(tag: u8, val: u64) {
    XCHECKS.with(|xc| xc.borrow_mut().push_back(XCheck(tag, val)));
}

fn expect_xcheck(tag: u8, val: u64) {
    let xc = XCHECKS.with(|xc| xc.borrow_mut().pop_front().unwrap());
    assert_eq!(xc, XCheck(tag, val));
}

#[test]
fn test_entry() {
    #[cross_check(yes)]
    fn abcd() { }

    abcd();
    expect_xcheck(xcheck::FUNCTION_ENTRY_TAG, 0x7c93ee4f_u64);
}

#[test]
#[should_panic]
fn test_no_xcheck() {
    #[cross_check(none)]
    fn abcd() { }

    abcd();
    expect_xcheck(xcheck::FUNCTION_ENTRY_TAG, 0x7c93ee4f_u64);
}

#[test]
fn test_custom_fn_name() {
    #[cross_check(yes, name="djb2")]
    fn abcd() { }

    abcd();
    expect_xcheck(xcheck::FUNCTION_ENTRY_TAG, 0x7c95b527_u64);
}

#[test]
fn test_custom_fn_id() {
    #[cross_check(yes, fixed=0x12345678)]
    fn abcd() { }

    abcd();
    expect_xcheck(xcheck::FUNCTION_ENTRY_TAG, 0x12345678_u64);
}
