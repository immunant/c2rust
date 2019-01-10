#![feature(plugin, custom_attribute)]
#![plugin(c2rust_xcheck_plugin)]
#![cross_check(none)]

#[macro_use]
extern crate c2rust_xcheck_runtime;

mod xcheck;
pub use xcheck::rb_xcheck; // Export rb_xcheck for the runtime

use c2rust_xcheck_runtime::xcheck::{FUNCTION_ARG_TAG, FUNCTION_ENTRY_TAG, FUNCTION_EXIT_TAG};
use xcheck::{expect_no_xchecks, expect_xcheck};

#[test]
fn test_entry() {
    #[cross_check(yes)]
    fn abcd() {}

    abcd();
    expect_xcheck(FUNCTION_ENTRY_TAG, 0x7c93ee4f_u64);
    expect_xcheck(FUNCTION_EXIT_TAG, 0x7c93ee4f_u64);
    expect_no_xchecks();
}

#[test]
#[should_panic]
fn test_no_xcheck() {
    #[cross_check(none)]
    fn abcd() {}

    abcd();
    expect_xcheck(FUNCTION_ENTRY_TAG, 0x7c93ee4f_u64);
    expect_xcheck(FUNCTION_EXIT_TAG, 0x7c93ee4f_u64);
    expect_no_xchecks();
}

#[test]
fn test_custom_fn_name() {
    #[cross_check(yes, entry(djb2 = "djb2"))]
    fn abcd() {}

    abcd();
    expect_xcheck(FUNCTION_ENTRY_TAG, 0x7c95b527_u64);
    expect_xcheck(FUNCTION_EXIT_TAG, 0x7c93ee4f_u64);
    expect_no_xchecks();
}

#[test]
fn test_custom_fn_id() {
    #[cross_check(yes, entry(fixed = 0x12345678))]
    fn abcd() {}

    abcd();
    expect_xcheck(FUNCTION_ENTRY_TAG, 0x12345678_u64);
    expect_xcheck(FUNCTION_EXIT_TAG, 0x7c93ee4f_u64);
    expect_no_xchecks();
}

#[test]
fn test_entry_disabled() {
    #[cross_check(yes, entry(disabled))]
    fn abcd() {}

    abcd();
    expect_xcheck(FUNCTION_EXIT_TAG, 0x7c93ee4f_u64);
    expect_no_xchecks();
}

#[test]
fn test_exit_disabled() {
    #[cross_check(yes, exit(disabled))]
    fn abcd() {}

    abcd();
    expect_xcheck(FUNCTION_ENTRY_TAG, 0x7c93ee4f_u64);
    expect_no_xchecks();
}

#[test]
fn test_all_args_default() {
    #[cross_check(yes, all_args)]
    fn abcd(_a: u8, _b: u64) {}

    abcd(0x7fu8, 1u64);
    expect_xcheck(FUNCTION_ENTRY_TAG, 0x7c93ee4f_u64);
    expect_xcheck(FUNCTION_ARG_TAG, 0x7f_u64);
    expect_xcheck(FUNCTION_ARG_TAG, 0x0f0f0f0f_0f0f0f0f_u64);
    expect_xcheck(FUNCTION_EXIT_TAG, 0x7c93ee4f_u64);
    expect_no_xchecks();
}

#[test]
fn test_all_args_fixed() {
    #[cross_check(yes, all_args(fixed = 0x1234))]
    fn abcd(_a: u8, _b: u64) {}

    abcd(0x7fu8, 1u64);
    expect_xcheck(FUNCTION_ENTRY_TAG, 0x7c93ee4f_u64);
    expect_xcheck(FUNCTION_ARG_TAG, 0x1234_u64);
    expect_xcheck(FUNCTION_ARG_TAG, 0x1234_u64);
    expect_xcheck(FUNCTION_EXIT_TAG, 0x7c93ee4f_u64);
    expect_no_xchecks();
}

#[test]
fn test_all_args_disabled() {
    #[cross_check(yes, all_args(disabled))]
    fn abcd(_a: u8, _b: u64) {}

    abcd(0x7fu8, 1u64);
    expect_xcheck(FUNCTION_ENTRY_TAG, 0x7c93ee4f_u64);
    expect_xcheck(FUNCTION_EXIT_TAG, 0x7c93ee4f_u64);
    expect_no_xchecks();
}

#[test]
fn test_args_override() {
    #[cross_check(yes, args(_a(fixed = 0x1234), _b(none)))]
    fn abcd(_a: u8, _b: u64) {}

    abcd(0x7fu8, 1u64);
    expect_xcheck(FUNCTION_ENTRY_TAG, 0x7c93ee4f_u64);
    expect_xcheck(FUNCTION_ARG_TAG, 0x1234_u64);
    expect_xcheck(FUNCTION_EXIT_TAG, 0x7c93ee4f_u64);
    expect_no_xchecks();
}
