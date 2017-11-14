#![feature(plugin, custom_attribute)]
#![plugin(cross_check_plugin)]
#![cross_check(none)]

#[macro_use]
extern crate cross_check_derive;
#[macro_use]
extern crate cross_check_runtime;

#[no_mangle]
pub extern fn rb_xcheck(tag: u8, val: u64) {
    println!("tag:{} val:{}", tag, val);
}

#[test]
fn test_entry() {
    #[cross_check(yes)]
    fn foo() { }

    foo();
    // TODO: add expect_xcheck calls
}
