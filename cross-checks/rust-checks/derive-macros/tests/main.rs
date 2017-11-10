
#![feature(trace_macros)]
//trace_macros!(true);

#[macro_use]
extern crate cross_check_derive;
extern crate cross_check_runtime;

use cross_check_runtime::hash::CrossCheckHash as XCH;
use cross_check_runtime::hash::simple::SimpleHasher;
use cross_check_runtime::hash::djb2::Djb2Hasher;

macro_rules! test_struct {
    ([$($attrs:meta)*]
     {$($field_def:ident:$field_ty:ty),*}
     ($($field_init:ident:$val:expr),*)
     $test_fn:expr) => {
        #[derive(CrossCheckHash)]
        #[cross_check_hash($($attrs)*)]
        struct TestStruct {
            $($field_def: $field_ty),*
        };
        let ts = TestStruct { $($field_init: $val),* };
        $test_fn(ts)
    }
}

#[test]
fn test_custom_hash() {
    fn custom1<XCHA, XCHS>(_a: &TestStruct, _: usize) -> u64 {
        0x12345678
    }

    test_struct!([custom_hash="custom1"]
                 {}
                 ()
                 |ts| {
        assert_eq!(
            XCH::cross_check_hash::<Djb2Hasher, Djb2Hasher>(&ts),
            0x12345678);
    });
}

#[test]
fn test_empty_struct_djb2() {
    test_struct!([]
                 {}
                 ()
                 |ts| {
        assert_eq!(
            XCH::cross_check_hash::<Djb2Hasher, Djb2Hasher>(&ts),
            5381_u64);
    });
}
