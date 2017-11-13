
#![feature(trace_macros)]
//trace_macros!(true);

#[macro_use]
extern crate cross_check_derive;
extern crate cross_check_runtime;

use cross_check_runtime::hash::CrossCheckHash as XCH;
use cross_check_runtime::hash::simple::SimpleHasher;
use cross_check_runtime::hash::djb2::Djb2Hasher;

// This macro creates a structure with the given structure and field attributes,
// builds and initializes an object with the given values, then
// passes the object to the provided test function
macro_rules! test_struct {
    ([$($attrs:meta),*]
     {$([$($field_attrs:meta),*] $field:ident : $field_ty:ty = $field_val:expr),*}
     $test_fn:expr) => {
        #[derive(CrossCheckHash)]
        #[cross_check_hash($($attrs),*)]
        struct TestStruct {
            $(#[cross_check_hash($($field_attrs),*)] $field: $field_ty),*
        };
        let ts = TestStruct { $($field: $field_val),* };
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
                 |ts| {
        assert_eq!(
            XCH::cross_check_hash::<Djb2Hasher, Djb2Hasher>(&ts),
            5381_u64);
    });
}

#[test]
fn test_simple_one_field() {
    test_struct!([]
                 { [] x: u64 = 0x12345678 }
                 |ts| {
        assert_eq!(
            XCH::cross_check_hash::<SimpleHasher, SimpleHasher>(&ts),
            0x12345678_u64);
    });
}

#[test]
fn test_field_hasher() {
    test_struct!([field_hasher="SimpleHasher"]
                 { [] x: u64 = 0x12345678 }
                 |ts| {
        assert_eq!(
            XCH::cross_check_hash::<Djb2Hasher, SimpleHasher>(&ts),
            0x12345678_u64);
    });
}

#[test]
fn test_ahasher() {
    test_struct!([ahasher="SimpleHasher"]
                 { [] x: u64 = 0x12345678 }
                 |ts| {
        assert_eq!(
            XCH::cross_check_hash::<Djb2Hasher, SimpleHasher>(&ts),
            0x12345678_u64);
    });
}

#[test]
fn test_shasher() {
    test_struct!([shasher="SimpleHasher"]
                 { [] x: u64 = 0x12345678 }
                 |ts| {
        assert_eq!(
            XCH::cross_check_hash::<SimpleHasher, Djb2Hasher>(&ts),
            0x12345678_u64);
    });
}

#[test]
fn test_skip_field() {
    test_struct!([]
                 { [none] x: u64 = 0x12345678 }
                 |ts| {
        assert_eq!(
            XCH::cross_check_hash::<Djb2Hasher, Djb2Hasher>(&ts),
            5381_u64);
    });
}

#[test]
fn test_fixed_hash() {
    test_struct!([]
                 { [fixed_hash="0x0f0f0f0f0f0f0f0f"] x: u64 = 0x12345678 }
                 |ts| {
        assert_eq!(
            XCH::cross_check_hash::<SimpleHasher, SimpleHasher>(&ts),
            1u64);
    });
}

#[test]
fn test_custom_field_hash() {
    fn custom_hash<XCHA, XCHS, S, F>(h: &mut XCHA, _: &S, field: F, _: usize)
        where XCHA: ::cross_check_runtime::hash::CrossCheckHasher,
              F: ::std::borrow::Borrow<u64> {
        assert_eq!(*field.borrow(), 0x12345678);
        h.write_u64(0x0f0f0f0f0f0f0f0f)
    }
    test_struct!([]
                 { [custom_hash="custom_hash"] x: u64 = 0x12345678 }
                 |ts| {
        assert_eq!(
            XCH::cross_check_hash::<SimpleHasher, SimpleHasher>(&ts),
            1u64);
    });
}

#[test]
fn test_custom_hash_skip() {
    fn custom_hash<XCHA, XCHS, S, F>(_: &mut XCHA, _: &S, field: F, _: usize)
        where XCHA: ::cross_check_runtime::hash::CrossCheckHasher,
              F: ::std::borrow::Borrow<u64> {
        assert_eq!(*field.borrow(), 0x12345678);
    }
    test_struct!([]
                 { [custom_hash="custom_hash"] x: u64 = 0x12345678 }
                 |ts| {
        assert_eq!(
            XCH::cross_check_hash::<Djb2Hasher, Djb2Hasher>(&ts),
            5381u64);
    });
}
