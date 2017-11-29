#![feature(plugin, custom_attribute)]
#![feature(trace_macros)]
#![plugin(cross_check_plugin)]
#![cross_check(none)]

#[macro_use]
extern crate cross_check_derive;
#[macro_use]
extern crate cross_check_runtime;

mod xcheck;
pub use xcheck::rb_xcheck; // Export rb_xcheck for the runtime

use xcheck::{expect_xcheck, expect_no_xchecks};

use cross_check_runtime::hash::simple::SimpleHasher;
use cross_check_runtime::hash::djb2::Djb2Hasher;
use cross_check_runtime::xcheck::UNKNOWN_TAG;

//trace_macros!(true);

// This macro creates a structure with the given structure and field attributes,
// builds and initializes an object with the given values, then
// passes the object to the provided test function
macro_rules! test_struct {
    ([$($attrs:meta),*]
     {$([$($field_attrs:meta),*] $field:ident : $field_ty:ty = $field_val:expr),*}
     [$(($ahasher:path, $shasher:path, $val:expr)),*]) => {
        #[cross_check(yes, $($attrs),*)]
        struct TestStruct {
            $(#[cross_check($($field_attrs),*)] $field: $field_ty),*
        };
        let ts = TestStruct { $($field: $field_val),* };
        $({
            cross_check_value!(UNKNOWN_TAG, ts, $ahasher, $shasher);
            expect_xcheck(UNKNOWN_TAG, $val);
        })*
        expect_no_xchecks();
    }
}

#[test]
fn test_custom_hash() {
    fn custom1<XCHA, XCHS>(_a: &TestStruct, _: usize) -> u64 {
        0x12345678
    }
    test_struct!([custom_hash="custom1"]
                 {}
                 [(Djb2Hasher, Djb2Hasher, 0x12345678)]);
}

#[test]
fn test_empty_struct_djb2() {
    test_struct!([]
                 {}
                 [(Djb2Hasher, Djb2Hasher, 5381_u64)]);
}

#[test]
fn test_simple_one_field() {
    test_struct!([]
                 { [] x: u64 = 0x12345678 }
                 [(SimpleHasher, SimpleHasher, 0x12345678_u64)]);
}

#[test]
fn test_field_hasher() {
    test_struct!([field_hasher="SimpleHasher"]
                 { [] x: u64 = 0x12345678 }
                 [(Djb2Hasher, SimpleHasher, 0x12345678_u64)]);
}

#[test]
fn test_ahasher() {
    test_struct!([ahasher="SimpleHasher"]
                 { [] x: u64 = 0x12345678 }
                 [(Djb2Hasher, SimpleHasher, 0x12345678_u64)]);
}

#[test]
fn test_shasher() {
    test_struct!([shasher="SimpleHasher"]
                 { [] x: u64 = 0x12345678 }
                 [(SimpleHasher, Djb2Hasher, 0x12345678_u64)]);
}

#[test]
fn test_skip_field() {
    test_struct!([]
                 { [none] x: u64 = 0x12345678 }
                 [(Djb2Hasher, Djb2Hasher, 5381_u64)]);
}

#[test]
fn test_fixed_hash() {
    test_struct!([]
                 { [fixed=0x0f0f0f0f0f0f0f0f] x: u64 = 0x12345678 }
                 [(SimpleHasher, SimpleHasher, 1u64)]);
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
                 { [custom="custom_hash"] x: u64 = 0x12345678 }
                 [(SimpleHasher, SimpleHasher, 1u64)]);
}

#[test]
fn test_custom_hash_skip() {
    fn custom_hash<XCHA, XCHS, S, F>(_: &mut XCHA, _: &S, field: F, _: usize)
        where XCHA: ::cross_check_runtime::hash::CrossCheckHasher,
              F: ::std::borrow::Borrow<u64> {
        assert_eq!(*field.borrow(), 0x12345678);
    }
    test_struct!([]
                 { [custom="custom_hash"] x: u64 = 0x12345678 }
                 [(Djb2Hasher, Djb2Hasher, 5381_u64)]);
}

#[test]
fn test_skip_multi_fields() {
    {
        test_struct!([]
                     { []     x: u64 = 0x12345678,
                       [none] y: u64 = 0x34567812,
                       [none] z: u64 = 0x87654321 }
                     [(SimpleHasher, SimpleHasher, 0x12345678_u64)]);
    }
    {
        test_struct!([]
                     { [none] x: u64 = 0x12345678,
                       []     y: u64 = 0x34567812,
                       [none] z: u64 = 0x87654321 }
                     [(SimpleHasher, SimpleHasher, 0x34567812_u64)]);
    }
    {
        test_struct!([]
                     { [none] x: u64 = 0x12345678,
                       [none] y: u64 = 0x34567812,
                       []     z: u64 = 0x87654321 }
                     [(SimpleHasher, SimpleHasher, 0x87654321u64)]);
    }
}

#[test]
fn test_multi_field_hash() {
    test_struct!([]
                 { [] x: u16 = 0xa5a5,
                   [] y: u64 = 1 }
                 [(Djb2Hasher, SimpleHasher, 0x3d17c937_u64)]);
}
