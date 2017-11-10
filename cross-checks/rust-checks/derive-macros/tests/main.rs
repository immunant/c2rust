
#[macro_use]
extern crate cross_check_derive;
extern crate cross_check_runtime;

use cross_check_runtime::hash::CrossCheckHash as XCH;
use cross_check_runtime::hash::simple::SimpleHasher;
use cross_check_runtime::hash::djb2::Djb2Hasher;

#[test]
fn test_custom_hash() {
    #[derive(CrossCheckHash)]
    #[cross_check_hash(custom_hash="custom1")]
    struct A {
        _a: u64,
    }

    fn custom1<XCHA, XCHS>(_a: &A, _: usize) -> u64 {
        0x12345678
    }

    let a = A { _a: 0x1234 };
    assert_eq!(
        XCH::cross_check_hash::<Djb2Hasher, Djb2Hasher>(&a),
        0x12345678);
}
