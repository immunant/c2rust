#[macro_use]
extern crate c2rust_xcheck_derive;
extern crate c2rust_xcheck_runtime;

extern crate c2rust_bitfields;
#[macro_use]
extern crate c2rust_bitfields_derive;

use c2rust_xcheck_runtime::hash::jodyhash::JodyHasher;
use c2rust_xcheck_runtime::hash::simple::SimpleHasher;
use c2rust_xcheck_runtime::hash::CrossCheckHash as XCH;

#[test]
fn test_bitfields() {
    #[repr(C)]
    #[derive(BitfieldStruct, CrossCheckHash, Default)]
    struct Foo {
        #[bitfield(name = "a", ty = "u32", bits = "0..=6")]
        #[bitfield(name = "b", ty = "u32", bits = "7..=17")]
        a_b: [u8; 3],

        #[bitfield(padding)]
        _pad: [u8; 1],
    }

    // These tests should match the ones in struct10.c
    let mut x = Foo::default();
    x.set_a(42);
    x.set_b(1337);
    let x_hash = XCH::cross_check_hash::<JodyHasher, SimpleHasher>(&x);
    assert_eq!(x_hash, Some(0x24e75f75c47e329a));

    let x = Foo { a_b: [0xAA, 0x55, 0xAA], _pad: [0x55] };
    let x_hash = XCH::cross_check_hash::<JodyHasher, SimpleHasher>(&x);
    assert_eq!(x_hash, Some(0x24e75fad2461b12c));

    let x = Foo { a_b: [0x55, 0xAA, 0x55], _pad: [0xAA] };
    let x_hash = XCH::cross_check_hash::<JodyHasher, SimpleHasher>(&x);
    assert_eq!(x_hash, Some(0xc3e72e2d630778ed));

    let x = Foo { a_b: [0x78, 0x56, 0x34], _pad: [0x12] };
    let x_hash = XCH::cross_check_hash::<JodyHasher, SimpleHasher>(&x);
    assert_eq!(x_hash, Some(0xb6e8a1efb3617525));
}
