use super::CrossCheckHasher;
use core::hash::Hasher;
use core::mem;

#[derive(Debug, Default)]
pub struct SimpleHasher(Option<u64>);

// Constant used to mix the type into the hash.
// It has the property that small values of ty
// impact about half the bits in the output.
const MIX_CONSTANT: u64 = 0x5a5a_5a5a_5a5a_5a5a_u64;

// A simple and fast hasher for single typed values, e.g., a single u64
// If its write_xxx function gets called multiple times in one invocation,
// it will panic
impl SimpleHasher {
    #[inline]
    fn write_typed(&mut self, ty: u8, i: u64) {
        if self.0.is_some() {
            panic!("Tried to add hash multiple values with SimpleHasher");
        }
        self.0 = Some(i ^ (MIX_CONSTANT.wrapping_mul(ty.into())));
    }
}

enum HashType {
    U8 = 0,
    U16,
    U32,
    U64,
    USize,
    I8,
    I16,
    I32,
    I64,
    ISize,
    Bool,
    Char,
    F32,
    F64,
}

macro_rules! impl_primitive_hash {
    ($in_ty:ident, $write_meth:ident, $hash_ty:ident) => {
        #[inline]
        fn $write_meth(&mut self, i: $in_ty) {
            #[allow(clippy::cast_lossless)]
            self.write_typed(HashType::$hash_ty as u8, i as u64);
        }
    };
}

impl Hasher for SimpleHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0.unwrap()
    }

    #[inline]
    fn write(&mut self, _bytes: &[u8]) {
        unimplemented!();
    }

    impl_primitive_hash!(u8, write_u8, U8);
    impl_primitive_hash!(u16, write_u16, U16);
    impl_primitive_hash!(u32, write_u32, U32);
    impl_primitive_hash!(u64, write_u64, U64);
    impl_primitive_hash!(usize, write_usize, USize);
    impl_primitive_hash!(i8, write_i8, I8);
    impl_primitive_hash!(i16, write_i16, I16);
    impl_primitive_hash!(i32, write_i32, I32);
    impl_primitive_hash!(i64, write_i64, I64);
    impl_primitive_hash!(isize, write_isize, ISize);
}

impl CrossCheckHasher for SimpleHasher {
    impl_primitive_hash!(bool, write_bool, Bool);
    impl_primitive_hash!(char, write_char, Char);

    #[inline]
    fn write_f32(&mut self, i: f32) {
        self.write_typed(
            HashType::F32 as u8,
            unsafe { mem::transmute::<f32, u32>(i) }.into(),
        );
    }

    #[inline]
    fn write_f64(&mut self, i: f64) {
        self.write_typed(HashType::F64 as u8, unsafe {
            mem::transmute::<f64, u64>(i)
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let h = SimpleHasher::default();
        assert_eq!(h.0, None);
    }

    #[test]
    #[should_panic]
    fn test_empty_finish() {
        let h = SimpleHasher::default();
        h.finish();
    }

    #[test]
    #[should_panic]
    fn test_double_write() {
        let mut h = SimpleHasher::default();
        h.write_u64(0u64);
        h.write_u64(1u64);
        h.finish();
    }

    #[test]
    fn test_values() {
        macro_rules! test_value {
            ($v:expr, $write_meth:ident, $expected:expr) => {
                let mut h = SimpleHasher::default();
                h.$write_meth($v);
                assert_eq!(h.finish(), $expected);
            };
        }
        use core::f32::consts as consts_f32;
        use core::f64::consts as consts_f64;
        // TODO: also test usize and isize???
        test_value!(0x000000000000012_u8, write_u8, 0x0000000000000012_u64);
        test_value!(0x000000000001234_u16, write_u16, 0x5a5a5a5a5a5a486e_u64);
        test_value!(0x000000012345678_u32, write_u32, 0xb4b4b4b4a680e2cc_u64);
        test_value!(0x123456789abcdef_u64, write_u64, 0x0e2c4a6886a4c2e1_u64);
        test_value!(0x000000000000012_i8, write_i8, 0xc3c3c3c3c3c3c3d0_u64);
        test_value!(0x000000000001234_i16, write_i16, 0x1e1e1e1e1e1e0c28_u64);
        test_value!(0x000000012345678_i32, write_i32, 0x787878786a4c2e0e_u64);
        test_value!(0x123456789abcdef_i64, write_i64, 0xd3f197b55b791f3f_u64);
        test_value!(false, write_bool, 0x8787878787878784_u64);
        test_value!(true, write_bool, 0x8787878787878785_u64);
        test_value!(1.0_f32, write_f32, 0x3c3c3c3c03bc3c38_u64);
        test_value!(consts_f32::PI, write_f32, 0x3c3c3c3c7c7533e3_u64);
        test_value!(1.0_f64, write_f64, 0xa966969696969692_u64);
        test_value!(consts_f64::PI, write_f64, 0xd69fb76dc2d2bb8a_u64);
    }
}
