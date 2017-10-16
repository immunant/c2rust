
use std::hash::Hasher;
use std::mem;
use super::XCheckHasher;

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
        self.0 = Some(i ^ (MIX_CONSTANT * (ty as u64)));
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

    impl_primitive_hash!(u8,    write_u8,    U8);
    impl_primitive_hash!(u16,   write_u16,   U16);
    impl_primitive_hash!(u32,   write_u32,   U32);
    impl_primitive_hash!(u64,   write_u64,   U64);
    impl_primitive_hash!(usize, write_usize, USize);
    impl_primitive_hash!(i8,    write_i8,    I8);
    impl_primitive_hash!(i16,   write_i16,   I16);
    impl_primitive_hash!(i32,   write_i32,   I32);
    impl_primitive_hash!(i64,   write_i64,   I64);
    impl_primitive_hash!(isize, write_isize, ISize);
}

impl XCheckHasher for SimpleHasher {
    impl_primitive_hash!(bool,  write_bool,  Bool);
    impl_primitive_hash!(char,  write_char,  Char);

    #[inline]
    fn write_f32(&mut self, i: f32) {
        self.write_typed(HashType::F32 as u8,
                         unsafe { mem::transmute::<f32, u32>(i) } as u64);
    }

    #[inline]
    fn write_f64(&mut self, i: f64) {
        self.write_typed(HashType::F64 as u8,
                         unsafe { mem::transmute::<f64, u64>(i) });
    }
}

