#![cfg_attr(feature = "no_std", no_std)]

extern crate c2rust_bitfields_derive;

pub use c2rust_bitfields_derive::BitfieldStruct;

pub trait FieldType: Sized {
    const IS_SIGNED: bool;

    #[cfg(not(feature = "no_std"))]
    const TOTAL_BIT_SIZE: usize = ::std::mem::size_of::<Self>() * 8;
    #[cfg(feature = "no_std")]
    const TOTAL_BIT_SIZE: usize = ::core::mem::size_of::<Self>() * 8;

    fn get_bit(&self, bit: usize) -> bool;

    fn set_field(&self, field: &mut [u8], bit_range: (usize, usize)) {
        fn zero_bit(byte: &mut u8, n_bit: u64) {
            let bit = 1 << n_bit;

            *byte &= !bit as u8;
        }

        fn one_bit(byte: &mut u8, n_bit: u64) {
            let bit = 1 << n_bit;

            *byte |= bit as u8;
        }

        let (lhs_bit, rhs_bit) = bit_range;

        for (i, bit_index) in (lhs_bit..=rhs_bit).enumerate() {
            let byte_index = bit_index / 8;
            let byte = &mut field[byte_index];

            if self.get_bit(i) {
                one_bit(byte, (bit_index % 8) as u64);
            } else {
                zero_bit(byte, (bit_index % 8) as u64);
            }
        }
    }

    fn get_field(field: &[u8], bit_range: (usize, usize)) -> Self;
}

macro_rules! impl_int {
    ($($typ: ident),+) => {
        $(
            impl FieldType for $typ {
                const IS_SIGNED: bool = $typ::min_value() != 0;

                fn get_bit(&self, bit: usize) -> bool {
                    ((*self >> bit) & 1) == 1
                }

                fn get_field(field: &[u8], bit_range: (usize, usize)) -> Self {
                    let (lhs_bit, rhs_bit) = bit_range;
                    let mut val = 0;

                    for (i, bit_index) in (lhs_bit..=rhs_bit).enumerate() {
                        let byte_index = bit_index / 8;
                        let byte = field[byte_index];
                        let bit = 1 << (bit_index % 8);
                        let read_bit = byte & bit;

                        if read_bit != 0 {
                            let write_bit = 1 << i;

                            val |= write_bit;
                        }
                    }

                    // If the int type is signed, sign extend unconditionally
                    if Self::IS_SIGNED {
                        let bit_width = rhs_bit - lhs_bit + 1;
                        let unused_bits = Self::TOTAL_BIT_SIZE - bit_width;

                        val <<= unused_bits;
                        val >>= unused_bits;
                    }

                    val
                }
            }
        )+
    };
}

impl_int!{u8, u16, u32, u64, u128, i8, i16, i32, i64, i128}

impl FieldType for bool {
    const IS_SIGNED: bool = false;

    fn get_bit(&self, _bit: usize) -> bool {
        *self
    }

    fn get_field(field: &[u8], bit_range: (usize, usize)) -> Self {
        let (lhs_bit, rhs_bit) = bit_range;
        let mut val = false;

        for bit_index in lhs_bit..=rhs_bit {
            let byte_index = bit_index / 8;
            let byte = field[byte_index];
            let bit = 1 << (bit_index % 8);
            let read_bit = byte & bit;

            if read_bit != 0 {
                val = true;
            }
        }

        val
    }
}
