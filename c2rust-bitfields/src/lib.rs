extern crate c2rust_bitfields_derive;

pub use c2rust_bitfields_derive::BitfieldStruct;

mod private {
    pub struct Wrapper<T>(pub T);
}

use private::Wrapper;

pub trait BoolOrInt: Sized {
    fn is_signed() -> bool;

    fn wrapped_into<T>(self) -> T where Wrapper<Self>: Into<T> {
        Wrapper(self).into()
    }

    fn calculate_total_bit_size() -> usize {
        #[cfg(not(feature = "no_std"))]
        let ret = ::std::mem::size_of::<Self>() * 8;
        #[cfg(feature = "no_std")]
        let ret = ::core::mem::size_of::<Self>() * 8;

        ret
    }
}

macro_rules! impl_int {
    ($($typ: ident),+) => {
        $(
            impl BoolOrInt for $typ {
                fn is_signed() -> bool {
                    $typ::min_value() != 0
                }
            }

            impl Into<$typ> for Wrapper<$typ> {
                fn into(self) -> $typ {
                    self.0
                }
            }

            impl Into<bool> for Wrapper<$typ> {
                fn into(self) -> bool {
                    self.0 != 0
                }
            }
        )+
    };
}

impl_int!{u8, u16, u32, u64, u128, i8, i16, i32, i64, i128}

impl BoolOrInt for bool {
    fn is_signed() -> bool {
        false
    }
}
