/// Marker trait for types for which a bit pattern of all zeros represents a valid instance of the
/// type.
pub unsafe trait Nullable {}

macro_rules! impl_nullable {
    ($($T:ty,)*) => {
        $(
            unsafe impl Nullable for $T {}
        )*
    };
}

impl_nullable! {
    i8, i16, i32, i64, i128, isize,
    u8, u16, u32, u64, u128, usize,
    f32, f64,
    bool, (),
}

unsafe impl<T> Nullable for *const T {}
unsafe impl<T> Nullable for *mut T {}
