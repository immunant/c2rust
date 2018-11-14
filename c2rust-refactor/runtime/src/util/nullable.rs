use std::rc::Rc;
use std::sync::Arc;

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

unsafe impl<'a, T: ?Sized> Nullable for Option<&'a T> {}
unsafe impl<'a, T: ?Sized> Nullable for Option<&'a mut T> {}
unsafe impl<T: ?Sized> Nullable for Option<Box<T>> {}
unsafe impl<T: ?Sized> Nullable for Option<Rc<T>> {}
unsafe impl<T: ?Sized> Nullable for Option<Arc<T>> {}
unsafe impl<T> Nullable for Option<Vec<T>> {}
unsafe impl Nullable for Option<String> {}
