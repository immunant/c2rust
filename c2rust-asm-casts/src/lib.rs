use std::marker::PhantomData;

/// Pseudo-structure that provides the inner type definition
/// and cast functions for every pair of types used
/// in C2Rust's implementation of tied inline assembly operands.
/// For two tied operands of types `In` and `Out`, this
/// implementation provides the smallest type that can
/// hold both operands, along with the casts to convert
/// each operand to this type.
pub struct AsmCast<Out, In>(PhantomData<(Out, In)>);

/// This trait implements the cast functions for the type pair
pub trait AsmCastTrait<Out, In> {
    type Type;

    fn cast_in(_: &mut Out, x: In) -> Self::Type;
    fn cast_out(out: &mut Out, _: In, x: Self::Type);
}

macro_rules! impl_triple {
	{<$($param:ident),*> ($out:ty, $in:ty) => $inner:ty} => {
		impl<$($param),*> AsmCastTrait<$out, $in> for AsmCast<$out, $in> {
			type Type = $inner;

			fn cast_in(_: &mut $out, x: $in) -> Self::Type {
				x as Self::Type
			}

			fn cast_out(out: &mut $out, _: $in, x: Self::Type) {
				*out = x as $out;
			}
		}
	}
}

macro_rules! impl_triple2 {
	{<$($param:ident),*> ($out:ty, $in:ty) => $inner:ty} => {
        impl_triple!{<$($param),*> ($out, $in) => $inner}
        impl_triple!{<$($param),*> ($in, $out) => $inner}
	}
}

macro_rules! impl_pair_higher {
	{$inner:ty: [$(<$($param:ident),*> $ty1:ty),*]} => {
        impl_triple!{<> ($inner, $inner) => $inner}
		$(impl_triple2!{<$($param),*> ($inner, $ty1) => $inner})*
	}
}

macro_rules! impl_triple_list {
	{$inner:ty: [$($ty1:ty: ($(<$($param:ident),*> $ty2:ty),*)),*]} => {
		$($(impl_triple2!{<$($param),*> ($ty1, $ty2) => $inner})*)*
	}
}

// Any pair of pointers
impl_triple! {<T, U> (*const T, *const U) => usize}
impl_triple! {<T, U> (*const T, *mut U) => usize}
impl_triple! {<T, U> (*mut T, *const U) => usize}
impl_triple! {<T, U> (*mut T, *mut U) => usize}

impl_pair_higher! {u8:  [<>i8]}
impl_pair_higher! {u16: [<>u8, <>i8, <>i16]}
impl_pair_higher! {u32: [<>u8, <>u16, <>i8, <>i16, <>i32]}
impl_pair_higher! {u64: [<>u8, <>u16, <>u32, <>i8, <>i16, <>i32, <>i64, <>usize, <>isize]}
impl_pair_higher! {i8:  []}
impl_pair_higher! {i16: [<>u8, <>i8]}
impl_pair_higher! {i32: [<>u8, <>u16, <>i8, <>i16]}
impl_pair_higher! {i64: [<>u8, <>u16, <>u32, <>i8, <>i16, <>i32, <>usize, <>isize]}
impl_pair_higher! {usize: [<>u8, <>u16, <>i8, <>i16, <>isize]}
impl_pair_higher! {isize: [<>u8, <>u16, <>i8, <>i16]}

// Types that are always smaller than a pointer
impl_triple_list! {usize: [u8: (<T>*const T, <T>*mut T),
                          u16: (<T>*const T, <T>*mut T),
                          i8: (<T>*const T, <T>*mut T),
                          i16: (<T>*const T, <T>*mut T),
                          usize: (<T>*const T, <T>*mut T),
                          isize: (<T>*const T, <T>*mut T)]
}

// Types that are always larger than a pointer
impl_triple_list! {u64: [u64: (<T>*const T, <T>*mut T)]}
impl_triple_list! {i64: [i64: (<T>*const T, <T>*mut T)]}

// Target-specific types
#[cfg(target_pointer_width = "64")]
impl_triple_list! {u64: [usize: (<>u32, <>i32),
                        isize: (<>u32, <>i32),
                        u32: (<T>*const T, <T>*mut T),
                        i32: (<T>*const T, <T>*mut T)]
}

#[cfg(target_pointer_width = "32")]
impl_triple_list! {u32: [usize: (<>u32, <>i32),
                        isize: (<>u32, <>i32),
                        u32: (<T>*const T, <T>*mut T),
                        i32: (<T>*const T, <T>*mut T)]
}

#[cfg(target_pointer_width = "16")]
impl_triple_list! {u32: [usize: (<>u32, <>i32),
                        isize: (<>u32, <>i32),
                        u32: (<T>*const T, <T>*mut T),
                        i32: (<T>*const T, <T>*mut T)]
}

// FIXME: for now, we assume that we have a 16-, 32- or 64-bit architecture
// we'll need to handle other sizes if we ever encounter any

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_coverage() {
        macro_rules! test_combo {
            ($ty1:ty, [$($ty2:ty),*]) => {
                $({
                    let x = 42usize as $ty1;
                    let mut y: $ty2 = 0 as $ty2;
                    let z = AsmCast::cast_in(&mut y, x) + 1;
                    AsmCast::cast_out(&mut y, x, z);
                    assert_eq!(y as u64, 43);
                })*
            }
        }

        // Test all combinations of types to make sure we cover them
        test_combo!(u8, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
        test_combo!(u16, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
        test_combo!(u32, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
        test_combo!(u64, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
        test_combo!(usize, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
        test_combo!(i8, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
        test_combo!(i16, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
        test_combo!(i32, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
        test_combo!(i64, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
        test_combo!(isize, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
        test_combo!(*const u16, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
        test_combo!(*mut u16, [u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, *const u8, *mut u8]);
    }
}
