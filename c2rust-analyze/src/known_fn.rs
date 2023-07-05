use crate::context::PermissionSet;

#[cfg(test)]
macro_rules! const_slice {
    ($ty:ty, []) => {{
        &[]
    }};
    ($ty:ty, $array:expr) => {{
        const ARRAY: [$ty; $array.len()] = $array;
        &ARRAY
    }};
}

#[cfg(test)]
macro_rules! perms_annotation {
    ([$($($perm:ident)|*),*]) => {{
        [$(PermissionSet::union_all([$(PermissionSet::$perm,)*]),)*]
    }};
}

#[derive(Debug, PartialEq, Eq)]
pub struct KnownFnTy {
    name: &'static str,
    ty: &'static str,
    perms: &'static [PermissionSet],
}

impl KnownFnTy {
    /// Check that we annotated the right number of [`PermissionSet`]s
    /// that corresponds to the number of raw pointers in [`Self::ty`].
    #[cfg(test)]
    const fn checked(self) -> Self {
        let ty = self.ty.as_bytes();
        let mut num_ptrs = 0;
        let mut i = 0;
        while i < ty.len() {
            if ty[i] == b'*' {
                num_ptrs += 1;
            }
            i += 1;
        }
        assert!(self.perms.len() == num_ptrs);
        self
    }
}

#[cfg(test)]
macro_rules! known_fn_ty {
    ($ty:ty: $perms:tt) => {{
        KnownFnTy {
            name: "",
            ty: stringify!($ty),
            perms: const_slice!(PermissionSet, perms_annotation!($perms)),
        }
        .checked()
    }};
    ($ty:ty) => {{
        KnownFnTy {
            name: "",
            ty: stringify!($ty),
            perms: &[],
        }
        .checked()
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_perms_annotation() {
        assert_eq!(
            perms_annotation!([WRITE | OFFSET_ADD]),
            [PermissionSet::WRITE | PermissionSet::OFFSET_ADD]
        );
    }

    #[test]
    fn two_perms_annotations() {
        assert_eq!(
            perms_annotation!([WRITE, WRITE | OFFSET_ADD]),
            [
                PermissionSet::WRITE,
                PermissionSet::WRITE | PermissionSet::OFFSET_ADD
            ]
        );
    }

    #[test]
    fn check_known_fn_ty_eq() {
        KnownFnTy {
            name: "",
            ty: "*mut *const",
            perms: const_slice!(PermissionSet, [PermissionSet::empty(); 2]),
        }
        .checked();
    }

    #[test]
    #[should_panic]
    fn check_known_fn_ty_lt() {
        KnownFnTy {
            name: "",
            ty: "*const",
            perms: const_slice!(PermissionSet, [PermissionSet::empty(); 2]),
        }
        .checked();
    }

    #[test]
    #[should_panic]
    fn check_known_fn_ty_gt() {
        KnownFnTy {
            name: "",
            ty: "*const *const *mut",
            perms: const_slice!(PermissionSet, [PermissionSet::empty(); 2]),
        }
        .checked();
    }

    #[test]
    fn known_fn_ty() {
        assert_eq!(
            known_fn_ty!(*const i32: [READ]),
            KnownFnTy {
                name: "",
                ty: "*const i32",
                perms: const_slice!(PermissionSet, [PermissionSet::READ]),
            }
        );
    }
}
