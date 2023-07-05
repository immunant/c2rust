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

    #[cfg(test)]
    const fn named(self, name: &'static str) -> Self {
        let Self { name: _, ty, perms } = self;
        Self { name, ty, perms }
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

#[derive(Debug, PartialEq, Eq)]
pub struct KnownFn {
    name: &'static str,
    inputs: &'static [KnownFnTy],
    output: KnownFnTy,
}

#[cfg(test)]
macro_rules! known_fns {
    {
        mod $module:ident {

            $(
                fn $name:ident(
                    $($arg_name:ident: $arg_ty:ty$(: $arg_perms:tt)?,)*
                ) -> $return_ty:ty$(: $return_perms:tt)?
            );*;

        }
    } => {{
        use $module::*;

        const_slice!(KnownFn, [$({
            unsafe extern "C" fn $name($($arg_name: $arg_ty,)*) -> $return_ty {
                $(let _ = $arg_name;)*
                todo!()
            }
            // ensure the definitions match
            [$name, $module::$name];

            KnownFn {
                name: stringify!($name),
                inputs: const_slice!(KnownFnTy, [$(known_fn_ty!($arg_ty$(: $arg_perms)?).named(stringify!($arg_name)),)*]),
                output: known_fn_ty!($return_ty$(: $return_perms)?),
            }
        },)*])
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
    fn known_fn_ty_no_perms() {
        assert_eq!(
            known_fn_ty!(()),
            KnownFnTy {
                name: "",
                ty: "()",
                perms: &[],
            }
        );
    }

    #[test]
    fn known_fn_ty_with_one_perms() {
        assert_eq!(
            known_fn_ty!(*mut c_char: [WRITE | OFFSET_ADD]),
            KnownFnTy {
                name: "",
                ty: "*mut c_char",
                perms: const_slice!(
                    PermissionSet,
                    [PermissionSet::union_all([
                        PermissionSet::WRITE,
                        PermissionSet::OFFSET_ADD
                    ]),]
                ),
            }
        );
    }

    #[test]
    fn known_fn_ty_with_two_perms() {
        assert_eq!(
            known_fn_ty!(*mut *mut c_char: [WRITE, WRITE | OFFSET_ADD]),
            KnownFnTy {
                name: "",
                ty: "*mut *mut c_char",
                perms: const_slice!(
                    PermissionSet,
                    [
                        PermissionSet::union_all([PermissionSet::WRITE,]),
                        PermissionSet::union_all([PermissionSet::WRITE, PermissionSet::OFFSET_ADD])
                    ]
                ),
            }
        );
    }

    #[test]
    fn __errno_location() {
        assert_eq!(
            known_fns! {
                mod libc {

                    fn __errno_location() -> *mut c_int: [READ | WRITE];

                }
            },
            const_slice!(
                KnownFn,
                [KnownFn {
                    name: "__errno_location",
                    inputs: &[],
                    output: KnownFnTy {
                        name: "",
                        ty: "*mut c_int",
                        perms: const_slice!(
                            PermissionSet,
                            [PermissionSet::union_all([
                                PermissionSet::READ,
                                PermissionSet::WRITE
                            ])]
                        )
                    }
                }]
            )
        );
    }

    #[test]
    fn read() {
        assert_eq!(
            known_fns! {
                mod libc {

                    fn read(
                        fd: c_int,
                        buf: *mut c_void: [WRITE | OFFSET_ADD],
                        count: size_t,
                    ) -> ssize_t;

                }
            },
            const_slice!(
                KnownFn,
                [KnownFn {
                    name: "read",
                    inputs: const_slice!(
                        KnownFnTy,
                        [
                            KnownFnTy {
                                name: "fd",
                                ty: "c_int",
                                perms: &[],
                            },
                            KnownFnTy {
                                name: "buf",
                                ty: "*mut c_void",
                                perms: const_slice!(
                                    PermissionSet,
                                    [PermissionSet::union_all([
                                        PermissionSet::WRITE,
                                        PermissionSet::OFFSET_ADD
                                    ])]
                                ),
                            },
                            KnownFnTy {
                                name: "count",
                                ty: "size_t",
                                perms: &[],
                            },
                        ]
                    ),
                    output: KnownFnTy {
                        name: "",
                        ty: "ssize_t",
                        perms: &[],
                    }
                }]
            )
        );
    }

    #[test]
    fn strtol() {
        assert_eq!(
            known_fns! {
                mod libc {

                    fn strtol(
                        s: *const c_char: [READ | OFFSET_ADD],
                        endp: *mut *mut c_char: [WRITE, WRITE | OFFSET_ADD],
                        base: c_int,
                    ) -> c_long;

                }
            },
            const_slice!(
                KnownFn,
                [KnownFn {
                    name: "strtol",
                    inputs: const_slice!(
                        KnownFnTy,
                        [
                            KnownFnTy {
                                name: "s",
                                ty: "*const c_char",
                                perms: const_slice!(
                                    PermissionSet,
                                    [PermissionSet::union_all([
                                        PermissionSet::READ,
                                        PermissionSet::OFFSET_ADD
                                    ])]
                                ),
                            },
                            KnownFnTy {
                                name: "endp",
                                ty: "*mut *mut c_char",
                                perms: const_slice!(
                                    PermissionSet,
                                    [
                                        PermissionSet::union_all([PermissionSet::WRITE,]),
                                        PermissionSet::union_all([
                                            PermissionSet::WRITE,
                                            PermissionSet::OFFSET_ADD
                                        ])
                                    ]
                                ),
                            },
                            KnownFnTy {
                                name: "base",
                                ty: "c_int",
                                perms: &[],
                            },
                        ]
                    ),
                    output: KnownFnTy {
                        name: "",
                        ty: "c_long",
                        perms: &[],
                    }
                }]
            )
        );
    }
}
