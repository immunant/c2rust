use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

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

#[derive(Debug, Eq)]
pub struct KnownFnTy {
    pub name: &'static str,
    pub ty: &'static str,
    pub perms: &'static [PermissionSet],
    pub source: &'static str,
}

impl KnownFnTy {
    fn eq_fields(&self) -> impl Eq + '_ {
        let Self {
            name,
            ty,
            perms,
            source: _,
        } = self;
        (name, ty, perms)
    }
}

impl PartialEq for KnownFnTy {
    fn eq(&self, other: &Self) -> bool {
        self.eq_fields() == other.eq_fields()
    }
}

impl Display for KnownFnTy {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.source)
    }
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
        let Self {
            name: _,
            ty,
            perms,
            source,
        } = self;
        Self {
            name,
            ty,
            perms,
            source,
        }
    }
}

#[cfg(test)]
macro_rules! known_fn_ty {
    ($ty:ty: $perms:tt) => {{
        KnownFnTy {
            name: "",
            ty: stringify!($ty),
            perms: const_slice!(PermissionSet, perms_annotation!($perms)),
            source: stringify!($ty: $perms),
        }
        .checked()
    }};
    ($ty:ty) => {{
        KnownFnTy {
            name: "",
            ty: stringify!($ty),
            perms: &[],
            source: stringify!($ty),
        }
        .checked()
    }};
}

#[derive(Debug, Eq)]
pub struct KnownFn {
    pub name: &'static str,
    pub inputs: &'static [KnownFnTy],
    pub output: KnownFnTy,
    pub source: &'static str,
}

impl KnownFn {
    fn eq_fields(&self) -> impl Eq + '_ {
        let Self {
            name,
            inputs,
            output,
            source: _,
        } = self;
        (name, inputs, output)
    }
}

impl PartialEq for KnownFn {
    fn eq(&self, other: &Self) -> bool {
        self.eq_fields() == other.eq_fields()
    }
}

impl Display for KnownFn {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(&self.source.replace('\n', ""))
    }
}

#[cfg(test)]
macro_rules! known_fns {
    {
        mod $module:ident {

            $(
                $(#[$attr:meta])?
                fn $name:ident(
                    $($arg_name:ident: $arg_ty:ty$(: $arg_perms:tt)?,)*
                ) -> $return_ty:ty$(: $return_perms:tt)?
            );*;

        }
    } => {{
        use $module::*;

        const_slice!(KnownFn, [$(
            $(#[$attr])?
            {
                unsafe extern "C" fn $name($($arg_name: $arg_ty,)*) -> $return_ty {
                    $(let _ = $arg_name;)*
                    todo!()
                }
                // ensure the definitions match
                [$name, $module::$name];

                let source = stringify!(
                    fn $name(
                        $($arg_name: $arg_ty$(: $arg_perms)?,)*
                    ) -> $return_ty$(: $return_perms)?
                );

                KnownFn {
                    name: stringify!($name),
                    inputs: const_slice!(KnownFnTy, [$(known_fn_ty!($arg_ty$(: $arg_perms)?).named(stringify!($arg_name)),)*]),
                    output: known_fn_ty!($return_ty$(: $return_perms)?),
                    source,
                }
            }
        ,)*])
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
            source: "",
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
            source: "",
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
            source: "",
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
                source: "",
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
                source: "",
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
                source: "",
            }
        );
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    #[test]
    fn __errno_location() {
        assert_eq!(
            known_fns! {
                mod libc {

                    #[cfg(target_os = "linux")]
                    fn __errno_location() -> *mut c_int: [READ | WRITE];

                    #[cfg(target_os = "macos")]
                    fn __error() -> *mut c_int: [READ | WRITE];

                }
            },
            const_slice!(
                KnownFn,
                [KnownFn {
                    #[cfg(target_os = "linux")]
                    name: "__errno_location",
                    #[cfg(target_os = "macos")]
                    name: "__error",
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
                        ),
                        source: "",
                    },
                    source: "",
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
                                source: "",
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
                                source: "",
                            },
                            KnownFnTy {
                                name: "count",
                                ty: "size_t",
                                perms: &[],
                                source: "",
                            },
                        ]
                    ),
                    output: KnownFnTy {
                        name: "",
                        ty: "ssize_t",
                        perms: &[],
                        source: "",
                    },
                    source: "",
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
                                source: "",
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
                                source: "",
                            },
                            KnownFnTy {
                                name: "base",
                                ty: "c_int",
                                perms: &[],
                                source: "",
                            },
                        ]
                    ),
                    output: KnownFnTy {
                        name: "",
                        ty: "c_long",
                        perms: &[],
                        source: "",
                    },
                    source: "",
                }]
            )
        );
    }
}
