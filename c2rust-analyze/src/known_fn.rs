use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::iter;

use crate::context::LFnSig;
use crate::context::LTy;
use crate::context::PermissionSet;
use crate::pointer_id::PointerId;
use crate::util::PhantomLifetime;

macro_rules! const_slice {
    ($ty:ty, []) => {{
        &[]
    }};
    ($ty:ty, $array:expr) => {{
        const ARRAY: [$ty; $array.len()] = $array;
        &ARRAY
    }};
}

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

    /// Determine the [`PermissionSet`]s that should constrain [`PointerId`]s
    /// contained in this [`KnownFnTy`].
    ///
    /// This is determined by matching the corresponding [`PointerId`]s from the [`LTy`]
    /// to the [`PermissionSet`]s of the [`KnownFnTy`].
    /// The [`PermissionSet`]s in a [`KnownFnTy`] correspond
    /// to the literal `*` pointers in the type left-to-right,
    /// which means they correspond to the [`PointerId`]s in the [`LTy`] from outside to inside,
    /// i.e. by iterating using [`LTy::iter`].
    /// We skip non-ptr [`PointerId`]s in such iteration,
    /// which should just be the innermost [`LTy`] after all of the pointers have been stripped.
    ///
    /// We also first check if the [`LTy`] and [`KnownFnTy`] match in number of pointers,
    /// as they could potentially differ if the [`LFnSig`] was not a [`libc`] `fn`,
    /// and print a warning if they don't match.
    /// Ideally we would check that the types equal,
    /// but the [`KnownFnTy`]'s type is only recorded as a literal string,
    /// and I'm not sure how that could be parsed back into a [`Ty`].
    ///
    /// [`LTy::iter`]: crate::labeled_ty::LabeledTyS::iter
    /// [`Ty`]: rustc_middle::ty::Ty
    pub fn ptr_perms<'a, 'tcx: 'a>(
        &'a self,
        lty: LTy<'tcx>,
    ) -> impl Iterator<Item = (PointerId, PermissionSet)> + PhantomLifetime<'tcx> + 'a {
        [(lty, self)]
            .into_iter()
            .filter(|(lty, known_ty)| {
                let lty_num_ptrs = lty.iter().filter(|lty| !lty.label.is_none()).count();
                let known_ty_num_ptrs = known_ty.perms.len();
                let matching = lty_num_ptrs == known_ty_num_ptrs;
                if !matching {
                    ::log::warn!(
                        "declared `extern \"C\" fn` type \
                 \n\tknown_ty: ({known_ty_num_ptrs}) {known_ty}\
                 \n\tlty: ({lty_num_ptrs}) {lty:?}\
            "
                    )
                }
                matching
            })
            .flat_map(|(lty, known_ty)| {
                iter::zip(
                    lty.iter().map(|lty| lty.label).filter(|ptr| !ptr.is_none()),
                    known_ty.perms,
                )
            })
            .map(|(ptr, &perms)| (ptr, perms))
    }
}

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

impl KnownFn {
    pub fn inputs_and_output(&self) -> impl Iterator<Item = &KnownFnTy> {
        self.inputs.iter().chain([&self.output])
    }

    /// Determine the [`PermissionSet`]s that should constrain [`PointerId`]s
    /// contained in the signatures of this [`KnownFn`].
    ///
    /// This is determined by matching the corresponding [`PointerId`]s from the [`LFnSig`]
    /// to the [`PermissionSet`]s of the [`KnownFn`].
    ///
    /// First we check if the [`LFnSig`] and [`KnownFn`] match in number of args/inputs.
    /// They should, but we double check since it could be a non-[`libc`] `fn` with the same name.
    /// [`KnownFn`]s are checked against the [`libc`] definition for it,
    /// but the `c2rust transpile`d version is platform-specific
    /// based on translating the declaration in the C headers.
    /// Thus, we just skip ones that don't match,
    /// and we print a warning if they don't match as well.
    ///
    /// Then we iterate over the [`LFnSig`] and [`KnownFn`]'s inputs and output,
    /// `flat_map`ping them to each [`KnownFnTy::ptr_perms`].
    pub fn ptr_perms<'a, 'tcx>(
        &'a self,
        fn_sig: &'a LFnSig<'tcx>,
    ) -> impl Iterator<Item = (PointerId, PermissionSet)> + PhantomLifetime<'tcx> + 'a {
        [(fn_sig, self)]
            .into_iter()
            .filter(|(fn_sig, known_fn)| {
                // Filter instead of asserting because we want the error to occur at the call site,
                // where it will be correctly scoped to the calling function
                // and mark only those functions as having failed.
                let matching = fn_sig.inputs.len() == known_fn.inputs.len();
                if !matching {
                    ::log::warn!(
                        "declared `extern \"C\" fn {}` does not match known fn in number of args:\
                     \n\tknown_fn: ({}) {}\
                     \n\tfn_sig: ({}) {:?}\
                ",
                        known_fn.name,
                        known_fn.inputs.len(),
                        known_fn,
                        fn_sig.inputs.len(),
                        fn_sig.inputs,
                    );
                }
                matching
            })
            .flat_map(|(fn_sig, known_fn)| {
                iter::zip(fn_sig.inputs_and_output(), known_fn.inputs_and_output())
            })
            .flat_map(|(lty, known_ty)| known_ty.ptr_perms(lty))
    }
}

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
                        PermissionSet::OFFSET_ADD,
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
                    fn __errno_location() -> *mut c_int: [READ | WRITE | NON_NULL];

                    #[cfg(target_os = "macos")]
                    fn __error() -> *mut c_int: [READ | WRITE | NON_NULL];

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
                                PermissionSet::WRITE,
                                PermissionSet::NON_NULL,
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
                                        PermissionSet::OFFSET_ADD,
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
                        s: *const c_char: [READ | OFFSET_ADD | NON_NULL],
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
                                        PermissionSet::OFFSET_ADD,
                                        PermissionSet::NON_NULL,
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
                                            PermissionSet::OFFSET_ADD,
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

pub const fn all_known_fns() -> &'static [KnownFn] {
    known_fns! {
        mod libc {

            #[cfg(target_os = "linux")]
            fn __errno_location() -> *mut c_int: [READ | WRITE | NON_NULL];

            #[cfg(target_os = "macos")]
            fn __error() -> *mut c_int: [READ | WRITE | NON_NULL];

            fn _exit(
                status: c_int,
            ) -> !;

            fn abort() -> !;

            fn abs(
                i: c_int,
            ) -> c_int;

            fn accept(
                socket: c_int,
                address: *mut sockaddr: [WRITE],
                address_len: *mut socklen_t: [READ | WRITE],
            ) -> c_int;

            #[cfg(target_os = "linux")]
            fn accept4(
                fd: c_int,
                addr: *mut sockaddr: [WRITE],
                len: *mut socklen_t: [READ | WRITE],
                flg: c_int,
            ) -> c_int;

            fn access(
                path: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                amode: c_int,
            ) -> c_int;

            fn read(
                fd: c_int,
                buf: *mut c_void: [WRITE | OFFSET_ADD],
                count: size_t,
            ) -> ssize_t;

            fn write(
                fd: c_int,
                buf: *const c_void: [READ | OFFSET_ADD],
                count: size_t,
            ) -> ssize_t;

            fn strtol(
                s: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                endp: *mut *mut c_char: [WRITE, WRITE | OFFSET_ADD],
                base: c_int,
            ) -> c_long;

        }
    }
}
