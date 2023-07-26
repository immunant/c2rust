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

/// Since [`PermissionSet`]s are allowed to be dropped in any assignment,
/// the conservation approach for annotations is to
/// add everything on arguments and nothing on returns.
///
/// For example, for [`NON_NULL`], it is not always clearly documented
/// if an argument can or cannot be `NULL`,
/// though it is generally more clearly documented for the return type.
/// Furthermore, some functions are not supposed to accept `NULL`,
/// but instead of it being UB if called with `NULL`,
/// the function/syscall returns a normal error like `EFAULT`.
///
/// Thus, due to the way [`PermissionSet`]s can be dropped on assignment,
/// we annotate any argument with [`NON_NULL`] if `NULL` might cause UB,
/// and we annotate returns with [`NON_NULL`] only if it is strictly guaranteed to be non-`NULL`.
///
/// [`NON_NULL`]: PermissionSet::NON_NULL
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

            // fn __ctype_b_loc;

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

            fn alarm(
                seconds: c_uint,
            ) -> c_uint;

            fn atoi(
                s: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            fn bind(
                socket: c_int,
                address: *const sockaddr: [READ],
                address_len: socklen_t,
            ) -> c_int;

            fn calloc(
                nobj: size_t,
                size: size_t,
            ) -> *mut c_void: [READ | WRITE | OFFSET_ADD | FREE];

            fn chdir(
                dir: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            fn chmod(
                path: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                mode: mode_t,
            ) -> c_int;

            fn chroot(
                name: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            fn clock_gettime(
                clk_id: clockid_t,
                tp: *mut timespec: [WRITE],
            ) -> c_int;

            fn close(
                fd: c_int,
            ) -> c_int;

            fn closelog() -> ();

            fn dlclose(
                handle: *mut c_void: [READ | WRITE | NON_NULL],
            ) -> c_int;

            // Not `WRITE` even though it's `*mut` since future calls may overwrite the returned memory,
            // as it may be statically allocated and reused.  It is not meant to be modified.
            fn dlerror() -> *mut c_char: [READ | OFFSET_ADD];

            fn dlopen(
                filename: *const c_char: [READ | OFFSET_ADD],
                flag: c_int,
            ) -> *mut c_void: [READ | WRITE];

            fn dlsym(
                handle: *mut c_void: [READ | WRITE], // TODO(kkysen) may not be a pointer
                symbol: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> *mut c_void: [READ | WRITE | OFFSET_ADD | OFFSET_SUB | FREE]; // Could be anything

            fn dup(
                fd: c_int,
            ) -> c_int;

            fn dup2(
                src: c_int,
                dst: c_int,
            ) -> c_int;

            #[cfg(target_os = "linux")]
            fn epoll_create1(
                flags: c_int,
            ) -> c_int;

            #[cfg(target_os = "linux")]
            fn epoll_ctl(
                epfd: c_int,
                op: c_int,
                fd: c_int,
                // Declared as `*mut`, but docs say it's only read.
                event: *mut epoll_event: [READ],
            ) -> c_int;

            #[cfg(target_os = "linux")]
            fn epoll_wait(
                epfd: c_int,
                events: *mut epoll_event: [WRITE | OFFSET_ADD | NON_NULL],
                maxevents: c_int,
                timeout: c_int,
            ) -> c_int;

            fn execv(
                prog: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                argv: *const *const c_char: [READ | OFFSET_ADD | NON_NULL, READ | OFFSET_ADD],
            ) -> c_int;

            fn execve(
                prog: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                argv: *const *const c_char: [READ | OFFSET_ADD | NON_NULL, READ | OFFSET_ADD],
                envp: *const *const c_char: [READ | OFFSET_ADD | NON_NULL, READ | OFFSET_ADD],
            ) -> c_int;

            fn exit(
                status: c_int,
            ) -> !;

            #[cfg(target_os = "linux")]
            fn explicit_bzero(
                s: *mut c_void: [WRITE | OFFSET_ADD | NON_NULL],
                len: size_t,
            ) -> ();

            fn fchdir(
                dirfd: c_int,
            ) -> c_int;

            // fn fcntl(
            //     fd: c_int,
            //     cmd: c_int,
                   // TODO(kkysen) varargs
            //     ...
            // ) -> c_int,

            fn fflush(
                file: *mut FILE: [READ | WRITE | NON_NULL],
            ) -> c_int;

            fn fork() -> pid_t;

            // fn fprintf(
            //     stream: *mut FILE: [READ | WRITE | NON_NULL],
            //     format: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                   // TODO(kkysen) varargs, printf-style
            //     ...
            // ) -> c_int;

            fn fputs(
                s: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                stream: *mut FILE: [READ | WRITE | NON_NULL],
            ) -> c_int;

            fn free(
                p: *mut c_void: [FREE],
            ) -> ();

            fn freeaddrinfo(
                // TODO(kkysen) Should this be `FREE`, as it could be manually `free`d,
                // but is not meant to be `free`d directly?
                res: *mut addrinfo: [READ | WRITE],
            ) -> ();

            fn fstat(
                fildes: c_int,
                buf: *mut stat: [WRITE | NON_NULL],
            ) -> c_int;

            fn ftruncate(
                fd: c_int,
                length: off_t,
            ) -> c_int;

            fn gai_strerror(
                errcode: c_int,
            ) -> *const c_char: [READ | OFFSET_ADD | NON_NULL];

            fn getaddrinfo(
                node: *const c_char: [READ | OFFSET_ADD],
                service: *const c_char: [READ | OFFSET_ADD],
                hints: *const addrinfo: [READ],
                // TODO(kkysen) Should the 2nd ptr be `FREE`, as it could be manually `free`d,
                // but is not meant to be `free`d directly, but by `freeaddrinfo`?.
                res: *mut *mut addrinfo: [WRITE | NON_NULL, WRITE | NON_NULL],
            ) -> c_int;

            fn getcwd(
                // `READ` because the return type is `READ`.
                buf: *mut c_char: [READ | WRITE | OFFSET_ADD],
                size: size_t,
                // glibc's extension will allocate if `buf` is `NULL`.
            ) -> *mut c_char: [READ | WRITE | OFFSET_ADD | FREE];

            fn getegid() -> gid_t;

            #[cfg(target_os = "linux")]
            fn getentropy(
                buf: *mut c_void: [WRITE | OFFSET_ADD | NON_NULL],
                buflen: size_t,
            ) -> c_int;

            fn getenv(
                s: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                // Not `WRITE` even though it's `*mut` since future calls may overwrite the returned memory,
                // as it may be statically allocated and reused.  It is not meant to be modified.
            ) -> *mut c_char: [READ | OFFSET_ADD];

            fn geteuid() -> uid_t;

            fn getgid() -> gid_t;

            fn getgrgid(
                gid: gid_t,
                // Not `WRITE` even though it's `*mut` since future calls may overwrite the returned memory,
                // as it may be statically allocated and reused.  It is not meant to be modified.
            ) -> *mut group: [READ];

            fn getgrnam(
                name: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                // Not `WRITE` even though it's `*mut` since future calls may overwrite the returned memory,
                // as it may be statically allocated and reused.  It is not meant to be modified.
            ) -> *mut group: [READ];

            fn getloadavg(
                loadavg: *mut c_double: [WRITE | OFFSET_ADD | NON_NULL],
                nelem: c_int,
            ) -> c_int;

            fn getnameinfo(
                sa: *const sockaddr: [READ | NON_NULL],
                salen: socklen_t,
                host: *mut c_char: [READ | OFFSET_ADD],
                hostlen: socklen_t,
                serv: *mut c_char: [READ | OFFSET_ADD],
                sevlen: socklen_t,
                flags: c_int,
            ) -> c_int;

            fn getopt(
                argc: c_int,
                // The outer `*const` array is actually mutated unless `$POSIXLY_CORRECT` is set.
                argv: *const *mut c_char: [READ | WRITE | OFFSET_ADD | NON_NULL, READ | OFFSET_ADD],
                optstr: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            fn getpeername(
                socket: c_int,
                address: *mut sockaddr: [WRITE | NON_NULL],
                address_len: *mut socklen_t: [READ | WRITE | NON_NULL],
            ) -> c_int;

            fn getpid() -> pid_t;

            fn getppid() -> pid_t;

            fn getpwnam(
                name: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                // Not `WRITE` even though it's `*mut` since future calls may overwrite the returned memory,
                // as it may be statically allocated and reused.  It is not meant to be modified.
            ) -> *mut passwd: [READ];

            #[cfg(target_os = "linux")]
            fn getrlimit(
                resource: __rlimit_resource_t,
                rlim: *mut rlimit: [WRITE | NON_NULL],
            ) -> c_int;

            #[cfg(target_os = "macos")]
            fn getrlimit(
                resource: c_int,
                rlim: *mut rlimit: [WRITE | NON_NULL],
            ) -> c_int;

            fn getsockname(
                socket: c_int,
                address: *mut sockaddr: [WRITE | NON_NULL],
                address_len: *mut socklen_t: [READ | WRITE | NON_NULL],
            ) -> c_int;

            fn getsockopt(
                sockfd: c_int,
                level: c_int,
                optname: c_int,
                optval: *mut c_void: [WRITE | NON_NULL],
                optlen: *mut socklen_t: [READ | WRITE | NON_NULL],
            ) -> c_int;

            fn getuid() -> uid_t;

            // fn glob(
            //     pattern: *const c_char,
            //     flags: c_int,
                   // TODO(kkysen) Not yet sure how to handle fn ptrs.
            //     errfunc: Option<extern "C" fn(epath: *const c_char, errno: c_int) -> c_int>,
            //     pglob: *mut glob_t,
            // ) -> c_int;

            fn globfree(
                pglob: *mut glob_t: [READ | WRITE | NON_NULL],
            ) -> ();

            fn gmtime_r(
                time_p: *const time_t: [READ | NON_NULL],
                // `READ` because it's returned.
                result: *mut tm: [READ | WRITE | NON_NULL],
            ) -> *mut tm: [READ | WRITE];

            // TODO(kkysen) Not in `libc` crate.
            // fn htonl;

            // TODO(kkysen) Not in `libc` crate.
            // fn htons;

            // TODO(kkysen) Not in `libc` crate.
            // fn inet_ntop;

            // TODO(kkysen) Not in `libc` crate.
            // fn inet_pton;

            #[cfg(target_os = "linux")]
            fn initgroups(
                user: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                group: gid_t,
            ) -> c_int;

            #[cfg(target_os = "macos")]
            fn initgroups(
                user: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                basegid: c_int,
            ) -> c_int;

            #[cfg(target_os = "linux")]
            fn inotify_add_watch(
                fd: c_int,
                path: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                mask: u32,
            ) -> c_int;

            #[cfg(target_os = "linux")]
            fn inotify_init() -> c_int;

            #[cfg(target_os = "linux")]
            fn inotify_init1(
                flags: c_int,
            ) -> c_int;

            #[cfg(target_os = "linux")]
            fn inotify_rm_watch(
                fd: c_int,
                wd: c_int,
            ) -> c_int;

            // fn ioctl(
            //     fd: c_int,
            //     request: c_ulong,
                   // TODO(kkysen) varargs
            //     ...
            // ) -> c_int;

            fn kill(
                pid: pid_t,
                sig: c_int,
            ) -> c_int;

            fn listen(
                socket: c_int,
                backlog: c_int,
            ) -> c_int;

            fn localtime_r(
                time_p: *const time_t: [READ | NON_NULL],
                // `READ` because it's returned (if there's no error).
                result: *mut tm: [READ | WRITE | NON_NULL],
            ) -> *mut tm: [READ | WRITE];

            fn lseek(
                fd: c_int,
                offset: off_t,
                whence: c_int,
            ) -> off_t;

            fn lstat(
                path: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                buf: *mut stat: [WRITE | NON_NULL],
            ) -> c_int;

            fn malloc(
                size: size_t,
            ) -> *mut c_void: [READ | WRITE | OFFSET_ADD | FREE]; // TODO(kkysen) OFFSET_ADD?

            fn memchr(
                // `WRITE` because the return type is derived from `cx`'s provenance.
                cx: *const c_void: [READ | WRITE | OFFSET_ADD | NON_NULL],
                c: c_int,
                n: size_t,
            ) -> *mut c_void: [READ | WRITE | OFFSET_ADD | OFFSET_SUB];

            fn memcmp(
                cx: *const c_void: [READ | OFFSET_ADD | NON_NULL],
                ct: *const c_void: [READ | OFFSET_ADD | NON_NULL],
                n: size_t,
            ) -> c_int;

            fn memcpy(
                dest: *mut c_void: [WRITE | OFFSET_ADD | NON_NULL],
                src: *const c_void: [READ | OFFSET_ADD | NON_NULL],
                n: size_t,
                // Same as `dest`, but while `dest` shouldn't be `NULL`,
                // it can without error, and thus this can return `NULL`.
            ) -> *mut c_void: [WRITE | OFFSET_ADD];

            fn memmove(
                // `dest` and `src` can overlap, which seems like it may be UB if they are slices,
                // but since we can't safely produce those overlapping slices,
                // we would instead end up with two `&[Cell<T>]`s in possibly overlapping cases.
                dest: *mut c_void: [WRITE | OFFSET_ADD | NON_NULL],
                src: *const c_void: [READ | OFFSET_ADD | NON_NULL],
                n: size_t,
                // Same as `dest`, but while `dest` shouldn't be `NULL`,
                // it can without error, and thus this can return `NULL`.
            ) -> *mut c_void: [WRITE | OFFSET_ADD];

            // TODO(kkysen) Not in `libc` crate.
            // fn mempcpy;

            fn memset(
                dest: *mut c_void: [WRITE | OFFSET_ADD | NON_NULL],
                c: c_int,
                n: size_t,
                // Same as `dest`, but while `dest` shouldn't be `NULL`,
                // it can without error, and thus this can return `NULL`.
            ) -> *mut c_void: [WRITE | OFFSET_ADD];

            #[cfg(target_os = "linux")]
            fn mkostemp(
                template: *mut c_char: [READ | WRITE | OFFSET_ADD | NON_NULL],
                flags: c_int,
            ) -> c_int;

            fn mmap(
                // not yet a valid pointer
                addr: *mut c_void: [NONE],
                len: size_t,
                prot: c_int,
                flags: c_int,
                fd: c_int,
                offset: off_t,
                // TODO(kkysen) not always a pointer, can be -1
            ) -> *mut c_void: [READ | WRITE | OFFSET_ADD | NON_NULL];

            fn munmap(
                addr: *mut c_void: [OFFSET_ADD | NON_NULL],
                len: size_t,
            ) -> c_int;

            // TODO(kkysen) Not in `libc` crate.
            // fn ntohs;

            // fn open(
            //     path: *const c_char,
            //     oflag: c_int,
                   // TODO(kkysen) varargs
            //     ...
            // ) -> c_int;

            fn openlog(
                ident: *const c_char: [READ | OFFSET_ADD],
                logopt: c_int,
                facility: c_int,
            ) -> ();

            fn perror(
                s: *const c_char: [READ | OFFSET_ADD],
            ) -> ();

            fn pipe(
                fds: *mut c_int: [WRITE | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            #[cfg(target_os = "linux")]
            fn pipe2(
                fds: *mut c_int: [WRITE | OFFSET_ADD | NON_NULL],
                flags: c_int,
            ) -> c_int;

            fn poll(
                fds: *mut pollfd: [READ | WRITE | OFFSET_ADD | NON_NULL],
                nfds: nfds_t,
                timeout: c_int,
            ) -> c_int;

            // fn prctl(
            //     option: c_int,
                   // TODO(kkysen) varargs
            //     ...
            // ) -> c_int;

            fn pread(
                fd: c_int,
                buf: *mut c_void: [WRITE | OFFSET_ADD | NON_NULL],
                count: size_t,
                offset: off_t,
            ) -> ssize_t;

            // fn printf(
            //     format: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                   // TODO(kkysen) varargs, printf-style
            //     ...
            // ) -> c_int;

            fn puts(
                s: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            fn pwrite(
                fd: c_int,
                buf: *const c_void: [READ | OFFSET_ADD | NON_NULL],
                count: size_t,
                offset: off_t,
            ) -> ssize_t;

            fn pwritev(
                fd: c_int,
                iov: *const iovec: [READ | OFFSET_ADD | NON_NULL],
                iovcnt: c_int,
                offset: off_t,
            ) -> ssize_t;

            fn raise(
                signum: c_int,
            ) -> c_int;

            fn rand() -> c_int;

            fn read(
                fd: c_int,
                buf: *mut c_void: [WRITE | OFFSET_ADD],
                count: size_t,
            ) -> ssize_t;

            fn realloc(
                p: *mut c_void: [READ | WRITE | OFFSET_ADD | FREE],
                size: size_t,
            ) -> *mut c_void: [READ | WRITE | OFFSET_ADD | FREE];

            fn recv(
                socket: c_int,
                buf: *mut c_void: [WRITE | OFFSET_ADD | NON_NULL],
                len: size_t,
                flags: c_int,
            ) -> ssize_t;

            fn rename(
                oldname: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                newname: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            fn select(
                nfds: c_int,
                readfds: *mut fd_set: [READ | WRITE],
                writefds: *mut fd_set: [READ | WRITE],
                errorfds: *mut fd_set: [READ | WRITE],
                timeout: *mut timeval: [READ | WRITE],
            ) -> c_int;

            #[cfg(target_os = "linux")]
            fn sendfile(
                out_fd: c_int,
                in_fd: c_int,
                offset: *mut off_t: [READ | WRITE],
                count: size_t,
            ) -> ssize_t;

            #[cfg(target_os = "macos")]
            fn sendfile(
                fd: c_int,
                s: c_int,
                offset: off_t,
                len: *mut off_t: [READ | WRITE | NON_NULL],
                // The docs don't indicate that it is modified, but it is not declared `*const`.
                hdtr: *mut sf_hdtr: [READ],
                flags: c_int,
            ) -> c_int;

            fn setenv(
                name: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                val: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                overwrite: c_int,
            ) -> c_int;

            fn setgid(
                gid: gid_t,
            ) -> c_int;

            #[cfg(target_os = "linux")]
            fn setgroups(
                ngroups: size_t,
                ptr: *const gid_t: [READ | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            #[cfg(target_os = "macos")]
            fn setgroups(
                ngroups: c_int,
                ptr: *const gid_t: [READ | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            fn setlocale(
                category: c_int,
                locale: *const c_char: [READ | OFFSET_ADD],
                // Not `WRITE` even though it's `*mut` since future calls may overwrite the returned memory,
                // as it may be statically allocated and reused.  It is not meant to be modified.
            ) -> *mut c_char: [READ | OFFSET_ADD];

            #[cfg(target_os = "linux")]
            fn setrlimit(
                resource: __rlimit_resource_t,
                rlim: *const rlimit: [READ | NON_NULL],
            ) -> c_int;

            #[cfg(target_os = "macos")]
            fn setrlimit(
                resource: c_int,
                rlim: *const rlimit: [READ | NON_NULL],
            ) -> c_int;

            fn setsid() -> pid_t;

            fn setsockopt(
                socket: c_int,
                level: c_int,
                name: c_int,
                value: *const c_void: [READ | NON_NULL],
                option_len: socklen_t,
            ) -> c_int;

            fn setuid(
                uid: uid_t,
            ) -> c_int;

            fn shutdown(
                socket: c_int,
                how: c_int,
            ) -> c_int;

            fn sigaction(
                signum: c_int,
                act: *const sigaction: [READ],
                oldact: *mut sigaction: [WRITE],
            ) -> c_int;

            fn sigemptyset(
                set: *mut sigset_t: [WRITE | NON_NULL],
            ) -> c_int;

            // fn signal(
            //     signum: c_int,
                   // TODO(kkysen) `libc::sighandler_t` is `size_t`, but may be a fn ptr.
            //     handler: sighandler_t,
            // ) -> sighandler_t;

            // fn snprintf(
            //     s: *mut c_char: [WRITE | OFFSET_ADD | NON_NULL],
            //     n: size_t,
            //     format: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                   // TODO(kkysen) varargs, printf-style
            //     ...
            // ) -> c_int;

            fn socket(
                domain: c_int,
                ty: c_int,
                protocol: c_int,
            ) -> c_int;

            #[cfg(target_os = "linux")]
            fn splice(
                fd_in: c_int,
                off_in: *mut loff_t: [READ | WRITE],
                fd_out: c_int,
                off_out: *mut loff_t: [READ | WRITE],
                len: size_t,
                flags: c_uint,
            ) -> ssize_t;

            fn srand(
                seed: c_uint,
            ) -> ();

            // TODO(kkysen) Not in `libc` crate.
            // fn srandom;

            fn stat(
                path: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                buf: *mut stat: [WRITE | NON_NULL],
            ) -> c_int;

            fn strchr(
                // `WRITE` because the return type is derived from `cs`'s provenance.
                cs: *const c_char: [READ | WRITE | OFFSET_ADD | NON_NULL],
                c: c_int,
            ) -> *mut c_char: [READ | WRITE | OFFSET_ADD];

            fn strcmp(
                cs: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                ct: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            fn strcspn(
                cs: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                ct: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> size_t;

            fn strdup(
                cs: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> *mut c_char: [READ | WRITE | OFFSET_ADD | FREE];

            fn strerror_r(
                errnum: c_int,
                buf: *mut c_char: [WRITE | OFFSET_ADD | NON_NULL],
                buflen: size_t,
            ) -> c_int;

            fn strftime(
                s: *mut c_char: [WRITE | OFFSET_ADD | NON_NULL],
                max: size_t,
                format: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                tm: *const tm: [READ | NON_NULL],
            ) -> size_t;

            fn strlen(
                cs: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> size_t;

            fn strncasecmp(
                s1: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                s2: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                n: size_t,
            ) -> c_int;

            fn strncmp(
                cs: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                ct: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                n: size_t,
            ) -> c_int;

            fn strrchr(
                // `WRITE` because the return type is derived from `cs`'s provenance.
                cs: *const c_char: [READ | WRITE | OFFSET_ADD | NON_NULL],
                c: c_int,
            ) -> *mut c_char: [READ | WRITE | OFFSET_ADD];

            fn strstr(
                // `WRITE` because it's returned.
                cs: *const c_char: [READ | WRITE | OFFSET_ADD | NON_NULL],
                ct: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> *mut c_char: [READ | WRITE | OFFSET_ADD];

            fn strtol(
                s: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                endp: *mut *mut c_char: [WRITE, WRITE | OFFSET_ADD],
                base: c_int,
            ) -> c_long;

            fn strtoll(
                s: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                endp: *mut *mut c_char: [WRITE, WRITE | OFFSET_ADD],
                base: c_int,
            ) -> c_longlong;

            fn strtoul(
                s: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                endp: *mut *mut c_char: [WRITE, WRITE | OFFSET_ADD],
                base: c_int,
            ) -> c_ulong;

            fn sysconf(
                name: c_int,
            ) -> c_long;

            // fn syslog(
            //     priority: c_int,
            //     message: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                   // TODO(kkysen) varargs, printf-style
            //     ...
            // );

            fn time(
                time: *mut time_t: [WRITE],
            ) -> time_t;

            fn timegm(
                tm: *mut tm: [READ | WRITE | NON_NULL],
            ) -> time_t;

            // TODO(kkysen) Not in `libc` crate.
            // fn tzset;

            fn unlink(
                c: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            fn unsetenv(
                name: *const c_char: [READ | OFFSET_ADD | NON_NULL],
            ) -> c_int;

            // TODO(kkysen) Not in `libc` crate.
            // fn vsnprintf;

            fn waitpid(
                pid: pid_t,
                status: *mut c_int: [WRITE],
                options: c_int,
            ) -> pid_t;

            fn write(
                fd: c_int,
                buf: *const c_void: [READ | OFFSET_ADD],
                count: size_t,
            ) -> ssize_t;

            fn writev(
                fd: c_int,
                iov: *const iovec: [READ | OFFSET_ADD | NON_NULL],
                iovcnt: c_int,
            ) -> ssize_t;

        }
    }
}
