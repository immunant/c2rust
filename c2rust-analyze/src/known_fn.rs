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

            // fn atoi(
            //     s: *const c_char,
            // ) -> c_int;

            // fn bind(
            //     socket: c_int,
            //     address: *const sockaddr,
            //     address_len: socklen_t,
            // ) -> c_int;

            // fn calloc(
            //     nobj: size_t,
            //     size: size_t,
            // ) -> *mut c_void;

            // fn chdir(
            //     dir: *const c_char,
            // ) -> c_int;

            // fn chmod(
            //     path: *const c_char,
            //     mode: mode_t,
            // ) -> c_int;

            // fn chroot(
            //     name: *const c_char,
            // ) -> c_int;

            // fn clock_gettime(
            //     clk_id: clockid_t,
            //     tp: *mut timespec,
            // ) -> c_int;

            fn close(
                fd: c_int,
            ) -> c_int;

            fn closelog() -> ();

            // fn dlclose(
            //     handle: *mut c_void,
            // ) -> c_int;

            // fn dlerror() -> *mut c_char;

            // fn dlopen(
            //     filename: *const c_char,
            //     flag: c_int,
            // ) -> *mut c_void;

            // fn dlsym(
            //     handle: *mut c_void,
            //     symbol: *const c_char,
            // ) -> *mut c_void;

            fn dup(
                fd: c_int,
            ) -> c_int;
            
            fn dup2(
                src: c_int,
                dst: c_int,
            ) -> c_int;

            fn epoll_create1(
                flags: c_int,
            ) -> c_int;

            // fn epoll_ctl(
            //     epfd: c_int,
            //     op: c_int,
            //     fd: c_int,
            //     event: *mut epoll_event,
            // ) -> c_int;

            // fn epoll_wait(
            //     epfd: c_int,
            //     events: *mut epoll_event,
            //     maxevents: c_int,
            //     timeout: c_int,
            // ) -> c_int;

            // fn execv(
            //     prog: *const c_char,
            //     argv: *const *const c_char,
            // ) -> c_int;

            // fn execve(
            //     prog: *const c_char,
            //     argv: *const *const c_char,
            //     envp: *const *const c_char,
            // ) -> c_int;

            fn exit(
                status: c_int,
            ) -> !;

            // fn explicit_bzero(
            //     s: *mut c_void,
            //     len: size_t,
            // ) -> ();

            fn fchdir(
                dirfd: c_int,
            ) -> c_int;

            // fn fcntl(
            //     fd: c_int,
            //     cmd: c_int,
            //     ...
            // ) -> c_int,

            // fn fflush(
            //     file: *mut FILE,
            // ) -> c_int;

            fn fork() -> pid_t;

            // fn fprintf(
            //     stream: *mut FILE,
            //     format: *const c_char,
            //     ...
            // ) -> c_int;

            // fn fputs(
            //     s: *const c_char,
            //     stream: *mut FILE,
            // ) -> c_int;

            // fn free(
            //     p: *mut c_void,
            // ) -> ();

            // fn freeaddrinfo(
            //     res: *mut addrinfo,
            // ) -> ();

            // fn fstat(
            //     fildes: c_int,
            //     buf: *mut stat,
            // ) -> c_int;

            fn ftruncate(
                fd: c_int,
                length: off_t,
            ) -> c_int;

            // fn gai_strerror(
            //     errcode: c_int,
            // ) -> *const c_char;

            // fn getaddrinfo(
            //     node: *const c_char,
            //     service: *const c_char,
            //     hints: *const addrinfo,
            //     res: *mut *mut addrinfo,
            // ) -> c_int;

            // fn getcwd(
            //     buf: *mut c_char,
            //     size: size_t,
            // ) -> *mut c_char;

            fn getegid() -> gid_t;

            // fn getentropy(
            //     buf: *mut c_void,
            //     buflen: size_t,
            // ) -> c_int;

            // fn getenv(
            //     s: *const c_char,
            // ) -> *mut c_char;

            fn geteuid() -> uid_t;

            fn getgid() -> gid_t;

            // fn getgrgid(
            //     gid: gid_t,
            // ) -> *mut group;

            // fn getgrnam(
            //     name: *const c_char,
            // ) -> *mut group;

            // fn getloadavg(
            //     loadavg: *mut c_double,
            //     nelem: c_int,
            // ) -> c_int;

            // fn getnameinfo(
            //     sa: *const sockaddr,
            //     salen: socklen_t,
            //     host: *mut c_char,
            //     hostlen: socklen_t,
            //     serv: *mut c_char,
            //     sevlen: socklen_t,
            //     flags: c_int,
            // ) -> c_int;

            // fn getopt(
            //     argc: c_int,
            //     argv: *const *mut c_char,
            //     optstr: *const c_char,
            // ) -> c_int;

            // fn getpeername(
            //     socket: c_int,
            //     address: *mut sockaddr,
            //     address_len: *mut socklen_t,
            // ) -> c_int;

            fn getpid() -> pid_t;

            fn getppid() -> pid_t;

            // fn getpwnam(
            //     name: *const c_char,
            // ) -> *mut passwd;

            // fn getrlimit(
            //     resource: __rlimit_resource_t,
            //     rlim: *mut rlimit,
            // ) -> c_int;

            // fn getsockname(
            //     socket: c_int,
            //     address: *mut sockaddr,
            //     address_len: *mut socklen_t,
            // ) -> c_int;

            // fn getsockopt(
            //     sockfd: c_int,
            //     level: c_int,
            //     optname: c_int,
            //     optval: *mut c_void,
            //     optlen: *mut socklen_t,
            // ) -> c_int;

            fn getuid() -> uid_t;

            // fn glob(
            //     pattern: *const c_char,
            //     flags: c_int,
            //     errfunc: Option<extern "C" fn(epath: *const c_char, errno: c_int) -> c_int>,
            //     pglob: *mut glob_t,
            // ) -> c_int;

            // fn globfree(
            //     pglob: *mut glob_t,
            // ) -> ();

            // fn gmtime_r(
            //     time_p: *const time_t,
            //     result: *mut tm,
            // ) -> *mut tm;

            // fn htonl;

            // fn htons;

            // fn inet_ntop;

            // fn inet_pton;

            // fn initgroups(
            //     user: *const c_char,
            //     group: gid_t,
            // ) -> c_int;

            // fn inotify_add_watch(
            //     fd: c_int,
            //     path: *const c_char,
            //     mask: u32,
            // ) -> c_int;

            fn inotify_init() -> c_int;

            fn inotify_init1(
                flags: c_int,
            ) -> c_int;

            fn inotify_rm_watch(
                fd: c_int,
                wd: c_int,
            ) -> c_int;

            // fn ioctl(
            //     fd: c_int,
            //     request: c_ulong,
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

            // fn localtime_r(
            //     time_p: *const time_t,
            //     result: *mut tm,
            // ) -> *mut tm;

            fn lseek(
                fd: c_int,
                offset: off_t,
                whence: c_int,
            ) -> off_t;

            // fn lstat(
            //     path: *const c_char,
            //     buf: *mut stat,
            // ) -> c_int;

            // fn malloc(
            //     size: size_t,
            // ) -> *mut c_void;

            // fn memchr(
            //     cx: *const c_void,
            //     c: c_int,
            //     n: size_t,
            // ) -> *mut c_void;

            // fn memcmp(
            //     cx: *const c_void,
            //     ct: *const c_void,
            //     n: size_t,
            // ) -> c_int;

            // fn memcpy(
            //     dest: *mut c_void,
            //     src: *const c_void,
            //     n: size_t,
            // ) -> *mut c_void;
            
            // TODO(kkysen) Note: A `&mut []` for `dest` and `&[]` for `src` would be UB since they can overlap.
            // fn memmove(
            //     dest: *mut c_void,
            //     src: *const c_void,
            //     n: size_t,
            // ) -> *mut c_void;

            // fn mempcpy;

            // fn memset(
            //     dest: *mut c_void,
            //     c: c_int,
            //     n: size_t,
            // ) -> *mut c_void;

            // fn mkostemp(
            //     template: *mut c_char,
            //     flags: c_int,
            // ) -> c_int;

            // fn mmap(
            //     addr: *mut c_void,
            //     len: size_t,
            //     prot: c_int,
            //     flags: c_int,
            //     fd: c_int,
            //     offset: off_t,
            // ) -> *mut c_void;

            // fn munmap(
            //     addr: *mut c_void,
            //     len: size_t,
            // ) -> c_int;

            // fn ntohs;

            // fn open(
            //     path: *const c_char,
            //     oflag: c_int,
            //     ...
            // ) -> c_int;

            // fn openlog(
            //     ident: *const c_char,
            //     logopt: c_int,
            //     facility: c_int,
            // ) -> ();

            // fn perror(
            //     s: *const c_char,
            // ) -> ();

            // fn pipe(
            //     fds: *mut c_int,
            // ) -> c_int;

            // fn pipe2(
            //     fds: *mut c_int,
            //     flags: c_int,
            // ) -> c_int;

            // fn poll(
            //     fds: *mut pollfd,
            //     nfds: nfds_t,
            //     timeout: c_int,
            // ) -> c_int;

            // fn prctl(
            //     option: c_int,
            //     ...
            // ) -> c_int;

            // fn pread(
            //     fd: c_int,
            //     buf: *mut c_void,
            //     count: size_t,
            //     offset: off_t,
            // ) -> ssize_t;

            // fn printf(
            //     format: *const c_char,
            //     ...
            // ) -> c_int;

            // fn puts(
            //     s: *const c_char,
            // ) -> c_int;

            // fn pwrite(
            //     fd: c_int,
            //     buf: *const c_void,
            //     count: size_t,
            //     offset: off_t,
            // ) -> ssize_t;

            // fn pwritev(
            //     fd: c_int,
            //     iov: *const iovec,
            //     iovcnt: c_int,
            //     offset: off_t,
            // ) -> ssize_t;

            fn raise(
                signum: c_int,
            ) -> c_int;

            fn rand() -> c_int;

            fn read(
                fd: c_int,
                buf: *mut c_void: [WRITE | OFFSET_ADD],
                count: size_t,
            ) -> ssize_t;

            // fn realloc(
            //     p: *mut c_void,
            //     size: size_t,
            // ) -> *mut c_void;

            // fn recv(
            //     socket: c_int,
            //     buf: *mut c_void,
            //     len: size_t,
            //     flags: c_int,
            // ) -> ssize_t;

            // fn rename(
            //     oldname: *const c_char,
            //     newname: *const c_char,
            // ) -> c_int;

            // fn select(
            //     nfds: c_int,
            //     readfds: *mut fd_set,
            //     writefds: *mut fd_set,
            //     errorfds: *mut fd_set,
            //     timeout: *mut timeval,
            // ) -> c_int;

            // fn sendfile(
            //     out_fd: c_int,
            //     in_fd: c_int,
            //     offset: *mut off_t,
            //     count: size_t,
            // ) -> ssize_t;

            // fn setenv(
            //     name: *const c_char,
            //     val: *const c_char,
            //     overwrite: c_int,
            // ) -> c_int;

            fn setgid(
                gid: gid_t,
            ) -> c_int;

            // fn setgroups(
            //     ngroups: size_t,
            //     ptr: *const gid_t,
            // ) -> c_int;

            // fn setlocale(
            //     category: c_int,
            //     locale: *const c_char,
            // ) -> *mut c_char;

            // fn setrlimit(
            //     resource: __rlimit_resource_t,
            //     rlim: *const rlimit,
            // ) -> c_int;

            fn setsid() -> pid_t;

            // fn setsockopt(
            //     socket: c_int,
            //     level: c_int,
            //     name: c_int,
            //     value: *const c_void,
            //     option_len: socklen_t,
            // ) -> c_int;

            fn setuid(
                uid: uid_t,
            ) -> c_int;

            fn shutdown(
                socket: c_int,
                how: c_int,
            ) -> c_int;

            // fn sigaction(
            //     signum: c_int,
            //     act: *const sigaction,
            //     oldact: *mut sigaction,
            // ) -> c_int;

            // fn sigemptyset(
            //     set: *mut sigset_t,
            // ) -> c_int;

            fn signal(
                signum: c_int,
                handler: sighandler_t,
            ) -> sighandler_t;

            // fn snprintf(
            //     s: *mut c_char,
            //     n: size_t,
            //     format: *const c_char,
            //     ...
            // ) -> c_int;

            fn socket(
                domain: c_int,
                ty: c_int,
                protocol: c_int,
            ) -> c_int;

            // fn splice(
            //     fd_in: c_int,
            //     off_in: *mut loff_t,
            //     fd_out: c_int,
            //     off_out: *mut loff_t,
            //     len: size_t,
            //     flags: c_uint,
            // ) -> ssize_t;

            fn srand(
                seed: c_uint,
            ) -> ();

            // fn srandom;

            // fn stat(
            //     path: *const c_char,
            //     buf: *mut stat,
            // ) -> c_int;

            // fn strchr(
            //     cs: *const c_char,
            //     c: c_int,
            // ) -> *mut c_char;

            // fn strcmp(
            //     cs: *const c_char,
            //     ct: *const c_char,
            // ) -> c_int;

            // fn strcspn(
            //     cs: *const c_char,
            //     ct: *const c_char,
            // ) -> size_t;

            // fn strdup(
            //     cs: *const c_char,
            // ) -> *mut c_char;

            // fn strerror_r(
            //     errnum: c_int,
            //     buf: *mut c_char,
            //     buflen: size_t,
            // ) -> c_int;

            // fn strftime(
            //     s: *mut c_char,
            //     max: size_t,
            //     format: *const c_char,
            //     tm: *const tm,
            // ) -> size_t;

            // fn strlen(
            //     cs: *const c_char,
            // ) -> size_t;

            // fn strncasecmp(
            //     s1: *const c_char,
            //     s2: *const c_char,
            //     n: size_t,
            // ) -> c_int;

            // fn strncmp(
            //     cs: *const c_char,
            //     ct: *const c_char,
            //     n: size_t,
            // ) -> c_int;

            // fn strrchr(
            //     cs: *const c_char,
            //     c: c_int,
            // ) -> *mut c_char;

            // fn strstr(
            //     cs: *const c_char,
            //     ct: *const c_char,
            // ) -> *mut c_char;

            fn strtol(
                s: *const c_char: [READ | OFFSET_ADD | NON_NULL],
                endp: *mut *mut c_char: [WRITE, WRITE | OFFSET_ADD],
                base: c_int,
            ) -> c_long;

            // fn strtoll(
            //     s: *const c_char,
            //     endp: *mut *mut c_char,
            //     base: c_int,
            // ) -> c_longlong;

            // fn strtoul(
            //     s: *const c_char,
            //     endp: *mut *mut c_char,
            //     base: c_int,
            // ) -> c_ulong;

            fn sysconf(
                name: c_int,
            ) -> c_long;

            // fn syslog(
            //     priority: c_int,
            //     message: *const c_char,
            //     ...
            // );

            // fn time(
            //     time: *mut time_t,
            // ) -> time_t;

            // fn timegm(
            //     tm: *mut tm,
            // ) -> time_t;

            // fn tzset;

            // fn unlink(
            //     c: *const c_char,
            // ) -> c_int;

            // fn unsetenv(
            //     name: *const c_char,
            // ) -> c_int;

            // fn vsnprintf;

            // fn waitpid(
            //     pid: pid_t,
            //     status: *mut c_int,
            //     options: c_int,
            // ) -> pid_t;

            fn write(
                fd: c_int,
                buf: *const c_void: [READ | OFFSET_ADD],
                count: size_t,
            ) -> ssize_t;

            // fn writev(
            //     fd: c_int,
            //     iov: *const iovec,
            //     iovcnt: c_int,
            // ) -> ssize_t;

        }
    }
}
