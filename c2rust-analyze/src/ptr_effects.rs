use rustc_hir::Unsafety;
use rustc_middle::ty::{self, FnSig, GenericArgKind, Ty, TyCtxt};

/// Assume that no `trait` `impl`s from the local crate operate on raw pointers
/// and thus potentially expose local [`PointerId`]s
/// to generic functions from other crates that use these `trait`s.
///
/// If this is not true, then we have to assume that such generic functions
/// can operate on any raw pointers passed in (e.x. through arguments, and [`Deep`]ly)
/// as well as any global pointers and pointers to globals.
///
/// This would cause all those pointers to be heavily constrained (see [`PermissionSet::pessimistic`]),
/// which would then infect a ton of other pointers and make static analysis very ineffective.
///
/// [`PointerId`]: crate::pointer_id::PointerId
/// [`Deep`]: PtrEffects::Deep
/// [`PermissionSet::pessimistic`]: crate::context::PermissionSet::pessimistic
pub fn assume_no_impls_using_raw_ptrs() -> bool {
    // TODO make this into an argument of sorts
    true
}

/// Assume that no `extern` functions access
/// other functions and globals through FFI in a sort of reverse dependency.
///
/// Doing so would mean that they can operate on any raw pointers through globals,
/// which would cause all those pointers to be heavily constrained (see [`PermissionSet::pessimistic`]),
/// which would then infect a ton of other pointers and make static analysis very ineffective.
///
/// [`PermissionSet::pessimistic`]: crate::context::PermissionSet::pessimistic
pub fn assume_no_reverse_ffi_dependencies() -> bool {
    // TODO make this into an argument of sorts
    true
}

/// The scope of effects on pointer permissions a function call can have.
#[derive(Debug)]
pub enum PtrEffects {
    /// There are no effects on pointer permissions.
    ///
    /// That is, the function called cannot access any pointers in the local crate,
    /// and thus has no effect on any pointer permissions,
    /// a.k.a. it is trivial.
    None,

    /// There are effects on pointer permissions of pointers directly passed.
    ///
    /// That is, arguments and return types that are directly passed are accessible.
    Shallow,

    /// There are effects on the pointer permissions of pointers that are part of the data passed.
    ///
    /// That is, any pointers within the data passed (argument and return types) are accessible.
    ///
    /// This is [`Self::Shallow`] applied recursively into types.
    Deep,

    /// There are effects on the pointer permissions of pointers
    /// that are part of the data passed and all pointers to and through globals.
    ///
    /// This is [`Self::Deep`] plus the globals.
    All,
}

impl PtrEffects {
    /// Determine the [`PtrEffects`] of a call to a function with [`FnSig`].
    ///
    /// The way this is calculated is:
    ///
    /// 1. If the [`FnSig`] is [`unsafe`], then we assume that
    ///    the function may be `extern` as well, i.e. it has a [`DefKind::ForeignMod`].
    ///    The reason why we can't check this directly is that for a [`FnPtr`],
    ///    we don't know the [`FnDef`] definition, which we could use to check if it's `extern`.
    ///    We also can't just check the [`Abi`], as that doesn't necessary correlate with being `extern`.
    ///
    /// 2. If the [`FnSig`] is [`unsafe`] or `extern`,
    ///    then if we [`assume_no_reverse_ffi_dependencies`],
    ///    we return [`Self::Deep`], as `unsafe` casts/transmutes and FFI
    ///    can be used to access any pointers within the data passed.
    ///    Otherwise, we return [`Self::All`],
    ///    as other functions and globals can be accessed through FFI.
    ///
    /// 3. If any argument and return types are directly raw pointers,
    ///    then we return [`Self::Shallow`].
    ///    We don't need to check this recursively at all,
    ///    because another function call (or aggregate initialization)
    ///    would've handled the raw pointer directly.
    ///
    /// 4. Otherwise, we return [`Self::None`], as the function is trivial
    ///    and has no effects on pointer permissions.
    ///
    /// [`unsafe`]: Unsafety::Unsafe
    /// [`DefKind::ForeignMod`]: rustc_hir::def::DefKind::ForeignMod
    /// [`FnPtr`]: ty::FnPtr
    /// [`FnDef`]: ty::FnDef
    /// [`Abi`]: rustc_target::spec::abi::Abi
    pub fn of_fn_sig(fn_sig: FnSig) -> Self {
        let is_unsafe = fn_sig.unsafety == Unsafety::Unsafe;
        let has_direct_raw_ptr = || fn_sig.inputs_and_output.iter().any(|ty| ty.is_unsafe_ptr());
        if is_unsafe {
            if assume_no_reverse_ffi_dependencies() {
                Self::Deep
            } else {
                Self::All
            }
        } else if has_direct_raw_ptr() {
            Self::Shallow
        } else {
            Self::None
        }
    }

    /// Determine the [`PtrEffects`] of a call to `ty`.
    ///
    /// The way this is calculated is:
    ///
    /// 1. Unless [`assume_no_impls_using_raw_ptrs`] is on,
    ///    we check if any of the generic type parameters are a local [`Ty`] (in the local crate).
    ///    If any are, then we return [`Self::All`],
    ///    as that generic [`Ty`] from the local crate can be used to expose any local pointers.
    ///
    /// 2. We then defer to [`Self::of_fn_sig`].
    pub fn of_call<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Self {
        let substs = match *ty.kind() {
            ty::FnDef(_, substs) => &**substs,
            _ => &[][..],
        };
        let has_local_ty = || {
            substs
                .iter()
                .filter_map(|generic| match generic.unpack() {
                    GenericArgKind::Type(ty) => Some(ty),
                    _ => None,
                })
                .any(is_ty_local)
        };
        if !assume_no_impls_using_raw_ptrs() && has_local_ty() {
            Self::All
        } else {
            // We don't care about lifetimes, so `.skip_binder()` is fine.
            let fn_sig = ty.fn_sig(tcx).skip_binder();
            Self::of_fn_sig(fn_sig)
        }
    }
}

fn is_ty_local(ty: Ty) -> bool {
    match ty.kind() {
        // Primitives are never local.
        ty::Bool | ty::Char | ty::Int(_) | ty::Uint(_) | ty::Float(_) | ty::Str | ty::Never => {
            false
        }

        // Generic primitives may contain local types, but are not themselves local.
        ty::Array(_, _) | ty::Slice(_) | ty::RawPtr(_) | ty::Ref(_, _, _) | ty::Tuple(_) => false,

        // These have [`DefId`]s, so we can check directly.
        ty::Adt(adt_def, _) => adt_def.did().is_local(),
        ty::Foreign(did)
        | ty::FnDef(did, _)
        | ty::Closure(did, _)
        | ty::Generator(did, _, _)
        | ty::Opaque(did, _) => did.is_local(),

        // These could have multiple definitions, so we have to assume it's local.
        ty::FnPtr(_) | ty::Dynamic(_, _) => true,

        ty::GeneratorWitness(_)
        | ty::Projection(_)
        | ty::Param(_)
        | ty::Bound(_, _)
        | ty::Placeholder(_)
        | ty::Infer(_)
        | ty::Error(_) => panic!("unexpected Ty in is_ty_local: {ty:?}"),
    }
}
