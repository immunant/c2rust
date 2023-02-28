use rustc_hir::Unsafety;
use rustc_middle::ty::{
    self, Binder, EarlyBinder, FnSig, GenSig, GenericArgKind, Subst, Ty, TyCtxt,
};

pub trait IsTrivial<'tcx> {
    /// Something [`is_trivial`] if it has no effect on pointer permissions,
    /// and thus requires no special handling.
    /// That is, it must contain no raw pointers.
    ///
    /// See [`Trivial`] for more info.
    ///
    /// [`is_trivial`]: Self::is_trivial
    /// [`Trivial`]: crate::util::Callee::Trivial
    fn is_trivial(&self, tcx: TyCtxt<'tcx>) -> bool;
}

impl<'tcx, T: IsTrivial<'tcx>> IsTrivial<'tcx> for &T {
    fn is_trivial(&self, tcx: TyCtxt<'tcx>) -> bool {
        (*self).is_trivial(tcx)
    }
}

fn are_all_trivial<'tcx, T, I>(tcx: TyCtxt<'tcx>, ts: I) -> bool
where
    I: IntoIterator<Item = T>,
    T: IsTrivial<'tcx>,
{
    ts.into_iter().all(|t| t.is_trivial(tcx))
}

impl<'tcx> IsTrivial<'tcx> for Binder<'tcx, FnSig<'tcx>> {
    /// A [`Binder`]`<`[`FnSig`]`>` [`is_trivial`] if
    /// the [`FnSig`] without the [`Binder`] is trivial.
    ///
    /// Lifetimes do not affect [`Trivial`]ity,
    /// so we can ignore higher-kinded lifetimes from the [`Binder`].
    ///
    /// [`is_trivial`]: IsTrivial::is_trivial
    /// [`Trivial`]: crate::util::Callee::Trivial
    fn is_trivial(&self, tcx: TyCtxt<'tcx>) -> bool {
        // We don't care about higher-kinded lifetimes here, as we don't care about lifetimes at all.
        self.skip_binder().is_trivial(tcx)
    }
}

impl<'tcx> IsTrivial<'tcx> for FnSig<'tcx> {
    /// A [`FnSig`] [`is_trivial`] if
    /// all of its argument and return types are [`Trivial`].
    ///
    /// [`is_trivial`]: IsTrivial::is_trivial
    /// [`Trivial`]: crate::util::Callee::Trivial
    fn is_trivial(&self, tcx: TyCtxt<'tcx>) -> bool {
        let Self {
            inputs_and_output,
            c_variadic: _,
            unsafety,
            abi: _,
        } = *self;
        inputs_and_output
            .iter()
            .map(|ty| TyWithSafety { ty, unsafety })
            .all(|ty| ty.is_trivial(tcx))
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

/// A [`Ty`] with it's calling function's [`Unsafety`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct TyWithSafety<'tcx> {
    ty: Ty<'tcx>,
    unsafety: Unsafety,
}

impl<'tcx> TyWithSafety<'tcx> {
    fn not_sure_yet(&self, is_trivial: bool) -> bool {
        let Self { ty, unsafety } = *self;
        let kind = ty.kind();
        eprintln!("assuming non-trivial for now as a safe backup (guessed {is_trivial:?}): ty.kind() = {kind:?}, ty = {ty:?}, unsafety = {unsafety}");
        false
    }

    /// Determine if a [`Ty`]s triviality should be determined deeply,
    /// i.e. using [`Self::deep_is_trivial`],
    /// or using [`Self::shallow_is_trivial`].
    ///
    /// A [`Ty`] [is_deep] if it is
    /// * from an [`unsafe`] function call, or
    /// * local to the current crate
    ///
    /// [is_deep]: Self::is_deep
    /// [`unsafe`]: Unsafety::Unsafe
    fn is_deep(&self) -> bool {
        let Self { ty, unsafety } = *self;
        unsafety == Unsafety::Unsafe || is_ty_local(ty)
    }

    /// A shallow [`Ty`] [`is_trivial`] if
    /// it's not itself a raw pointer,
    /// and all of its [`GenericArg`] [`Ty`]s are trivial.
    ///
    /// Note that the [`GenericArg`] [`Ty`]s might not also be shallow.
    ///
    /// [`is_trivial`]: IsTrivial::is_trivial
    /// [`GenericArg`]: ty::GenericArg
    fn shallow_is_trivial(&self, tcx: TyCtxt<'tcx>) -> bool {
        let Self { ty, unsafety } = *self;
        // Note that [`Ty::walk`] already handles caching [`Ty`]s.
        !ty.is_unsafe_ptr()
            && ty
                .walk()
                .filter_map(|generic| match generic.unpack() {
                    GenericArgKind::Type(ty) => Some(ty),
                    _ => None,
                })
                .filter(|inner_ty| *inner_ty != ty) // don't infinitely recurse
                .map(|ty| Self { ty, unsafety })
                .all(|ty| ty.is_trivial(tcx))
    }

    /// A deep [`Ty`] [`is_trivial`] if
    /// it's not itself a raw pointer,
    /// and all of its inner [`Ty`]s are trivial,
    /// i.e. if they do not contain any raw pointers internally.
    ///
    /// Note that the inner [`Ty`]s might not also be deep.
    ///
    /// [`is_trivial`]: IsTrivial::is_trivial
    fn deep_is_trivial(&self, tcx: TyCtxt<'tcx>) -> bool {
        let Self { ty, unsafety } = *self;
        let inner_ty = |ty: Ty<'tcx>| Self { ty, unsafety };

        // TODO need to handle self-referential (through refs) types, which otherwise cause infinite recursion
        match *ty.kind() {
            ty::RawPtr(..) => false, // raw pointers are never trivial, can break out early

            ty::Bool | ty::Char | ty::Str | ty::Int(..) | ty::Uint(..) | ty::Float(..) => true, // primitive types are trivial

            ty::Never => true,

            ty::Foreign(..) => false, // no introspection into a foreign, extern type, but as it's extern, it likely contains raw pointers

            // delegate to the inner type
            ty::Ref(_, ty, _) | ty::Slice(ty) | ty::Array(ty, _) => inner_ty(ty).is_trivial(tcx),

            // delegate to all inner types
            ty::Tuple(tys) => are_all_trivial(tcx, tys.iter().map(inner_ty)),
            ty::Adt(adt_def, substs) => are_all_trivial(
                tcx,
                adt_def
                    .all_fields()
                    .map(|field| field.ty(tcx, substs))
                    .map(inner_ty),
            ),

            // don't know, as `dyn Trait` could be anything
            ty::Dynamic(trait_ty, _reg) => {
                eprintln!("unsure how to check `dyn Trait` for accessible pointers, so assuming non-trivial: ty = {self:?}, trait_ty = {trait_ty:?}");
                false
            }

            // For the below [`TyKind`]s, we're not yet certain about their triviality,
            // so they use `not_sure_yet` to return `false` as a safe backup,
            // along with printing a message about this assumption,
            // which includes a potential guess.
            // This allows us to keep the potentially correct code for these,
            // even if we haven't verified yet if they're implemented correctly.

            // function ptrs/defs carry no data, so they should be trivial
            // but for now, assume they're non-trivial as a safe backup
            ty::FnPtr(..) | ty::FnDef(..) => self.not_sure_yet(true),

            // the function part of the closure should be trivial,
            // so just check the enclosed type (upvars) for triviality,
            // but for now, assume they're non-trivial as a safe backup
            ty::Closure(_, substs) => {
                self.not_sure_yet(inner_ty(substs.as_closure().tupled_upvars_ty()).is_trivial(tcx))
            }

            // similar to closures, check all possible types created by the generator
            ty::Generator(_, substs, _) => self.not_sure_yet({
                let generator = substs.as_generator();
                let GenSig {
                    resume_ty,
                    yield_ty,
                    return_ty,
                } = generator.sig();
                are_all_trivial(
                    tcx,
                    [
                        generator.tupled_upvars_ty(),
                        resume_ty,
                        yield_ty,
                        return_ty,
                        generator.witness(),
                    ]
                    .map(inner_ty),
                )
            }),

            // try to get the actual type and delegate to it
            ty::Opaque(did, substs) => self.not_sure_yet(
                inner_ty(EarlyBinder(tcx.type_of(did)).subst(tcx, substs)).is_trivial(tcx),
            ),

            // not sure how to handle yet, and may never come up anyways
            ty::GeneratorWitness(..)
            | ty::Projection(..)
            | ty::Error(_)
            | ty::Infer(_)
            | ty::Placeholder(..)
            | ty::Bound(..)
            | ty::Param(..) => self.not_sure_yet(false),
        }
    }
}

impl<'tcx> IsTrivial<'tcx> for TyWithSafety<'tcx> {
    /// A [`TyWithSafety`] [`is_trivial`] depending on if
    /// the [`Ty`] is [shallow] or [deep].
    /// That is, the [`Ty`] [is_deep] if it is
    /// * from an [`unsafe`] function call, or
    /// * local to the current crate
    ///
    /// See
    /// * [Self::is_deep]
    /// * [Self::shallow_is_trivial]
    /// * [Self::deep_is_trivial]
    ///
    /// Note that we ignore the possibility that a function may perform
    /// int-to-ptr casts (a la [`std::ptr::from_exposed_addr`]) internally,
    /// as handling such casts is very difficult and out of scope for now.
    ///
    /// References are allowed, because the existence of that reference in the first place
    /// carries much stronger semantics, so in the case that the reference is casted to a raw pointer,
    /// we can simply use the pointer permissions guaranteed by that reference.
    ///
    /// [`is_trivial`]: IsTrivial::is_trivial
    /// [is_deep]: Self::is_deep
    /// [shallow]: Self::shallow_is_trivial
    /// [deep]: Self::deep_is_trivial
    /// [`unsafe`]: Unsafety::Unsafe
    fn is_trivial(&self, tcx: TyCtxt<'tcx>) -> bool {
        if self.is_deep() {
            self.deep_is_trivial(tcx)
        } else {
            self.shallow_is_trivial(tcx)
        }
    }
}
