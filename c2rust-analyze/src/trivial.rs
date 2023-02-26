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
        match self.unsafety {
            Unsafety::Unsafe => are_all_trivial(tcx, self.inputs_and_output.iter().map(UnsafeTy)),
            Unsafety::Normal => are_all_trivial(tcx, self.inputs_and_output.iter().map(SafeTy)),
        }
    }
}

/// A [`Ty`] from a [safe](Unsafety::Normal) function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SafeTy<'tcx>(Ty<'tcx>);

impl<'tcx> IsTrivial<'tcx> for SafeTy<'tcx> {
    /// A [`SafeTy`] [`is_trivial`] if
    /// itself and its [`GenericArg`]s are not themselves (directly) a (raw) pointer.
    ///
    /// [`is_trivial`]: IsTrivial::is_trivial
    /// [`GenericArg`]: ty::GenericArg
    fn is_trivial(&self, _tcx: TyCtxt<'tcx>) -> bool {
        // Note that [`Ty::walk`] already handles caching [`Ty`]s.
        self.0
            .walk()
            .filter_map(|generic| match generic.unpack() {
                GenericArgKind::Type(ty) => Some(ty),
                _ => None,
            })
            .all(|ty| !ty.is_unsafe_ptr())
    }
}

/// A [`Ty`] from an [`unsafe`](Unsafety::Unsafe) function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct UnsafeTy<'tcx>(Ty<'tcx>);

impl<'tcx> IsTrivial<'tcx> for UnsafeTy<'tcx> {
    /// An [`UnsafeTy`] [`is_trivial`] if
    /// it is not a (raw) pointer and contains (recursively) no (raw) pointers.
    ///
    /// [`is_trivial`]: IsTrivial::is_trivial
    fn is_trivial(&self, tcx: TyCtxt<'tcx>) -> bool {
        let not_sure_yet = |is_trivial: bool| {
            let kind = self.0.kind();
            eprintln!("assuming non-trivial for now as a safe backup (guessed {is_trivial:?}): ty.kind() = {kind:?}, ty = {self:?}");
            false
        };

        match *self.0.kind() {
            ty::RawPtr(..) => false, // raw pointers are never trivial, can break out early

            ty::Bool | ty::Char | ty::Str | ty::Int(..) | ty::Uint(..) | ty::Float(..) => true, // primitive types are trivial

            ty::Never => true,

            ty::Foreign(..) => false, // no introspection into a foreign, extern type, but as it's extern, it likely contains raw pointers

            // delegate to the inner type
            ty::Ref(_, ty, _) | ty::Slice(ty) | ty::Array(ty, _) => UnsafeTy(ty).is_trivial(tcx),

            // delegate to all inner types
            ty::Tuple(tys) => are_all_trivial(tcx, tys.iter().map(UnsafeTy)),
            ty::Adt(adt_def, substs) => are_all_trivial(
                tcx,
                adt_def
                    .all_fields()
                    .map(|field| field.ty(tcx, substs))
                    .map(UnsafeTy),
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
            ty::FnPtr(..) | ty::FnDef(..) => not_sure_yet(true),

            // the function part of the closure should be trivial,
            // so just check the enclosed type (upvars) for triviality,
            // but for now, assume they're non-trivial as a safe backup
            ty::Closure(_, substs) => {
                not_sure_yet(UnsafeTy(substs.as_closure().tupled_upvars_ty()).is_trivial(tcx))
            }

            // similar to closures, check all possible types created by the generator
            ty::Generator(_, substs, _) => not_sure_yet({
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
                    .map(UnsafeTy),
                )
            }),

            // try to get the actual type and delegate to it
            ty::Opaque(did, substs) => not_sure_yet(
                UnsafeTy(EarlyBinder(tcx.type_of(did)).subst(tcx, substs)).is_trivial(tcx),
            ),

            // not sure how to handle yet, and may never come up anyways
            ty::GeneratorWitness(..)
            | ty::Projection(..)
            | ty::Error(_)
            | ty::Infer(_)
            | ty::Placeholder(..)
            | ty::Bound(..)
            | ty::Param(..) => not_sure_yet(false),
        }
    }
}
