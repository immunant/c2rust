//! Provides a wrapper around `rustc::ty::Ty` with a label attached to each type constructor.  This
//!
//! Labeled type data is manipulated by reference, the same as with `Ty`s, and the data is stored
//! in the same arena as the underlying `Ty`s.
use std::fmt;
use std::marker::PhantomData;
use arena::DroplessArena;
use rustc::ty::{Ty, TypeVariants};

use type_map;


/// The actual data for a labeled type.
///
/// This struct shouldn't be constructed directly - instead, use `LabeledTyCtxt` methods to build
/// instances inside the tcx arena and return `LabeledTy` references.
///
/// Labeled types have to mimic the tree structure of the underlying `Ty`, so that each type
/// constructor in the tree can have its own label.  But maintaining a custom copy of
/// `TypeVariants` would be annoying, so instead, we let labeled types form arbitrary trees, and
/// make the `LabeledTyCtxt` responsible for making those trees match the `Ty`'s structure.
#[derive(Clone, PartialEq, Eq)]
pub struct LabeledTyS<'tcx, L: 'tcx> {
    /// The underlying type.
    pub ty: Ty<'tcx>,
    /// The arguments of this type constructor.  The number and meaning of these arguments depends
    /// on which type constructor this is (specifically, which `TypeVariants` variant is used for
    /// `self.ty.sty`).
    pub args: &'tcx [LabeledTy<'tcx, L>],
    /// The label for the current type constructor.
    pub label: L,
}

/// A labeled type.  Like `rustc::ty::Ty`, this is a reference to some arena-allocated data.
pub type LabeledTy<'tcx, L> = &'tcx LabeledTyS<'tcx, L>;

impl<'tcx, L: fmt::Debug> fmt::Debug for LabeledTyS<'tcx, L> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}#{:?}{:?}", self.label, self.ty, self.args)
    }
}

impl<'tcx, L> LabeledTyS<'tcx, L> {
    pub fn for_each_label<F: FnMut(&'tcx L)>(&'tcx self, callback: &mut F) {
        callback(&self.label);
        for &arg in self.args {
            arg.for_each_label(callback);
        }
    }
}


/// Context for constructing `LabeledTy`s.
pub struct LabeledTyCtxt<'tcx, L: 'tcx> {
    arena: &'tcx DroplessArena,
    _marker: PhantomData<L>,
}

impl<'tcx, L: Clone> LabeledTyCtxt<'tcx, L> {
    /// Build a new `LabeledTyCtxt`.  The `arena` must be the same one used by the `TyCtxt` that
    /// built the underlying `Ty`s to be labeled.
    pub fn new(arena: &'tcx DroplessArena) -> LabeledTyCtxt<'tcx, L> {
        LabeledTyCtxt {
            arena: arena,
            _marker: PhantomData,
        }
    }

    /// Manually construct a slice in the context's arena.
    pub fn mk_slice(&self, ltys: &[LabeledTy<'tcx, L>]) -> &'tcx [LabeledTy<'tcx, L>] {
        if ltys.len() == 0 {
            return &[];
        }
        self.arena.alloc_slice(ltys)
    }

    /// Manually construct a labeled type.  Note that this does not do any checks on `args`!  The
    /// caller is responsible for making sure the number of arguments matches `ty.sty`.
    pub fn mk(&self, ty: Ty<'tcx>, args: &'tcx [LabeledTy<'tcx, L>], label: L) -> LabeledTy<'tcx, L> {
        self.arena.alloc(LabeledTyS {
            ty: ty,
            args: args,
            label: label,
        })
    }


    /// Label a `Ty` using a callback.  The callback runs at every type constructor to produce a
    /// label for that node in the tree.
    pub fn label<F: FnMut(Ty<'tcx>) -> L>(&self, ty: Ty<'tcx>, f: &mut F) -> LabeledTy<'tcx, L> {
        use rustc::ty::TypeVariants::*;
        let label = f(ty);
        match ty.sty {
            // Types with no arguments
            TyBool |
            TyChar |
            TyInt(_) |
            TyUint(_) |
            TyFloat(_) |
            TyStr |
            TyNever => self.mk(ty, &[], label),

            // Types with arguments
            TyAdt(_, substs) => {
                let args = substs.types().map(|t| self.label(t, f)).collect::<Vec<_>>();
                self.mk(ty, self.mk_slice(&args), label)
            },
            TyArray(elem, _) => {
                let args = [self.label(elem, f)];
                self.mk(ty, self.mk_slice(&args), label)
            },
            TySlice(elem) => {
                let args = [self.label(elem, f)];
                self.mk(ty, self.mk_slice(&args), label)
            },
            TyRawPtr(mty) => {
                let args = [self.label(mty.ty, f)];
                self.mk(ty, self.mk_slice(&args), label)
            },
            TyRef(_, mty) => {
                let args = [self.label(mty.ty, f)];
                self.mk(ty, self.mk_slice(&args), label)
            },
            TyFnDef(_, substs) => {
                let args = substs.types().map(|ty| self.label(ty, f)).collect::<Vec<_>>();
                self.mk(ty, self.mk_slice(&args), label)
            },
            TyFnPtr(ref sig) => {
                let args = sig.0.inputs_and_output.iter()
                    .map(|ty| self.label(ty, f)).collect::<Vec<_>>();
                self.mk(ty, self.mk_slice(&args), label)
            },
            TyTuple(ref elems, _) => {
                let args = elems.iter().map(|ty| self.label(ty, f)).collect::<Vec<_>>();
                self.mk(ty, self.mk_slice(&args), label)
            },

            // Types that aren't actually supported by this code yet
            TyDynamic(..) |
            TyClosure(..) |
            TyGenerator(..) |
            TyProjection(..) |
            TyAnon(..) |
            TyParam(..) |
            TyInfer(..) |
            TyError => self.mk(ty, &[], label),
        }
    }

    /// Label multiple `Ty`s using a callback.
    pub fn label_slice<F>(&self,
                          tys: &[Ty<'tcx>],
                          f: &mut F) -> &'tcx [LabeledTy<'tcx, L>]
            where F: FnMut(Ty<'tcx>) -> L {
        self.mk_slice(&tys.iter().map(|ty| self.label(ty, f)).collect::<Vec<_>>())
    }


    /// Substitute in arguments for any type parameter references (`TyParam`) in a labeled type.
    /// Panics if `lty` contains a reference to a type parameter that is past the end of `substs`
    /// (usually this means the caller is providing the wrong list of type arguments as `substs`).
    ///
    /// TODO: This produces a `LabeledTy` with the right structure, but doesn't actually do
    /// substitution on the underlying `Ty`s!  This means if you substitute `u32` for `T`, you can
    /// end up with a `LabeledTy` whose `ty` is `S<T>`, but whose args are `[u32]`.  By some
    /// miracle, this hasn't broken anything yet, but we may need to fix it eventually.
    pub fn subst(&self,
                 lty: LabeledTy<'tcx, L>,
                 substs: &[LabeledTy<'tcx, L>]) -> LabeledTy<'tcx, L> {
        match lty.ty.sty {
            TypeVariants::TyParam(ref tp) => {
                substs[tp.idx as usize]
            },
            _ => self.mk(lty.ty, self.subst_slice(lty.args, substs), lty.label.clone()),
        }
    }

    /// Substitute arguments in multiple labeled types.
    pub fn subst_slice(&self,
                       ltys: &[LabeledTy<'tcx, L>],
                       substs: &[LabeledTy<'tcx, L>]) -> &'tcx [LabeledTy<'tcx, L>] {
        self.mk_slice(&ltys.iter().map(|lty| self.subst(lty, substs)).collect::<Vec<_>>())
    }


    /// Run a callback to replace the labels on a type.
    pub fn relabel<L2, F>(&self, lty: LabeledTy<'tcx, L2>, func: &mut F) -> LabeledTy<'tcx, L>
            where F: FnMut(&L2) -> L {
        let args = self.relabel_slice(lty.args, func);
        self.mk(lty.ty, args, func(&lty.label))
    }

    /// Replace the labels on several labeled types.
    pub fn relabel_slice<L2, F>(&self,
                                ltys: &'tcx [LabeledTy<'tcx, L2>],
                                func: &mut F) -> &'tcx [LabeledTy<'tcx, L>]
            where F: FnMut(&L2) -> L {
        let ltys = ltys.iter().cloned().map(|lty| self.relabel(lty, func)).collect::<Vec<_>>();
        self.mk_slice(&ltys)
    }
}


impl<'tcx, L: fmt::Debug> type_map::Type for LabeledTy<'tcx, L> {
    fn sty(&self) -> &TypeVariants {
        &self.ty.sty
    }

    fn num_args(&self) -> usize {
        self.args.len()
    }

    fn arg(&self, idx: usize) -> Self {
        self.args[idx]
    }
}
