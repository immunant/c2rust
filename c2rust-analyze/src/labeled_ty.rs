//! Provides a wrapper around `rustc::ty::Ty` with a label attached to each type constructor.  We
//! use this, for example, to attach a Polonius `Origin` to every reference type.  Labeled type
//! data is manipulated by reference, the same as with `Ty`s, and the data is stored in the same
//! arena as the underlying `Ty`s.
use rustc_arena::DroplessArena;
use rustc_middle::ty::subst::{GenericArg, GenericArgKind};
use rustc_middle::ty::{Ty, TyCtxt, TyKind, TypeAndMut};
use std::convert::TryInto;
use std::fmt;
use std::marker::PhantomData;
use std::ops::Deref;
use std::slice;

/// The actual data for a labeled type.
///
/// This struct shouldn't be constructed directly - instead, use `LabeledTyCtxt` methods to build
/// instances inside the tcx arena and return `LabeledTy` references.
///
/// Labeled types have to mimic the tree structure of the underlying `Ty`, so that each type
/// constructor in the tree can have its own label.  But maintaining a custom copy of
/// `TyKind` would be annoying, so instead, we let labeled types form arbitrary trees, and
/// make the `LabeledTyCtxt` responsible for making those trees match the `Ty`'s structure.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct LabeledTyS<'tcx, L: 'tcx> {
    /// The underlying type.
    pub ty: Ty<'tcx>,
    /// The arguments of this type constructor.  The number and meaning of these arguments depends
    /// on which type constructor this is (specifically, which `TyKind` variant is used for
    /// `self.ty.sty`).
    pub args: &'tcx [LabeledTy<'tcx, L>],
    /// The label for the current type constructor.
    pub label: L,
}

impl<'tcx, L> LabeledTyS<'tcx, L> {
    pub fn kind(&self) -> &'tcx TyKind<'tcx> {
        self.ty.kind()
    }
}

/// A labeled type.  Like `rustc::ty::Ty`, this is a reference to some arena-allocated data.
pub type LabeledTy<'tcx, L> = &'tcx LabeledTyS<'tcx, L>;

impl<'tcx, L: fmt::Debug> fmt::Debug for LabeledTyS<'tcx, L> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}#{:?}{:?}", self.label, self.ty, self.args)
    }
}

impl<'tcx, L: Copy> LabeledTyS<'tcx, L> {
    pub fn for_each_label<F: FnMut(L)>(&'tcx self, callback: &mut F) {
        callback(self.label);
        for &arg in self.args {
            arg.for_each_label(callback);
        }
    }
}

impl<'tcx, L> LabeledTyS<'tcx, L> {
    pub fn iter(&'tcx self) -> LabeledTyIter<'tcx, L> {
        LabeledTyIter::new(self)
    }
}

pub struct LabeledTyIter<'tcx, L> {
    root: Option<LabeledTy<'tcx, L>>,
    stack: Vec<slice::Iter<'tcx, LabeledTy<'tcx, L>>>,
}

impl<'tcx, L> LabeledTyIter<'tcx, L> {
    fn new(lty: LabeledTy<'tcx, L>) -> LabeledTyIter<'tcx, L> {
        LabeledTyIter {
            root: Some(lty),
            stack: Vec::new(),
        }
    }
}

impl<'tcx, L> Iterator for LabeledTyIter<'tcx, L> {
    type Item = LabeledTy<'tcx, L>;

    fn next(&mut self) -> Option<LabeledTy<'tcx, L>> {
        if let Some(lty) = self.root.take() {
            if !lty.args.is_empty() {
                self.stack.push(lty.args.iter());
            }
            return Some(lty);
        }

        loop {
            match self.stack.last_mut()?.next() {
                Some(lty) => {
                    if !lty.args.is_empty() {
                        self.stack.push(lty.args.iter());
                    }
                    return Some(lty);
                }
                None => {
                    self.stack.pop();
                }
            }
        }
    }
}

/// Context for constructing `LabeledTy`s.
pub struct LabeledTyCtxt<'tcx, L: 'tcx> {
    tcx: TyCtxt<'tcx>,
    _marker: PhantomData<L>,
}

impl<'tcx, L> Clone for LabeledTyCtxt<'tcx, L> {
    fn clone(&self) -> LabeledTyCtxt<'tcx, L> {
        *self
    }
}

impl<'tcx, L> Copy for LabeledTyCtxt<'tcx, L> {}

impl<'tcx, L: Copy> LabeledTyCtxt<'tcx, L> {
    /// Build a new `LabeledTyCtxt`.  The `arena` must be the same one used by the `TyCtxt` that
    /// built the underlying `Ty`s to be labeled.
    pub fn new(tcx: TyCtxt<'tcx>) -> LabeledTyCtxt<'tcx, L> {
        LabeledTyCtxt {
            tcx,
            _marker: PhantomData,
        }
    }

    pub fn arena(&self) -> &'tcx DroplessArena {
        &self.tcx.arena.dropless
    }

    /// Manually construct a slice in the context's arena.
    pub fn mk_slice(&self, ltys: &[LabeledTy<'tcx, L>]) -> &'tcx [LabeledTy<'tcx, L>] {
        if ltys.is_empty() {
            return &[];
        }
        self.arena().alloc_slice(ltys)
    }

    /// Manually construct a labeled type.  Note that this does not do any checks on `args`!  The
    /// caller is responsible for making sure the number of arguments matches `ty.sty`.
    pub fn mk(
        &self,
        ty: Ty<'tcx>,
        args: &'tcx [LabeledTy<'tcx, L>],
        label: L,
    ) -> LabeledTy<'tcx, L> {
        self.arena().alloc(LabeledTyS { ty, args, label })
    }

    /// Label a `Ty` using a callback.  The callback runs at every type constructor to produce a
    /// label for that node in the tree.
    pub fn label<F: FnMut(Ty<'tcx>) -> L>(&self, ty: Ty<'tcx>, f: &mut F) -> LabeledTy<'tcx, L> {
        use rustc_type_ir::TyKind::*;
        let label = f(ty);
        match ty.kind() {
            // Types with no arguments
            Bool | Char | Int(_) | Uint(_) | Float(_) | Str | Foreign(_) | Never => {
                self.mk(ty, &[], label)
            }

            // Types with arguments
            Adt(_, substs) => {
                let args = substs.types().map(|t| self.label(t, f)).collect::<Vec<_>>();
                self.mk(ty, self.mk_slice(&args), label)
            }
            &Array(elem, _) => {
                let args = [self.label(elem, f)];
                self.mk(ty, self.mk_slice(&args), label)
            }
            &Slice(elem) => {
                let args = [self.label(elem, f)];
                self.mk(ty, self.mk_slice(&args), label)
            }
            RawPtr(mty) => {
                let args = [self.label(mty.ty, f)];
                self.mk(ty, self.mk_slice(&args), label)
            }
            &Ref(_, mty, _) => {
                let args = [self.label(mty, f)];
                self.mk(ty, self.mk_slice(&args), label)
            }
            FnDef(_, substs) => {
                let args = substs
                    .types()
                    .map(|ty| self.label(ty, f))
                    .collect::<Vec<_>>();
                self.mk(ty, self.mk_slice(&args), label)
            }
            FnPtr(ref sig) => {
                let args = sig
                    .skip_binder()
                    .inputs_and_output
                    .iter()
                    .map(|ty| self.label(ty, f))
                    .collect::<Vec<_>>();
                self.mk(ty, self.mk_slice(&args), label)
            }
            Tuple(elems) => {
                let args = elems.iter().map(|ty| self.label(ty, f)).collect::<Vec<_>>();
                self.mk(ty, self.mk_slice(&args), label)
            }

            // Types that aren't actually supported by this code yet
            Dynamic(..) | Closure(..) | Generator(..) | GeneratorWitness(..) | Projection(..)
            | Opaque(..) | Param(..) | Bound(..) | Placeholder(..) | Infer(..) | Error(..) => {
                self.mk(ty, &[], label)
            }
        }
    }

    /// Label multiple `Ty`s using a callback.
    #[allow(dead_code)]
    pub fn _label_slice<F>(&self, tys: &[Ty<'tcx>], f: &mut F) -> &'tcx [LabeledTy<'tcx, L>]
    where
        F: FnMut(Ty<'tcx>) -> L,
    {
        self.mk_slice(&tys.iter().map(|&ty| self.label(ty, f)).collect::<Vec<_>>())
    }

    /// Substitute in arguments for any type parameter references (`Param`) in a labeled type.
    /// Panics if `lty` contains a reference to a type parameter that is past the end of `substs`
    /// (usually this means the caller is providing the wrong list of type arguments as `substs`).
    ///
    /// TODO: This produces a `LabeledTy` with the right structure, but doesn't actually do
    /// substitution on the underlying `Ty`s!  This means if you substitute `u32` for `T`, you can
    /// end up with a `LabeledTy` whose `ty` is `S<T>`, but whose args are `[u32]`.  By some
    /// miracle, this hasn't broken anything yet, but we may need to fix it eventually.
    #[allow(dead_code)]
    pub fn subst(
        &self,
        lty: LabeledTy<'tcx, L>,
        substs: &[LabeledTy<'tcx, L>],
    ) -> LabeledTy<'tcx, L> {
        if let TyKind::Param(ref ty) = lty.ty.kind() {
            if let Some(p) = substs.get(ty.index as usize) {
                return p;
            }
        }

        self.mk(lty.ty, self.subst_slice(lty.args, substs), lty.label)
    }

    /// Substitute arguments in multiple labeled types.
    pub fn subst_slice(
        &self,
        ltys: &[LabeledTy<'tcx, L>],
        substs: &[LabeledTy<'tcx, L>],
    ) -> &'tcx [LabeledTy<'tcx, L>] {
        self.mk_slice(
            &ltys
                .iter()
                .map(|lty| self.subst(lty, substs))
                .collect::<Vec<_>>(),
        )
    }

    /// Run a callback to replace the labels on a type.
    pub fn relabel<L2, F>(&self, lty: LabeledTy<'tcx, L2>, func: &mut F) -> LabeledTy<'tcx, L>
    where
        F: FnMut(LabeledTy<'tcx, L2>) -> L,
    {
        self.relabel_with_args(lty, &mut |lty, _| func(lty))
    }

    /// Replace the labels on several labeled types.
    pub fn _relabel_slice<L2, F>(
        &self,
        ltys: &[LabeledTy<'tcx, L2>],
        func: &mut F,
    ) -> &'tcx [LabeledTy<'tcx, L>]
    where
        F: FnMut(LabeledTy<'tcx, L2>) -> L,
    {
        self.relabel_slice_with_args(ltys, &mut |lty, _| func(lty))
    }

    /// Run a callback to replace the labels on a type.
    pub fn relabel_with_args<L2, F>(
        &self,
        lty: LabeledTy<'tcx, L2>,
        func: &mut F,
    ) -> LabeledTy<'tcx, L>
    where
        F: FnMut(LabeledTy<'tcx, L2>, &'tcx [LabeledTy<'tcx, L>]) -> L,
    {
        let args = self.relabel_slice_with_args(lty.args, func);
        self.mk(lty.ty, args, func(lty, args))
    }

    /// Replace the labels on several labeled types.
    pub fn relabel_slice_with_args<L2, F>(
        &self,
        ltys: &[LabeledTy<'tcx, L2>],
        func: &mut F,
    ) -> &'tcx [LabeledTy<'tcx, L>]
    where
        F: FnMut(LabeledTy<'tcx, L2>, &'tcx [LabeledTy<'tcx, L>]) -> L,
    {
        let ltys = ltys
            .iter()
            .cloned()
            .map(|lty| self.relabel_with_args(lty, func))
            .collect::<Vec<_>>();
        self.mk_slice(&ltys)
    }

    /// Perform a bottom-up rewrite on a type and convert it to unlabeled form.
    pub fn rewrite_unlabeled<F>(&self, lty: LabeledTy<'tcx, L>, func: &mut F) -> Ty<'tcx>
    where
        F: FnMut(Ty<'tcx>, &[Ty<'tcx>], L) -> Ty<'tcx>,
    {
        use rustc_type_ir::TyKind::*;
        let args = lty
            .args
            .iter()
            .map(|&lty| self.rewrite_unlabeled(lty, func))
            .collect::<Vec<_>>();

        let ty = match *lty.ty.kind() {
            Bool | Char | Int(_) | Uint(_) | Float(_) | Str | Foreign(_) | Never => lty.ty,

            Adt(adt, substs) => {
                // Copy `substs`, but replace all types with those from `args`.
                let mut it = args.iter().cloned();
                let substs = self
                    .tcx
                    .mk_substs(substs.iter().map(|arg| match arg.unpack() {
                        GenericArgKind::Type(_) => it.next().unwrap().into(),
                        GenericArgKind::Lifetime(rg) => GenericArg::from(rg),
                        GenericArgKind::Const(cn) => GenericArg::from(cn),
                    }));
                assert!(it.next().is_none());
                self.tcx.mk_adt(adt, substs)
            }
            Array(_, len) => {
                let &[elem]: &[_; 1] = args[..].try_into().unwrap();
                self.tcx.mk_ty(Array(elem, len))
            }
            Slice(_) => {
                let &[elem]: &[_; 1] = args[..].try_into().unwrap();
                self.tcx.mk_slice(elem)
            }
            RawPtr(mty) => {
                let &[target]: &[_; 1] = args[..].try_into().unwrap();
                self.tcx.mk_ptr(TypeAndMut {
                    ty: target,
                    mutbl: mty.mutbl,
                })
            }
            Ref(rg, _, mutbl) => {
                let &[target]: &[_; 1] = args[..].try_into().unwrap();
                self.tcx.mk_ref(rg, TypeAndMut { ty: target, mutbl })
            }
            FnDef(def_id, substs) => {
                // Copy `substs`, but replace all types with those from `args`.
                let mut it = args.iter().cloned();
                let substs = self
                    .tcx
                    .mk_substs(substs.iter().map(|arg| match arg.unpack() {
                        GenericArgKind::Type(_) => it.next().unwrap().into(),
                        GenericArgKind::Lifetime(rg) => GenericArg::from(rg),
                        GenericArgKind::Const(cn) => GenericArg::from(cn),
                    }));
                assert!(it.next().is_none());
                self.tcx.mk_fn_def(def_id, substs)
            }
            FnPtr(ref _sig) => {
                // TODO: replace all the types under the binder
                todo!()
            }
            Tuple(_) => self.tcx.mk_tup(args.iter().cloned()),

            // Types that aren't actually supported by this code yet
            Dynamic(..) | Closure(..) | Generator(..) | GeneratorWitness(..) | Projection(..)
            | Opaque(..) | Param(..) | Bound(..) | Placeholder(..) | Infer(..) | Error(..) => {
                lty.ty
            }
        };

        func(ty, &args, lty.label)
    }
}

impl<'tcx, L> Deref for LabeledTyCtxt<'tcx, L> {
    type Target = TyCtxt<'tcx>;
    fn deref(&self) -> &TyCtxt<'tcx> {
        &self.tcx
    }
}
