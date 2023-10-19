use crate::context::{AnalysisCtxt, LTy, LTyCtxt};
use crate::pointer_id::PointerId;
use rustc_middle::mir::Body;
use rustc_middle::ty::{Ty, TyCtxt, TyKind, TypeFolder, TypeSuperFoldable, TypeAndMut};
use std::mem;
use std::ops::Index;

mod constraint_set;
mod solve;
mod type_check;

pub use self::constraint_set::{CTy, Constraint, ConstraintSet};
pub use self::solve::{solve_constraints, PointeeTypes};

pub fn generate_constraints<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    mir: &Body<'tcx>,
) -> ConstraintSet<'tcx> {
    type_check::visit(acx, mir)
}

/// For each pointer in `lty`, if the pointer has a sole unique type listed in `pointee_types`,
/// replace its original pointee type with the one obtained from `pointee_types`.  For example, if
/// `lty` is `*mut /*p1*/ c_void` and `pointee_types` records `u8` for `p1`, this function will
/// return `*mut /*p1*/ u8`.  This transformation is applied recursively in cases where the new
/// type obtained from `pointee_types` itself contains a pointer.
pub fn expand_type<'tcx>(
    lcx: LTyCtxt<'tcx>,
    pointee_types: &impl Index<PointerId, Output = PointeeTypes<'tcx>>,
    lty: LTy<'tcx>,
) -> LTy<'tcx> {
    let mut folder = ExpandTypeFolder {
        lcx,
        old_ltys: Vec::new(),
        new_ltys: Vec::new(),
        pointee_types,
    };
    let (new_ty, new_ltys) = folder.enter(vec![lty], |folder| folder.fold_ty(lty.ty));
    assert_eq!(new_ltys.len(), 1);
    assert_eq!(new_ty, new_ltys[0].ty);
    new_ltys[0]
}

struct ExpandTypeFolder<'a, 'tcx, PT> {
    lcx: LTyCtxt<'tcx>,
    old_ltys: Vec<LTy<'tcx>>,
    new_ltys: Vec<LTy<'tcx>>,
    pointee_types: &'a PT,
}

impl<'tcx, PT> ExpandTypeFolder<'_, 'tcx, PT> {
    pub fn enter<R>(
        &mut self,
        old_ltys: impl Into<Vec<LTy<'tcx>>>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> (R, Vec<LTy<'tcx>>) {
        let old_ltys = old_ltys.into();
        let n = old_ltys.len();
        let saved_old = mem::replace(&mut self.old_ltys, old_ltys.into());
        let saved_new = mem::replace(&mut self.new_ltys, Vec::with_capacity(n));
        let r = f(self);
        self.old_ltys = saved_old;
        let updated_new = mem::replace(&mut self.new_ltys, saved_new);
        (r, updated_new)
    }
}

impl<'tcx, PT> TypeFolder<'tcx> for ExpandTypeFolder<'_, 'tcx, PT>
where PT: Index<PointerId, Output = PointeeTypes<'tcx>> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        *self.lcx
    }

    fn fold_ty(&mut self, ty: Ty<'tcx>) -> Ty<'tcx> {
        if let Some(&old_lty) = self.old_ltys.get(self.new_ltys.len()) {
            assert_eq!(old_lty.ty, ty);

            if matches!(ty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)) {
                let ptr = old_lty.label;
                if !ptr.is_none() {
                    if let Some(pointee_lty) = self.pointee_types[ptr].get_sole_lty() {
                        let (new_pointee_ty, new_args) = self.enter(vec![pointee_lty], |folder| {
                            folder.fold_ty(pointee_lty.ty)
                        });
                        assert_eq!(new_args.len(), 1);
                        assert_eq!(new_pointee_ty, new_args[0].ty);
                        let new_ty_kind = match *ty.kind() {
                            TyKind::Ref(rg, _, mutbl) => TyKind::Ref(rg, new_pointee_ty, mutbl),
                            TyKind::RawPtr(mty) => TyKind::RawPtr(TypeAndMut {
                                ty: new_pointee_ty,
                                mutbl: mty.mutbl,
                            }),
                            _ => unreachable!(),
                        };
                        let new_ty = self.tcx().mk_ty(new_ty_kind);
                        let new_args = self.lcx.mk_slice(&new_args);
                        let new_lty = self.lcx.mk(new_ty, new_args, old_lty.label);
                        self.new_ltys.push(new_lty);
                        return new_ty;
                    }
                }
            }

            let (new_ty, new_args) = self.enter(old_lty.args, |folder| {
                ty.super_fold_with(folder)
            });
            let new_args = self.lcx.mk_slice(&new_args);
            let new_lty = self.lcx.mk(new_ty, new_args, old_lty.label);
            self.new_ltys.push(new_lty);
            new_ty
        } else {
            let (new_ty, _) = self.enter(vec![], |folder| {
                ty.super_fold_with(folder)
            });
            new_ty
        }
    }
}
