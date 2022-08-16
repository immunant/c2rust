use super::DataflowConstraints;
use crate::context::{AnalysisCtxt, LTy, PermissionSet, PointerId};
use crate::util::{self, describe_rvalue, Callee, RvalueDesc};
use rustc_middle::mir::visit::{MutatingUseContext, NonMutatingUseContext, PlaceContext};
use rustc_middle::mir::{
    BinOp, Body, Mutability, Operand, Place, PlaceRef, ProjectionElem, Rvalue, Statement,
    StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::TyKind;

/// Visitor that walks over the MIR, computing types of rvalues/operands/places and generating
/// constraints as a side effect.
struct TypeChecker<'tcx, 'a> {
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    mir: &'a Body<'tcx>,
    constraints: DataflowConstraints,
    equiv_constraints: Vec<(PointerId, PointerId)>,
}

impl<'tcx> TypeChecker<'tcx, '_> {
    fn add_edge(&mut self, src: PointerId, dest: PointerId) {
        // Copying `src` to `dest` can discard permissions, but can't add new ones.
        self.constraints.add_subset(dest, src);
    }

    fn add_equiv(&mut self, a: PointerId, b: PointerId) {
        self.equiv_constraints.push((a, b));
    }

    fn record_access(&mut self, ptr: PointerId, mutbl: Mutability) {
        eprintln!("record_access({:?}, {:?})", ptr, mutbl);
        if ptr == PointerId::NONE {
            return;
        }
        match mutbl {
            Mutability::Mut => {
                self.constraints
                    .add_all_perms(ptr, PermissionSet::READ | PermissionSet::WRITE);
            }
            Mutability::Not => {
                self.constraints.add_all_perms(ptr, PermissionSet::READ);
            }
        }
    }

    pub fn visit_place(&mut self, pl: Place<'tcx>, ctx: PlaceContext) -> LTy<'tcx> {
        self.visit_place_ref(pl.as_ref(), ctx)
    }

    pub fn visit_place_ref(&mut self, pl: PlaceRef<'tcx>, ctx: PlaceContext) -> LTy<'tcx> {
        let mut lty = self.acx.local_tys[pl.local];
        let mut prev_deref_ptr = None;

        for proj in pl.projection {
            match proj {
                ProjectionElem::Deref => {
                    // All derefs except the last are loads, to retrieve the pointer for the next
                    // deref.  The last deref is either a load or a store, depending on `ctx`.
                    if let Some(ptr) = prev_deref_ptr.take() {
                        self.record_access(ptr, Mutability::Not);
                    }
                    prev_deref_ptr = Some(lty.label);
                    assert_eq!(lty.args.len(), 1);
                    lty = lty.args[0];
                }

                ProjectionElem::Field(f, _field_ty) => match lty.ty.kind() {
                    TyKind::Tuple(..) => {
                        lty = lty.args[f.as_usize()];
                    }
                    _ => todo!("field of {:?}", lty),
                },

                ref proj => panic!("unsupported projection {:?} in {:?}", proj, pl),
            }
        }

        if let Some(ptr) = prev_deref_ptr.take() {
            match ctx {
                PlaceContext::NonMutatingUse(..) => {
                    self.record_access(ptr, Mutability::Not);
                }
                PlaceContext::MutatingUse(..) => {
                    self.record_access(ptr, Mutability::Mut);
                }
                PlaceContext::NonUse(..) => {}
            }
        }

        lty
    }

    pub fn visit_rvalue(&mut self, rv: &Rvalue<'tcx>) -> PointerId {
        eprintln!("visit_rvalue({:?}), desc = {:?}", rv, describe_rvalue(rv));

        match describe_rvalue(rv) {
            Some(RvalueDesc::Project { base, proj: _ }) => {
                let ctx = PlaceContext::NonMutatingUse(NonMutatingUseContext::Copy);
                let base_ty = self.visit_place_ref(base, ctx);
                base_ty.label
            }
            Some(RvalueDesc::AddrOfLocal { local, proj: _ }) => self.acx.addr_of_local[local],
            None => match *rv {
                Rvalue::Use(ref op) => self.visit_operand(op).label,
                Rvalue::BinaryOp(BinOp::Offset, _) => todo!("visit_rvalue BinOp::Offset"),
                Rvalue::BinaryOp(..) => PointerId::NONE,
                Rvalue::CheckedBinaryOp(BinOp::Offset, _) => todo!("visit_rvalue BinOp::Offset"),
                Rvalue::CheckedBinaryOp(..) => PointerId::NONE,
                Rvalue::Cast(_, _, ty) => {
                    assert!(!matches!(ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..)));
                    PointerId::NONE
                }
                _ => panic!("TODO: handle assignment of {:?}", rv),
            },
        }
    }

    pub fn visit_operand(&mut self, op: &Operand<'tcx>) -> LTy<'tcx> {
        match *op {
            Operand::Copy(pl) => {
                let ctx = PlaceContext::NonMutatingUse(NonMutatingUseContext::Copy);
                self.visit_place(pl, ctx)
            }
            Operand::Move(pl) => {
                let ctx = PlaceContext::NonMutatingUse(NonMutatingUseContext::Move);
                self.visit_place(pl, ctx)
            }
            Operand::Constant(ref c) => {
                let ty = c.ty();
                // TODO
                self.acx.lcx().label(ty, &mut |_| PointerId::NONE)
            }
        }
    }

    fn do_assign(&mut self, pl_ptr: PointerId, rv_ptr: PointerId) {
        if pl_ptr != PointerId::NONE || rv_ptr != PointerId::NONE {
            assert!(pl_ptr != PointerId::NONE);
            assert!(rv_ptr != PointerId::NONE);
            self.add_edge(rv_ptr, pl_ptr);
        }
    }

    fn do_unify_pointees(&mut self, pl_lty: LTy<'tcx>, rv_lty: LTy<'tcx>) {
        if pl_lty.label == PointerId::NONE && rv_lty.label == PointerId::NONE {
            return;
        }
        assert!(pl_lty.label != PointerId::NONE);
        assert!(rv_lty.label != PointerId::NONE);
        assert_eq!(pl_lty.args.len(), 1);
        assert_eq!(rv_lty.args.len(), 1);

        let pl_pointee = pl_lty.args[0];
        let rv_pointee = rv_lty.args[0];
        assert_eq!(pl_pointee.ty, rv_pointee.ty);
        for (pl_sub_lty, rv_sub_lty) in pl_pointee.iter().zip(rv_pointee.iter()) {
            eprintln!("equate {:?} = {:?}", pl_sub_lty, rv_sub_lty);
            if pl_sub_lty.label != PointerId::NONE || rv_sub_lty.label != PointerId::NONE {
                assert!(pl_sub_lty.label != PointerId::NONE);
                assert!(rv_sub_lty.label != PointerId::NONE);
                self.add_equiv(pl_sub_lty.label, rv_sub_lty.label);
            }
        }
    }

    pub fn visit_statement(&mut self, stmt: &Statement<'tcx>) {
        eprintln!("visit_statement({:?})", stmt);
        // TODO(spernsteiner): other `StatementKind`s will be handled in the future
        #[allow(clippy::single_match)]
        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;
                let ctx = PlaceContext::MutatingUse(MutatingUseContext::Store);
                let pl_lty = self.visit_place(pl, ctx);
                let pl_ptr = pl_lty.label;

                // TODO: combine these
                let rv_ptr = self.visit_rvalue(rv);
                let rv_lty = self.acx.type_of(rv);

                self.do_assign(pl_ptr, rv_ptr);
                self.do_unify_pointees(pl_lty, rv_lty);
            }
            // TODO(spernsteiner): handle other `StatementKind`s
            _ => (),
        }
    }

    pub fn visit_terminator(&mut self, term: &Terminator<'tcx>) {
        eprintln!("visit_terminator({:?})", term.kind);
        let tcx = self.acx.tcx();
        // TODO(spernsteiner): other `TerminatorKind`s will be handled in the future
        #[allow(clippy::single_match)]
        match term.kind {
            TerminatorKind::Call {
                ref func,
                ref args,
                destination,
                target,
                ..
            } => {
                let func_ty = func.ty(self.mir, tcx);
                eprintln!("callee = {:?}", util::ty_callee(tcx, func_ty));
                match util::ty_callee(tcx, func_ty) {
                    Some(Callee::PtrOffset { .. }) => {
                        // We handle this like a pointer assignment.

                        // `target` must be `Some` because the function doesn't diverge.
                        // TODO(kkysen) I kept the `.unwrap()` so that the behavior is identical.  Do we need this?
                        target.unwrap();
                        let ctx = PlaceContext::MutatingUse(MutatingUseContext::Store);
                        let pl_lty = self.visit_place(destination, ctx);
                        assert!(args.len() == 2);
                        let rv_lty = self.visit_operand(&args[0]);
                        self.do_assign(pl_lty.label, rv_lty.label);
                        let perms = PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB;
                        self.constraints.add_all_perms(rv_lty.label, perms);
                    }
                    None => {}
                }
            }
            // TODO(spernsteiner): handle other `TerminatorKind`s
            _ => (),
        }
    }
}

pub fn visit<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    mir: &Body<'tcx>,
) -> (DataflowConstraints, Vec<(PointerId, PointerId)>) {
    let mut tc = TypeChecker {
        acx,
        mir,
        constraints: DataflowConstraints::default(),
        equiv_constraints: Vec::new(),
    };

    for bb_data in mir.basic_blocks().iter() {
        for stmt in bb_data.statements.iter() {
            tc.visit_statement(stmt);
        }
        tc.visit_terminator(bb_data.terminator());
    }

    (tc.constraints, tc.equiv_constraints)
}
