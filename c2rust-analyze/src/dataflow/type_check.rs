use rustc_middle::mir::{
    Body, Statement, StatementKind, Terminator, TerminatorKind, Rvalue, BinOp, Place, PlaceRef,
    Operand, ProjectionElem, Mutability,
};
use rustc_middle::mir::visit::{PlaceContext, NonMutatingUseContext, MutatingUseContext};
use rustc_middle::ty::TyKind;
use crate::context::{PermissionSet, PointerId, AnalysisCtxt, LTy};
use crate::util::{self, describe_rvalue, RvalueDesc, Callee};
use super::DataflowConstraints;


/// Visitor that walks over the MIR, computing types of rvalues/operands/places and generating
/// constraints as a side effect.
struct TypeChecker<'tcx, 'a> {
    acx: &'a AnalysisCtxt<'tcx>,
    mir: &'a Body<'tcx>,
    constraints: DataflowConstraints,
}

impl<'tcx> TypeChecker<'tcx, '_> {
    fn add_edge(&mut self, src: PointerId, dest: PointerId) {
        // Copying `src` to `dest` can discard permissions, but can't add new ones.
        self.constraints.add_subset(dest, src);
    }

    fn record_access(&mut self, ptr: PointerId, mutbl: Mutability) {
        eprintln!("record_access({:?}, {:?})", ptr, mutbl);
        if ptr == PointerId::NONE {
            return;
        }
        match mutbl {
            Mutability::Mut => {
                self.constraints.add_all_perms(ptr, PermissionSet::READ | PermissionSet::WRITE);
            },
            Mutability::Not => {
                self.constraints.add_all_perms(ptr, PermissionSet::READ);
            },
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
                },

                ProjectionElem::Field(f, _field_ty) => {
                    match lty.ty.kind() {
                        TyKind::Tuple(..) => {
                            lty = lty.args[f.as_usize()];
                        },
                        _ => todo!("field of {:?}", lty),
                    }
                },

                ref proj => panic!("unsupported projection {:?} in {:?}", proj, pl),
            }
        }

        if let Some(ptr) = prev_deref_ptr.take() {
            match ctx {
                PlaceContext::NonMutatingUse(..) => {
                    self.record_access(ptr, Mutability::Not);
                },
                PlaceContext::MutatingUse(..) => {
                    self.record_access(ptr, Mutability::Mut);
                },
                PlaceContext::NonUse(..) => {},
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
            },
            Some(RvalueDesc::AddrOfLocal { local, proj: _ }) => {
                self.acx.addr_of_local[local]
            },
            None => match *rv {
                Rvalue::Use(ref op) => self.visit_operand(op).label,
                Rvalue::BinaryOp(BinOp::Offset, _) => todo!("visit_rvalue BinOp::Offset"),
                Rvalue::BinaryOp(..) => PointerId::NONE,
                Rvalue::CheckedBinaryOp(BinOp::Offset, _) => todo!("visit_rvalue BinOp::Offset"),
                Rvalue::CheckedBinaryOp(..) => PointerId::NONE,
                Rvalue::Cast(_, _, ty) => {
                    assert!(!matches!(ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..)));
                    PointerId::NONE
                },
                _ => panic!("TODO: handle assignment of {:?}", rv),
            },
        }        
    }

    pub fn visit_operand(
        &mut self,
        op: &Operand<'tcx>,
    ) -> LTy<'tcx> {
        match *op {
            Operand::Copy(pl) => {
                let ctx = PlaceContext::NonMutatingUse(NonMutatingUseContext::Copy);
                self.visit_place(pl, ctx)
            },
            Operand::Move(pl) => {
                let ctx = PlaceContext::NonMutatingUse(NonMutatingUseContext::Move);
                self.visit_place(pl, ctx)
            },
            Operand::Constant(ref c) => {
                let ty = c.ty();
                // TODO
                self.acx.lcx.label(ty, &mut |_| PointerId::NONE)
            },
        }
    }

    fn do_assign(&mut self, pl_ptr: PointerId, rv_ptr: PointerId) {
        if pl_ptr != PointerId::NONE || rv_ptr != PointerId::NONE {
            assert!(pl_ptr != PointerId::NONE);
            assert!(rv_ptr != PointerId::NONE);
            self.add_edge(rv_ptr, pl_ptr);
        }
    }

    pub fn visit_statement(&mut self, stmt: &Statement<'tcx>) {
        eprintln!("visit_statement({:?})", stmt);
        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;
                let ctx = PlaceContext::MutatingUse(MutatingUseContext::Store);
                let pl_lty = self.visit_place(pl, ctx);
                let pl_ptr = pl_lty.label;

                let rv_ptr = self.visit_rvalue(rv);

                self.do_assign(pl_ptr, rv_ptr);
            },
            _ => {},
        }
    }

    pub fn visit_terminator(&mut self, term: &Terminator<'tcx>) {
        eprintln!("visit_terminator({:?})", term.kind);
        let tcx = self.acx.tcx;
        match term.kind {
            TerminatorKind::Call { ref func, ref args, destination, .. } => {
                let func_ty = func.ty(self.mir, tcx);
                eprintln!("callee = {:?}", util::ty_callee(tcx, func_ty));
                match util::ty_callee(tcx, func_ty) {
                    Some(Callee::PtrOffset { .. }) => {
                        // We handle this like a pointer assignment.

                        // `destination` must be `Some` because the function doesn't diverge.
                        let destination = destination.unwrap();
                        let ctx = PlaceContext::MutatingUse(MutatingUseContext::Store);
                        let pl_lty = self.visit_place(destination.0, ctx);
                        assert!(args.len() == 2);
                        let rv_lty = self.visit_operand(&args[0]);
                        self.do_assign(pl_lty.label, rv_lty.label);
                        let perms = PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB;
                        self.constraints.add_all_perms(rv_lty.label, perms);
                    },
                    None => {},
                }
            },
            _ => {},
        }
    }
}

pub fn visit<'tcx>(
    acx: &AnalysisCtxt<'tcx>,
    mir: &Body<'tcx>,
) -> DataflowConstraints {
    let mut tc = TypeChecker {
        acx,
        mir,
        constraints: DataflowConstraints::default(),
    };

    for bb_data in mir.basic_blocks().iter() {
        for stmt in bb_data.statements.iter() {
            tc.visit_statement(stmt);
        }
        tc.visit_terminator(bb_data.terminator());
    }

    tc.constraints
}


