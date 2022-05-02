use std::collections::HashMap;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{
    Body, Statement, StatementKind, Terminator, TerminatorKind, Rvalue, Place, PlaceRef, Operand,
    BorrowKind, Local, LocalDecl, Location, ProjectionElem, Mutability,
};
use rustc_middle::mir::visit::{PlaceContext, NonMutatingUseContext, MutatingUseContext};
use crate::context::{PermissionSet, PointerId, AnalysisCtxt, LTy};
use crate::util::{describe_rvalue, RvalueDesc};
use super::DataflowConstraints;


/// Visitor that walks over the MIR, computing types of rvalues/operands/places and generating
/// constraints as a side effect.
struct TypeChecker<'tcx, 'a> {
    acx: &'a AnalysisCtxt<'tcx>,
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

    pub fn visit_statement(&mut self, stmt: &Statement<'tcx>) {
        eprintln!("visit_statement({:?})", stmt);
        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;
                let ctx = PlaceContext::MutatingUse(MutatingUseContext::Store);
                let pl_lty = self.visit_place(pl, ctx);
                let pl_ptr = pl_lty.label;

                let rv_ptr = self.visit_rvalue(rv);

                if pl_ptr != PointerId::NONE || rv_ptr != PointerId::NONE {
                    assert!(pl_ptr != PointerId::NONE);
                    assert!(rv_ptr != PointerId::NONE);
                    self.add_edge(rv_ptr, pl_ptr);
                }
            },
            _ => {},
        }
    }

    pub fn visit_terminator(&mut self, term: &Terminator<'tcx>) {
        match term.kind {
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
        constraints: DataflowConstraints::default(),
    };

    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        for (idx, stmt) in bb_data.statements.iter().enumerate() {
            tc.visit_statement(stmt);
        }
        tc.visit_terminator(bb_data.terminator());
    }

    tc.constraints
}


