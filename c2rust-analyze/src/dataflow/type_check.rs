use std::collections::HashMap;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{
    Body, Statement, StatementKind, Terminator, TerminatorKind, Rvalue, Place, Operand, BorrowKind,
    Local, LocalDecl, Location, ProjectionElem,
};
use crate::context::{PermissionSet, PointerId, AnalysisCtxt, LTy};
use crate::util::{describe_rvalue, RvalueDesc};
use super::DataflowConstraints;


struct TypeChecker<'tcx, 'a> {
    acx: &'a AnalysisCtxt<'tcx>,
    constraints: DataflowConstraints,
}

impl<'tcx> TypeChecker<'tcx, '_> {
    fn add_edge(&mut self, src: PointerId, dest: PointerId) {
        // Forward dataflow: if src is non-UNIQUE, then dest must be non-UNIQUE.
        self.constraints.add_edge(false, src, dest, PermissionSet::UNIQUE);
        // Backward dataflow: if dest is READ/WRITE, then src must also be READ/WRITE.
        self.constraints.add_edge(true, dest, src, PermissionSet::READ | PermissionSet::WRITE);
    }

    pub fn visit_place(&mut self, pl: Place<'tcx>) -> LTy<'tcx> {
        let mut lty = self.acx.local_tys[pl.local];
        for proj in pl.projection {
            match proj {
                ProjectionElem::Deref => {
                    assert_eq!(lty.args.len(), 1);
                    lty = lty.args[0];
                },

                ref proj => panic!("unsupported projection {:?} in {:?}", proj, pl),
            }
        }
        lty
    }

    pub fn visit_operand(&mut self, op: &Operand<'tcx>) -> LTy<'tcx> {
        match *op {
            Operand::Copy(pl) |
            Operand::Move(pl) => self.visit_place(pl),
            Operand::Constant(ref c) => {
                let ty = c.ty();
                // TODO
                self.acx.lcx.label(ty, &mut |_| PointerId::NONE)
            },
        }
    }

    pub fn visit_statement(&mut self, stmt: &Statement<'tcx>) {
        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;
                let pl_lty = self.visit_place(pl);
                let pl_ptr = pl_lty.label;
                if pl_ptr == PointerId::NONE {
                    return;
                }

                let rv_ptr = match describe_rvalue(rv) {
                    Some(RvalueDesc::Project { base, proj: _ }) => {
                        self.acx.ptr_of(base)
                            .unwrap_or_else(|| panic!("missing pointer ID for {:?}", base))
                    },
                    Some(RvalueDesc::AddrOfLocal { local, proj: _ }) => {
                        self.acx.addr_of_local[local]
                    },
                    None => panic!("TODO: handle assignment of {:?}", rv),
                };
                assert!(rv_ptr != PointerId::NONE);
                self.add_edge(rv_ptr, pl_ptr);
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


