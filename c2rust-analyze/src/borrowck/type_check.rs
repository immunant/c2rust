use crate::borrowck::atoms::{AllFacts, AtomMaps, Loan, Origin, Path, Point, SubPoint};
use crate::borrowck::{LTy, LTyCtxt, Label};
use crate::context::PermissionSet;
use crate::util::{self, Callee};
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{
    AggregateKind, BinOp, Body, BorrowKind, Local, LocalDecl, Location, Operand, Place, Rvalue,
    Statement, StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{TyCtxt, TyKind};
use std::collections::HashMap;

struct TypeChecker<'tcx, 'a> {
    tcx: TyCtxt<'tcx>,
    ltcx: LTyCtxt<'tcx>,
    facts: &'a mut AllFacts,
    maps: &'a mut AtomMaps<'tcx>,
    loans: &'a mut HashMap<Local, Vec<(Path, Loan, BorrowKind)>>,
    local_ltys: &'a [LTy<'tcx>],
    local_decls: &'a IndexVec<Local, LocalDecl<'tcx>>,

    current_location: Location,
}

impl<'tcx> TypeChecker<'tcx, '_> {
    fn current_point(&mut self, sub: SubPoint) -> Point {
        self.maps.point(
            self.current_location.block,
            self.current_location.statement_index,
            sub,
        )
    }

    pub fn visit_place(&mut self, pl: Place<'tcx>) -> LTy<'tcx> {
        let mut lty = self.local_ltys[pl.local.index()];
        for proj in pl.projection {
            lty = util::lty_project(lty, &proj);
        }
        lty
    }

    pub fn visit_operand(&mut self, op: &Operand<'tcx>) -> LTy<'tcx> {
        match *op {
            Operand::Copy(pl) | Operand::Move(pl) => self.visit_place(pl),
            Operand::Constant(ref c) => {
                let ty = c.ty();
                self.ltcx.label(ty, &mut |_| Label::default())
            }
        }
    }

    /// Create a new origin and issue an associated loan.  The loan is issued at
    /// `self.current_location`.
    fn issue_loan(&mut self, pl: Place<'tcx>, borrow_kind: BorrowKind) -> Origin {
        // Create a new origin and issue an associated loan.
        let origin = self.maps.origin();
        let path = self.maps.path(self.facts, pl);
        let loan = self.maps.loan();
        self.loans
            .entry(pl.local)
            .or_default()
            .push((path, loan, borrow_kind));
        let point = self.current_point(SubPoint::Mid);
        self.facts.loan_issued_at.push((origin, loan, point));
        eprintln!("issued loan {:?} = {:?} ({:?})", loan, pl, borrow_kind);
        origin
    }

    pub fn visit_rvalue(&mut self, rv: &Rvalue<'tcx>, expect_ty: LTy<'tcx>) -> LTy<'tcx> {
        match *rv {
            Rvalue::Use(Operand::Move(pl)) | Rvalue::Use(Operand::Copy(pl))
                if matches!(expect_ty.ty.kind(), TyKind::RawPtr(_)) =>
            {
                // Copy of a raw pointer.  We treat this as a reborrow.
                let perm = expect_ty.label.perm;
                let borrow_kind = if perm.contains(PermissionSet::UNIQUE) {
                    BorrowKind::Mut {
                        allow_two_phase_borrow: false,
                    }
                } else {
                    BorrowKind::Shared
                };

                let pl_deref = self.tcx.mk_place_deref(pl);
                let origin = self.issue_loan(pl_deref, borrow_kind);

                // Return a type with the new loan on the outermost `ref`.
                let ty = rv.ty(self.local_decls, *self.ltcx);
                let pl_lty = self.visit_place(pl_deref);
                let label = Label {
                    origin: Some(origin),
                    perm,
                };
                let lty = self.ltcx.mk(ty, self.ltcx.mk_slice(&[pl_lty]), label);
                lty
            }

            Rvalue::Use(ref op) => self.visit_operand(op),

            Rvalue::Ref(_, borrow_kind, pl) => {
                let perm = expect_ty.label.perm;
                let origin = self.issue_loan(pl, borrow_kind);

                // Return a type with the new loan on the outermost `ref`.
                let ty = rv.ty(self.local_decls, *self.ltcx);
                let pl_lty = self.visit_place(pl);
                let label = Label {
                    origin: Some(origin),
                    perm,
                };
                let lty = self.ltcx.mk(ty, self.ltcx.mk_slice(&[pl_lty]), label);
                lty
            }

            Rvalue::AddressOf(_, pl) => {
                let perm = expect_ty.label.perm;
                let borrow_kind = if perm.contains(PermissionSet::UNIQUE) {
                    BorrowKind::Mut {
                        allow_two_phase_borrow: false,
                    }
                } else {
                    BorrowKind::Shared
                };

                let origin = self.issue_loan(pl, borrow_kind);

                // Return a type with the new loan on the outermost `ref`.
                let ty = rv.ty(self.local_decls, *self.ltcx);
                let pl_lty = self.visit_place(pl);
                let label = Label {
                    origin: Some(origin),
                    perm,
                };
                let lty = self.ltcx.mk(ty, self.ltcx.mk_slice(&[pl_lty]), label);
                lty
            }

            Rvalue::BinaryOp(BinOp::Offset, _) | Rvalue::CheckedBinaryOp(BinOp::Offset, _) => {
                todo!("visit_rvalue BinOp::Offset")
            }
            Rvalue::BinaryOp(_, ref _ab) | Rvalue::CheckedBinaryOp(_, ref _ab) => {
                let ty = rv.ty(self.local_decls, *self.ltcx);
                self.ltcx.label(ty, &mut |ty| {
                    assert!(
                        !matches!(ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..)),
                        "pointer BinaryOp NYI"
                    );
                    Label::default()
                })
            }

            Rvalue::Cast(_, _, ty) => self.ltcx.label(ty, &mut |ty| {
                assert!(
                    !matches!(ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..)),
                    "pointer Cast NYI"
                );
                Label::default()
            }),

            Rvalue::Aggregate(ref kind, ref _ops) => match **kind {
                AggregateKind::Array(..) => {
                    // TODO
                    let ty = rv.ty(self.local_decls, *self.ltcx);
                    self.ltcx.label(ty, &mut |_ty| Label::default())
                },
                _ => panic!("unsupported rvalue AggregateKind {:?}", kind),
            },

            Rvalue::Len(..) => {
                let ty = rv.ty(self.local_decls, *self.ltcx);
                self.ltcx.label(ty, &mut |_| Label::default())
            },

            ref rv => panic!("unsupported rvalue {:?}", rv),
        }
    }

    fn do_assign(&mut self, pl_lty: LTy<'tcx>, rv_lty: LTy<'tcx>) {
        eprintln!("assign {:?} = {:?}", pl_lty, rv_lty);

        let pl_origin = pl_lty.label.origin;
        let rv_origin = rv_lty.label.origin;
        if let (Some(pl_origin), Some(rv_origin)) = (pl_origin, rv_origin) {
            let point = self.current_point(SubPoint::Mid);
            self.facts.subset_base.push((rv_origin, pl_origin, point));
        }
    }

    pub fn visit_statement(&mut self, stmt: &Statement<'tcx>) {
        // TODO(spernsteiner): other `StatementKind`s will be handled in the future
        #[allow(clippy::single_match)]
        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;
                let pl_lty = self.visit_place(pl);
                let rv_lty = self.visit_rvalue(rv, pl_lty);
                self.do_assign(pl_lty, rv_lty);
            }
            // TODO(spernsteiner): handle other `StatementKind`s
            _ => (),
        }
    }

    pub fn visit_terminator(&mut self, term: &Terminator<'tcx>) {
        eprintln!("borrowck: visit_terminator({:?})", term.kind);
        // TODO(spernsteiner): other `TerminatorKind`s will be handled in the future
        #[allow(clippy::single_match)]
        match term.kind {
            TerminatorKind::Call {
                ref func,
                ref args,
                destination,
                target: _,
                ..
            } => {
                let func_ty = func.ty(self.local_decls, *self.ltcx);
                eprintln!("callee = {:?}", util::ty_callee(*self.ltcx, func_ty));
                match util::ty_callee(*self.ltcx, func_ty) {
                    Some(Callee::PtrOffset { .. }) => {
                        // We handle this like a pointer assignment.
                        let pl_lty = self.visit_place(destination);
                        assert!(args.len() == 2);
                        let rv_lty = self.visit_operand(&args[0]);
                        self.do_assign(pl_lty, rv_lty);
                    }
                    Some(Callee::SliceAsPtr { .. }) => {
                        // TODO: handle this like a cast
                    },
                    Some(Callee::MiscBuiltin) => {},
                    Some(Callee::Other { .. }) => {
                        // TODO
                    },
                    None => {}
                }
            }
            // TODO(spernsteiner): handle other `TerminatorKind`s
            _ => (),
        }
    }
}

pub fn visit<'tcx>(
    tcx: TyCtxt<'tcx>,
    ltcx: LTyCtxt<'tcx>,
    facts: &mut AllFacts,
    maps: &mut AtomMaps<'tcx>,
    loans: &mut HashMap<Local, Vec<(Path, Loan, BorrowKind)>>,
    local_ltys: &[LTy<'tcx>],
    mir: &Body<'tcx>,
) {
    let mut tc = TypeChecker {
        tcx,
        ltcx,
        facts,
        maps,
        loans,
        local_ltys,
        local_decls: &mir.local_decls,
        current_location: Location::START,
    };

    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        for (idx, stmt) in bb_data.statements.iter().enumerate() {
            tc.current_location = Location {
                block: bb,
                statement_index: idx,
            };
            tc.visit_statement(stmt);
        }

        tc.current_location = Location {
            block: bb,
            statement_index: bb_data.statements.len(),
        };
        tc.visit_terminator(bb_data.terminator());
    }
}
