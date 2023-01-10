use crate::borrowck::atoms::{AllFacts, AtomMaps, Loan, Origin, Path, Point, SubPoint};
use crate::borrowck::{LTy, LTyCtxt, Label, OriginKind};
use crate::context::PermissionSet;
use crate::util::{self, Callee};
use crate::AdtMetadataTable;
use assert_matches::assert_matches;
use indexmap::IndexMap;
use rustc_hir::def_id::DefId;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{
    AggregateKind, BinOp, Body, BorrowKind, Field, Local, LocalDecl, Location, Operand, Place,
    Rvalue, Statement, StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{AdtDef, FieldDef, Ty, TyCtxt, TyKind};
use std::collections::HashMap;

struct TypeChecker<'tcx, 'a> {
    tcx: TyCtxt<'tcx>,
    ltcx: LTyCtxt<'tcx>,
    facts: &'a mut AllFacts,
    maps: &'a mut AtomMaps<'tcx>,
    loans: &'a mut HashMap<Local, Vec<(Path, Loan, BorrowKind)>>,
    local_ltys: &'a [LTy<'tcx>],
    field_permissions: &'a HashMap<DefId, PermissionSet>,
    local_decls: &'a IndexVec<Local, LocalDecl<'tcx>>,
    current_location: Location,
    adt_metadata: &'a AdtMetadataTable<'tcx>,
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
        let mut lty: LTy = self.local_ltys[pl.local.index()];

        let mut adt_func = |base_lty: LTy<'tcx>,
                            base_adt_def: AdtDef,
                            field: Field,
                            field_ty: Ty<'tcx>| {
            let base_origin_param_map: IndexMap<OriginKind, Origin> = base_lty
                .label
                .origin_params
                .map(|params| IndexMap::from_iter(params.to_vec()))
                .unwrap_or_default();

            let mut field_origin_param_map = IndexMap::new();

            let field_def: &FieldDef = &base_adt_def.non_enum_variant().fields[field.index()];
            let perm = self.field_permissions[&field_def.did];
            let base_metadata = &self.adt_metadata.table[&base_adt_def.did()];
            let field_metadata = &base_metadata.field_info[&field_def.did];

            if let Some(field_adt_metadata) = self.adt_metadata.table.get(&field_def.did) {
                /*
                    If we're in this block, it means the current field is an ADT.

                    That field's type may have its own lifetime parameters. In the following:

                    ```
                    struct Foo<'a> {
                        a: &'a i32
                    }

                    struct Bar<'b> {
                        foo: Foo<'b>
                    }

                    fn some_func() {
                        let bar: Bar<'0> = ...;
                        bar.foo.a: &'? i32 = ...;
                    }
                    ```

                    We want to know that the lifetime `'?` gets resolved to the concrete
                    origin `'0`. To do this, a mapping needs to be made between `bar.foo`
                    lifetime parameter `'b` (which is already paired with concrete lifetime
                    `'0`) and `Foo` lifetime parameter `'a`. This mapping is created below.
                */
                for (field_lifetime_param, field_struct_lifetime_param) in field_metadata
                    .lifetime_params
                    .iter()
                    .zip(field_adt_metadata.lifetime_params.iter())
                {
                    if let Some((base_lifetime_param, og)) =
                        base_origin_param_map.get_key_value(field_lifetime_param)
                    {
                        eprintln!(
                                "mapping {base_adt_def:?} lifetime parameter {base_lifetime_param:?} to \
                                {base_adt_def:?}.{:} struct definition lifetime parameter {field_struct_lifetime_param:?}, \
                                corresponding to its lifetime parameter {field_lifetime_param:?} within {base_adt_def:?}",
                                field_def.name
                            );
                        field_origin_param_map.insert(*field_struct_lifetime_param, *og);
                    }
                }
            }

            let origin_params: Option<&_> = if field_origin_param_map.is_empty() {
                None
            } else {
                let field_origin_params: Vec<_> = field_origin_param_map.into_iter().collect();
                Some(self.ltcx.arena().alloc_slice(&field_origin_params[..]))
            };
            let mut field_lifetimes = field_metadata.lifetime.clone();
            self.ltcx.label(field_ty, &mut |ty| {
                let origin = match ty.kind() {
                    TyKind::Ref(..) | TyKind::RawPtr(..) => {
                        // remove the outermost lifetime when moving a
                        // reference layer deeper, to reflect the corresponding
                        // dereferenced-value's lifetime. e.g. we would want a label
                        // for `*(&'a mut &'b mut foo)` to have an origin
                        // corresponding to `'b`, so we remove `'a` from the lifetime
                        // set so that `'b` can be retrieved in the recursive call to
                        // ltcx.label with type kind `Ref(foo)`
                        field_lifetimes.swap_remove_index(0)
                    }
                    _ => field_lifetimes.get_index(0).cloned(),
                }
                .and_then(|o| {
                    eprintln!("field origin {o:?}, {base_origin_param_map:?}");
                    base_origin_param_map.get(&o)
                })
                .cloned();

                Label {
                    origin,
                    perm,
                    origin_params,
                }
            })
        };

        for proj in pl.projection {
            lty = util::lty_project(lty, &proj, &mut adt_func);
        }
        eprintln!("final label for {pl:?}: {:?}", lty);
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
                    origin_params: None,
                    perm,
                };
                let lty = self.ltcx.mk(ty, self.ltcx.mk_slice(&[pl_lty]), label);
                lty
            }

            Rvalue::Use(ref op) => self.visit_operand(op),

            Rvalue::Ref(_, borrow_kind, pl) => {
                // Return a type with the new loan on the outermost `ref`.
                let perm = expect_ty.label.perm;
                let origin = self.issue_loan(pl, borrow_kind);
                let label = Label {
                    origin: Some(origin),
                    origin_params: None,
                    perm,
                };
                let ty = rv.ty(self.local_decls, *self.ltcx);
                let pl_lty = self.visit_place(pl);
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
                    origin_params: None,
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

            Rvalue::Cast(_, _, ty) => self.ltcx.label(ty, &mut |_ty| {
                // TODO: handle Unsize casts at minimum
                /*
                assert!(
                    !matches!(ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..)),
                    "pointer Cast NYI"
                );
                */
                Label::default()
            }),

            Rvalue::Aggregate(ref kind, ref _ops) => match **kind {
                AggregateKind::Array(..) => {
                    let ty = rv.ty(self.local_decls, *self.ltcx);
                    // TODO: create fresh origins for all pointers in `ty`, and generate subset
                    // relations between the regions of the array and the regions of its elements
                    self.ltcx.label(ty, &mut |_ty| Label::default())
                }
                _ => panic!("unsupported rvalue AggregateKind {:?}", kind),
            },

            Rvalue::Len(..) => {
                let ty = rv.ty(self.local_decls, *self.ltcx);
                self.ltcx.label(ty, &mut |_| Label::default())
            }

            Rvalue::UnaryOp(_, ref op) => self.visit_operand(op),

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

        for (pl_lty, rv_lty) in pl_lty.iter().zip(rv_lty.iter()) {
            let pl_origin_params = pl_lty.label.origin_params;
            let rv_origin_params = rv_lty.label.origin_params;
            if let (Some(pl_origin_params), Some(rv_origin_params)) =
                (pl_origin_params, rv_origin_params)
            {
                assert_eq!(pl_origin_params.len(), rv_origin_params.len());
                for (pl_origin_param, rv_origin_param) in
                    pl_origin_params.iter().zip(rv_origin_params.iter())
                {
                    let point = self.current_point(SubPoint::Mid);
                    self.facts
                        .subset_base
                        .push((rv_origin_param.1, pl_origin_param.1, point));
                }
            }
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
                    }
                    Some(Callee::MiscBuiltin) => {}
                    Some(Callee::Other { .. }) => {
                        // TODO
                    }
                    Some(Callee::Malloc) => {
                        // TODO
                    }
                    Some(Callee::Calloc) => {
                        // TODO
                    }
                    Some(Callee::Realloc) => {
                        // We handle this like a pointer assignment.
                        let pl_lty = self.visit_place(destination);
                        let rv_lty = assert_matches!(&args[..], [p, _] => {
                            self.visit_operand(p)
                        });

                        self.do_assign(pl_lty, rv_lty);
                    }
                    Some(Callee::Free) => {
                        let _pl_lty = self.visit_place(destination);
                        let _rv_lty = assert_matches!(&args[..], [p] => {
                            self.visit_operand(p)
                        });
                    }
                    Some(Callee::IsNull) => {
                        let _rv_lty = assert_matches!(&args[..], [p] => {
                            self.visit_operand(p)
                        });
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
    tcx: TyCtxt<'tcx>,
    ltcx: LTyCtxt<'tcx>,
    facts: &mut AllFacts,
    maps: &mut AtomMaps<'tcx>,
    loans: &mut HashMap<Local, Vec<(Path, Loan, BorrowKind)>>,
    local_ltys: &[LTy<'tcx>],
    field_permissions: &HashMap<DefId, PermissionSet>,
    mir: &Body<'tcx>,
    adt_metadata: &AdtMetadataTable<'tcx>,
) {
    let mut tc = TypeChecker {
        tcx,
        ltcx,
        facts,
        maps,
        loans,
        local_ltys,
        field_permissions,
        local_decls: &mir.local_decls,
        current_location: Location::START,
        adt_metadata,
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
