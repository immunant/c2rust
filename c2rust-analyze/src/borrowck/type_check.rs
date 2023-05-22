use crate::borrowck::atoms::{AllFacts, AtomMaps, Loan, Origin, Path, Point, SubPoint};
use crate::borrowck::{construct_adt_origins, LTy, LTyCtxt, Label, OriginParam};
use crate::c_void_casts::CVoidCasts;
use crate::context::PermissionSet;
use crate::panic_detail;
use crate::util::{self, ty_callee, Callee};
use crate::AdtMetadataTable;
use assert_matches::assert_matches;
use indexmap::IndexMap;
use rustc_hir::def_id::DefId;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{
    AggregateKind, BinOp, Body, BorrowKind, CastKind, Field, Local, LocalDecl, Location, Operand,
    Place, Rvalue, Statement, StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{AdtDef, FieldDef, TyCtxt, TyKind};
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

    pub fn field_lty(&self, base_lty: LTy<'tcx>, base_adt_def: AdtDef, field: Field) -> LTy<'tcx> {
        let base_origin_param_map: IndexMap<OriginParam, Origin> =
            IndexMap::from_iter(base_lty.label.origin_params.to_vec());
        let field_def: &FieldDef = &base_adt_def.non_enum_variant().fields[field.index()];
        let perm = self.field_permissions[&field_def.did];
        let base_metadata = &self.adt_metadata.table[&base_adt_def.did()];
        let field_metadata = &base_metadata.field_info[&field_def.did];

        self.ltcx.relabel(
                field_metadata.origin_args,
                &mut |flty| match flty.kind() {
                    TyKind::Ref(..) | TyKind::RawPtr(..) => {
                        let origin = {
                            assert!(flty.label.len() == 1);
                            Some(flty.label[0])
                        }
                        .map(|oa| {
                            OriginParam::try_from(&oa)
                                .expect("'static lifetimes not yet supported")
                        })
                        .and_then(|o| {
                            eprintln!(
                                "finding {o:?} in {base_adt_def:?} {base_origin_param_map:?}"
                            );
                            base_origin_param_map.get(&o)
                        })
                        .cloned();

                        Label {
                            origin,
                            perm,
                            origin_params: &[],
                        }
                    }
                    TyKind::Adt(fadt_def, _) => {
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
                            lifetime argument `'b` (which is already paired with concrete lifetime
                            `'0`) and `Foo` lifetime parameter `'a`. This mapping is created below.
                        */
                        let mut field_origin_param_map = vec![];
                        eprintln!("{:?}", fadt_def.did());
                        let field_adt_metadata = if let Some(field_adt_metadata) = self.adt_metadata.table.get(&fadt_def.did()) {
                            field_adt_metadata
                        } else {
                            return Label {
                                origin: None,
                                origin_params: &[],
                                perm
                            };
                        };

                        for (field_lifetime_arg, field_struct_lifetime_param) in flty
                            .label
                            .iter().zip(field_adt_metadata.lifetime_params.iter())
                        {
                            let field_lifetime_param =
                                if let Ok(param) = OriginParam::try_from(field_lifetime_arg) {
                                    param
                                } else {
                                    panic!("'static lifetimes are not yet supported")
                                };

                            if let Some((base_lifetime_param, og)) =
                                base_origin_param_map.get_key_value(&field_lifetime_param)
                            {
                                eprintln!(
                                        "mapping {base_adt_def:?} lifetime parameter {base_lifetime_param:?} to \
                                        {base_adt_def:?}.{:} struct definition lifetime parameter {field_struct_lifetime_param:?}, \
                                        corresponding to its lifetime parameter {field_lifetime_param:?} within {base_adt_def:?}",
                                        field_def.name
                                    );
                                field_origin_param_map.push((*field_struct_lifetime_param, *og));
                            }
                        }
                        let origin_params= self.ltcx.arena().alloc_from_iter(field_origin_param_map.into_iter());
                        Label {
                            origin: None,
                            origin_params,
                            perm
                        }
                    }
                    _ => {
                        Label {
                            origin: None,
                            origin_params: &[],
                            perm
                        }
                    }
                },
            )
    }

    pub fn visit_place(&self, pl: Place<'tcx>) -> LTy<'tcx> {
        let mut lty: LTy = self.local_ltys[pl.local.index()];
        for proj in pl.projection {
            lty = util::lty_project(lty, &proj, &mut |lty, adt, f| self.field_lty(lty, adt, f));
        }
        eprintln!("final label for {pl:?}: {:?}", lty);
        lty
    }

    pub fn visit_operand(&self, op: &Operand<'tcx>) -> LTy<'tcx> {
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
            Rvalue::Use(Operand::Copy(pl)) if matches!(expect_ty.ty.kind(), TyKind::RawPtr(_)) => {
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
                    origin_params: &[],
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
                    origin_params: &[],
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
                    origin_params: &[],
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
            Rvalue::Cast(CastKind::PointerFromExposedAddress, ref op, _ty) => {
                // We support only one case here, which is the case of null pointers
                // constructed via casts such as `0 as *const T`
                if let Some(true) = op.constant().cloned().map(util::is_null_const) {
                    let adt_metadata = self.adt_metadata;

                    // Here we relabel `expect_ty` to utilize the permissions it carries
                    // but substitute the rest of its `Label`s' parts with fresh origins
                    // Othwerise, this is conceptually similar to labeling the cast target
                    // `ty`. We would simply do that, but do not have the information necessary
                    // to set its permissions.
                    self.ltcx.relabel(expect_ty, &mut |lty| {
                        let perm = lty.label.perm;
                        match lty.ty.kind() {
                            TyKind::Ref(_, _, _) | TyKind::RawPtr(_) => {
                                let origin = Some(self.maps.origin());
                                Label {
                                    origin,
                                    origin_params: &[],
                                    perm,
                                }
                            }
                            TyKind::Adt(..) => {
                                let origin_params = construct_adt_origins(
                                    &self.ltcx,
                                    adt_metadata,
                                    &lty.ty,
                                    self.maps,
                                );
                                Label {
                                    origin: None,
                                    origin_params,
                                    perm,
                                }
                            }
                            _ => Label {
                                origin: None,
                                origin_params: &[],
                                perm,
                            },
                        }
                    })
                } else {
                    panic!("Creating non-null pointers from exposed addresses not supported");
                }
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
            Rvalue::Aggregate(ref kind, ref ops) => match **kind {
                AggregateKind::Array(..) => {
                    let ty = rv.ty(self.local_decls, *self.ltcx);
                    // TODO: create fresh origins for all pointers in `ty`, and generate subset
                    // relations between the regions of the array and the regions of its elements
                    self.ltcx.label(ty, &mut |_ty| Label::default())
                }
                AggregateKind::Tuple => {
                    for (op_idx, op) in ops.iter().enumerate() {
                        let op_lty = self.visit_operand(op);
                        self.do_assign(expect_ty.args[op_idx], op_lty);
                    }
                    expect_ty
                }
                AggregateKind::Adt(adt_did, ..) => {
                    /*
                        Generic types are not yet supported because of situations such as the
                        following:

                        ```rust
                        struct S<T> {
                            s: T
                        };

                        fn foo() {
                            let s = S<&'1 u32> { s: &'1 0 }
                        }
                        ```

                        Here the `FieldMetadata` for `S:s` has an empty `origin_args` slice
                        because these are gathered before the lifetime parameters are resolved
                        by traversing the struct definition. Additionally, the label for the
                        `Operand` corresponding to `s` has an `Origin('1)` (because it is
                        a reference). Because the `OriginArgs` are missing for the field, the
                        labeled types for the field operand and for the field declaration do not
                        have the same shape, and so `do_assign` cannot create subset relations.

                        Additionally, the regions referenced within the operand types are
                        erased, and so it also would not be possible to know which ADT
                        OriginParams those correspond to because there would need to be a
                        non-erased region to compare with.
                    */
                    assert_eq!(expect_ty.args.len(), 0, "Generic types not yet supported.");

                    let adt_def = self.tcx.adt_def(adt_did);
                    for (fid, op) in ops.iter().enumerate() {
                        let field_lty = self.field_lty(expect_ty, adt_def, Field::from(fid));
                        let op_lty = self.visit_operand(op);
                        eprintln!("pseudo-assigning fields {field_lty:?} = {op_lty:?}");
                        self.do_assign(field_lty, op_lty);
                    }

                    eprintln!("Aggregate literal label: {expect_ty:?}");
                    expect_ty
                }
                _ => panic!("unsupported rvalue AggregateKind {:?}", kind),
            },

            Rvalue::Len(..) => {
                let ty = rv.ty(self.local_decls, *self.ltcx);
                self.ltcx.label(ty, &mut |_| Label::default())
            }

            Rvalue::UnaryOp(_, ref op) => self.visit_operand(op),

            Rvalue::Repeat(ref op, _) => {
                if op.ty(self.local_decls, self.tcx).is_any_ptr() {
                    todo!("Repeat types over pointers not yet implemented");
                }
                let ty = rv.ty(self.local_decls, *self.ltcx);
                // TODO: create fresh origins for all pointers in `ty`, and generate subset
                // relations between the regions of the array and the regions of its elements
                self.ltcx.label(ty, &mut |_ty| Label::default())
            }
            ref rv => panic!("unsupported rvalue {:?}", rv),
        }
    }

    fn do_assign(&mut self, pl_lty: LTy<'tcx>, rv_lty: LTy<'tcx>) {
        eprintln!("assign {:?} = {:?}", pl_lty, rv_lty);

        let mut add_subset_base = |pl: Origin, rv: Origin| {
            let point = self.current_point(SubPoint::Mid);
            self.facts.subset_base.push((rv, pl, point));
        };

        let pl_origin = pl_lty.label.origin;
        let rv_origin = rv_lty.label.origin;
        if let (Some(pl_origin), Some(rv_origin)) = (pl_origin, rv_origin) {
            add_subset_base(pl_origin, rv_origin);
        }

        for (pl_lty, rv_lty) in pl_lty.iter().zip(rv_lty.iter()) {
            let pl_origin_params = pl_lty.label.origin_params;
            let rv_origin_params = rv_lty.label.origin_params;
            assert_eq!(pl_origin_params.len(), rv_origin_params.len());
            for (&(_, pl_origin), &(_, rv_origin)) in
                pl_origin_params.iter().zip(rv_origin_params.iter())
            {
                add_subset_base(pl_origin, rv_origin)
            }
        }
    }

    pub fn visit_statement(&mut self, stmt: &Statement<'tcx>) {
        let _g = panic_detail::set_current_span(stmt.source_info.span);
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
        let _g = panic_detail::set_current_span(term.source_info.span);
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
                let callee = ty_callee(*self.ltcx, func_ty);
                eprintln!("callee = {callee:?}");
                match callee {
                    Callee::Trivial => {}
                    Callee::UnknownDef { .. } => {
                        // TODO
                    }
                    Callee::LocalDef { .. } => {
                        // TODO
                    }
                    Callee::PtrOffset { .. } => {
                        // We handle this like a pointer assignment.
                        let pl_lty = self.visit_place(destination);
                        assert!(args.len() == 2);
                        let rv_lty = self.visit_operand(&args[0]);
                        self.do_assign(pl_lty, rv_lty);
                    }
                    Callee::SliceAsPtr { .. } => {
                        // TODO: handle this like a cast
                    }
                    Callee::Malloc => {
                        // TODO
                    }
                    Callee::Calloc => {
                        // TODO
                    }
                    Callee::Realloc => {
                        // We handle this like a pointer assignment.
                        let pl_lty = self.visit_place(destination);
                        let rv_lty = assert_matches!(&args[..], [p, _] => {
                            self.visit_operand(p)
                        });

                        self.do_assign(pl_lty, rv_lty);
                    }
                    Callee::Free => {
                        let _pl_lty = self.visit_place(destination);
                        let _rv_lty = assert_matches!(&args[..], [p] => {
                            self.visit_operand(p)
                        });
                    }
                    Callee::IsNull => {
                        let _rv_lty = assert_matches!(&args[..], [p] => {
                            self.visit_operand(p)
                        });
                    }
                }
            }
            // TODO(spernsteiner): handle other `TerminatorKind`s
            _ => (),
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn visit_body<'tcx>(
    tcx: TyCtxt<'tcx>,
    ltcx: LTyCtxt<'tcx>,
    facts: &mut AllFacts,
    maps: &mut AtomMaps<'tcx>,
    loans: &mut HashMap<Local, Vec<(Path, Loan, BorrowKind)>>,
    local_ltys: &[LTy<'tcx>],
    field_permissions: &HashMap<DefId, PermissionSet>,
    mir: &Body<'tcx>,
    adt_metadata: &AdtMetadataTable<'tcx>,
    c_void_casts: &CVoidCasts<'tcx>,
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

    for (block, bb_data) in mir.basic_blocks().iter_enumerated() {
        for (idx, stmt) in bb_data.statements.iter().enumerate() {
            let loc = Location {
                block,
                statement_index: idx,
            };
            tc.current_location = loc;

            if !c_void_casts.should_skip_stmt(loc) {
                tc.visit_statement(stmt);
            }
        }

        tc.current_location = Location {
            block,
            statement_index: bb_data.statements.len(),
        };
        tc.visit_terminator(bb_data.terminator());
    }
}
