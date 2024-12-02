use crate::context::{AnalysisCtxt, Assignment, FlagSet, LTy, PermissionSet};
use crate::last_use::{self, LastUse};
use crate::panic_detail;
use crate::pointee_type::PointeeTypes;
use crate::pointer_id::{GlobalPointerTable, PointerTable};
use crate::type_desc::{self, Ownership, Quantity, TypeDesc};
use crate::util::{ty_callee, Callee};
use rustc_middle::mir::{
    BasicBlock, Body, BorrowKind, Location, Operand, Place, PlaceElem, PlaceRef, Rvalue, Statement,
    StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{Ty, TyKind};
use std::collections::HashMap;
use super::mir_op::{SubLoc, PlaceAccess};

#[derive(Clone, Debug)]
pub struct SublocInfo<'tcx> {
    /// The type of this node in the original MIR.
    pub old_ty: SublocType<'tcx>,
    /// The type this node would have after rewriting, if the MIR was unchanged.  For example, if
    /// this node is an `Operand` that reads from a `Local`, this would give the rewritten type of
    /// that `Local`.
    pub new_ty: SublocType<'tcx>,
    /// The type required by the surrounding context.  For example, in `StatementKind::Assign`, the
    /// `expect_ty` fields of the `Place` and `Rvalue` will match, reflecting the fact that the LHS
    /// and RHS must have the same type.  If `new_ty != expect_ty`, then a cast must be inserted at
    /// this node.
    ///
    /// This is set to `new_ty` by the initial pass, but in a later pass may be updated to fix type
    /// errors that would be introduced by naïve rewriting.
    pub expect_ty: SublocType<'tcx>,
    /// If set, casts applied to this subloc can move the value if needed.  Otherwise, casts may
    /// only borrow the value.
    pub can_move: bool,
    /// If set, casts applied to this subloc can borrow the value mutably if needed.  Otherwise,
    /// casts may only borrow the value immutably (or move it, if `can_move` is set).
    pub can_mutate: bool,
    /// Indicates the type of access the surrounding context will perform on this subloc.  This is
    /// mainly relevant for `Place`s.
    pub access: PlaceAccess,
}

#[derive(Clone, Copy, Debug)]
pub enum SublocType<'tcx> {
    // TODO: modify to allow arbitrary nesting of `SublocType`s, maybe using `LabeledTy` or
    // `rustc_type_ir`.
    Ptr(TypeDesc<'tcx>),
    Other(Ty<'tcx>),
}


struct CollectInfoVisitor<'a, 'tcx> {
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    perms: &'a GlobalPointerTable<PermissionSet>,
    flags: &'a GlobalPointerTable<FlagSet>,
    pointee_types: PointerTable<'a, PointeeTypes<'tcx>>,
    last_use: &'a LastUse,
    mir: &'a Body<'tcx>,
    loc: Location,
    sub_loc: Vec<SubLoc>,
    info_map: HashMap<(Location, Vec<SubLoc>), SublocInfo<'tcx>>,
}

impl<'a, 'tcx> CollectInfoVisitor<'a, 'tcx> {
    pub fn new(
        acx: &'a AnalysisCtxt<'a, 'tcx>,
        asn: &'a Assignment,
        pointee_types: PointerTable<'a, PointeeTypes<'tcx>>,
        last_use: &'a LastUse,
        mir: &'a Body<'tcx>,
    ) -> CollectInfoVisitor<'a, 'tcx> {
        let perms = asn.perms();
        let flags = asn.flags();
        CollectInfoVisitor {
            acx,
            perms,
            flags,
            pointee_types,
            last_use,
            mir,
            loc: Location {
                block: BasicBlock::from_usize(0),
                statement_index: 0,
            },
            sub_loc: Vec::new(),

            info_map: HashMap::new(),
        }
    }

    fn enter<F: FnOnce(&mut Self) -> R, R>(&mut self, sub: SubLoc, f: F) -> R {
        self.sub_loc.push(sub);
        let r = f(self);
        self.sub_loc.pop();
        r
    }

    fn enter_dest<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::Dest, f)
    }

    fn enter_rvalue<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::Rvalue, f)
    }

    fn enter_call_arg<F: FnOnce(&mut Self) -> R, R>(&mut self, i: usize, f: F) -> R {
        self.enter(SubLoc::CallArg(i), f)
    }

    fn enter_rvalue_operand<F: FnOnce(&mut Self) -> R, R>(&mut self, i: usize, f: F) -> R {
        self.enter(SubLoc::RvalueOperand(i), f)
    }

    fn enter_rvalue_place<F: FnOnce(&mut Self) -> R, R>(&mut self, i: usize, f: F) -> R {
        self.enter(SubLoc::RvaluePlace(i), f)
    }

    fn enter_operand_place<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::OperandPlace, f)
    }

    fn enter_place_deref_pointer<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::PlaceDerefPointer, f)
    }

    fn enter_place_field_base<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::PlaceFieldBase, f)
    }

    fn enter_place_index_array<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::PlaceIndexArray, f)
    }


    fn lty_to_subloc_types(&self, lty: LTy<'tcx>) -> (SublocType<'tcx>, SublocType<'tcx>) {
        // TODO: Doing this correctly is potentially quite complex.  This function needs to handle
        // (1) FIXED, (2) `pointee_types`, (3) pointer-to-pointer, and (4) existing references in
        // partially-safe code.  Furthermore, the old and new `SublocType`s should be arranged to
        // have the same or similar pointee types when possible.
        //
        // All `LTy` to `SublocType` conversions go through this function, so once it supports a
        // feature, that feature should work throughout the visitor.

        let tcx = self.acx.tcx();
        let ptr = lty.label;

        if ptr.is_none() {
            // Non-pointer case.
            return (SublocType::Other(lty.ty), SublocType::Other(lty.ty));
        }

        let old_pointee_ty = match *lty.ty.kind() {
            TyKind::Ref(_, ty, _) => ty,
            TyKind::RawPtr(mt) => mt.ty,
            TyKind::Adt(adt_def, substs) if adt_def.is_box() => substs.type_at(0),
            _ => unreachable!("type {lty:?} has PointerId but has non-pointer TyKind?"),
        };
        let old_ptr_desc = type_desc::unpack_pointer_type(tcx, lty.ty, old_pointee_ty);
        let old_desc = old_ptr_desc.to_type_desc(old_pointee_ty);
        let mut new_desc = type_desc::perms_to_desc_with_pointee(
            tcx,
            old_pointee_ty,
            lty.ty,
            self.perms[ptr],
            self.flags[ptr],
        );

        if let Some(pointee_lty) = self.pointee_types[ptr].get_sole_lty() {
            new_desc.pointee_ty = pointee_lty.ty;
        }

        (SublocType::Ptr(old_desc), SublocType::Ptr(new_desc))
    }

    fn emit_info(&mut self, info: SublocInfo<'tcx>) {
        let key = (self.loc, self.sub_loc.clone());
        let old = self.info_map.insert(key, info);
        assert!(old.is_none(), "duplicate entry for {:?} {:?}", self.loc, self.sub_loc);
    }

    fn emit(
        &mut self,
        lty: LTy<'tcx>,
        can_move: bool,
        can_mutate: bool,
        access: PlaceAccess,
    ) {
        self.emit_adjust(lty, can_move, can_mutate, access, |x| x)
    }

    fn emit_adjust(
        &mut self,
        lty: LTy<'tcx>,
        can_move: bool,
        can_mutate: bool,
        access: PlaceAccess,
        adjust_new_ty: impl FnOnce(SublocType<'tcx>) -> SublocType<'tcx>,
    ) {
        let (old_ty, new_ty) = self.lty_to_subloc_types(lty);
        let new_ty = adjust_new_ty(new_ty);
        self.emit_info(SublocInfo {
            old_ty,
            new_ty,
            expect_ty: new_ty,
            can_move,
            can_mutate,
            access,
        });
    }

    /// Helper for temporaries / rvalues.  These can always be moved and mutated, since they are
    /// only temporary.
    fn emit_temp(&mut self, lty: LTy<'tcx>) {
        self.emit_temp_adjust(lty, |x| x)
    }

    fn emit_temp_adjust(
        &mut self,
        lty: LTy<'tcx>,
        adjust_new_ty: impl FnOnce(SublocType<'tcx>) -> SublocType<'tcx>,
    ) {
        self.emit_adjust(lty, true, true, PlaceAccess::Move, adjust_new_ty)
    }


    /// Check whether the `Place` identified by `which` within the current statement or terminator
    /// is the last use of a local.
    fn is_last_use(&self, which: last_use::WhichPlace) -> bool {
        self.last_use.is_last_use(self.loc, which)
    }

    fn current_sub_loc_as_which_place(&self) -> Option<last_use::WhichPlace> {
        // This logic is a bit imprecise, but should be correct in the cases where we actually call
        // it (namely, when the `Place`/`Rvalue` is simply a `Local`).
        let mut which = None;
        for sl in &self.sub_loc {
            match *sl {
                SubLoc::Dest => { which = Some(last_use::WhichPlace::Lvalue); },
                SubLoc::Rvalue => {
                    which = Some(last_use::WhichPlace::Operand(0));
                },
                SubLoc::CallArg(i) => {
                    // In a call, `WhichPlace::Operand(0)` refers to the callee function.
                    which = Some(last_use::WhichPlace::Operand(i + 1));
                },
                SubLoc::RvalueOperand(i) => {
                    which = Some(last_use::WhichPlace::Operand(i));
                },
                SubLoc::RvaluePlace(_) => {},
                SubLoc::OperandPlace => {},
                SubLoc::PlaceDerefPointer => {},
                SubLoc::PlaceFieldBase => {},
                SubLoc::PlaceIndexArray => {},
            }
        }
        which
    }

    /// Like `is_last_use`, but infers `WhichPlace` based on `self.sub_loc`.
    fn current_sub_loc_is_last_use(&self) -> bool {
        if let Some(which) = self.current_sub_loc_as_which_place() {
            self.is_last_use(which)
        } else {
            false
        }
    }

    fn visit_statement(&mut self, stmt: &Statement<'tcx>, loc: Location) {
        let _g = panic_detail::set_current_span(stmt.source_info.span);
        eprintln!(
            "subloc_info::visit_statement: {:?} @ {:?}: {:?}",
            loc, stmt.source_info.span, stmt
        );
        self.loc = loc;
        debug_assert!(self.sub_loc.is_empty());

        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;

                self.enter_dest(|v| {
                    v.visit_place(pl, PlaceAccess::Mut);
                });

                self.enter_rvalue(|v| {
                    v.visit_rvalue(rv);
                });
            }
            StatementKind::FakeRead(..) => {}
            StatementKind::SetDiscriminant { .. } => todo!("statement {:?}", stmt),
            StatementKind::Deinit(..) => {}
            StatementKind::StorageLive(..) => {}
            StatementKind::StorageDead(..) => {}
            StatementKind::Retag(..) => {}
            StatementKind::AscribeUserType(..) => {}
            StatementKind::Coverage(..) => {}
            StatementKind::CopyNonOverlapping(..) => todo!("statement {:?}", stmt),
            StatementKind::Nop => {}
        }
    }

    fn visit_terminator(&mut self, term: &Terminator<'tcx>, loc: Location) {
        let tcx = self.acx.tcx();
        let _g = panic_detail::set_current_span(term.source_info.span);
        self.loc = loc;
        debug_assert!(self.sub_loc.is_empty());

        match term.kind {
            TerminatorKind::Goto { .. } => {}
            TerminatorKind::SwitchInt { .. } => {}
            TerminatorKind::Resume => {}
            TerminatorKind::Abort => {}
            TerminatorKind::Return => {}
            TerminatorKind::Unreachable => {}
            TerminatorKind::Drop { .. } => {}
            TerminatorKind::DropAndReplace { .. } => {}
            TerminatorKind::Call {
                ref func,
                ref args,
                destination,
                target: _,
                ..
            } => {
                let func_ty = func.ty(self.mir, tcx);
                let pl_ty = self.acx.type_of(destination);

                self.enter_rvalue(|v| {
                    for (i, arg) in args.iter().enumerate() {
                        v.enter_call_arg(i, |v| v.visit_operand(arg));
                    }
                });

                // Special cases for particular functions.
                match ty_callee(tcx, func_ty) {
                    // Normal call to a local function.
                    Callee::LocalDef { def_id, substs: _ } => {
                        if let Some(lsig) = self.acx.gacx.fn_sigs.get(&def_id) {
                            self.enter_rvalue(|v| v.emit_temp(lsig.output));
                        }
                    }

                    Callee::PtrOffset { .. } => {
                        // Set the call result type to match the destination type as closely as we
                        // can.  The argument will be downcast to this type, then offset.  However,
                        // our `PtrOffset` rewrite can't output `Single` or `Array` directly, so in
                        // those cases, the rewrite will operate on `Slice` and downcast afterward.
                        self.enter_rvalue(|v| v.emit_temp_adjust(pl_ty, |mut slty| {
                            if let SublocType::Ptr(ref mut desc) = slty {
                                match desc.qty {
                                    Quantity::Single | Quantity::Array => {
                                        desc.qty = Quantity::Slice;
                                    },
                                    Quantity::Slice | Quantity::OffsetPtr => {},
                                }
                            }
                            slty
                        }));
                    }

                    Callee::SliceAsPtr { .. } => {
                        // Set result type equal to the argument type.  The rewriter will insert an
                        // appropriate cast to convert to the destination type.
                        assert_eq!(args.len(), 1);
                        let arg_lty = self.acx.type_of(&args[0]);
                        self.enter_rvalue(|v| v.emit_temp(arg_lty))
                    }

                    Callee::Memcpy | Callee::Memset => {
                        // Match the type of the first (`dest`) argument exactly.  The rewrite is
                        // responsible for preserving any combination of `Ownership` and
                        // `Quantity`.
                        assert_eq!(args.len(), 3);
                        let arg_lty = self.acx.type_of(&args[0]);
                        self.enter_rvalue(|v| v.emit_temp(arg_lty));
                    }

                    Callee::IsNull => {
                        // Result type is simply `bool`, which should be the same as the dest type.
                        self.enter_rvalue(|v| v.emit_temp(pl_ty));
                    }

                    Callee::Null { .. } => {
                        // Match the destination type, but `null()` always outputs a nullable
                        // pointer.
                        self.enter_rvalue(|v| v.emit_temp_adjust(pl_ty, |mut slty| {
                            if let SublocType::Ptr(ref mut desc) = slty {
                                desc.option = true;
                            }
                            slty
                        }));
                    }

                    Callee::Malloc | Callee::Calloc | Callee::Realloc => {
                        // Match the destination type, but always produce a non-optional `Box`.
                        self.enter_rvalue(|v| v.emit_temp_adjust(pl_ty, |mut slty| {
                            if let SublocType::Ptr(ref mut desc) = slty {
                                desc.option = false;
                                desc.own = Ownership::Box;
                            }
                            slty
                        }));
                    }

                    Callee::Free => {
                        // Result type is simply `()`, which should be the same as the dest type.
                        self.enter_rvalue(|v| v.emit_temp(pl_ty));
                    }

                    _ => {}
                }
            }
            TerminatorKind::Assert { .. } => {}
            TerminatorKind::Yield { .. } => {}
            TerminatorKind::GeneratorDrop => {}
            TerminatorKind::FalseEdge { .. } => {}
            TerminatorKind::FalseUnwind { .. } => {}
            TerminatorKind::InlineAsm { .. } => todo!("terminator {:?}", term),
        }
    }

    fn visit_rvalue(&mut self, rv: &Rvalue<'tcx>) {
        eprintln!("subloc_info::visit_rvalue: {:?}", rv);

        let rv_lty = self.acx.type_of_rvalue(rv, self.loc);
        let can_move = false;   // TODO
        let can_mutate = false; // TODO
        self.emit(rv_lty, can_move, can_mutate, PlaceAccess::Move);

        match *rv {
            Rvalue::Use(ref op) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(op));
            }
            Rvalue::Repeat(ref op, _) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(op));
            }
            Rvalue::Ref(_rg, kind, pl) => {
                let mutbl = match kind {
                    BorrowKind::Mut { .. } => true,
                    BorrowKind::Shared | BorrowKind::Shallow | BorrowKind::Unique => false,
                };
                self.enter_rvalue_place(0, |v| v.visit_place(pl, PlaceAccess::from_bool(mutbl)));
            }
            Rvalue::ThreadLocalRef(_def_id) => {
                todo!("Rvalue::ThreadLocalRef")
            }
            Rvalue::AddressOf(mutbl, pl) => {
                self.enter_rvalue_place(0, |v| v.visit_place(pl, PlaceAccess::from_mutbl(mutbl)));
            }
            Rvalue::Len(pl) => {
                self.enter_rvalue_place(0, |v| v.visit_place(pl, PlaceAccess::Imm));
            }
            Rvalue::Cast(_kind, ref op, _ty) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(op));
            }
            Rvalue::BinaryOp(_bop, ref ops) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(&ops.0));
                self.enter_rvalue_operand(1, |v| v.visit_operand(&ops.1));
            }
            Rvalue::CheckedBinaryOp(_bop, ref ops) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(&ops.0));
                self.enter_rvalue_operand(1, |v| v.visit_operand(&ops.1));
            }
            Rvalue::NullaryOp(..) => {}
            Rvalue::UnaryOp(_uop, ref op) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(op));
            }
            Rvalue::Discriminant(pl) => {
                self.enter_rvalue_place(0, |v| v.visit_place(pl, PlaceAccess::Imm));
            }
            Rvalue::Aggregate(ref _kind, ref ops) => {
                for (i, op) in ops.iter().enumerate() {
                    self.enter_rvalue_operand(i, |v| v.visit_operand(op));
                }
            }
            Rvalue::ShallowInitBox(ref op, _ty) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(op));
            }
            Rvalue::CopyForDeref(pl) => {
                self.enter_rvalue_place(0, |v| v.visit_place(pl, PlaceAccess::Imm));
            }
        }
    }

    fn visit_operand(&mut self, op: &Operand<'tcx>) {
        match *op {
            Operand::Copy(pl) | Operand::Move(pl) => {
                let pl_lty = self.acx.type_of(pl);
                // Moving out of a `DynOwned` pointer requires `Mut` access rather than `Move`.
                // TODO: this should probably check `desc.dyn_owned` rather than perms directly
                let access = if !pl_lty.label.is_none() && self.perms[pl_lty.label].contains(PermissionSet::FREE) {
                    PlaceAccess::Mut
                } else {
                    PlaceAccess::Move
                };
                self.enter_operand_place(|v| v.visit_place(pl, access));
            }
            Operand::Constant(..) => {}
        }
    }

    /*
    /// Like [`Self::visit_operand`], but takes an expected `TypeDesc` instead of an expected `LTy`.
    fn visit_operand_desc(&mut self, op: &Operand<'tcx>) {
        match *op {
            Operand::Copy(pl) | Operand::Move(pl) => {
                let pl_lty = self.acx.type_of(pl);
                // Moving out of a `DynOwned` pointer requires `Mut` access rather than `Move`.
                // TODO: this should probably check `desc.dyn_owned` rather than perms directly
                let access = if !pl_lty.label.is_none() && self.perms[pl_lty.label].contains(PermissionSet::FREE) {
                    PlaceAccess::Mut
                } else {
                    PlaceAccess::Move
                };
                self.enter_operand_place(|v| v.visit_place(pl, access, RequireSinglePointer::Yes));

                if !pl_lty.label.is_none() {
                    self.emit_cast_lty_desc(pl_lty, expect_desc);
                }
            }
            Operand::Constant(..) => {}
        }
    }
    */

    fn visit_place(
        &mut self,
        pl: Place<'tcx>,
        access: PlaceAccess,
    ) {
        let mut ltys = Vec::with_capacity(1 + pl.projection.len());
        ltys.push(self.acx.type_of(pl.local));
        for proj in pl.projection {
            let prev_lty = ltys.last().copied().unwrap();
            ltys.push(self.acx.projection_lty(prev_lty, &proj));
        }
        self.visit_place_ref(pl.as_ref(), &ltys, access);
    }

    /// Generate rewrites for a `Place` represented as a `PlaceRef`.  `proj_ltys` gives the `LTy`
    /// for the `Local` and after each projection.  `access` describes how the place is being used:
    /// immutably, mutably, or being moved out of.
    fn visit_place_ref(
        &mut self,
        pl: PlaceRef<'tcx>,
        proj_ltys: &[LTy<'tcx>],
        access: PlaceAccess,
    ) {
        let (&last_proj, rest) = match pl.projection.split_last() {
            Some(x) => x,
            None => {
                // `pl` is just a local with no projections.
                let pl_lty = proj_ltys[0];
                let can_move = self.current_sub_loc_is_last_use();
                // We assume all locals are mutable, or at least can be made mutable if needed.
                let can_mutate = true;
                self.emit(pl_lty, can_move, can_mutate, access);
                return;
            },
        };

        // TODO: downgrade Move to Imm if the new type is Copy

        let proj_lty = proj_ltys[pl.projection.len()];
        let can_move = false;   // TODO
        let can_mutate = false; // TODO
        self.emit(proj_lty, can_move, can_mutate, access);

        let base_pl = PlaceRef {
            local: pl.local,
            projection: rest,
        };
        match last_proj {
            PlaceElem::Deref => {
                self.enter_place_deref_pointer(|v| {
                    v.visit_place_ref(base_pl, proj_ltys, access);
                });
            }
            PlaceElem::Field(_idx, _ty) => {
                self.enter_place_field_base(|v| v.visit_place_ref(base_pl, proj_ltys, access));
            }
            PlaceElem::Index(_) | PlaceElem::ConstantIndex { .. } | PlaceElem::Subslice { .. } => {
                self.enter_place_index_array(|v| v.visit_place_ref(base_pl, proj_ltys, access));
            }
            PlaceElem::Downcast(_, _) => todo!("PlaceElem::Downcast"),
        }
    }
}

pub fn collect_subloc_info<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    pointee_types: PointerTable<PointeeTypes<'tcx>>,
    last_use: &LastUse,
    mir: &Body<'tcx>,
) -> HashMap<(Location, Vec<SubLoc>), SublocInfo<'tcx>> {
    let mut v = CollectInfoVisitor::new(acx, asn, pointee_types, last_use, mir);

    for (bb_id, bb) in mir.basic_blocks().iter_enumerated() {
        for (i, stmt) in bb.statements.iter().enumerate() {
            let loc = Location {
                block: bb_id,
                statement_index: i,
            };
            v.visit_statement(stmt, loc);
        }

        if let Some(ref term) = bb.terminator {
            let loc = Location {
                block: bb_id,
                statement_index: bb.statements.len(),
            };
            v.visit_terminator(term, loc);
        }
    }

    v.info_map
}
