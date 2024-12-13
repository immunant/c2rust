use crate::context::{AnalysisCtxt, Assignment, FlagSet, LTy, PermissionSet};
use crate::last_use::{self, LastUse};
use crate::panic_detail;
use crate::pointee_type::PointeeTypes;
use crate::pointer_id::{GlobalPointerTable, PointerTable};
use crate::type_desc::{self, Ownership, Quantity, TypeDesc};
use crate::util::{ty_callee, Callee, UnknownDefCallee};
use log::info;
use rustc_hir::def_id::DefId;
use rustc_middle::mir::{
    BasicBlock, Body, BorrowKind, Location, Operand, Place, PlaceElem, PlaceRef, Rvalue, Statement,
    StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{Ty, TyCtxt, TyKind};
use std::cell::Cell;
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SublocType<'tcx> {
    // TODO: modify to allow arbitrary nesting of `SublocType`s, maybe using `LabeledTy` or
    // `rustc_type_ir`.
    Ptr(TypeDesc<'tcx>),
    Other(Ty<'tcx>),
}


pub struct SublocFnSig<'tcx> {
    pub inputs: Box<[SublocType<'tcx>]>,
    pub output: SublocType<'tcx>,
    pub c_variadic: bool,
}

pub struct SublocGlobalTypes<'tcx> {
    pub fn_sigs: HashMap<DefId, SublocFnSig<'tcx>>,
    pub field_tys: HashMap<DefId, SublocType<'tcx>>,
    pub static_tys: HashMap<DefId, SublocType<'tcx>>,
}


struct TypeConversionContext<'a, 'tcx> {
    tcx: TyCtxt<'tcx>,
    perms: &'a GlobalPointerTable<PermissionSet>,
    flags: &'a GlobalPointerTable<FlagSet>,
    pointee_types: PointerTable<'a, PointeeTypes<'tcx>>,
}

impl<'a, 'tcx> TypeConversionContext<'a, 'tcx> {
    fn lty_to_subloc_types(&self, lty: LTy<'tcx>) -> (SublocType<'tcx>, SublocType<'tcx>) {
        // TODO: Doing this correctly is potentially quite complex.  This function needs to handle
        // (1) FIXED, (2) `pointee_types`, (3) pointer-to-pointer, and (4) existing references in
        // partially-safe code.  Furthermore, the old and new `SublocType`s should be arranged to
        // have the same or similar pointee types when possible.
        //
        // All `LTy` to `SublocType` conversions go through this function, so once it supports a
        // feature, that feature should work throughout the visitor.

        let tcx = self.tcx;
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

    fn lty_to_new_subloc_type(&self, lty: LTy<'tcx>) -> SublocType<'tcx> {
        self.lty_to_subloc_types(lty).1
    }
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
        let conv_cx = TypeConversionContext {
            tcx: self.acx.tcx(),
            perms: self.perms,
            flags: self.flags,
            pointee_types: self.pointee_types,
        };
        conv_cx.lty_to_subloc_types(lty)
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

                self.enter_dest(|v| {
                    v.visit_place(destination, PlaceAccess::Mut);
                });

                self.enter_rvalue(|v| {
                    for (i, arg) in args.iter().enumerate() {
                        v.enter_call_arg(i, |v| v.visit_operand(arg));
                    }
                });

                info!("loc {loc:?}, callee {:?}", ty_callee(tcx, func_ty));
                // Special cases for particular functions.
                match ty_callee(tcx, func_ty) {
                    // Normal call to a local function.
                    Callee::LocalDef { def_id, substs: _ } => {
                        if let Some(lsig) = self.acx.gacx.fn_sigs.get(&def_id) {
                            self.enter_rvalue(|v| v.emit_temp(lsig.output));
                        }
                    }

                    Callee::UnknownDef(UnknownDefCallee::Direct {
                        ty: _,
                        def_id,
                        substs: _,
                        is_foreign: true,
                    }) if self.acx.gacx.known_fn(def_id).is_some() => {
                        // As this is actually a known `fn`, we can treat it as a normal local
                        // call.  It should have a signature in `gacx.fn_sigs`.
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

                    Callee::Trivial |
                    Callee::IsNull |
                    Callee::Free |
                    Callee::SizeOf { .. } => {
                        // These return non-pointers, so the result type should be identical to the
                        // dest type.
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
                self.enter_rvalue_operand(0, |v| {
                    v.visit_operand_with_rvalue_lty(op, Some(rv_lty));
                });
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

    fn visit_operand(
        &mut self,
        op: &Operand<'tcx>,
    ) {
        self.visit_operand_with_rvalue_lty(op, None)
    }

    fn visit_operand_with_rvalue_lty(
        &mut self,
        op: &Operand<'tcx>,
        // Used for a workaround related to string literals
        rvalue_lty: Option<LTy<'tcx>>,
    ) {
        // Currently, `type_of(op)` fails for constant pointers to non-`static`s, such as string
        // literals.  As a workaround, we handle string literals in `Rvalue::Use` by using the
        // `type_of_rvalue` instead.
        let op_is_constant_pointer =
            op.constant().is_some() &&
            op.ty(self.mir, self.acx.tcx()).is_any_ptr();
        if !op_is_constant_pointer {
            let op_lty = self.acx.type_of(op);
            let can_move = false;   // TODO
            let can_mutate = false; // TODO
            self.emit(op_lty, can_move, can_mutate, PlaceAccess::Move);
        } else {
            // Special case for constant pointers
            // TODO: fix `type_of(op)` and remove this workaround
            if let Some(rvalue_lty) = rvalue_lty {
                let can_move = false;   // TODO
                let can_mutate = false; // TODO
                self.emit(rvalue_lty, can_move, can_mutate, PlaceAccess::Move);
            }
        }

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


/// This struct is identical to [`SublocInfo`], but `new_ty` and `expect_ty` (the fields that are
/// potentially modified by the `TypeChecker` pass) are wrapped in `Cell`.
///
/// We use `Cell` instead of `&mut` for mutating `SublocInfo`s to make it easy to hold references
/// to multiple different `SublocInfo`s at the same and across other method calls.
struct TypeCheckerSublocInfo<'tcx> {
    pub old_ty: SublocType<'tcx>,
    pub new_ty: Cell<SublocType<'tcx>>,
    pub expect_ty: Cell<SublocType<'tcx>>,
    pub can_move: bool,
    pub can_mutate: bool,
    pub access: PlaceAccess,
}

impl<'tcx> From<SublocInfo<'tcx>> for TypeCheckerSublocInfo<'tcx> {
    fn from(x: SublocInfo<'tcx>) -> TypeCheckerSublocInfo<'tcx> {
        let SublocInfo { old_ty, new_ty, expect_ty, can_move, can_mutate, access } = x;
        let new_ty = Cell::new(new_ty);
        let expect_ty = Cell::new(expect_ty);
        TypeCheckerSublocInfo { old_ty, new_ty, expect_ty, can_move, can_mutate, access }
    }
}

impl<'tcx> From<TypeCheckerSublocInfo<'tcx>> for SublocInfo<'tcx> {
    fn from(x: TypeCheckerSublocInfo<'tcx>) -> SublocInfo<'tcx> {
        let TypeCheckerSublocInfo { old_ty, new_ty, expect_ty, can_move, can_mutate, access } = x;
        let new_ty = new_ty.into_inner();
        let expect_ty = expect_ty.into_inner();
        SublocInfo { old_ty, new_ty, expect_ty, can_move, can_mutate, access }
    }
}

struct TypeCheckerInfoMap<'tcx>(
    HashMap<(Location, Vec<SubLoc>), TypeCheckerSublocInfo<'tcx>>
);

impl<'tcx> From<HashMap<(Location, Vec<SubLoc>), SublocInfo<'tcx>>> for TypeCheckerInfoMap<'tcx> {
    fn from(x: HashMap<(Location, Vec<SubLoc>), SublocInfo<'tcx>>) -> TypeCheckerInfoMap<'tcx> {
        let y = x.into_iter().map(|(k, v)| (k, v.into())).collect();
        TypeCheckerInfoMap(y)
    }
}

impl<'tcx> From<TypeCheckerInfoMap<'tcx>> for HashMap<(Location, Vec<SubLoc>), SublocInfo<'tcx>> {
    fn from(x: TypeCheckerInfoMap<'tcx>) -> HashMap<(Location, Vec<SubLoc>), SublocInfo<'tcx>> {
        x.0.into_iter().map(|(k, v)| (k, v.into())).collect()
    }
}

/// `SublocInfo` type checker.  The goal of this pass is to ensure that the code is well-typed
/// after applying type rewrites and casts.  Specifically, for each operation in the MIR,
/// performing that operation on inputs whose types match the child nodes' `expect_ty`s should
/// produce an output that matches the operation's `new_ty`.  This can be achieved by modifying the
/// children's `expect_ty`s, the operation's `new_ty`, or both, though it should be done such that
/// each node's `new_ty` can be converted to its `expect_ty` through some valid cast.
struct TypeChecker<'a, 'tcx> {
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    globals: &'a SublocGlobalTypes<'tcx>,
    info_map: &'a TypeCheckerInfoMap<'tcx>,
    mir: &'a Body<'tcx>,
    precise_loc: (Location, Vec<SubLoc>),
}

impl<'a, 'tcx> TypeChecker<'a, 'tcx> {
    pub fn new(
        acx: &'a AnalysisCtxt<'a, 'tcx>,
        globals: &'a SublocGlobalTypes<'tcx>,
        info_map: &'a TypeCheckerInfoMap<'tcx>,
        mir: &'a Body<'tcx>,
    ) -> TypeChecker<'a, 'tcx> {
        TypeChecker {
            acx, globals, info_map, mir,
            precise_loc: (
                Location {
                    block: BasicBlock::from_usize(0),
                    statement_index: 0,
                },
                Vec::new(),
            ),
        }
    }

    fn loc(&self) -> Location {
        self.precise_loc.0
    }

    fn loc_mut(&mut self) -> &mut Location {
        &mut self.precise_loc.0
    }

    fn sub_loc(&self) -> &[SubLoc] {
        &self.precise_loc.1
    }

    fn sub_loc_mut(&mut self) -> &mut Vec<SubLoc> {
        &mut self.precise_loc.1
    }


    fn info(&mut self, path: &[SubLoc]) -> &'a TypeCheckerSublocInfo<'tcx> {
        let old_len = self.sub_loc().len();
        self.sub_loc_mut().extend_from_slice(&path);
        let r = self.info_map.0.get(&self.precise_loc).unwrap_or_else(|| panic!(
            "no subloc_info for {:?}, {:?} + {:?}", self.loc(), self.sub_loc(), path
        ));
        self.sub_loc_mut().truncate(old_len);
        r
    }


    fn enter<F: FnOnce(&mut Self) -> R, R>(&mut self, sub: SubLoc, f: F) -> R {
        self.sub_loc_mut().push(sub);
        let r = f(self);
        self.sub_loc_mut().pop();
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


    fn visit_statement(&mut self, stmt: &Statement<'tcx>, loc: Location) {
        let _g = panic_detail::set_current_span(stmt.source_info.span);
        eprintln!(
            "subloc_info::TypeChecker::visit_statement: {:?} @ {:?}: {:?}",
            loc, stmt.source_info.span, stmt
        );
        *self.loc_mut() = loc;
        debug_assert!(self.sub_loc().is_empty());

        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;

                let pl_info = self.enter_dest(|v| v.visit_place(pl));
                let rv_info = self.enter_rvalue(|v| v.visit_rvalue(rv));
                // Cast RHS to match LHS.
                rv_info.expect_ty.set(pl_info.new_ty.get());
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
        let _g = panic_detail::set_current_span(term.source_info.span);
        eprintln!(
            "subloc_info::TypeChecker::visit_terminator: {:?} @ {:?}: {:?}",
            loc, term.source_info.span, term
        );
        *self.loc_mut() = loc;
        debug_assert!(self.sub_loc().is_empty());

        let tcx = self.acx.tcx();

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
                let pl_info = self.enter_dest(|v| v.visit_place(destination));
                let rv_info = self.enter_rvalue(|v| {
                    let func_ty = func.ty(self.mir, tcx);
                    let callee = ty_callee(tcx, func_ty);
                    v.visit_call(callee, args)
                });
                // Cast RHS to match LHS.
                rv_info.expect_ty.set(pl_info.new_ty.get());


                // TODO: visit the call, args, etc

                /*
                let pl_ty = self.acx.type_of(destination);

                    for (i, arg) in args.iter().enumerate() {
                        v.enter_call_arg(i, |v| v.visit_operand(arg));
                    }
                });

                // Special cases for particular functions.
                */
            }
            TerminatorKind::Assert { .. } => {}
            TerminatorKind::Yield { .. } => {}
            TerminatorKind::GeneratorDrop => {}
            TerminatorKind::FalseEdge { .. } => {}
            TerminatorKind::FalseUnwind { .. } => {}
            TerminatorKind::InlineAsm { .. } => todo!("terminator {:?}", term),
        }
    }

    fn visit_place(&mut self, pl: Place<'tcx>) -> &'a TypeCheckerSublocInfo<'tcx> {
        let info = self.info(&[]);

        // TODO

        info
    }

    fn visit_operand(&mut self, op: &Operand<'tcx>) -> &'a TypeCheckerSublocInfo<'tcx> {
        let info = self.info(&[]);

        // TODO

        info
    }

    fn visit_rvalue(&mut self, rv: &Rvalue<'tcx>) -> &'a TypeCheckerSublocInfo<'tcx> {
        let info = self.info(&[]);

        // TODO

        info
    }

    fn visit_call(
        &mut self,
        callee: Callee<'tcx>,
        args: &[Operand<'tcx>],
    ) -> &'a TypeCheckerSublocInfo<'tcx> {
        let info = self.info(&[]);

        match callee {
            // Normal call to a local function.
            Callee::LocalDef { def_id, substs: _ } => {
                let sig = self.globals.fn_sigs.get(&def_id)
                    .unwrap_or_else(|| panic!("missing sig for {def_id:?}"));
                for (i, (arg, &ty)) in args.iter().zip(sig.inputs.iter()).enumerate() {
                    let arg_info = self.enter_call_arg(i, |v| v.visit_operand(arg));
                    // Cast argument to function parameter type.
                    arg_info.expect_ty.set(ty);
                }
            }

            Callee::PtrOffset { .. } => {
                assert_eq!(args.len(), 2);
                let arg0_info = self.enter_call_arg(0, |v| v.visit_operand(&args[0]));
                self.enter_call_arg(1, |v| v.visit_operand(&args[1]));
                // Result type is always the same as the input type.  The collection pass
                // constrains the input type to be either `Slice` or `OffsetPtr`.
                info.new_ty.set(arg0_info.expect_ty.get());
            }

            Callee::SliceAsPtr { .. } => {
                assert_eq!(args.len(), 1);
                let arg0_info = self.enter_call_arg(0, |v| v.visit_operand(&args[0]));
                // Result type is always the same as the input type.
                info.new_ty.set(arg0_info.expect_ty.get());
            }

            /*
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
            */

            //_ => todo!("visit_call {callee:?}"),
            _ => info!("TODO: visit_call {callee:?}"),

        }

        info
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

pub fn collect_global_subloc_info<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    pointee_types: PointerTable<PointeeTypes<'tcx>>,
) -> SublocGlobalTypes<'tcx> {
    let conv_cx = TypeConversionContext {
        tcx: acx.tcx(),
        perms: &asn.perms,
        flags: &asn.flags,
        pointee_types,
    };

    let fn_sigs = acx.gacx.fn_sigs.iter().map(|(&def_id, sig)| {
        let inputs = sig.inputs.iter().map(|lty| conv_cx.lty_to_new_subloc_type(lty)).collect();
        let output = conv_cx.lty_to_new_subloc_type(sig.output);
        (def_id, SublocFnSig {
            inputs, output,
            c_variadic: sig.c_variadic,
        })
    }).collect::<HashMap<_, _>>();

    let field_tys = acx.gacx.field_ltys.iter().map(|(&def_id, &lty)| {
        (def_id, conv_cx.lty_to_new_subloc_type(lty))
    }).collect::<HashMap<_, _>>();

    let static_tys = acx.gacx.static_tys.iter().map(|(&def_id, &lty)| {
        (def_id, conv_cx.lty_to_new_subloc_type(lty))
    }).collect::<HashMap<_, _>>();

    SublocGlobalTypes { fn_sigs, field_tys, static_tys }
}

pub fn typecheck_subloc_info<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    globals: &SublocGlobalTypes<'tcx>,
    info_map: HashMap<(Location, Vec<SubLoc>), SublocInfo<'tcx>>,
    mir: &Body<'tcx>,
) -> HashMap<(Location, Vec<SubLoc>), SublocInfo<'tcx>> {
    let info_map = TypeCheckerInfoMap::from(info_map);
    let mut v = TypeChecker::new(acx, globals, &info_map, mir);

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

    info_map.into()
}
