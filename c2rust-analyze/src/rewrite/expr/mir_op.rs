//! Rewriting of expressions comes with one extra bit of complexity: sometimes the code we're
//! modifying has had autoderef and/or autoref `Adjustment`s applied to it. To avoid unexpectedly
//! changing which adjustments get applied, we "materialize" the `Adjustment`s, making them
//! explicit in the source code. For example, `vec.len()`, which implicitly applies deref and ref
//! adjustments to `vec`, would be converted to `(&*vec).len()`, where the deref and ref operations
//! are explicit, and might be further rewritten from there. However, we don't want to materialize
//! all adjustments, as this would make even non-rewritten code extremely verbose, so we try to
//! materialize adjustments only on code that's subject to some rewrite.

use crate::context::{AnalysisCtxt, Assignment, FlagSet, LTy, PermissionSet, PointerId};
use crate::pointer_id::PointerTable;
use crate::type_desc::{self, Ownership, Quantity, TypeDesc};
use crate::util::{ty_callee, Callee};
use rustc_ast::Mutability;
use rustc_middle::mir::{
    BasicBlock, Body, Location, Operand, Place, Rvalue, Statement, StatementKind, Terminator,
    TerminatorKind,
};
use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SubLoc {
    /// The LHS of an assignment or call.  `StatementKind::Assign/TerminatorKind::Call -> Place`
    Dest,
    /// The RHS of an assignment.  `StatementKind::Assign -> Rvalue`
    AssignRvalue,
    /// The Nth argument of a call.  `TerminatorKind::Call -> Operand`
    CallArg(usize),
    /// The Nth operand of an rvalue.  `Rvalue -> Operand`
    RvalueOperand(usize),
    /// The place referenced by an operand.  `Operand::Move/Operand::Copy -> Place`
    OperandPlace,
    /// The pointer used in the Nth innermost deref within a place.  `Place -> Place`
    PlacePointer(usize),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum RewriteKind {
    /// Replace `ptr.offset(i)` with something like `&ptr[i..]`.
    OffsetSlice { mutbl: bool },
    /// Replace `slice` with `&slice[0]`.
    SliceFirst { mutbl: bool },
    /// Replace `ptr` with `&*ptr`, converting `&mut T` to `&T`.
    MutToImm,
    /// Remove a call to `as_ptr` or `as_mut_ptr`.
    RemoveAsPtr,
    /// Replace &raw with & or &raw mut with &mut
    RawToRef { mutbl: bool },
    /// Replace `y` in `let x = y` with `Cell::new(y)`, i.e. `let x = Cell::new(y)`
    /// TODO: ensure `y` implements `Copy`
    CellNew,
    /// Replace `*y` with `Cell::get(y)` where `y` is a pointer
    CellGet,
    /// Replace `*y = x` with `Cell::set(x)` where `y` is a pointer
    CellSet,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MirRewrite {
    pub kind: RewriteKind,
    pub sub_loc: Vec<SubLoc>,
}

struct ExprRewriteVisitor<'a, 'tcx> {
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    perms: PointerTable<'a, PermissionSet>,
    flags: PointerTable<'a, FlagSet>,
    rewrites: &'a mut HashMap<Location, Vec<MirRewrite>>,
    mir: &'a Body<'tcx>,
    loc: Location,
    sub_loc: Vec<SubLoc>,
}

impl<'a, 'tcx> ExprRewriteVisitor<'a, 'tcx> {
    pub fn new(
        acx: &'a AnalysisCtxt<'a, 'tcx>,
        asn: &'a Assignment,
        rewrites: &'a mut HashMap<Location, Vec<MirRewrite>>,
        mir: &'a Body<'tcx>,
    ) -> ExprRewriteVisitor<'a, 'tcx> {
        let perms = asn.perms();
        let flags = asn.flags();
        ExprRewriteVisitor {
            acx,
            perms,
            flags,
            rewrites,
            mir,
            loc: Location {
                block: BasicBlock::from_usize(0),
                statement_index: 0,
            },
            sub_loc: Vec::new(),
        }
    }

    fn enter<F: FnOnce(&mut Self) -> R, R>(&mut self, sub: SubLoc, f: F) -> R {
        self.sub_loc.push(sub);
        let r = f(self);
        self.sub_loc.pop();
        r
    }

    #[allow(dead_code)]
    fn _enter_dest<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::Dest, f)
    }

    fn enter_assign_rvalue<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::AssignRvalue, f)
    }

    fn enter_call_arg<F: FnOnce(&mut Self) -> R, R>(&mut self, i: usize, f: F) -> R {
        self.enter(SubLoc::CallArg(i), f)
    }

    fn enter_rvalue_operand<F: FnOnce(&mut Self) -> R, R>(&mut self, i: usize, f: F) -> R {
        self.enter(SubLoc::RvalueOperand(i), f)
    }

    #[allow(dead_code)]
    fn _enter_operand_place<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::OperandPlace, f)
    }

    #[allow(dead_code)]
    fn _enter_place_pointer<F: FnOnce(&mut Self) -> R, R>(&mut self, i: usize, f: F) -> R {
        self.enter(SubLoc::PlacePointer(i), f)
    }

    fn visit_statement(&mut self, stmt: &Statement<'tcx>, loc: Location) {
        self.loc = loc;
        debug_assert!(self.sub_loc.is_empty());

        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;

                if matches!(rv, Rvalue::Cast(..)) && self.acx.c_void_casts.should_skip_stmt(loc) {
                    // This is a cast to or from `void*` associated with a `malloc`, `free`, or
                    // other libc call.
                    //
                    // TODO: we should probably emit a rewrite here to remove the cast; then when
                    // we implement rewriting of the actual call, it won't need to deal with
                    // additional rewrites at a separate location from the call itself.
                    return;
                }

                let pl_lty = self.acx.type_of(pl);

                if pl.is_indirect() && self.acx.local_tys[pl.local].ty.is_any_ptr() {
                    // FIXME: should use pl_lty instead
                    let local_lty = self.acx.local_tys[pl.local];
                    let perms = self.perms[local_lty.label];
                    let flags = self.flags[local_lty.label];
                    let desc = type_desc::perms_to_desc(local_lty.ty, perms, flags);
                    if desc.own == Ownership::Cell {
                        // this is an assignment like `*x = 2` but `x` has CELL permissions
                        self.enter_assign_rvalue(|v| v.emit(RewriteKind::CellSet))
                    }
                }

                #[allow(clippy::single_match)]
                match rv {
                    Rvalue::Use(rv_op) => {
                        let local_ty = self.acx.local_tys[pl.local].ty;
                        let local_addr = self.acx.addr_of_local[pl.local];
                        let perms = self.perms[local_addr];
                        let flags = self.flags[local_addr];
                        let desc = type_desc::local_perms_to_desc(local_ty, perms, flags);
                        if desc.own == Ownership::Cell {
                            // this is an assignment like `let x = 2` but `x` has CELL permissions
                            self.enter_assign_rvalue(|v| {
                                v.enter_rvalue_operand(0, |v| v.emit(RewriteKind::CellNew))
                            })
                        }

                        if let Some(rv_place) = rv_op.place() {
                            if rv_place.is_indirect()
                                && self.acx.local_tys[rv_place.local].ty.is_any_ptr()
                            {
                                let local_lty = self.acx.local_tys[rv_place.local];
                                let perms = self.perms[local_lty.label];
                                let flags = self.flags[local_lty.label];
                                let desc = type_desc::perms_to_desc(local_lty.ty, perms, flags);
                                if desc.own == Ownership::Cell {
                                    // this is an assignment like `let x = *y` but `y` has CELL permissions
                                    self.enter_assign_rvalue(|v| {
                                        v.enter_rvalue_operand(0, |v| v.emit(RewriteKind::CellGet))
                                    })
                                }
                            }
                        }
                    }
                    _ => {}
                };

                self.enter_assign_rvalue(|v| v.visit_rvalue(rv, pl_lty));
                // TODO: visit place
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

                // Special cases for particular functions.
                match ty_callee(tcx, func_ty) {
                    Callee::PtrOffset { .. } => {
                        self.visit_ptr_offset(&args[0], pl_ty);
                        return;
                    }
                    Callee::SliceAsPtr { .. } => {
                        self.visit_slice_as_ptr(&args[0], pl_ty);
                        return;
                    }
                    _ => {}
                }

                // General case: cast `args` to match the signature of `func`.
                let poly_sig = func_ty.fn_sig(tcx);
                let sig = tcx.erase_late_bound_regions(poly_sig);

                for (i, _op) in args.iter().enumerate() {
                    if i >= sig.inputs().len() {
                        // This is a call to a variadic function, and we've gone past the end of
                        // the declared arguments.
                        // TODO: insert a cast to turn `op` back into its original declared type
                        // (i.e. upcast the chosen reference type back to a raw pointer)
                        continue;
                    }

                    // TODO: get the `LTy` to use for the callee's argument
                    // let expect_ty = ...;
                    // self.enter_call_arg(i, |v| v.visit_operand(op, expect_ty));
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

    fn visit_rvalue(&mut self, rv: &Rvalue<'tcx>, expect_ty: LTy<'tcx>) {
        // TODO: most of these cases need to recurse into operands/places to find derefs
        match *rv {
            Rvalue::Use(ref op) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(op, expect_ty));
            }
            Rvalue::Repeat(ref _op, _) => {
                // TODO
            }
            Rvalue::Ref(_rg, _kind, _pl) => {
                // TODO
            }
            Rvalue::ThreadLocalRef(_def_id) => {
                // TODO
            }
            Rvalue::AddressOf(mutbl, _pl) => {
                let desc = type_desc::perms_to_desc(
                    expect_ty.ty,
                    self.perms[expect_ty.label],
                    self.flags[expect_ty.label],
                );
                self.enter_rvalue_operand(0, |v| match desc.own {
                    Ownership::Cell => v.emit(RewriteKind::RawToRef { mutbl: false }),
                    Ownership::Imm | Ownership::Mut => v.emit(RewriteKind::RawToRef {
                        mutbl: mutbl == Mutability::Mut,
                    }),
                    _ => (),
                });
            }
            Rvalue::Len(_pl) => {
                // TODO
            }
            Rvalue::Cast(_kind, ref _op, _ty) => {
                // TODO
            }
            Rvalue::BinaryOp(_bop, ref _ops) => {
                // TODO
            }
            Rvalue::CheckedBinaryOp(_bop, ref _ops) => {
                // TODO
            }
            Rvalue::NullaryOp(..) => {}
            Rvalue::UnaryOp(_uop, ref _op) => {
                // TODO
            }
            Rvalue::Discriminant(_pl) => {
                // TODO
            }
            Rvalue::Aggregate(ref _kind, ref _ops) => {
                // TODO
            }
            Rvalue::ShallowInitBox(ref _op, _ty) => {
                // TODO
            }
            Rvalue::CopyForDeref(pl) => {
                self.enter_rvalue_operand(0, |v| v.visit_place(pl, expect_ty));
            }
        }
    }

    fn visit_operand(&mut self, op: &Operand<'tcx>, expect_ty: LTy<'tcx>) {
        match *op {
            Operand::Copy(pl) | Operand::Move(pl) => {
                self.visit_place(pl, expect_ty);
            }
            Operand::Constant(..) => {}
        }
    }

    fn visit_place(&mut self, pl: Place<'tcx>, expect_ty: LTy<'tcx>) {
        let ptr_lty = self.acx.type_of(pl);
        if !ptr_lty.label.is_none() {
            self.emit_ptr_cast(ptr_lty, expect_ty);
        }
        // TODO: walk over `pl` to handle all derefs (casts, `*x` -> `(*x).get()`)
    }

    fn visit_operand_desc(&mut self, op: &Operand<'tcx>, expect_desc: TypeDesc<'tcx>) {
        match *op {
            Operand::Copy(pl) | Operand::Move(pl) => {
                let ptr_lty = self.acx.type_of(pl);
                if !ptr_lty.label.is_none() {
                    self.emit_cast(ptr_lty, expect_desc);
                }

                // TODO: walk over `pl` to handle all derefs (casts, `*x` -> `(*x).get()`)
            }
            Operand::Constant(..) => {}
        }
    }

    fn visit_ptr_offset(&mut self, op: &Operand<'tcx>, result_ty: LTy<'tcx>) {
        // Compute the expected type for the argument, and emit a cast if needed.
        let result_ptr = result_ty.label;
        let result_desc =
            type_desc::perms_to_desc(result_ty.ty, self.perms[result_ptr], self.flags[result_ptr]);
        let result_own = result_desc.own;
        let result_qty = result_desc.qty;

        let arg_expect_desc = TypeDesc {
            own: result_own,
            qty: match result_qty {
                Quantity::Single => Quantity::Slice,
                Quantity::Slice => Quantity::Slice,
                Quantity::OffsetPtr => Quantity::OffsetPtr,
            },
            pointee_ty: result_desc.pointee_ty,
        };

        self.enter_call_arg(0, |v| v.visit_operand_desc(op, arg_expect_desc));

        // Emit `OffsetSlice` for the offset itself.
        let mutbl = matches!(result_own, Ownership::Mut);

        self.emit(RewriteKind::OffsetSlice { mutbl });

        // If the result is `Single`, also insert an upcast.
        if result_qty == Quantity::Single {
            self.emit(RewriteKind::SliceFirst { mutbl });
        }
    }

    fn visit_slice_as_ptr(&mut self, op: &Operand<'tcx>, result_lty: LTy<'tcx>) {
        let op_lty = self.acx.type_of(op);
        let op_ptr = op_lty.label;
        let result_ptr = result_lty.label;

        let op_desc = type_desc::perms_to_desc(op_lty.ty, self.perms[op_ptr], self.flags[op_ptr]);
        let op_own = op_desc.own;
        let op_qty = op_desc.qty;
        let result_desc = type_desc::perms_to_desc(
            result_lty.ty,
            self.perms[result_ptr],
            self.flags[result_ptr],
        );
        let result_own = result_desc.own;
        let result_qty = result_desc.qty;

        if op_own == result_own && op_qty == result_qty {
            // Input and output types will be the same after rewriting, so the `as_ptr` call is not
            // needed.
            self.emit(RewriteKind::RemoveAsPtr);
        }
    }

    fn emit(&mut self, rw: RewriteKind) {
        self.rewrites
            .entry(self.loc)
            .or_insert_with(Vec::new)
            .push(MirRewrite {
                kind: rw,
                sub_loc: self.sub_loc.clone(),
            });
    }

    fn emit_ptr_cast(&mut self, ptr_lty: LTy<'tcx>, expect_lty: LTy<'tcx>) {
        assert!(expect_lty.label != PointerId::NONE);

        let desc2 = type_desc::perms_to_desc(
            expect_lty.ty,
            self.perms[expect_lty.label],
            self.flags[expect_lty.label],
        );

        self.emit_cast(ptr_lty, desc2);
    }

    fn emit_cast(&mut self, ptr_lty: LTy<'tcx>, expect_desc: TypeDesc<'tcx>) {
        assert!(ptr_lty.label != PointerId::NONE);

        let ptr_desc = type_desc::perms_to_desc(
            ptr_lty.ty,
            self.perms[ptr_lty.label],
            self.flags[ptr_lty.label],
        );
        let own1 = ptr_desc.own;
        let qty1 = ptr_desc.qty;
        let own2 = expect_desc.own;
        let qty2 = expect_desc.qty;

        if (own1, qty1) == (own2, qty2) {
            return;
        }

        if qty1 == qty2 && (own1, own2) == (Ownership::Mut, Ownership::Imm) {
            self.emit(RewriteKind::MutToImm);
            return;
        }

        eprintln!(
            "unsupported cast kind: {:?} {:?} -> {:?}",
            self.perms[ptr_lty.label],
            (own1, qty1),
            (own2, qty2)
        );
    }
}

pub fn gen_mir_rewrites<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    mir: &Body<'tcx>,
) -> HashMap<Location, Vec<MirRewrite>> {
    let mut out = HashMap::new();

    let mut v = ExprRewriteVisitor::new(acx, asn, &mut out, mir);

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

    out
}
