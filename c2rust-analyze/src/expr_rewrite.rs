use crate::context::{AnalysisCtxt, Assignment, FlagSet, LTy, PermissionSet, PointerId};
use crate::pointer_id::PointerTable;
use crate::type_desc::{self, Ownership, Quantity};
use crate::util::{self, Callee};
use rustc_middle::mir::{
    BasicBlock, Body, Location, Operand, Place, Rvalue, Statement, StatementKind, Terminator,
    TerminatorKind,
};
use rustc_span::{Span, DUMMY_SP};

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ExprLoc {
    pub stmt: Location,
    pub span: Span,
    pub sub: Vec<SubLoc>,
}

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
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ExprRewrite {
    pub loc: ExprLoc,
    pub kinds: Vec<RewriteKind>,
}

struct ExprRewriteVisitor<'a, 'tcx> {
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    perms: PointerTable<'a, PermissionSet>,
    flags: PointerTable<'a, FlagSet>,
    rewrites: &'a mut Vec<ExprRewrite>,
    mir: &'a Body<'tcx>,
    loc: ExprLoc,
}

impl<'a, 'tcx> ExprRewriteVisitor<'a, 'tcx> {
    pub fn new(
        acx: &'a AnalysisCtxt<'a, 'tcx>,
        asn: &'a Assignment,
        rewrites: &'a mut Vec<ExprRewrite>,
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
            loc: ExprLoc {
                stmt: Location {
                    block: BasicBlock::from_usize(0),
                    statement_index: 0,
                },
                span: DUMMY_SP,
                sub: Vec::new(),
            },
        }
    }

    fn enter<F: FnOnce(&mut Self) -> R, R>(&mut self, sub: SubLoc, f: F) -> R {
        self.loc.sub.push(sub);
        let r = f(self);
        self.loc.sub.pop();
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
        self.loc = ExprLoc {
            stmt: loc,
            span: stmt.source_info.span,
            sub: Vec::new(),
        };

        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;
                let pl_ty = self.acx.type_of(pl);
                self.enter_assign_rvalue(|v| v.visit_rvalue(rv, pl_ty));
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
        self.loc = ExprLoc {
            stmt: loc,
            span: term.source_info.span,
            sub: Vec::new(),
        };

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

                if let Some(callee) = util::ty_callee(tcx, func_ty) {
                    // Special cases for particular functions.
                    match callee {
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
            Rvalue::AddressOf(_mutbl, _pl) => {
                // TODO
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
        if let Some(ptr) = self.acx.ptr_of(pl) {
            let expect_ptr = expect_ty.label;
            self.emit_ptr_cast(ptr, expect_ptr);
        }
        // TODO: walk over `pl` to handle all derefs (casts, `*x` -> `(*x).get()`)
    }

    fn visit_operand_desc(
        &mut self,
        op: &Operand<'tcx>,
        expect_own: Ownership,
        expect_qty: Quantity,
    ) {
        match *op {
            Operand::Copy(pl) | Operand::Move(pl) => {
                if let Some(ptr) = self.acx.ptr_of(pl) {
                    self.emit_cast(ptr, expect_own, expect_qty);
                }

                // TODO: walk over `pl` to handle all derefs (casts, `*x` -> `(*x).get()`)
            }
            Operand::Constant(..) => {}
        }
    }

    fn visit_ptr_offset(&mut self, op: &Operand<'tcx>, result_ty: LTy<'tcx>) {
        // Compute the expected type for the argument, and emit a cast if needed.
        let result_ptr = result_ty.label;
        let (result_own, result_qty) =
            type_desc::perms_to_desc(self.perms[result_ptr], self.flags[result_ptr]);

        let arg_expect_own = result_own;
        // TODO: infer `arg_expect_qty` based on the type of offset this is (positive / unknown)
        let arg_expect_qty = match result_qty {
            Quantity::Single => Quantity::Slice,
            Quantity::Slice => Quantity::Slice,
            Quantity::OffsetPtr => todo!("OffsetPtr"),
        };

        self.enter_call_arg(0, |v| {
            v.visit_operand_desc(op, arg_expect_own, arg_expect_qty)
        });

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

        let (op_own, op_qty) = type_desc::perms_to_desc(self.perms[op_ptr], self.flags[op_ptr]);
        let (result_own, result_qty) =
            type_desc::perms_to_desc(self.perms[result_ptr], self.flags[result_ptr]);

        if op_own == result_own && op_qty == result_qty {
            // Input and output types will be the same after rewriting, so the `as_ptr` call is not
            // needed.
            self.emit(RewriteKind::RemoveAsPtr);
        }
    }

    fn emit(&mut self, rw: RewriteKind) {
        if let Some(er) = self.rewrites.last_mut() {
            if er.loc == self.loc {
                er.kinds.push(rw);
                return;
            }
        }

        self.rewrites.push(ExprRewrite {
            loc: self.loc.clone(),
            kinds: vec![rw],
        });
    }

    fn emit_ptr_cast(&mut self, ptr: PointerId, expect_ptr: PointerId) {
        assert!(expect_ptr != PointerId::NONE);

        let (own2, qty2) = type_desc::perms_to_desc(self.perms[expect_ptr], self.flags[expect_ptr]);

        self.emit_cast(ptr, own2, qty2);
    }

    fn emit_cast(&mut self, ptr: PointerId, expect_own: Ownership, expect_qty: Quantity) {
        assert!(ptr != PointerId::NONE);

        let (own1, qty1) = type_desc::perms_to_desc(self.perms[ptr], self.flags[ptr]);
        let (own2, qty2) = (expect_own, expect_qty);

        if (own1, qty1) == (own2, qty2) {
            return;
        }

        if qty1 == qty2 && (own1, own2) == (Ownership::Mut, Ownership::Imm) {
            self.emit(RewriteKind::MutToImm);
            return;
        }

        eprintln!(
            "unsupported cast kind: {:?} {:?} -> {:?}",
            self.perms[ptr],
            (own1, qty1),
            (own2, qty2)
        );
    }
}

pub fn gen_expr_rewrites<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    mir: &Body<'tcx>,
) -> Vec<ExprRewrite> {
    // - walk over statements/terminators
    // - Assign: find RHS operands that need casting to match LHS
    // - Call: special case for `ptr.offset(i)`

    let mut out = Vec::new();

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
