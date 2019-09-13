//! Intraprocedural step of the analysis.

use log::Level;
use rustc::hir::def_id::DefId;
use rustc::mir::*;
use rustc::ty::{Ty, TyKind};
use rustc_index::vec::IndexVec;
use rustc_target::abi::VariantIdx;
use syntax::source_map::Span;

use crate::analysis::labeled_ty::{LabeledTy, LabeledTyCtxt};

use super::constraint::{ConstraintSet, Perm};
use super::context::{Ctxt, Instantiation};
use super::{FnSig, LFnSig, LTy, PermVar, Var};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Label<'lty> {
    /// Most `TyKind` get no constructor.
    None,

    /// Pointers and references get a permission annotation.
    ///
    /// Note this can be an arbitrary permission expression, not just a `PermVar`.  Taking the
    /// address of an lvalue gives a pointer whose permission is the lvalue's path permission,
    /// which can be arbitrary.
    Ptr(Perm<'lty>),

    /// `FnDef` ought to be labeled with something like an extra set of `Substs`, but for
    /// permissions instead of type/lifetimes.  However, every one of those `Substs` would simply
    /// consist of a list of sequentially numbered `InstVar`s.  So instead we store an index into
    /// the `insts` table, which can be used to reconstruct the permission arguments, and also
    /// allows storing extra information about the origin when available.
    FnDef(usize),
}

impl<'lty> Label<'lty> {
    fn perm(&self) -> Perm<'lty> {
        match *self {
            Label::Ptr(p) => p,
            _ => panic!("expected Label::Ptr"),
        }
    }
}

/// Type aliases for `intra`-specific labeled types.
type ITy<'lty, 'tcx> = LabeledTy<'lty, 'tcx, Label<'lty>>;
type IFnSig<'lty, 'tcx> = FnSig<'lty, 'tcx, Label<'lty>>;

/// Variant-local analysis context.  We run one of these for each function variant to produce the
/// initial (incomplete) summary.
pub struct IntraCtxt<'c, 'lty, 'a: 'lty, 'tcx: 'a> {
    cx: &'c mut Ctxt<'lty, 'tcx>,
    ilcx: LabeledTyCtxt<'lty, Label<'tcx>>,

    /// ID of the variant being processed.
    def_id: DefId,
    mir: &'a Body<'tcx>,
    bbid: BasicBlock,
    stmt_idx: usize,

    cset: ConstraintSet<'lty>,
    local_tys: IndexVec<Local, (Option<Span>, ITy<'lty, 'tcx>)>,
    next_local_var: u32,

    /// List of function instantiation sites.
    ///
    /// Conceptually, for each time a function is referenced, we must instantiate its polymorphic
    /// signature by substituting in some (inferred) concrete permissions for the function's
    /// `SigVar`s.  At this stage, since the only interprocedural information we have available is
    /// the number of `SigVar`s for each function, we simply replace all the `SigVar`s with fresh
    /// `InstVar`s and record info about the instantiation site for future reference.  The actual
    /// inference happens later, in `inter`, by copying the target function's constraints into the
    /// caller's constraint set and then simplifying.
    ///
    /// In reality, we also need to track anonymous instantiations.  When labeling a `TyFnSig`, we
    /// need to generate some new `InstVar`s to serve as its permission substs (see the comment on
    /// `Label::FnDef` above), and we do that by adding a new entry to `insts`.
    insts: Vec<Instantiation>,
    next_inst_var: u32,
}

impl<'c, 'lty, 'a: 'lty, 'tcx: 'a> IntraCtxt<'c, 'lty, 'a, 'tcx> {
    pub fn new(
        cx: &'c mut Ctxt<'lty, 'tcx>,
        def_id: DefId,
        mir: &'a Body<'tcx>,
    ) -> IntraCtxt<'c, 'lty, 'a, 'tcx> {
        let ilcx = LabeledTyCtxt::new(cx.arena);
        IntraCtxt {
            cx: cx,
            ilcx: ilcx,

            def_id: def_id,
            mir: mir,
            bbid: START_BLOCK,
            stmt_idx: !0,

            cset: ConstraintSet::new(),
            local_tys: IndexVec::new(),
            next_local_var: 0,

            insts: Vec::new(),
            next_inst_var: 0,
        }
    }

    fn enter_block(&mut self, bbid: BasicBlock) {
        self.bbid = bbid;
        // Obviously bogus statement index
        self.stmt_idx = !0;
    }

    fn enter_stmt(&mut self, idx: usize) {
        self.stmt_idx = idx;
    }

    pub fn init(&mut self) {
        let sig = self.cx.variant_func_sig(self.def_id);
        let sig = self.relabel_sig(sig);
        for (l, decl) in self.mir.local_decls.iter_enumerated() {
            let lty = if l.index() == 0 {
                sig.output
            } else if l.index() - 1 < self.mir.arg_count {
                sig.inputs[l.index() - 1]
            } else {
                self.local_ty(decl.ty)
            };

            let opt_span = decl.is_user_variable.as_ref().map(|ccc| match ccc {
                ClearCrossCrate::Clear => None,
                ClearCrossCrate::Set(binding) => Some(binding),
            }).flatten().map(|binding| match binding {
                BindingForm::Var(var) => Some(var.pat_span),
                _ => None,
            }).flatten();

            self.local_tys.push((opt_span, lty));
        }

        // Pick up any preset constraints for this variant.
        self.cset = self.cx.variant_summ(self.def_id).1.inst_cset.clone();
    }

    fn relabel_ty(&mut self, lty: LTy<'lty, 'tcx>) -> ITy<'lty, 'tcx> {
        self.ilcx.relabel(lty, &mut |&l| match l {
            Some(pv) => Label::Ptr(Perm::var(pv)),
            None => Label::None,
        })
    }

    fn relabel_sig(&mut self, sig: LFnSig<'lty, 'tcx>) -> IFnSig<'lty, 'tcx> {
        let mut f = |&l: &Option<_>| match l {
            Some(pv) => Label::Ptr(Perm::var(pv)),
            None => Label::None,
        };
        FnSig {
            inputs: self.ilcx.relabel_slice(sig.inputs, &mut f),
            output: self.ilcx.relabel(sig.output, &mut f),
        }
    }

    pub fn finish(mut self) {
        debug!("  original constraints:");
        if log_enabled!(Level::Debug) {
            for &(a, b) in self.cset.iter() {
                debug!("    {:?} <= {:?}", a, b);
            }
        }

        self.cset.remove_useless();
        self.cset.simplify_min_lhs(self.cx.arena);

        // self.cset.retain_perms(self.cx.arena, |p| match p {
        //     Perm::LocalVar(_) => false,
        //     _ => true,
        // });

        self.cset.simplify(self.cx.arena);

        debug!("  simplified constraints:");
        if log_enabled!(Level::Debug) {
            for &(a, b) in self.cset.iter() {
                debug!("    {:?} <= {:?}", a, b);
            }
        }

        // ITy -> LTy
        let mut f = |&l: &Label<'lty>| match l {
            Label::Ptr(p) => p.as_var().into_iter().filter(|pv| match pv {
                PermVar::Local(_) => true,
                _ => false,
            }).next(),
            _ => None,
        };

        let relabeled_locals = self.local_tys
            .raw
            .iter()
            .filter_map(|(opt_span, ity)| opt_span.map(|s| (s, self.cx.lcx.relabel(ity, &mut f))))
            .collect();

        let (func, var) = self.cx.variant_summ(self.def_id);
        var.inst_cset = self.cset;
        var.insts = self.insts;
        func.locals = dbg!(relabeled_locals);
    }

    fn local_ty(&mut self, ty: Ty<'tcx>) -> ITy<'lty, 'tcx> {
        let Self {
            ref mut cx,
            ref mut ilcx,
            ref mut next_local_var,
            ref mut next_inst_var,
            ref mut insts,
            ..
        } = *self;
        ilcx.label(ty, &mut |ty| match ty.kind {
            TyKind::Ref(_, _, _) | TyKind::RawPtr(_) => {
                let v = Var(*next_local_var);
                *next_local_var += 1;
                Label::Ptr(Perm::LocalVar(v))
            }

            TyKind::FnDef(def_id, _) => {
                let (func, var) = cx.variant_summ(def_id);
                let num_vars = func.num_sig_vars;

                let inst_idx = insts.len();
                insts.push(Instantiation {
                    callee: var.func_id,
                    span: None,
                    first_inst_var: *next_inst_var,
                });
                *next_inst_var += num_vars;

                Label::FnDef(inst_idx)
            }

            _ => Label::None,
        })
    }

    fn local_var_ty(&mut self, l: Local) -> ITy<'lty, 'tcx> {
        self.local_tys[l].1
    }

    fn static_ty(&mut self, def_id: DefId) -> ITy<'lty, 'tcx> {
        let lty = self.cx.static_ty(def_id);
        self.relabel_ty(lty)
    }

    /// Compute the type of an `Lvalue` and the maximum permissions for accessing it.
    fn place_lty(&mut self, lv: &Place<'tcx>) -> (ITy<'lty, 'tcx>, Perm<'lty>) {
        let (ty, perm, variant) = self.place_lty_downcast(lv);
        assert!(variant.is_none(), "expected non-Downcast result");
        (ty, perm)
    }

    fn place_lty_downcast(
        &mut self,
        lv: &Place<'tcx>,
    ) -> (ITy<'lty, 'tcx>, Perm<'lty>, Option<VariantIdx>) {
        if !lv.projection.is_empty() {
            let mut projection = lv.projection.iter().cloned().collect::<Vec<_>>();
            let last_elem = projection.pop().unwrap();
            let parent = Place {
                base: lv.base.clone(),
                projection: projection.into_boxed_slice(),
            };
            let (base_ty, base_perm, base_variant) = self.place_lty_downcast(&parent);

            // Sanity check
            match last_elem {
                ProjectionElem::Field(..) => {}
                _ => assert!(base_variant.is_none(), "expected non-Downcast result"),
            }

            match last_elem {
                // Access permissions for a deref are the minimum of all pointers along the
                // path to the value.
                ProjectionElem::Deref => (
                    base_ty.args[0],
                    self.cx.min_perm(base_perm, base_ty.label.perm()),
                    None,
                ),
                ProjectionElem::Field(f, _) => (
                    self.field_lty(
                        base_ty,
                        base_variant.unwrap_or(VariantIdx::from_usize(0)),
                        f,
                    ),
                    base_perm,
                    None,
                ),
                ProjectionElem::Index(ref _index_op) => (base_ty.args[0], base_perm, None),
                ProjectionElem::ConstantIndex { .. } => unimplemented!(),
                ProjectionElem::Subslice { .. } => unimplemented!(),
                ProjectionElem::Downcast(_, variant) => (base_ty, base_perm, Some(variant)),
            }
        } else {
            match lv.base {
                PlaceBase::Local(l) => (self.local_var_ty(l), Perm::move_(), None),

                PlaceBase::Static(ref s) => match s.kind {
                    StaticKind::Static => (self.static_ty(s.def_id), Perm::move_(), None),
                    StaticKind::Promoted(ref _p, _) => {
                        // TODO: test this
                        let pty = lv.ty(self.mir, self.cx.tcx);
                        let ty = pty.ty;
                        (self.local_ty(ty), Perm::read(), None)
                    }
                },
            }
        }
    }

    fn field_lty(&mut self, base_ty: ITy<'lty, 'tcx>, v: VariantIdx, f: Field) -> ITy<'lty, 'tcx> {
        match base_ty.ty.kind {
            TyKind::Adt(adt, _substs) => {
                let field_def = &adt.variants[v].fields[f.index()];
                let poly_ty = self.static_ty(field_def.did);
                self.ilcx.subst(poly_ty, &base_ty.args)
            }
            TyKind::Tuple(_tys_) => base_ty.args[f.index()],
            _ => unimplemented!(),
        }
    }

    fn rvalue_lty(&mut self, rv: &Rvalue<'tcx>) -> (ITy<'lty, 'tcx>, Perm<'lty>) {
        let ty = rv.ty(self.mir, self.cx.tcx);

        match *rv {
            Rvalue::Use(ref op) => self.operand_lty(op),
            Rvalue::Repeat(ref op, _len) => {
                let arr_ty = self.local_ty(ty);

                // Assign the operand to the array element.
                let (op_ty, op_perm) = self.operand_lty(op);
                self.propagate(arr_ty.args[0], op_ty, op_perm);

                (arr_ty, Perm::move_())
            }
            Rvalue::Ref(_, _, ref lv) => {
                let (ty, perm) = self.place_lty(lv);
                let args = self.ilcx.mk_slice(&[ty]);
                let ref_ty = self
                    .ilcx
                    .mk(rv.ty(self.mir, self.cx.tcx), args, Label::Ptr(perm));
                (ref_ty, Perm::move_())
            }
            Rvalue::Len(_) => (self.local_ty(ty), Perm::move_()),
            Rvalue::Cast(_, ref op, cast_raw_ty) => {
                let cast_ty = self.local_ty(cast_raw_ty);
                let (op_ty, op_perm) = self.operand_lty(op);
                self.propagate(cast_ty, op_ty, Perm::move_());
                (cast_ty, op_perm)
            }
            Rvalue::BinaryOp(op, ref a, ref _b) | Rvalue::CheckedBinaryOp(op, ref a, ref _b) => {
                match op {
                    BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Rem
                    | BinOp::BitXor
                    | BinOp::BitAnd
                    | BinOp::BitOr
                    | BinOp::Shl
                    | BinOp::Shr
                    | BinOp::Eq
                    | BinOp::Lt
                    | BinOp::Le
                    | BinOp::Ne
                    | BinOp::Ge
                    | BinOp::Gt => (self.local_ty(ty), Perm::move_()),

                    BinOp::Offset => self.operand_lty(a),
                }
            }
            Rvalue::NullaryOp(_op, _ty) => unimplemented!(),
            Rvalue::UnaryOp(op, ref _a) => match op {
                UnOp::Not | UnOp::Neg => (self.local_ty(ty), Perm::move_()),
            },
            Rvalue::Discriminant(ref _lv) => unimplemented!(),
            Rvalue::Aggregate(ref kind, ref ops) => match **kind {
                AggregateKind::Array(_elem_ty) => {
                    let array_ty = self.local_ty(ty);
                    for op in ops {
                        let (op_ty, op_perm) = self.operand_lty(op);
                        // REVIEW: array of struct inits in run_static_initializers
                        // cause this to return None. Not certain skipping is the
                        // correct course of action.
                        if let Some(arg0) = array_ty.args.get(0) {
                            self.propagate(arg0, op_ty, op_perm);
                        }
                    }
                    (array_ty, Perm::move_())
                }
                AggregateKind::Tuple => {
                    let tuple_ty = self.local_ty(ty);
                    for (&elem_ty, op) in tuple_ty.args.iter().zip(ops.iter()) {
                        let (op_ty, op_perm) = self.operand_lty(op);
                        self.propagate(elem_ty, op_ty, op_perm);
                    }
                    (tuple_ty, Perm::move_())
                }
                AggregateKind::Adt(adt, disr, _substs, _annot, union_variant) => {
                    let adt_ty = self.local_ty(ty);

                    if let Some(union_variant) = union_variant {
                        assert!(ops.len() == 1);
                        let field_def_id = adt.non_enum_variant().fields[union_variant].did;
                        let poly_field_ty = self.static_ty(field_def_id);
                        let field_ty = self.ilcx.subst(poly_field_ty, adt_ty.args);
                        let (op_ty, op_perm) = self.operand_lty(&ops[0]);
                        self.propagate(field_ty, op_ty, op_perm);
                    } else {
                        for (i, op) in ops.iter().enumerate() {
                            let field_def_id = adt.variants[disr].fields[i].did;
                            let poly_field_ty = self.static_ty(field_def_id);
                            let field_ty = self.ilcx.subst(poly_field_ty, adt_ty.args);
                            let (op_ty, op_perm) = self.operand_lty(op);
                            self.propagate(field_ty, op_ty, op_perm);
                        }
                    }

                    (adt_ty, Perm::move_())
                }
                AggregateKind::Closure(_, _) => unimplemented!(),
                AggregateKind::Generator(_, _, _) => unimplemented!(),
            },
        }
    }

    fn operand_lty(&mut self, op: &Operand<'tcx>) -> (ITy<'lty, 'tcx>, Perm<'lty>) {
        match *op {
            Operand::Copy(ref lv) => self.place_lty(lv),
            Operand::Move(ref lv) => self.place_lty(lv),
            Operand::Constant(ref c) => {
                debug!("CONSTANT {:?}: type = {:?}", c, c.literal.ty);
                let lty = self.local_ty(c.literal.ty);
                if let Label::FnDef(inst_idx) = lty.label {
                    self.insts[inst_idx].span = Some(c.span);
                }
                (lty, Perm::move_())
            }
        }
    }

    /// Handle an assignment, including the implicit assignments of function arguments and return
    /// values.  An assignment can include an implicit reborrow, reducing the permission of the
    /// topmost pointer type.  The resulting permission must be no higher than the permission of
    /// the RHS pointer, and also must be no higher than the permission of any pointer dereferenced
    /// on the path to the RHS.
    fn propagate(&mut self, lhs: ITy<'lty, 'tcx>, rhs: ITy<'lty, 'tcx>, path_perm: Perm<'lty>) {
        if let (Label::Ptr(l_perm), Label::Ptr(r_perm)) = (lhs.label, rhs.label) {
            self.propagate_perm(l_perm, r_perm);

            // This is the "collection hack".
            //
            // Cap the required `path_perm` at WRITE.  The logic here is that container methods for
            // removing (and freeing) elements or for reallocating internal storage shouldn't
            // require MOVE.
            let l_perm_capped = self.cx.min_perm(l_perm, Perm::write());
            self.propagate_perm(l_perm_capped, path_perm);
        } else if let (Label::FnDef(l_inst), Label::FnDef(r_inst)) = (lhs.label, rhs.label) {
            self.unify_inst_vars(l_inst, r_inst);
        }

        if lhs.args.len() == rhs.args.len() {
            for (&l_arg, &r_arg) in lhs.args.iter().zip(rhs.args.iter()) {
                self.propagate_eq(l_arg, r_arg);
            }
        }
    }

    fn propagate_eq(&mut self, lhs: ITy<'lty, 'tcx>, rhs: ITy<'lty, 'tcx>) {
        if let (Label::Ptr(l_perm), Label::Ptr(r_perm)) = (lhs.label, rhs.label) {
            self.propagate_perm(l_perm, r_perm);
            self.propagate_perm(r_perm, l_perm);
        } else if let (Label::FnDef(l_inst), Label::FnDef(r_inst)) = (lhs.label, rhs.label) {
            self.unify_inst_vars(l_inst, r_inst);
        }

        if lhs.args.len() == rhs.args.len() {
            for (&l_arg, &r_arg) in lhs.args.iter().zip(rhs.args.iter()) {
                self.propagate_eq(l_arg, r_arg);
            }
        }
    }

    fn propagate_perm(&mut self, p1: Perm<'lty>, p2: Perm<'lty>) {
        debug!("ADD: {:?} <= {:?}", p1, p2);
        self.cset.add(p1, p2);
    }

    fn unify_inst_vars(&mut self, idx1: usize, idx2: usize) {
        let (callee, first1, first2) = {
            let inst1 = &self.insts[idx1];
            let inst2 = &self.insts[idx2];
            assert!(
                inst1.callee == inst2.callee,
                "impossible - tried to unify unequal FnDefs ({:?} != {:?})",
                inst1.callee,
                inst2.callee
            );

            if inst1.first_inst_var == inst2.first_inst_var {
                // The vars are already the same - no work to do
                return;
            }

            (inst1.callee, inst1.first_inst_var, inst2.first_inst_var)
        };

        let num_vars = self.cx.variant_summ(callee).0.num_sig_vars;
        for offset in 0..num_vars {
            let p1 = Perm::InstVar(Var(first1 + offset));
            let p2 = Perm::InstVar(Var(first2 + offset));
            self.propagate_perm(p1, p2);
            self.propagate_perm(p2, p1);
        }
    }

    fn ty_fn_sig(&mut self, ty: ITy<'lty, 'tcx>) -> IFnSig<'lty, 'tcx> {
        match ty.ty.kind {
            TyKind::FnDef(did, _substs) => {
                let idx = expect!([ty.label] Label::FnDef(idx) => idx);
                let var_base = self.insts[idx].first_inst_var;

                let sig = self.cx.variant_func_sig(did);

                // First apply the permission substs.  Replace all `SigVar`s with `InstVar`s.
                let mut f = |p: &Option<_>| {
                    match *p {
                        Some(PermVar::Sig(v)) => Label::Ptr(Perm::InstVar(Var(var_base + v.0))),
                        Some(_) => panic!("found non-Sig PermVar in sig"),
                        None => Label::None,
                        // There's no way to write a FnDef type in a function signature, so it's
                        // reasonable to have no cases output `Label::FnDef`.
                    }
                };
                let poly_inputs = self.ilcx.relabel_slice(sig.inputs, &mut f);
                let poly_output = self.ilcx.relabel(sig.output, &mut f);

                // Now apply the type substs.
                FnSig {
                    inputs: self.ilcx.subst_slice(poly_inputs, ty.args),
                    output: self.ilcx.subst(poly_output, ty.args),
                }
            }

            TyKind::FnPtr(_) => FnSig {
                inputs: &ty.args[..ty.args.len() - 1],
                output: ty.args[ty.args.len() - 1],
            },

            TyKind::Closure(_, _) => unimplemented!(),

            _ => panic!("expected FnDef, FnPtr, or Closure"),
        }
    }

    pub fn handle_basic_block(&mut self, bbid: BasicBlock, bb: &BasicBlockData<'tcx>) {
        self.enter_block(bbid);
        debug!("  {:?}", bbid);

        for (idx, s) in bb.statements.iter().enumerate() {
            self.enter_stmt(idx);
            match s.kind {
                StatementKind::Assign(box(ref lv, ref rv)) => {
                    let (lv_ty, lv_perm) = self.place_lty(lv);
                    let (rv_ty, rv_perm) = self.rvalue_lty(rv);
                    self.propagate(lv_ty, rv_ty, rv_perm);
                    self.propagate_perm(Perm::write(), lv_perm);
                    debug!("    {:?}: {:?}", lv, lv_ty);
                    debug!("    ^-- {:?}: {:?}", rv, rv_ty);
                },
                StatementKind::FakeRead(..) |
                StatementKind::SetDiscriminant { .. } |
                StatementKind::StorageLive(_) |
                StatementKind::StorageDead(_) |
                // InlineAsm has some Lvalues and Operands, but we can't do anything useful
                // with them without analysing the actual asm code.
                StatementKind::InlineAsm { .. } |
                StatementKind::Retag { .. } |
                StatementKind::AscribeUserType(..) |
                StatementKind::Nop => {},
            }
        }

        match bb.terminator().kind {
            TerminatorKind::Goto { .. }
            | TerminatorKind::FalseEdges { .. }
            | TerminatorKind::FalseUnwind { .. }
            | TerminatorKind::SwitchInt { .. }
            | TerminatorKind::Resume
            | TerminatorKind::Return
            | TerminatorKind::Unreachable
            | TerminatorKind::Drop { .. }
            | TerminatorKind::Assert { .. }
            | TerminatorKind::Yield { .. }
            | TerminatorKind::GeneratorDrop
            | TerminatorKind::Abort => {}

            TerminatorKind::DropAndReplace {
                ref location,
                ref value,
                ..
            } => {
                let (loc_ty, loc_perm) = self.place_lty(location);
                let (val_ty, val_perm) = self.operand_lty(value);
                self.propagate(loc_ty, val_ty, val_perm);
                self.propagate_perm(Perm::write(), loc_perm);
                debug!("    {:?}: {:?}", location, loc_ty);
                debug!("    ^-- {:?}: {:?}", value, val_ty);
            }

            TerminatorKind::Call {
                ref func,
                ref args,
                ref destination,
                ..
            } => {
                debug!("    call {:?}", func);
                let (func_ty, _func_perm) = self.operand_lty(func);
                debug!("fty = {:?}", func_ty);
                let sig = self.ty_fn_sig(func_ty);

                // Note that `sig.inputs` may be shorter than `args`, if `func` is varargs.
                for (&sig_ty, arg) in sig.inputs.iter().zip(args.iter()) {
                    let (arg_ty, arg_perm) = self.operand_lty(arg);
                    self.propagate(sig_ty, arg_ty, arg_perm);
                    debug!("    (arg): {:?}", sig_ty);
                    debug!("    ^-- {:?}: {:?}", arg, arg_ty);
                }
                if let Some((ref dest, _)) = *destination {
                    let sig_ty = sig.output;
                    let (dest_ty, dest_perm) = self.place_lty(dest);
                    self.propagate(dest_ty, sig_ty, Perm::move_());
                    self.propagate_perm(Perm::write(), dest_perm);
                    debug!("    {:?}: {:?}", dest, dest_ty);
                    debug!("    ^-- (return): {:?}", sig_ty);
                }
            }
        }
    }
}
