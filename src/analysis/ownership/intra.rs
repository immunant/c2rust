//! Intraprocedural step of the analysis.

use rustc::hir::def_id::DefId;
use rustc::mir::*;
use rustc::ty::{Ty, TyCtxt, TypeVariants};
use rustc_data_structures::indexed_vec::{IndexVec, Idx};

use super::{Var, LTy, LFnSig, Instantiation};
use super::constraint::{ConstraintSet, Perm};
use super::context::Ctxt;


/// Function-local analysis context.  We run one of these for each function to produce the initial
/// (incomplete) summary.
pub struct IntraCtxt<'a, 'gcx: 'tcx, 'tcx: 'a> {
    cx: &'a mut Ctxt<'tcx>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,

    def_id: DefId,
    mir: &'a Mir<'tcx>,
    bbid: BasicBlock,
    stmt_idx: usize,

    cset: ConstraintSet<'tcx>,
    local_tys: IndexVec<Local, LTy<'tcx>>,
    next_local_var: u32,

    insts: Vec<Instantiation>,
    next_inst_var: u32,
}

fn collect_perms<'tcx>(ty: LTy<'tcx>) -> Vec<Perm<'tcx>> {
    let mut v = Vec::new();
    collect_perms_into(ty, &mut v);
    v
}

fn collect_perms_into<'tcx>(ty: LTy<'tcx>, v: &mut Vec<Perm<'tcx>>) {
    if let Some(p) = ty.label {
        v.push(p);
    }

    for &arg in ty.args {
        collect_perms_into(arg, v);
    }
}

impl<'a, 'gcx, 'tcx> IntraCtxt<'a, 'gcx, 'tcx> {
    pub fn new(cx: &'a mut Ctxt<'tcx>,
               tcx: TyCtxt<'a, 'gcx, 'tcx>,
               def_id: DefId,
               mir: &'a Mir<'tcx>) -> IntraCtxt<'a, 'gcx, 'tcx> {
        IntraCtxt {
            cx: cx,
            tcx: tcx,

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
        let sig = self.cx.fn_sig(self.def_id, self.tcx);
        for (l, decl) in self.mir.local_decls.iter_enumerated() {
            let lty =
                if l.index() == 0 { sig.output }
                else if l.index() - 1 < self.mir.arg_count { sig.inputs[l.index() - 1] }
                else { self.local_ty(decl.ty) };
            self.local_tys.push(lty);
        }
    }

    fn instantiate_fn(&mut self, did: DefId) -> LFnSig<'tcx> {
        eprintln!("INSTANTIATE {:?}", did);
        let var_base = self.next_inst_var;
        let sig = {
            let summ = self.cx.fn_summ(did, self.tcx);
            // Don't import any constraints.  Only the signature is initialized at this point.
            self.next_inst_var += summ.num_sig_vars;
            summ.sig
        };

        self.insts.push(Instantiation {
            callee: did,
            first_inst_var: var_base,
        });

        let mut f = |p| {
            match p {
                Perm::SigVar(v) => Perm::InstVar(Var(var_base + v.0)),
                p => p,
            }
        };
        LFnSig {
            inputs: self.cx.lcx.relabel_slice(sig.inputs, &mut |opt_p| opt_p.map(|p| f(p))),
            output: self.cx.lcx.relabel(sig.output, &mut |opt_p| opt_p.map(|p| f(p))),
        }
    }

    pub fn finish(mut self) {
        eprintln!("  original constraints:");
        for &(a, b) in self.cset.iter() {
            eprintln!("    {:?} <= {:?}", a, b);
        }

        self.cset.remove_useless();
        self.cset.simplify_min_lhs(self.cx.arena);

        self.cset.retain_perms(self.cx.arena, |p| {
            match p {
                Perm::LocalVar(_) => false,
                _ => true,
            }
        });

        self.cset.simplify(self.cx.arena);

        eprintln!("  simplified constraints:");
        for &(a, b) in self.cset.iter() {
            eprintln!("    {:?} <= {:?}", a, b);
        }

        let summ = self.cx.fn_summ(self.def_id, self.tcx);
        summ.cset = self.cset;
        summ.insts = self.insts;
    }

    fn local_ty(&mut self, ty: Ty<'tcx>) -> LTy<'tcx> {
        self.cx.local_ty(self.def_id, &mut self.next_local_var, ty)
    }

    fn local_var_ty(&mut self, l: Local) -> LTy<'tcx> {
        self.local_tys[l]
    }


    /// Compute the type of an `Lvalue` and the maximum permissions for accessing it.
    fn lvalue_lty(&mut self, lv: &Lvalue<'tcx>) -> (LTy<'tcx>, Perm<'tcx>) {
        let (ty, perm, variant) = self.lvalue_lty_downcast(lv);
        assert!(variant.is_none(), "expected non-Downcast result");
        (ty, perm)
    }

    fn lvalue_lty_downcast(&mut self,
                           lv: &Lvalue<'tcx>) -> (LTy<'tcx>, Perm<'tcx>, Option<usize>) {
        match *lv {
            Lvalue::Local(l) => (self.local_var_ty(l), Perm::move_(), None),

            Lvalue::Static(ref s) =>
                (self.cx.static_ty(s.def_id, self.tcx), Perm::move_(), None),

            Lvalue::Projection(ref p) => {
                let (base_ty, base_perm, base_variant) = self.lvalue_lty_downcast(&p.base);

                // Sanity check
                match p.elem {
                    ProjectionElem::Field(..) => {},
                    _ => assert!(base_variant.is_none(), "expected non-Downcast result"),
                }

                match p.elem {
                    // Access permissions for a deref are the minimum of all pointers along the
                    // path to the value.
                    ProjectionElem::Deref =>
                        (base_ty.args[0],
                         self.cx.min_perm(base_perm, base_ty.label.unwrap()),
                         None),
                    ProjectionElem::Field(f, _) =>
                        (self.field_lty(base_ty, base_variant.unwrap_or(0), f), base_perm, None),
                    ProjectionElem::Index(ref index_op) =>
                        (base_ty.args[0], base_perm, None),
                    ProjectionElem::ConstantIndex { .. } => unimplemented!(),
                    ProjectionElem::Subslice { .. } => unimplemented!(),
                    ProjectionElem::Downcast(_, variant) =>
                        (base_ty, base_perm, Some(variant)),
                }
            },
        }
    }

    fn field_lty(&mut self, base_ty: LTy<'tcx>, v: usize, f: Field) -> LTy<'tcx> {
        match base_ty.ty.sty {
            TypeVariants::TyAdt(adt, substs) => {
                let field_def = &adt.variants[v].fields[f.index()];
                let poly_ty = self.cx.static_ty(field_def.did, self.tcx);
                self.cx.lcx.subst(poly_ty, &base_ty.args)
            },
            TypeVariants::TyTuple(tys, _) => base_ty.args[f.index()],
            _ => unimplemented!(),
        }
    }

    fn rvalue_lty(&mut self, rv: &Rvalue<'tcx>) -> (LTy<'tcx>, Perm<'tcx>) {
        let ty = rv.ty(self.mir, self.tcx);

        match *rv {
            Rvalue::Use(ref op) => self.operand_lty(op),
            Rvalue::Repeat(ref op, len) => {
                let arr_ty = self.local_ty(ty);

                // Assign the operand to the array element.
                let (op_ty, op_perm) = self.operand_lty(op);
                self.propagate(arr_ty.args[0], op_ty, op_perm);

                (arr_ty, Perm::move_())
            },
            Rvalue::Ref(_, _, ref lv) => {
                let (ty, perm) = self.lvalue_lty(lv);
                let args = self.cx.lcx.mk_slice(&[ty]);
                let ref_ty = self.cx.lcx.mk(rv.ty(self.mir, self.tcx), args, Some(perm));
                (ref_ty, Perm::move_())
            },
            Rvalue::Len(_) => (self.local_ty(ty), Perm::move_()),
            Rvalue::Cast(_, ref op, cast_raw_ty) => {
                let cast_ty = self.local_ty(cast_raw_ty);
                let (op_ty, op_perm) = self.operand_lty(op);
                self.propagate(cast_ty, op_ty, Perm::move_());
                (cast_ty, op_perm)
            },
            Rvalue::BinaryOp(op, ref a, ref b) |
            Rvalue::CheckedBinaryOp(op, ref a, ref b) => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem |
                BinOp::BitXor | BinOp::BitAnd | BinOp::BitOr | BinOp::Shl | BinOp::Shr |
                BinOp::Eq | BinOp::Lt | BinOp::Le | BinOp::Ne | BinOp::Ge | BinOp::Gt =>
                    (self.local_ty(ty), Perm::move_()),

                BinOp::Offset => self.operand_lty(a),
            },
            Rvalue::NullaryOp(op, ty) => unimplemented!(),
            Rvalue::UnaryOp(op, ref a) => match op {
                UnOp::Not | UnOp::Neg => (self.local_ty(ty), Perm::move_()),
            },
            Rvalue::Discriminant(ref lv) => unimplemented!(),
            Rvalue::Aggregate(ref kind, ref ops) => {
                match **kind {
                    AggregateKind::Array(ty) => {
                        let array_ty = self.local_ty(ty);
                        for op in ops {
                            let (op_ty, op_perm) = self.operand_lty(op);
                            self.propagate(array_ty.args[0], op_ty, op_perm);
                        }
                        (array_ty, Perm::move_())
                    },
                    AggregateKind::Tuple => {
                        let tuple_ty = self.local_ty(ty);
                        for (&elem_ty, op) in tuple_ty.args.iter().zip(ops.iter()) {
                            let (op_ty, op_perm) = self.operand_lty(op);
                            self.propagate(elem_ty, op_ty, op_perm);
                        }
                        (tuple_ty, Perm::move_())
                    },
                    AggregateKind::Adt(adt, disr, substs, union_variant) => {
                        let adt_ty = self.local_ty(ty);

                        if let Some(union_variant) = union_variant {
                            assert!(ops.len() == 1);
                            let field_def_id = adt.variants[0].fields[union_variant].did;
                            let poly_field_ty = self.cx.static_ty(field_def_id, self.tcx);
                            let field_ty = self.cx.lcx.subst(poly_field_ty, adt_ty.args);
                            let (op_ty, op_perm) = self.operand_lty(&ops[0]);
                            self.propagate(field_ty, op_ty, op_perm);
                        } else {
                            for (i, op) in ops.iter().enumerate() {
                                let field_def_id = adt.variants[disr].fields[i].did;
                                let poly_field_ty = self.cx.static_ty(field_def_id, self.tcx);
                                let field_ty = self.cx.lcx.subst(poly_field_ty, adt_ty.args);
                                let (op_ty, op_perm) = self.operand_lty(op);
                                self.propagate(field_ty, op_ty, op_perm);
                            }
                        }

                        (adt_ty, Perm::move_())
                    },
                    AggregateKind::Closure(_, _) => unimplemented!(),
                }
            },
        }
    }

    fn operand_lty(&mut self, op: &Operand<'tcx>) -> (LTy<'tcx>, Perm<'tcx>) {
        match *op {
            Operand::Consume(ref lv) => self.lvalue_lty(lv),
            Operand::Constant(ref c) => {
                eprintln!("CONSTANT {:?}: type = {:?}", c, c.ty);
                (self.local_ty(c.ty), Perm::move_())
            },
        }
    }


    /// Handle an assignment, including the implicit assignments of function arguments and return
    /// values.  An assignment can include an implicit reborrow, reducing the permission of the
    /// topmost pointer type.  The resulting permission must be no higher than the permission of
    /// the RHS pointer, and also must be no higher than the permission of any pointer dereferenced
    /// on the path to the RHS.
    fn propagate(&mut self, lhs: LTy<'tcx>, rhs: LTy<'tcx>, path_perm: Perm<'tcx>) {
        if let (Some(l_perm), Some(r_perm)) = (lhs.label, rhs.label) {
            self.propagate_perm(l_perm, r_perm);

            // Cap the required `path_perm` at WRITE.  The logic here is that container methods for
            // removing (and freeing) elements or for reallocating internal storage shouldn't
            // require MOVE.
            let l_perm_capped = self.cx.min_perm(l_perm, Perm::write());
            self.propagate_perm(l_perm_capped, path_perm);
        }

        if lhs.args.len() == rhs.args.len() {
            for (&l_arg, &r_arg) in lhs.args.iter().zip(rhs.args.iter()) {
                self.propagate_eq(l_arg, r_arg);
            }
        }
    }

    fn propagate_eq(&mut self, lhs: LTy<'tcx>, rhs: LTy<'tcx>) {
        if let (Some(l_perm), Some(r_perm)) = (lhs.label, rhs.label) {
            self.propagate_perm(l_perm, r_perm);
            self.propagate_perm(r_perm, l_perm);
        }

        if lhs.args.len() == rhs.args.len() {
            for (&l_arg, &r_arg) in lhs.args.iter().zip(rhs.args.iter()) {
                self.propagate_eq(l_arg, r_arg);
            }
        }
    }

    fn propagate_perm(&mut self, p1: Perm<'tcx>, p2: Perm<'tcx>) {
        eprintln!("ADD: {:?} <= {:?}", p1, p2);
        self.cset.add(p1, p2);
    }


    fn ty_fn_sig(&mut self, ty: LTy<'tcx>) -> LFnSig<'tcx> {
        match ty.ty.sty {
            TypeVariants::TyFnDef(did, substs) => {
                let poly_sig = self.instantiate_fn(did);
                LFnSig {
                    inputs: self.cx.lcx.subst_slice(poly_sig.inputs, ty.args),
                    output: self.cx.lcx.subst(poly_sig.output, ty.args),
                }
            },
            TypeVariants::TyFnPtr(_) => {
                LFnSig {
                    inputs: &ty.args[.. ty.args.len() - 1],
                    output: ty.args[ty.args.len() - 1],
                }
            },
            TypeVariants::TyClosure(_, _) => unimplemented!(),

            _ => panic!("expected FnDef, FnPtr, or Closure"),
        }
    }

    pub fn handle_basic_block(&mut self, bbid: BasicBlock, bb: &BasicBlockData<'tcx>) {
        self.enter_block(bbid);
        eprintln!("  {:?}", bbid);

        for (idx, s) in bb.statements.iter().enumerate() {
            self.enter_stmt(idx);
            match s.kind {
                StatementKind::Assign(ref lv, ref rv) => {
                    let (lv_ty, lv_perm) = self.lvalue_lty(lv);
                    let (rv_ty, rv_perm) = self.rvalue_lty(rv);
                    self.propagate(lv_ty, rv_ty, rv_perm);
                    self.propagate_perm(Perm::write(), lv_perm);
                    eprintln!("    {:?}: {:?}", lv, lv_ty);
                    eprintln!("    ^-- {:?}: {:?}", rv, rv_ty);
                },
                StatementKind::SetDiscriminant { .. } |
                StatementKind::StorageLive(_) |
                StatementKind::StorageDead(_) |
                // InlineAsm has some Lvalues and Operands, but we can't do anything useful
                // with them without analysing the actual asm code.
                StatementKind::InlineAsm { .. } |
                StatementKind::EndRegion(_) |
                StatementKind::Nop => {},
            }
        }

        match bb.terminator().kind {
            TerminatorKind::Goto { .. } |
            TerminatorKind::SwitchInt { .. } |
            TerminatorKind::Resume |
            TerminatorKind::Return |
            TerminatorKind::Unreachable |
            TerminatorKind::Drop { .. } |
            TerminatorKind::Assert { .. } => {},

            TerminatorKind::DropAndReplace { ref location, ref value, .. } => {
                let (loc_ty, loc_perm) = self.lvalue_lty(location);
                let (val_ty, val_perm) = self.operand_lty(value);
                self.propagate(loc_ty, val_ty, val_perm);
                self.propagate_perm(Perm::write(), loc_perm);
                eprintln!("    {:?}: {:?}", location, loc_ty);
                eprintln!("    ^-- {:?}: {:?}", value, val_ty);
            },

            TerminatorKind::Call { ref func, ref args, ref destination, .. } => {
                eprintln!("    call {:?}", func);
                let (func_ty, _func_perm) = self.operand_lty(func);
                eprintln!("fty = {:?}", func_ty);
                let sig = self.ty_fn_sig(func_ty);

                // Note that `sig.inputs` may be shorter than `args`, if `func` is varargs.
                for (&sig_ty, arg) in sig.inputs.iter().zip(args.iter()) {
                    let (arg_ty, arg_perm) = self.operand_lty(arg);
                    self.propagate(sig_ty, arg_ty, arg_perm);
                    eprintln!("    (arg): {:?}", sig_ty);
                    eprintln!("    ^-- {:?}: {:?}", arg, arg_ty);
                }
                if let Some((ref dest, _)) = *destination {
                    let sig_ty = sig.output;
                    let (dest_ty, dest_perm) = self.lvalue_lty(dest);
                    self.propagate(dest_ty, sig_ty, Perm::move_());
                    self.propagate_perm(Perm::write(), dest_perm);
                    eprintln!("    {:?}: {:?}", dest, dest_ty);
                    eprintln!("    ^-- (return): {:?}", sig_ty);
                }
            },
        }
    }
}
