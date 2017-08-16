use std::cmp;
use std::collections::Bound;
use std::collections::BTreeSet;
use std::collections::hash_map::{HashMap, Entry};
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt;
use std::u32;

use arena::DroplessArena;
use rustc::hir;
use rustc::hir::def_id::{DefId, LOCAL_CRATE};
use rustc::mir::*;
use rustc::mir::tcx::LvalueTy;
use rustc::mir::traversal::{Postorder, ReversePostorder};
use rustc::ty::{Ty, TyS, TyCtxt, FnSig, Instance, TypeVariants, AdtDef};
use rustc::ty::subst::Substs;
use rustc::ty::fold::{TypeVisitor, TypeFoldable};
use rustc_data_structures::bitvec::BitVector;
use rustc_data_structures::indexed_vec::{IndexVec, Idx};
use syntax::ast;

use analysis::labeled_ty::{LabeledTy, LabeledTyCtxt};
use command::CommandState;
use driver;
use type_map::{self, TypeSource};


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct Var(u32);

impl Idx for Var {
    fn new(idx: usize) -> Var {
        assert!(idx as u32 as usize == idx);
        Var(idx as u32)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

type LTy<'tcx> = LabeledTy<'tcx, Option<Perm>>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct LFnSig<'tcx> {
    inputs: &'tcx [LTy<'tcx>],
    output: LTy<'tcx>,
}


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
enum ConcretePerm {
    Read,
    Write,
    Move,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
enum Perm {
    Concrete(ConcretePerm),
    StaticVar(Var),
    SigVar(Var),
    LocalVar(Var),
}

impl Perm {
    fn read() -> Perm {
        Perm::Concrete(ConcretePerm::Read)
    }

    fn write() -> Perm {
        Perm::Concrete(ConcretePerm::Write)
    }

    fn move_() -> Perm {
        Perm::Concrete(ConcretePerm::Move)
    }
}


struct ConstraintSet {
    less: BTreeSet<(Perm, Perm)>,
    greater: BTreeSet<(Perm, Perm)>,
}

fn perm_range(p: Perm) -> (Bound<(Perm, Perm)>, Bound<(Perm, Perm)>) {
    (Bound::Included((p, Perm::read())),
     Bound::Included((p, Perm::LocalVar(Var(!0)))))
}

impl ConstraintSet {
    fn new() -> ConstraintSet {
        ConstraintSet {
            less: BTreeSet::new(),
            greater: BTreeSet::new(),
        }
    }

    fn add(&mut self, a: Perm, b: Perm) {
        self.less.insert((a, b));
        self.greater.insert((b, a));
    }

    fn import(&mut self, other: &ConstraintSet) {
        eprintln!("IMPORT {} constraints", other.less.len());
        self.less.extend(other.less.iter().cloned().filter(|&(ref a, ref b)| {
            eprintln!("IMPORT CONSTRAINT: {:?} <= {:?}", a, b);
            true
        }));
        self.greater.extend(other.greater.iter().cloned());
    }

    fn import_substituted<F: Fn(Perm) -> Perm>(&mut self, other: &ConstraintSet, mut f: F) {
        eprintln!("IMPORT {} constraints (substituted)", other.less.len());
        self.less.extend(other.less.iter().map(|&(a, b)| {
            let (a2, b2) = (f(a), f(b));
            eprintln!("IMPORT CONSTRANT: {:?} <= {:?} (substituted from {:?} <= {:?})",
                      a2, b2, a, b);
            (a2, b2)
        }));
        self.greater.extend(other.greater.iter().map(|&(a, b)| (f(a), f(b))));
    }

    fn lower_bound(&self, p: Perm) -> ConcretePerm {
        match p {
            Perm::Concrete(p) => return p,
            _ => {},
        }

        let mut seen = HashSet::new();
        let mut queue = VecDeque::new();
        let mut bound = ConcretePerm::Read;

        seen.insert(p);
        queue.push_back(p);

        while let Some(cur) = queue.pop_front() {
            for &(_, next) in self.greater.range(perm_range(cur)) {
                match next {
                    Perm::Concrete(p) => {
                        bound = cmp::max(bound, p);
                    },
                    _ => {
                        if !seen.contains(&next) {
                            seen.insert(next);
                            queue.push_back(next);
                        }
                    },
                }
            }
        }

        bound
    }

    fn retain_perms<F: Fn(Perm) -> bool>(&mut self, filter: F) {
        let mut all_perms = HashSet::new();
        for &(p1, p2) in &self.less {
            all_perms.insert(p1);
            all_perms.insert(p2);
        }

        // Add edges routing around each removed permission.
        for &p in &all_perms {
            if filter(p) {
                continue;
            }

            // Perms less than `p`, and perms greater than `p`.
            let less = self.greater.range(perm_range(p)).map(|&(a, b)| b).collect::<Vec<_>>();
            let greater = self.less.range(perm_range(p)).map(|&(a, b)| b).collect::<Vec<_>>();

            for &l in &less {
                for &g in &greater {
                    self.add(l, g);
                }
            }
        }

        // Filter out edges involving removed permissions.
        let mut cset = ConstraintSet::new();
        for &(a, b) in self.less.iter().filter(|&&(a, b)| filter(a) && filter(b)) {
            cset.add(a, b);
        }
        *self = cset;
    }
}


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
enum TySource {
    /// Types that appear in the signature of a def.  These are actually the first 1+N locals of
    /// the def, where N is the number of args.
    Sig(DefId, usize),

    /// Types of local variables within the MIR for a def.  This does not include the locals
    /// corresponding to args and return.
    Local(DefId, usize),

    /// Types appearing in `Rvalue`s.  Note that `Rvalue`s appear only on the RHS of
    /// `StatementKind::Assign`.  We identify the rvalue by its containing function, basic block,
    /// and statement index.
    Rvalue(DefId, BasicBlock, usize),

    /// Types of static definitions.  This includes consts/statics as well as struct/enum fields.
    Static(DefId),

    // TODO: better handling of consts
    Const(usize),
}

struct FnSummary<'tcx> {
    sig: LFnSig<'tcx>,
    num_sig_vars: u32,
    cset: ConstraintSet,
}

struct Ctxt<'tcx> {
    lcx: LabeledTyCtxt<'tcx, Option<Perm>>,

    static_summ: HashMap<DefId, LTy<'tcx>>,
    static_cset: ConstraintSet,
    next_static_var: u32,

    fn_summ: HashMap<DefId, FnSummary<'tcx>>,
    /// Cache of labeled tys generated while processing function bodies.  We may process the same
    /// function multiple times, and it would be nice to avoid allocating a new bunch of `LTy`s
    /// each time around.  In this map, `fn_ty_cache[(did, i)]` is the labeled type produced while
    /// processing `did` when the "next local variable" counter was `i`.  This works as long as
    /// function processing visits MIR nodes in the same order and requests the same types each
    /// time.
    ///
    /// The `u32` in the value is the amount to advance `next_local_var` by, after retrieving a
    /// type from the cache.
    fn_ty_cache: HashMap<(DefId, u32), (LTy<'tcx>, u32)>,
}

impl<'tcx> Ctxt<'tcx> {
    fn new(arena: &'tcx DroplessArena) -> Ctxt<'tcx> {
        Ctxt {
            lcx: LabeledTyCtxt::new(arena),

            static_summ: HashMap::new(),
            static_cset: ConstraintSet::new(),
            next_static_var: 0,

            fn_summ: HashMap::new(),
            fn_ty_cache: HashMap::new(),
        }
    }

    fn static_ty<'a, 'gcx>(&mut self, did: DefId, tcx: TyCtxt<'a, 'gcx, 'tcx>) -> LTy<'tcx> {
        let next = &mut self.next_static_var;
        match self.static_summ.entry(did) {
            Entry::Vacant(e) => {
                *e.insert(self.lcx.label(tcx.type_of(did), &mut |ty| {
                    match ty.sty {
                        TypeVariants::TyRef(_, _) |
                        TypeVariants::TyRawPtr(_) => {
                            let v = Var(*next);
                            *next += 1;
                            Some(Perm::StaticVar(v))
                        },
                        _ => None,
                    }
                }))
            },

            Entry::Occupied(e) => *e.get(),
        }
    }

    fn fn_summ<'a, 'gcx>(&mut self,
                         did: DefId,
                         tcx: TyCtxt<'a, 'gcx, 'tcx>) -> &mut FnSummary<'tcx> {
        match self.fn_summ.entry(did) {
            Entry::Vacant(e) => {
                let sig = tcx.fn_sig(did);
                let mut counter = 0;

                let l_sig = {
                    let mut f = |ty: Ty<'tcx>| {
                        match ty.sty {
                            TypeVariants::TyRef(_, _) |
                            TypeVariants::TyRawPtr(_) => {
                                let v = Var(counter);
                                counter += 1;
                                Some(Perm::SigVar(v))
                            },
                            _ => None,
                        }
                    };

                    LFnSig {
                        inputs: self.lcx.label_slice(sig.0.inputs(), &mut f),
                        output: self.lcx.label(sig.0.output(), &mut f),
                    }
                };

                let cset = preload_constraints(tcx, did, l_sig)
                    .unwrap_or_else(ConstraintSet::new);

                e.insert(FnSummary {
                    sig: l_sig,
                    num_sig_vars: counter,
                    cset: cset,
                })
            },

            Entry::Occupied(e) => e.into_mut(),
        }
    }

    fn fn_sig<'a, 'gcx>(&mut self, did: DefId, tcx: TyCtxt<'a, 'gcx, 'tcx>) -> LFnSig<'tcx> {
        self.fn_summ(did, tcx).sig
    }

    fn local_ty(&mut self, did: DefId, next_local: &mut u32, ty: Ty<'tcx>) -> LTy<'tcx> {
        eprintln!("local_ty key = {:?}, {}", did, *next_local);
        match self.fn_ty_cache.entry((did, *next_local)) {
            Entry::Vacant(e) => {
                let first_local = *next_local;
                let lty = self.lcx.label(ty, &mut |ty| {
                    match ty.sty {
                        TypeVariants::TyRef(_, _) |
                        TypeVariants::TyRawPtr(_) => {
                            let v = Var(*next_local);
                            *next_local += 1;
                            Some(Perm::LocalVar(v))
                        },
                        _ => None,
                    }
                });
                // Avoid ambiguity if no vars were generated.
                if *next_local == first_local {
                    *next_local += 1;
                }
                let advance = *next_local - first_local;
                e.insert((lty, advance)).0
            },

            Entry::Occupied(e) => {
                *next_local += e.get().1;
                e.get().0
            },
        }
    }
}

fn preload_constraints(tcx: TyCtxt, def_id: DefId, sig: LFnSig) -> Option<ConstraintSet> {
    let mut cset = ConstraintSet::new();

    let path = tcx.absolute_item_path_str(def_id);
    match &path as &str {
        "core::ptr::<impl *const T>::offset" |
        "core::ptr::<impl *mut T>::offset" => {
            cset.add(sig.output.label.unwrap(),
                     sig.inputs[0].label.unwrap());
        },

        _ => return None,
    }

    eprintln!("PRELOAD CONSTRAINTS for {:?}", def_id);
    eprintln!("  {:?} -> {:?}", sig.inputs, sig.output);
    for &(a, b) in &cset.less {
        eprintln!("    {:?} <= {:?}", a, b);
    }

    Some(cset)
}


struct LocalCtxt<'a, 'gcx: 'tcx, 'tcx: 'a> {
    cx: &'a mut Ctxt<'tcx>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,

    def_id: DefId,
    mir: &'a Mir<'tcx>,
    bbid: BasicBlock,
    stmt_idx: usize,

    cset: ConstraintSet,
    local_tys: IndexVec<Local, LTy<'tcx>>,
    next_local_var: u32,
}

fn collect_perms(ty: LTy) -> Vec<Perm> {
    let mut v = Vec::new();
    collect_perms_into(ty, &mut v);
    v
}

fn collect_perms_into(ty: LTy, v: &mut Vec<Perm>) {
    if let Some(p) = ty.label {
        v.push(p);
    }

    for &arg in ty.args {
        collect_perms_into(arg, v);
    }
}

impl<'a, 'gcx, 'tcx> LocalCtxt<'a, 'gcx, 'tcx> {
    fn new(cx: &'a mut Ctxt<'tcx>,
           tcx: TyCtxt<'a, 'gcx, 'tcx>,
           def_id: DefId,
           mir: &'a Mir<'tcx>) -> LocalCtxt<'a, 'gcx, 'tcx> {
        LocalCtxt {
            cx: cx,
            tcx: tcx,

            def_id: def_id,
            mir: mir,
            bbid: START_BLOCK,
            stmt_idx: !0,

            cset: ConstraintSet::new(),
            local_tys: IndexVec::new(),
            next_local_var: 0,
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


    fn init(&mut self) {
        let sig = self.cx.fn_sig(self.def_id, self.tcx);
        for (l, decl) in self.mir.local_decls.iter_enumerated() {
            let lty =
                if l.index() == 0 { sig.output }
                else if l.index() - 1 < self.mir.arg_count { sig.inputs[l.index() - 1] }
                else { self.local_ty(decl.ty) };
            self.local_tys.push(lty);
        }

        self.do_import();
    }

    fn do_import(&mut self) {
        // Import constraints on statics
        // TODO: we should import these on-demand as we encounter each static
        self.cset.import(&self.cx.static_cset);
    }

    fn instantiate_fn(&mut self, did: DefId) -> LFnSig<'tcx> {
        eprintln!("INSTANTIATE {:?}", did);
        let var_base = self.next_local_var;
        let mut f = |p| {
            match p {
                Perm::SigVar(v) => Perm::LocalVar(Var(var_base + v.0)),
                p => p,
            }
        };
        let sig = {
            let summ = self.cx.fn_summ(did, self.tcx);
            self.cset.import_substituted(&summ.cset, |p| f(p));
            self.next_local_var += summ.num_sig_vars;
            summ.sig
        };

        LFnSig {
            inputs: self.cx.lcx.relabel_slice(sig.inputs, &mut |opt_p| opt_p.map(|p| f(p))),
            output: self.cx.lcx.relabel(sig.output, &mut |opt_p| opt_p.map(|p| f(p))),
        }
    }

    fn do_export(mut self) {
        eprintln!("  original constraints:");
        for &(a, b) in self.cset.less.iter() {
            eprintln!("    {:?} <= {:?}", a, b);
        }

        self.cset.retain_perms(|p| {
            match p {
                Perm::LocalVar(_) => false,
                _ => true,
            }
        });

        let mut save_cset = ConstraintSet::new();
        let perm_level = |p| match p {
            Perm::Concrete(_) => 0,
            Perm::StaticVar(_) => 1,
            Perm::SigVar(_) => 2,
            Perm::LocalVar(_) => 3,
        };
        for &(a, b) in self.cset.less.iter() {
            let level = cmp::max(perm_level(a), perm_level(b));
            match level {
                0 => {},    // Write <= Move and similar uselessness
                1 => {
                    self.cx.static_cset.add(a, b);
                },
                2 => {
                    save_cset.add(a, b);
                },
                3 => {
                    panic!("reduced cset still has LocalVars in it?")
                },
                _ => unreachable!(),
            }
        }

        eprintln!("  exporting constraints (condensed):");
        for &(a, b) in save_cset.less.iter() {
            eprintln!("    {:?} <= {:?}", a, b);
        }

        self.cx.fn_summ(self.def_id, self.tcx).cset = save_cset;
    }

    fn local_ty(&mut self, ty: Ty<'tcx>) -> LTy<'tcx> {
        self.cx.local_ty(self.def_id, &mut self.next_local_var, ty)
    }

    fn local_var_ty(&mut self, l: Local) -> LTy<'tcx> {
        self.local_tys[l]
    }


    /// Compute the type of an `Lvalue` and the maximum permissions for accessing it.
    fn lvalue_lty(&mut self, lv: &Lvalue<'tcx>) -> (LTy<'tcx>, Perm) {
        let (ty, perm, variant) = self.lvalue_lty_downcast(lv);
        assert!(variant.is_none(), "expected non-Downcast result");
        (ty, perm)
    }

    fn lvalue_lty_downcast(&mut self, lv: &Lvalue<'tcx>) -> (LTy<'tcx>, Perm, Option<usize>) {
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
                        // TODO - reimplement `min`
                        //(base_ty.args[0], self.min_perm(base_perm, base_ty.label.unwrap())),
                        (base_ty.args[0], base_ty.label.unwrap(), None),
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

    fn rvalue_lty(&mut self, rv: &Rvalue<'tcx>) -> (LTy<'tcx>, Perm) {
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
                        unimplemented!()
                            /*
                        let source = TySource::Rvalue(self.def_id, self.bbid, self.stmt_idx);
                        let elem_ty = self.cx.labeled(source, VarKind::Local, || ty);
                        // Record a pseudo-assignment from each array element operand to the array
                        // element type.
                        for op in ops {
                            let (op_ty, op_perm) = self.operand_lty(op);
                            self.propagate(elem_ty, op_ty, op_perm);
                        }
                        let args = self.cx.lcx.mk_slice(&[elem_ty]);
                        let arr_ty = self.cx.lcx.mk(rv.ty(self.mir, self.tcx), args, None);
                        (arr_ty, Perm::move_())
                        */
                    },
                    AggregateKind::Tuple => {
                        let tuple_ty = self.local_ty(ty);
                        for (&elem_ty, op) in tuple_ty.args.iter().zip(ops.iter()) {
                            let (op_ty, op_perm) = self.operand_lty(op);
                            self.propagate(elem_ty, op_ty, op_perm);
                        }
                        (tuple_ty, Perm::move_())
                    },
                    AggregateKind::Adt(def, disr, substs, union_variant) => {
                        unimplemented!()
                            /*
                        if let Some(union_variant) = union_variant {
                            assert!(ops.len() == 1);
                            let field_ty = self.def_field_lty(def, 0, union_variant);
                            let (op_ty, op_perm) = self.operand_lty(&ops[0]);
                            self.propagate(field_ty, op_ty, op_perm);
                        } else {
                            for (i, op) in ops.iter().enumerate() {
                                let field_ty = self.def_field_lty(def, disr, i);
                                let (op_ty, op_perm) = self.operand_lty(op);
                                self.propagate(field_ty, op_ty, op_perm);
                            }
                        }

                        // TODO: handle substs
                        let ty = self.cx.lcx.mk(rv.ty(self.mir, self.tcx), &[], None);
                        (ty, Perm::move_())
                        */
                    },
                    AggregateKind::Closure(_, _) => unimplemented!(),
                }
            },
        }
    }

    fn operand_lty(&mut self, op: &Operand<'tcx>) -> (LTy<'tcx>, Perm) {
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
    fn propagate(&mut self, lhs: LTy<'tcx>, rhs: LTy<'tcx>, max_perm: Perm) {
        if let (Some(l_perm), Some(r_perm)) = (lhs.label, rhs.label) {
            self.propagate_perm(l_perm, r_perm);
            self.propagate_perm(l_perm, max_perm);
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

    fn propagate_perm(&mut self, p1: Perm, p2: Perm) {
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

    fn handle_basic_block(&mut self, bbid: BasicBlock, bb: &BasicBlockData<'tcx>) {
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


struct LTySource<'a, 'gcx: 'tcx, 'tcx: 'a> {
    cx: &'a mut Ctxt<'tcx>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,

    // XXX - bit of a hack.  We keep the def id of the last call to `fn_sig`, and refer to that
    // inside the map_types callback to figure out the right scope for any SigVars in the type.
    // This relies on the fact that map_types invokes the next TypeSource method only once all
    // callback invocations resulting for the previous TypeSource call have been made.
    last_sig_did: Option<DefId>,
}

impl<'a, 'gcx, 'tcx> TypeSource for LTySource<'a, 'gcx, 'tcx> {
    type Type = LTy<'tcx>;
    type Signature = LFnSig<'tcx>;

    fn expr_type(&mut self, e: &ast::Expr) -> Option<Self::Type> {
        self.last_sig_did = None;
        None
    }

    fn pat_type(&mut self, p: &ast::Pat) -> Option<Self::Type> {
        self.last_sig_did = None;
        None
    }

    fn def_type(&mut self, did: DefId) -> Option<Self::Type> {
        self.last_sig_did = None;
        Some(self.cx.static_ty(did, self.tcx))
    }

    fn fn_sig(&mut self, did: DefId) -> Option<Self::Signature> {
        self.last_sig_did = Some(did);
        Some(self.cx.fn_sig(did, self.tcx))
    }

    fn closure_sig(&mut self, did: DefId) -> Option<Self::Signature> {
        self.last_sig_did = None;
        // TODO - should probably support this
        None
    }
}

impl<'tcx> type_map::Signature<LTy<'tcx>> for LFnSig<'tcx> {
    fn num_inputs(&self) -> usize {
        self.inputs.len()
    }

    fn input(&self, idx: usize) -> LTy<'tcx> {
        self.inputs[idx]
    }

    fn output(&self) -> LTy<'tcx> {
        self.output
    }
}


fn get_sig<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                           cx: &mut Ctxt<'tcx>,
                           def_id: DefId) -> LFnSig<'tcx> {
    let ty = tcx.type_of(def_id);
    match ty.sty {
        TypeVariants::TyFnDef(_, _) |
        TypeVariants::TyFnPtr(_) => cx.fn_sig(def_id, tcx),

        _ => {
            return LFnSig {
                inputs: &[],
                output: cx.static_ty(def_id, tcx),
            };
        },
    }
}


struct Pretty<'tcx, L: 'tcx>(LabeledTy<'tcx, L>);

fn pretty_slice<'a, 'tcx, L>(tys: &'a [LabeledTy<'tcx, L>]) -> &'a [Pretty<'tcx, L>] {
    unsafe { ::std::mem::transmute(tys) }
}


fn perm_label(p: Option<ConcretePerm>) -> &'static str {
    match p {
        Some(ConcretePerm::Read) => "READ ",
        Some(ConcretePerm::Write) => "WRITE ",
        Some(ConcretePerm::Move) => "MOVE ",
        None => "",
    }
}


struct PrettyLabel<L>(L);

impl fmt::Debug for PrettyLabel<ConcretePerm> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            ConcretePerm::Read => write!(fmt, "READ"),
            ConcretePerm::Write => write!(fmt, "WRITE"),
            ConcretePerm::Move => write!(fmt, "MOVE"),
        }
    }
}

impl<L> fmt::Debug for PrettyLabel<Option<L>> where L: Copy, PrettyLabel<L>: fmt::Debug {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Some(x) => write!(fmt, "{:?}", PrettyLabel(x)),
            None => Ok(()),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct PrintVar(Perm);

impl fmt::Debug for PrettyLabel<(ConcretePerm, PrintVar)> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}{:?}", PrettyLabel((self.0).0), PrettyLabel((self.0).1))
    }
}

impl fmt::Debug for PrettyLabel<PrintVar> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match (self.0).0 {
            Perm::Concrete(_) => Ok(()),
            Perm::StaticVar(v) => write!(fmt, "#s{}", v.index()),
            Perm::SigVar(v) => write!(fmt, "#f{}", v.index()),
            Perm::LocalVar(v) => write!(fmt, "#l{}", v.index()),
        }
    }
}



impl<'tcx, L> fmt::Debug for Pretty<'tcx, L> where L: Copy + fmt::Debug, PrettyLabel<L>: fmt::Debug {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.0.ty.sty {
            TypeVariants::TyRef(_, mty) =>
                write!(fmt, "&{} {:?} {:?}",
                       if mty.mutbl == hir::MutImmutable { "" } else { "mut" },
                       PrettyLabel(self.0.label),
                       Pretty(self.0.args[0])),
            TypeVariants::TyRawPtr(mty) =>
                write!(fmt, "*{} {:?} {:?}",
                       if mty.mutbl == hir::MutImmutable { "const" } else { "mut" },
                       PrettyLabel(self.0.label),
                       Pretty(self.0.args[0])),
            _ => write!(fmt, "{:?}", self.0),
        }
    }
}


pub fn analyze(st: &CommandState, cx: &driver::Ctxt) {
    let mut ctxt = Ctxt::new(cx.ty_arena());

    let mut fixed_vars = Vec::new();
    {
        let source = LTySource {
            cx: &mut ctxt,
            tcx: cx.ty_ctxt(),
            last_sig_did: None,
        };

        type_map::map_types(cx.hir_map(), source, &st.krate(), |source, ast_ty, lty| {
            eprintln!("match {:?} ({:?}) with {:?}", ast_ty, ast_ty.id, lty);
            if st.marked(ast_ty.id, "box") {
                if let Some(p) = lty.label {
                    fixed_vars.push((p, source.last_sig_did, Perm::move_()));
                }
            }

            if st.marked(ast_ty.id, "mut") {
                if let Some(p) = lty.label {
                    fixed_vars.push((p, source.last_sig_did, Perm::write()));
                }
            }

            if st.marked(ast_ty.id, "ref") {
                if let Some(p) = lty.label {
                    fixed_vars.push((p, source.last_sig_did, Perm::read()));
                }
            }
        });
    }

    // For any marked types that are in signatures, add constraints to the parent function's cset.
    for (p, did, min_perm) in fixed_vars {
        eprintln!("FIXED VAR: {:?} = {:?} (in {:?})", p, min_perm, did);
        match p {
            Perm::StaticVar(_) => ctxt.static_cset.add(min_perm, p),
            Perm::SigVar(_) => {
                let did = did.expect("expected DefId for SigVar");
                ctxt.fn_summ(did, cx.ty_ctxt()).cset.add(min_perm, p);
            }
            _ => panic!("expected StaticVar or SigVar, but got {:?}", p),
        }
    }


    let tcx = cx.ty_ctxt();
    for _ in 0 .. 5 {
        for &def_id in tcx.mir_keys(LOCAL_CRATE).iter() {
            let mir = tcx.optimized_mir(def_id);
            let mut local_cx = LocalCtxt::new(&mut ctxt, tcx, def_id, mir);

            eprintln!("\nmir for {:?}", def_id);

            local_cx.init();

            eprintln!("  locals:");
            for l in mir.local_decls.indices() {
                let (ty, _) = local_cx.lvalue_lty(&Lvalue::Local(l));
                eprintln!("    {:?}: {:?}", l, ty);
            }

            for (bbid, bb) in ReversePostorder::new(&mir, START_BLOCK) {
                local_cx.handle_basic_block(bbid, bb);
            }

            let mut new_lcx = LabeledTyCtxt::new(cx.ty_arena());
            eprintln!("  locals (after):");
            for l in mir.local_decls.indices() {
                let (ty, _) = local_cx.lvalue_lty(&Lvalue::Local(l));
                let ty2 = new_lcx.relabel(ty, &mut |&v: &Option<_>| {
                    if v.is_none() {
                        return "--";
                    }
                    match local_cx.cset.lower_bound(v.unwrap()) {
                        ConcretePerm::Read => "REF",
                        ConcretePerm::Write => "MUT",
                        ConcretePerm::Move => "BOX",
                    }
                });
                eprintln!("    {:?}: {:?}", l, ty2);
            }

            local_cx.do_export();
        }
    }


    eprintln!("\n === summary ===");
    let mut new_lcx = LabeledTyCtxt::new(cx.ty_arena());

    {
        let mut statics_sorted = ctxt.static_summ.iter().collect::<Vec<_>>();
        statics_sorted.sort_by_key(|&(k, _)| k);
        for (&def_id, &ty) in statics_sorted {
            let ty = new_lcx.relabel(ty, &mut |p| {
                p.as_ref().map(|&p| ctxt.static_cset.lower_bound(p))
            });

            eprintln!("{:?}: {:?}", def_id, Pretty(ty));
        }
    }

    let mut new_lcx = LabeledTyCtxt::new(cx.ty_arena());
    let mut fns_sorted = ctxt.fn_summ.iter().collect::<Vec<_>>();
    fns_sorted.sort_by_key(|&(k, _)| k);
    for (&def_id, summ) in fns_sorted {
        let cset = &summ.cset;
        let mut func2 = |p: &Option<_>| {
            p.map(|p| (cset.lower_bound(p), PrintVar(p)))
        };
        let inputs = new_lcx.relabel_slice(summ.sig.inputs, &mut func2);
        let output = new_lcx.relabel(summ.sig.output, &mut func2);
        eprintln!("{:?}:\n  {:?} -> {:?}", def_id, pretty_slice(inputs), Pretty(output));
        for &(a, b) in &cset.less {
            if a == b || a == Perm::read() || b == Perm::move_() {
                continue;
            }
            eprintln!("    {:?} <= {:?}", a, b);
        }
    }
}
