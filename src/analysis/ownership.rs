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


// - "permanent" vars:
//   - struct fields
//   - fn args
//   - fn substs
// - mono fn summaries: constraints on permanent vars (structs + the fn's args + substs)
// - struct constraints? - field must be BOX, or field1 <= field2
// - "local" vars:
//   - MIR locals
//   - function constraint instantiations (
// - constraint maps:
//   - set of "must be BOX"
//   - map of "k <= v" (if k is BOX then v must be BOX)

// * are constraints only ever added? - yes


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
    Var(Var),
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
     Bound::Included((p, Perm::Var(Var(!0)))))
}

fn var_range(v: Var) -> (Bound<(Perm, Perm)>, Bound<(Perm, Perm)>) {
    perm_range(Perm::Var(v))
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
        self.less.extend(other.less.iter().cloned().filter(|&(ref a, ref b)| {
            eprintln!("IMPORT CONSTRAINT: {:?} <= {:?}", a, b);
            true
        }));
        self.greater.extend(other.greater.iter().cloned());
    }

    fn import_lower_bounds<I: Iterator<Item=(Var, ConcretePerm)>>(&mut self, mut iter: I) {
        for (var, perm) in iter {
            eprintln!("FIELD IMPORT: {:?} = {:?}", var, perm);
            self.add(Perm::Concrete(perm), Perm::Var(var));
        }
    }

    fn var_lower_bound(&self, v: Var) -> ConcretePerm {
        let mut seen = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(v);
        let mut bound = ConcretePerm::Read;

        while let Some(cur) = queue.pop_front() {
            for &(_, next) in self.greater.range(var_range(cur)) {
                match next {
                    Perm::Concrete(p) => {
                        bound = cmp::max(bound, p);
                    },
                    Perm::Var(v) => {
                        if !seen.contains(&v) {
                            seen.insert(v);
                            queue.push_back(v);
                        }
                    },
                }
            }
        }

        bound
    }

    fn lower_bound(&self, a: Perm) -> ConcretePerm {
        match a {
            Perm::Concrete(p) => p,
            Perm::Var(v) => self.var_lower_bound(v),
        }
    }

    fn condense<I: IntoIterator<Item=Var>>(&mut self, vars: I) {
        let mut all_perms = HashSet::new();
        for &(p1, p2) in &self.less {
            all_perms.insert(p1);
            all_perms.insert(p2);
        }

        let vars = vars.into_iter().collect::<HashSet<_>>();

        // Add edges routing around each var not in `vars`.
        for &p in &all_perms {
            let v = match p {
                Perm::Var(v) => v,
                Perm::Concrete(_) => { continue; },
            };
            if vars.contains(&v) {
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

        // Filter out edges not involving `vars`.
        let is_var = |p| match p {
            Perm::Var(v) => vars.contains(&v),
            Perm::Concrete(_) => true,
        };

        let mut cset = ConstraintSet::new();
        for &(a, b) in self.less.iter().filter(|&&(a, b)| {
            match (a, b) {
                (Perm::Var(v1), Perm::Var(v2)) => vars.contains(&v1) && vars.contains(&v2),
                (Perm::Var(v1), Perm::Concrete(_)) => vars.contains(&v1),
                (Perm::Concrete(_), Perm::Var(v2)) => vars.contains(&v2),
                (Perm::Concrete(_), Perm::Concrete(_)) => false,
            }
        }) {
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum VarKind {
    Static,
    Sig(DefId),
    Local,
}

struct Vars {
    field_min: HashMap<Var, ConcretePerm>,
    sig_cset: HashMap<DefId, ConstraintSet>,
    sig_vars: HashMap<DefId, HashSet<Var>>,
    sig_accessed: HashSet<DefId>,

    next_var: u32,
}

impl Vars {
    fn new() -> Vars {
        Vars {
            field_min: HashMap::new(),
            sig_cset: HashMap::new(),
            sig_vars: HashMap::new(),
            sig_accessed: HashSet::new(),

            next_var: 0,
        }
    }

    fn fresh(&mut self, kind: VarKind) -> Var {
        let v = Var(self.next_var);
        self.next_var += 1;
        match kind {
            VarKind::Static => {
                self.field_min.insert(v, ConcretePerm::Read);
            },
            VarKind::Sig(def_id) => {
                self.sig_vars.entry(def_id).or_insert_with(HashSet::new).insert(v);
            },
            VarKind::Local => {},
        }
        v
    }

    fn collect_field_mins(&mut self, cset: &ConstraintSet) {
        for (&v, p) in self.field_min.iter_mut() {
            let new_min = cset.var_lower_bound(v);
            eprintln!("FIELD EXPORT: {:?} = {:?} -> {:?}", v, *p, new_min);
            *p = cmp::max(*p, new_min);
        }
    }

    fn ensure_sig_vars(&mut self, did: DefId) {
        self.sig_vars.entry(did).or_insert_with(HashSet::new);
    }

    fn add_sig_constraint(&mut self, did: DefId, a: Perm, b: Perm) {
        self.sig_cset.entry(did).or_insert_with(ConstraintSet::new).add(a, b)
    }
}

struct Ctxt<'tcx> {
    lcx: LabeledTyCtxt<'tcx, Option<Perm>>,

    /// Cache of already-labeled types.  We use this to avoid re-labeling the same type twice,
    /// which would produce variables that are independent when they shouldn't be.
    lty_map: HashMap<TySource, LTy<'tcx>>,

    min_map: HashMap<(Perm, Perm), Var>,

    vars: Vars,
}

impl<'tcx> Ctxt<'tcx> {
    fn new(arena: &'tcx DroplessArena) -> Ctxt<'tcx> {
        Ctxt {
            lcx: LabeledTyCtxt::new(arena),

            lty_map: HashMap::new(),
            min_map: HashMap::new(),

            vars: Vars::new(),
        }
    }

    fn labeled<F>(&mut self, source: TySource, kind: VarKind, mk_ty: F) -> LTy<'tcx>
            where F: FnOnce() -> Ty<'tcx> {
        let vars = &mut self.vars;

        match self.lty_map.entry(source) {
            Entry::Vacant(e) => {
                *e.insert(self.lcx.label(mk_ty(), &mut |ty| {
                    match ty.sty {
                        TypeVariants::TyRef(_, _) |
                        TypeVariants::TyRawPtr(_) => Some(Perm::Var(vars.fresh(kind))),
                        // TODO: handle Box<_>
                        _ => None,
                    }
                }))
            },

            Entry::Occupied(e) => *e.get(),
        }
    }

    fn labeled_sig<'a, 'gcx>(&mut self,
                             ty: LTy<'tcx>,
                             tcx: TyCtxt<'a, 'gcx, 'tcx>) -> LFnSig<'tcx> {
        match ty.ty.sty {
            TypeVariants::TyFnDef(def_id, _) => {
                let poly_sig = self.labeled_def_sig(def_id, tcx);
                LFnSig {
                    inputs: self.lcx.subst_slice(poly_sig.inputs, &ty.args),
                    output: self.lcx.subst(poly_sig.output, &ty.args),
                }
            },

            TypeVariants::TyFnPtr(_) => {
                LFnSig {
                    inputs: &ty.args[.. ty.args.len() - 1],
                    output: ty.args[ty.args.len() - 1],
                }
            },

            TypeVariants::TyClosure(_, _) => unimplemented!(),

            _ => unimplemented!(),
        }
    }

    fn labeled_def_sig<'a, 'gcx>(&mut self,
                                 def_id: DefId,
                                 tcx: TyCtxt<'a, 'gcx, 'tcx>) -> LFnSig<'tcx> {
        let sig = tcx.fn_sig(def_id);
        let inputs = sig.0.inputs().iter().enumerate()
            .map(|(i, &ty)| self.labeled(TySource::Sig(def_id, 1 + i),
                                         VarKind::Sig(def_id),
                                         || ty))
            .collect::<Vec<_>>();
        let output = self.labeled(TySource::Sig(def_id, 0),
                                  VarKind::Sig(def_id),
                                  || sig.0.output());
        LFnSig {
            inputs: &self.lcx.mk_slice(&inputs),
            output: output,
        }
    }

    fn min_perm_var(&mut self, p1: Perm, p2: Perm) -> Var {
        let vars = &mut self.vars;
        let ps = if p1 < p2 { (p1, p2) } else { (p2, p1) };
        *self.min_map.entry(ps).or_insert_with(|| {
            let v = vars.fresh(VarKind::Local);
            eprintln!("ADD MIN: {:?} <= min({:?}, {:?})", v, p1, p2);
            v
        })
    }
}

struct LocalCtxt<'a, 'gcx: 'tcx, 'tcx: 'a> {
    cx: &'a mut Ctxt<'tcx>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,

    def_id: DefId,
    mir: &'a Mir<'tcx>,
    bbid: BasicBlock,
    stmt_idx: usize,

    cset: ConstraintSet,
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


    fn do_import(&mut self) {
        self.cset.import_lower_bounds(
            self.cx.vars.field_min.iter().map(|(&v, &p)| (v, p)));
    }

    fn do_export(mut self) {
        self.cx.vars.collect_field_mins(&self.cset);

        eprintln!("exporting constraints");
        eprintln!("  initial:");
        for &(a, b) in self.cset.less.iter() {
            eprintln!("    {:?} <= {:?}", a, b);
        }

        let mut v = Vec::new();
        for i in 0 .. 1 + self.mir.arg_count {
            let (ty, _) = self.lvalue_lty(&Lvalue::Local(Local::new(i)));
            collect_perms_into(ty, &mut v);
        }
        eprintln!("  collected sig permissions:");
        for &p in &v {
            eprintln!("    {:?}", p);
        }
        self.cset.condense(v.into_iter().filter_map(|p| match p {
            Perm::Var(v) => Some(v),
            Perm::Concrete(_) => None,
        }));

        eprintln!("  condensed:");
        for &(a, b) in self.cset.less.iter() {
            eprintln!("    {:?} <= {:?}", a, b);
        }

        self.cx.vars.sig_cset.insert(self.def_id, self.cset);
    }

    fn import_fn_sig_constraints(&mut self, fn_ty: LTy<'tcx>) {
        let (def_id, substs) = match fn_ty.ty.sty {
            TypeVariants::TyFnDef(def_id, substs) => (def_id, substs),
            _ => return,
        };
        // TODO: handle substs
        // TODO: add callgraph edge

        if !self.cx.vars.sig_accessed.contains(&def_id) {
            self.cx.vars.sig_accessed.insert(def_id);
            // On first access, check if there are preloaded constraints for this def.
            if !self.cx.vars.sig_cset.contains_key(&def_id) {
                if let Some(cset) = self.find_preloaded_constraints(def_id) {
                    self.cx.vars.sig_cset.insert(def_id, cset);
                }
            }
        }

        if let Some(cset) = self.cx.vars.sig_cset.get(&def_id) {
            self.cset.import(cset);
        }
    }

    fn find_preloaded_constraints(&mut self, def_id: DefId) -> Option<ConstraintSet> {
        let path = self.tcx.absolute_item_path_str(def_id);

        let mut cset = ConstraintSet::new();
        let sig = self.cx.labeled_def_sig(def_id, self.tcx);

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


    /// Compute the type of an `Lvalue` and the maximum permissions for accessing it.
    fn lvalue_lty(&mut self, lv: &Lvalue<'tcx>) -> (LTy<'tcx>, Perm) {
        match *lv {
            Lvalue::Local(l) => {
                let idx = l.index();
                let source =
                    if idx < 1 + self.mir.arg_count { TySource::Sig(self.def_id, idx) }
                    else { TySource::Local(self.def_id, idx) };
                let mir = self.mir;
                (self.cx.labeled(source, VarKind::Local, || mir.local_decls[l].ty),
                 Perm::move_())
            },

            Lvalue::Static(ref s) => {
                (self.cx.labeled(TySource::Static(s.def_id), VarKind::Static, || s.ty),
                 Perm::move_())
            },

            Lvalue::Projection(ref p) => {
                let (base_ty, base_perm) = self.lvalue_lty(&p.base);
                match p.elem {
                    // Access permissions for a deref are the minimum of all pointers along the
                    // path to the value.
                    ProjectionElem::Deref =>
                        (base_ty.args[0], self.min_perm(base_perm, base_ty.label.unwrap())),
                    ProjectionElem::Field(f, _) => (self.field_lty(base_ty, f), base_perm),
                    ProjectionElem::Index(ref index_op) =>
                        (base_ty.args[0], base_perm),
                    ProjectionElem::ConstantIndex { .. } => unimplemented!(),
                    ProjectionElem::Subslice { .. } => unimplemented!(),
                    ProjectionElem::Downcast(_, _) => unimplemented!(),
                }
            },
        }
    }

    fn field_lty(&mut self, base_ty: LTy<'tcx>, f: Field) -> LTy<'tcx> {
        match base_ty.ty.sty {
            TypeVariants::TyAdt(def, substs) => {
                let poly_ty = self.def_field_lty(def, 0, f.index());
                self.cx.lcx.subst(poly_ty, &base_ty.args)
            },
            TypeVariants::TyTuple(tys, _) => base_ty.args[f.index()],
            _ => unimplemented!(),
        }
    }

    // TODO: handle substs
    fn def_field_lty(&mut self, adt: &'tcx AdtDef, variant: usize, field: usize) -> LTy<'tcx> {
        let field_def = &adt.variants[variant].fields[field];
        let source = TySource::Static(field_def.did);
        let tcx = self.tcx;
        self.cx.labeled(source, VarKind::Static, || {
            tcx.type_of(field_def.did)
        })
    }

    fn rvalue_lty(&mut self, rv: &Rvalue<'tcx>) -> (LTy<'tcx>, Perm) {
        match *rv {
            Rvalue::Use(ref op) => self.operand_lty(op),
            Rvalue::Repeat(ref op, len) => {
                let mir = self.mir;
                let tcx = self.tcx;
                let source = TySource::Rvalue(self.def_id, self.bbid, self.stmt_idx);
                let arr_ty = self.cx.labeled(source, VarKind::Local, || rv.ty(mir, tcx));

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
            Rvalue::Len(_) => {
                // Produces a usize
                let ty = self.cx.lcx.mk(rv.ty(self.mir, self.tcx), &[], None);
                (ty, Perm::move_())
            },
            Rvalue::Cast(_, ref op, ty) => {
                let source = TySource::Rvalue(self.def_id, self.bbid, self.stmt_idx);
                let cast_ty = self.cx.labeled(source, VarKind::Local, || ty);
                let (op_ty, op_perm) = self.operand_lty(op);
                self.propagate(cast_ty, op_ty, Perm::move_());
                (cast_ty, op_perm)
            },
            Rvalue::BinaryOp(op, ref a, ref b) |
            Rvalue::CheckedBinaryOp(op, ref a, ref b) => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem |
                BinOp::BitXor | BinOp::BitAnd | BinOp::BitOr | BinOp::Shl | BinOp::Shr |
                BinOp::Eq | BinOp::Lt | BinOp::Le | BinOp::Ne | BinOp::Ge | BinOp::Gt => {
                    // These ops only ever produce primitive values.
                    let ty = self.cx.lcx.mk(rv.ty(self.mir, self.tcx), &[], None);
                    (ty, Perm::move_())
                },

                BinOp::Offset => self.operand_lty(a),
            },
            Rvalue::NullaryOp(op, ty) => unimplemented!(),
            Rvalue::UnaryOp(op, ref a) => match op {
                UnOp::Not | UnOp::Neg => {
                    let ty = self.cx.lcx.mk(rv.ty(self.mir, self.tcx), &[], None);
                    (ty, Perm::move_())
                },
            },
            Rvalue::Discriminant(ref lv) => unimplemented!(),
            Rvalue::Aggregate(ref kind, ref ops) => {
                match **kind {
                    AggregateKind::Array(ty) => {
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
                    },
                    AggregateKind::Tuple => {
                        let args = ops.iter().map(|op| self.operand_lty(op).0).collect::<Vec<_>>();
                        let args = self.cx.lcx.mk_slice(&args);
                        let ty = self.cx.lcx.mk(rv.ty(self.mir, self.tcx), args, None);
                        (ty, Perm::move_())
                    },
                    AggregateKind::Adt(def, disr, substs, union_variant) => {
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
                let ty = self.cx.labeled(TySource::Const(c.ty as *const _ as usize),
                                         VarKind::Local,
                                         || c.ty);
                (ty, Perm::move_())
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

    fn min_perm(&mut self, p1: Perm, p2: Perm) -> Perm {
        // Handle some easy cases precisely
        match (p1, p2) {
            (Perm::Concrete(ConcretePerm::Read), _) => return p1,
            (_, Perm::Concrete(ConcretePerm::Read)) => return p2,
            (Perm::Concrete(ConcretePerm::Move), _) => return p2,
            (_, Perm::Concrete(ConcretePerm::Move)) => return p1,
            (_, _) => {},
        }

        // For other cases, we introduce a new var and constrain it to be less than both p1 and p2.
        // This works right when the min is used on the RHS of a constraint (`_ <= min(p1, p2)`),
        // but if it's used on the LHS, the constraint will have no effect.  The LHS case is hard
        // to handle, but only comes up in some tricky pointer-to-pointer cases, so it seems safe
        // to ignore for now.
        //
        // Note that chaining mins works: `min(p1, min(p2, p3))` produces constraints `p4 <= p1`
        // and `p4 <= min(p2, p3)` - the nested `min` is on the RHS.
        //
        // TODO: make the `min <= _` case work, or ensure it never happens
        let p = Perm::Var(self.cx.min_perm_var(p1, p2));
        self.propagate_perm(p, p1);
        self.propagate_perm(p, p2);
        p
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
                let sig = self.cx.labeled_sig(func_ty, self.tcx);
                self.import_fn_sig_constraints(func_ty);
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
}

impl<'a, 'gcx, 'tcx> TypeSource for LTySource<'a, 'gcx, 'tcx> {
    type Type = LTy<'tcx>;
    type Signature = LFnSig<'tcx>;

    fn expr_type(&mut self, e: &ast::Expr) -> Option<Self::Type> {
        None
    }

    fn pat_type(&mut self, p: &ast::Pat) -> Option<Self::Type> {
        None
    }

    fn def_type(&mut self, did: DefId) -> Option<Self::Type> {
        let tcx = self.tcx;
        Some(self.cx.labeled(TySource::Static(did), VarKind::Static, || tcx.type_of(did)))
    }

    fn fn_sig(&mut self, did: DefId) -> Option<Self::Signature> {
        Some(self.cx.labeled_def_sig(did, self.tcx))
    }

    fn closure_sig(&mut self, did: DefId) -> Option<Self::Signature> {
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
        TypeVariants::TyFnPtr(_) => {},

        _ => {
            return LFnSig {
                inputs: &[],
                output: cx.labeled(TySource::Static(def_id), VarKind::Static, || ty),
            };
        },
    }

    let sig = ty.fn_sig(tcx);
    let inputs = sig.0.inputs().iter().enumerate()
        .map(|(i, &ty)| cx.labeled(TySource::Sig(def_id, 1 + i), VarKind::Sig(def_id), || ty))
        .collect::<Vec<_>>();
    let output = cx.labeled(TySource::Sig(def_id, 0), VarKind::Sig(def_id), || sig.0.output());
    LFnSig {
        inputs: cx.lcx.mk_slice(&inputs),
        output: output,
    }
}

struct Pretty<'tcx, L: 'tcx>(LabeledTy<'tcx, L>);

fn perm_label(p: Option<ConcretePerm>) -> &'static str {
    match p {
        Some(ConcretePerm::Read) => "READ ",
        Some(ConcretePerm::Write) => "WRITE ",
        Some(ConcretePerm::Move) => "MOVE ",
        None => "",
    }
}

fn pretty_slice<'a, 'tcx, L>(tys: &'a [LabeledTy<'tcx, L>]) -> &'a [Pretty<'tcx, L>] {
    unsafe { ::std::mem::transmute(tys) }
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

impl fmt::Debug for PrettyLabel<(ConcretePerm, Option<Var>)> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if let Some(v) = (self.0).1 {
            write!(fmt, "{:?}#{}", PrettyLabel((self.0).0), v.index())
        } else {
            write!(fmt, "{:?}#?", PrettyLabel((self.0).0))
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

    let mut fixed_vars = HashMap::new();
    {
        let source = LTySource {
            cx: &mut ctxt,
            tcx: cx.ty_ctxt(),
        };

        type_map::map_types(cx.hir_map(), source, &st.krate(), |source, ast_ty, lty| {
            eprintln!("match {:?} ({:?}) with {:?}", ast_ty, ast_ty.id, lty);
            if st.marked(ast_ty.id, "box") {
                if let Some(Perm::Var(v)) = lty.label {
                    fixed_vars.insert(v, Perm::move_());
                }
            }

            if st.marked(ast_ty.id, "mut") {
                if let Some(Perm::Var(v)) = lty.label {
                    fixed_vars.insert(v, Perm::write());
                }
            }

            if st.marked(ast_ty.id, "ref") {
                if let Some(Perm::Var(v)) = lty.label {
                    fixed_vars.insert(v, Perm::read());
                }
            }
        });
    }

    // For any marked types that are in signatures, add constraints to the parent function's cset.
    let mut sig_constraints = Vec::new();
    for (&did, vars) in &ctxt.vars.sig_vars {
        for &v in vars {
            if let Some(p) = fixed_vars.remove(&v) {
                sig_constraints.push((did, v, p));
            }
        }
    }

    for (did, v, p) in sig_constraints {
        ctxt.vars.add_sig_constraint(did, Perm::Var(v), p);
        ctxt.vars.add_sig_constraint(did, p, Perm::Var(v));
    }

    // Check the remaining marked types for ones that should be added to the field constraints.
    for (&v, &p) in &fixed_vars {
        if ctxt.vars.field_min.contains_key(&v) {
            let p = match p {
                Perm::Concrete(p) => p,
                _ => panic!("expected Perm::Concrete"),
            };
            ctxt.vars.field_min.insert(v, p);
        }
    }



    let tcx = cx.ty_ctxt();
    for _ in 0 .. 5 {
        for &def_id in tcx.mir_keys(LOCAL_CRATE).iter() {
            let mir = tcx.optimized_mir(def_id);
            let mut local_cx = LocalCtxt::new(&mut ctxt, tcx, def_id, mir);
            local_cx.do_import();

            eprintln!("\nmir for {:?}", def_id);
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
        let mut lty_map_sorted = ctxt.lty_map.iter().collect::<Vec<_>>();
        lty_map_sorted.sort_by_key(|&(k, _)| k);
        for (&k, &v) in lty_map_sorted {
            let def_id = match k {
                TySource::Static(def_id) => def_id,
                _ => continue,
            };

            let ty = new_lcx.relabel(v, &mut |p| {
                p.as_ref().and_then(|&p| match p {
                    Perm::Var(v) => ctxt.vars.field_min.get(&v).cloned()
                        .map(|p| (p, Some(v))),
                    Perm::Concrete(p) => Some((p, None)),
                })
            });

            eprintln!("{:?}: {:?}", def_id, Pretty(ty));
        }
    }

    let mut new_lcx = LabeledTyCtxt::new(cx.ty_arena());
    let mut def_ids = tcx.mir_keys(LOCAL_CRATE).iter().cloned().collect::<Vec<_>>();
    def_ids.sort();
    for def_id in def_ids {
        let sig = get_sig(cx.ty_ctxt(), &mut ctxt, def_id);
        let cset = ctxt.vars.sig_cset.get(&def_id);
        let mut func2 = |p: &Option<_>| {
            p.as_ref().and_then(|&p| match p {
                Perm::Var(v) => cset.as_ref().map(|cset| cset.lower_bound(p))
                    .map(|p| (p, Some(v))),
                Perm::Concrete(p) => Some((p, None)),
            })
        };
        let inputs = new_lcx.relabel_slice(sig.inputs, &mut func2);
        let output = new_lcx.relabel(sig.output, &mut func2);
        eprintln!("{:?}:\n  {:?} -> {:?}", def_id, pretty_slice(inputs), Pretty(output));
        if let Some(cset) = cset {
            for &(a, b) in &cset.less {
                if a == b || a == Perm::read() || b == Perm::move_() {
                    continue;
                }
                eprintln!("    {:?} <= {:?}", a, b);
            }
        }
    }
}








/*
struct PtrCountVisitor {
    count: usize,
}

impl<'tcx> TypeVisitor<'tcx> for PtrCountVisitor {
    fn visit_ty(&mut self, ty: ty::Ty<'tcx>) -> bool {
        use rustc::ty::TypeVariants::*;
        match ty.sty {
            TyRawPtr(mty) => {
                self.count += 1;
            },
            _ => {},
        }

        ty.super_visit_with(self)
    }
}

fn count_ty_ptrs(ty: ty::Ty) -> usize {
    let mut v = PtrCountVisitor {
        count: 0,
    };
    v.visit_ty(ty);
    v.count
}

fn count_subst_ptrs(substs: &Substs) -> usize {
    substs.types().map(count_ty_ptrs).sum()
}

fn count_fn_sig_ptrs(fn_sig: FnSig) -> usize {
    fn_sig.inputs_and_output.iter().cloned().map(count_ty_ptrs).sum()
}


struct VarAlloc {
    next: u32,
}

impl VarAlloc {
    fn new() -> VarAlloc {
        VarAlloc {
            next: FIRST_USER_VAR,
        }
    }

    fn alloc(&mut self) -> Var {
        let v = Var(self.next);
        self.next += 1;
        v
    }

    fn alloc_n(&mut self, n: usize) -> Vec<Var> {
        assert!(n as u32 as usize == n);
        let mut v = Vec::with_capacity(n);
        for i in 0 .. n as u32 {
            v.push(Var(self.next + i));
        }
        self.next += n as u32;
        v
    }
}

struct SigInfo {
}

impl SigInfo {
    fn from_instance<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                                     inst: Instance<'tcx>,
                                     alloc: &mut VarAlloc) -> SigInfo {
        let sig = tcx.fn_sig(inst.def_id()).0;
        SigInfo {
            sig_vars: alloc.alloc_n(count_fn_sig_ptrs(sig)),
            subst_vars: alloc.alloc_n(count_subst_ptrs(inst.substs)),
            cmap: ConstraintMap::new(),
        }
    }
}

struct Ctxt<'tcx> {
    alloc: VarAlloc,
    global_cmap: ConstraintMap,
    sigs: HashSet<Instance<'tcx>, SigInfo>,
}
*/

// global vars & constraints
// mono fn summaries:
// - vars + constraints
// - var names are sequentially numbered following the globals
//   - label subst tys first.  these will be connected to type annotations at callsites
//   - label unsubstituted arg tys next.  these will be connected to the fn's arg annotations
//   - treat return ty as arg #0.  this aligns with MIR locals
// on update:
// - import global v&c
// - label the subst & local vars.  the first few locals' names should align with the names in the
//   summary.
// - scan the MIR for the function, adding constraints (and occasionally vars) on demand
// - function call handling:
//   - generate fresh vars for the substs and args in the callee signature
//   - substitute the fresh variable names for the originals in the callee's constraints
//   - add substituted constraints to the current set
//   - add constraints for movement into args / out of return
