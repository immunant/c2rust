use std::collections::hash_map::{HashMap, Entry};
use std::collections::VecDeque;
use std::u32;

use rustc::hir::def_id::{DefId, LOCAL_CRATE};
use rustc::mir::*;
use rustc::mir::traversal::{Postorder, ReversePostorder};
use rustc::ty::{Ty, TyS, TyCtxt, FnSig, Instance, TypeVariants};
use rustc::ty::subst::Substs;
use rustc::ty::fold::{TypeVisitor, TypeFoldable};
use rustc_data_structures::indexed_vec::{IndexVec, Idx};

use analysis::labeled_ty::{LabeledTy, LabeledTyCtxt};
use command::CommandState;
use driver;


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

type LTy<'tcx> = LabeledTy<'tcx, Perm>;

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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum Perm {
    NonPtr,
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum TySource {
    /// Types that appear in the signature of a def.  These are actually the first 1+N locals of
    /// the def, where N is the number of args.
    Sig(DefId, usize),

    /// Types of local variables within the MIR for a def.  This does not include the locals
    /// corresponding to args and return.
    Local(DefId, usize),

    /// Types of the fields of structs.
    StructField(DefId, usize),

    /// Types appearing in `Rvalue::Cast`.  Note that `Rvalue`s appear only on the RHS of
    /// `StatementKind::Assign`.  We identify the cast by its containing function, basic block, and
    /// statement index.
    Cast(DefId, BasicBlock, usize),

    // TODO: better handling of consts
    Const(usize),
}

struct Ctxt<'tcx> {
    /// Assignment of concrete permissions to permission variables.
    assign: IndexVec<Var, ConcretePerm>,

    lcx: LabeledTyCtxt<'tcx, Perm>,

    /// Cache of already-labeled types.  We use this to avoid re-labeling the same type twice,
    /// which would produce variables that are independent when they shouldn't be.
    lty_map: HashMap<TySource, LTy<'tcx>>,
}

impl<'tcx> Ctxt<'tcx> {
    fn labeled<F>(&mut self, source: TySource, mk_ty: F) -> LTy<'tcx>
            where F: FnOnce() -> Ty<'tcx> {
        let assign = &mut self.assign;
        match self.lty_map.entry(source) {
            Entry::Vacant(e) => {
                *e.insert(self.lcx.label(mk_ty(), &mut |ty| {
                    match ty.sty {
                        TypeVariants::TyRef(_, _) |
                        TypeVariants::TyRawPtr(_) => Perm::Var(assign.push(ConcretePerm::Read)),
                        // TODO: handle Box<_>
                        _ => Perm::NonPtr,
                    }
                }))
            },

            Entry::Occupied(e) => *e.get(),
        }
    }

    fn labeled_sig<'a, 'gcx>(&mut self,
                             ty: LTy<'tcx>,
                             tcx: TyCtxt<'a, 'gcx, 'tcx>) -> LFnSig<'tcx> {
        eprintln!(" * labeling sig for {:?}", ty);
        match ty.ty.sty {
            TypeVariants::TyFnDef(def_id, _) => {
                let sig = tcx.fn_sig(def_id);
                let inputs = sig.0.inputs().iter().enumerate()
                    .map(|(i, &ty)| self.labeled(TySource::Sig(def_id, 1 + i), || ty))
                    .collect::<Vec<_>>();
                let output = self.labeled(TySource::Sig(def_id, 0), || sig.0.output());

                LFnSig {
                    inputs: self.lcx.subst_slice(&inputs, &ty.args),
                    output: self.lcx.subst(output, &ty.args),
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

    fn concrete_perm(&self, perm: Perm) -> ConcretePerm {
        match perm {
            Perm::NonPtr => panic!("expected pointer permission"),
            Perm::Concrete(p) => p,
            Perm::Var(v) => self.assign[v],
        }
    }

    fn concrete_perm_opt(&self, perm: Perm) -> Option<ConcretePerm> {
        match perm {
            Perm::NonPtr => None,
            Perm::Concrete(p) => Some(p),
            Perm::Var(v) => Some(self.assign[v]),
        }
    }

    fn propagate_perm(&mut self, lperm: Perm, rperm: Perm) {
        if let Perm::Var(v) = rperm {
            let perm = self.concrete_perm(lperm);
            if perm > self.assign[v] {
                self.assign[v] = perm;
            }
        }
    }
}

struct LocalCtxt<'a, 'gcx: 'tcx, 'tcx: 'a> {
    cx: &'a mut Ctxt<'tcx>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,

    def_id: DefId,
    mir: &'a Mir<'tcx>,
    bbid: BasicBlock,
    stmt_idx: usize,
}

impl<'a, 'gcx, 'tcx> LocalCtxt<'a, 'gcx, 'tcx> {
    fn enter_block(&mut self, bbid: BasicBlock) {
        self.bbid = bbid;
        // Obviously bogus statement index
        self.stmt_idx = !0;
    }

    fn enter_stmt(&mut self, idx: usize) {
        self.stmt_idx = idx;
    }


    fn lvalue_lty(&mut self, lv: &Lvalue<'tcx>) -> LTy<'tcx> {
        match *lv {
            Lvalue::Local(l) => {
                let idx = l.index();
                let source =
                    if idx < 1 + self.mir.arg_count { TySource::Sig(self.def_id, idx) }
                    else { TySource::Local(self.def_id, idx) };
                let mir = self.mir;
                self.cx.labeled(source, || mir.local_decls[l].ty)
            },

            Lvalue::Static(ref s) => {
                unimplemented!()
            },

            Lvalue::Projection(ref p) => {
                let base_ty = self.lvalue_lty(&p.base);
                match p.elem {
                    ProjectionElem::Deref => base_ty.args[0],
                    ProjectionElem::Field(f, _) => self.field_lty(base_ty, f),
                    ProjectionElem::Index(_) => unimplemented!(),
                    ProjectionElem::ConstantIndex { .. } => unimplemented!(),
                    ProjectionElem::Subslice { .. } => unimplemented!(),
                    ProjectionElem::Downcast(_, _) => unimplemented!(),
                }
            },
        }
    }

    fn field_lty(&mut self, base_ty: LTy<'tcx>, f: Field) -> LTy<'tcx> {
        match base_ty.ty.sty {
            TypeVariants::TyAdt(adt, substs) => {
                let source = TySource::StructField(adt.did, f.index());
                let tcx = self.tcx;
                let poly_ty = self.cx.labeled(source, || {
                    tcx.type_of(adt.struct_variant().fields[f.index()].did)
                });
                self.cx.lcx.subst(poly_ty, &base_ty.args)
            },
            TypeVariants::TyTuple(tys, _) => base_ty.args[f.index()],
            _ => unimplemented!(),
        }
    }

    fn rvalue_lty(&mut self, rv: &Rvalue<'tcx>) -> LTy<'tcx> {
        match *rv {
            Rvalue::Use(ref op) => self.operand_lty(op),
            Rvalue::Repeat(ref op, len) => unimplemented!(),
            Rvalue::Ref(_, _, ref lv) => self.lvalue_lty(lv),
            Rvalue::Len(_) => unimplemented!(),
            Rvalue::Cast(_, ref op, ty) => {
                let source = TySource::Cast(self.def_id, self.bbid, self.stmt_idx);
                let cast_ty = self.cx.labeled(source, || ty);
                let op_ty = self.operand_lty(op);
                eprintln!("propagate cast:\n  {:?}\n  {:?}", cast_ty, op_ty);
                self.propagate(cast_ty, op_ty);
                cast_ty
            },
            Rvalue::BinaryOp(op, ref a, ref b) |
            Rvalue::CheckedBinaryOp(op, ref a, ref b) => match op {
                BinOp::Offset => self.operand_lty(a),
                _ => unimplemented!(),
            },
            Rvalue::NullaryOp(op, ty) => unimplemented!(),
            Rvalue::UnaryOp(op, ref a) => match op {
                UnOp::Not |
                UnOp::Neg => self.operand_lty(a),
            },
            Rvalue::Discriminant(ref lv) => unimplemented!(),
            Rvalue::Aggregate(ref kind, ref ops) => {
                match **kind {
                    AggregateKind::Array(_) => unimplemented!(),
                    AggregateKind::Tuple => {
                        let args = ops.iter().map(|op| self.operand_lty(op)).collect::<Vec<_>>();
                        let args = self.cx.lcx.mk_slice(&args);
                        self.cx.lcx.mk(rv.ty(self.mir, self.tcx), args, Perm::NonPtr)
                    },
                    AggregateKind::Adt(_, _, _, _) => unimplemented!(),
                    AggregateKind::Closure(_, _) => unimplemented!(),
                }
            },
        }
    }

    fn operand_lty(&mut self, op: &Operand<'tcx>) -> LTy<'tcx> {
        match *op {
            Operand::Consume(ref lv) => self.lvalue_lty(lv),
            Operand::Constant(ref c) =>
                self.cx.labeled(TySource::Const(c.ty as *const _ as usize), || c.ty),
        }
    }


    fn propagate(&mut self, lhs: LTy<'tcx>, rhs: LTy<'tcx>) {
        self.cx.propagate_perm(lhs.label, rhs.label);

        if lhs.args.len() == rhs.args.len() {
            for (&l_arg, &r_arg) in lhs.args.iter().zip(rhs.args.iter()) {
                self.propagate(l_arg, r_arg);
            }
        }
    }

    fn handle_basic_block(&mut self, bbid: BasicBlock, bb: &BasicBlockData<'tcx>) {
        self.enter_block(bbid);
        eprintln!("  {:?}", bbid);

        match bb.terminator().kind {
            TerminatorKind::Goto { .. } |
            TerminatorKind::SwitchInt { .. } |
            TerminatorKind::Resume |
            TerminatorKind::Return |
            TerminatorKind::Unreachable |
            TerminatorKind::Drop { .. } |
            TerminatorKind::Assert { .. } => {},

            TerminatorKind::DropAndReplace { ref location, ref value, .. } => {
                let loc_ty = self.lvalue_lty(location);
                let val_ty = self.operand_lty(value);
                self.propagate(loc_ty, val_ty);
                eprintln!("    {:?}: {:?}", location, loc_ty);
                eprintln!("    ^-- {:?}: {:?}", value, val_ty);
            },

            TerminatorKind::Call { ref func, ref args, ref destination, .. } => {
                eprintln!("    call {:?}", func);
                let func_ty = self.operand_lty(func);
                let sig = self.cx.labeled_sig(func_ty, self.tcx);
                // Note that `sig.inputs` may be shorter than `args`, if `func` is varargs.
                for (&sig_ty, arg) in sig.inputs.iter().zip(args.iter()) {
                    let arg_ty = self.operand_lty(arg);
                    self.propagate(sig_ty, arg_ty);
                    eprintln!("    (arg): {:?}", sig_ty);
                    eprintln!("    ^-- {:?}: {:?}", arg, arg_ty);
                }
                if let Some((ref dest, _)) = *destination {
                    let sig_ty = sig.output;
                    let dest_ty = self.lvalue_lty(dest);
                    self.propagate(dest_ty, sig_ty);
                    eprintln!("    {:?}: {:?}", dest, dest_ty);
                    eprintln!("    ^-- (return): {:?}", sig_ty);
                }
            },
        }

        for (idx, s) in bb.statements.iter().enumerate().rev() {
            self.enter_stmt(idx);
            match s.kind {
                StatementKind::Assign(ref lv, ref rv) => {
                    let lv_ty = self.lvalue_lty(lv);
                    let rv_ty = self.rvalue_lty(rv);
                    self.propagate(lv_ty, rv_ty);
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
    }
}


fn get_sig<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                           cx: &mut Ctxt<'tcx>,
                           def_id: DefId) -> LFnSig<'tcx> {
    let sig = tcx.fn_sig(def_id);
    let inputs = sig.0.inputs().iter().enumerate()
        .map(|(i, &ty)| cx.labeled(TySource::Sig(def_id, 1 + i), || ty))
        .collect::<Vec<_>>();
    let output = cx.labeled(TySource::Sig(def_id, 0), || sig.0.output());
    LFnSig {
        inputs: cx.lcx.mk_slice(&inputs),
        output: output,
    }
}


pub fn analyze(st: &CommandState, cx: &driver::Ctxt) {
    let mut ctxt = Ctxt {
        assign: IndexVec::new(),
        lcx: LabeledTyCtxt::new(cx.ty_arena()),
        lty_map: HashMap::new(),
    };


    // TODO: make this less special-cased
    for &(id, label) in st.marks().iter() {
        if label == "free" {
            if let Some(def_id) = cx.hir_map().opt_local_def_id(id) {
                let sig = get_sig(cx.ty_ctxt(), &mut ctxt, def_id);
                ctxt.propagate_perm(Perm::move_(), sig.inputs[0].label);
                eprintln!("FREE: arg var = {:?}", sig.inputs[0].label);
            }
        }
    }


    let tcx = cx.ty_ctxt();
    for _ in 0 .. 5 {   // TODO iterate to reach a fixed point
        for &def_id in tcx.mir_keys(LOCAL_CRATE).iter() {
            let mir = tcx.optimized_mir(def_id);
            let mut local_cx = LocalCtxt {
                cx: &mut ctxt,
                tcx: cx.ty_ctxt(),
                def_id: def_id,
                mir: mir,
                bbid: START_BLOCK,
                stmt_idx: !0,
            };

            eprintln!("mir for {:?}", def_id);
            for _ in 0 .. 5 {   // TODO iterate to reach a fixed point
                for (bbid, bb) in Postorder::new(&mir, START_BLOCK) {
                    local_cx.handle_basic_block(bbid, bb);
                }
            }
        }
    }

    let mut def_ids = tcx.mir_keys(LOCAL_CRATE).iter().cloned().collect::<Vec<_>>();
    def_ids.sort();
    let mut new_lcx = LabeledTyCtxt::new(cx.ty_arena());
    for def_id in def_ids {
        let sig = get_sig(cx.ty_ctxt(), &mut ctxt, def_id);
        let mut func = |&v: &_| {
            match ctxt.concrete_perm_opt(v) {
                None => "--",
                Some(p) => match p {
                    ConcretePerm::Read => "REF",
                    ConcretePerm::Write => "MUT",
                    ConcretePerm::Move => "BOX",
                },
            }
        };
        let inputs = new_lcx.relabel_slice(sig.inputs, &mut func);
        let output = new_lcx.relabel(sig.output, &mut func);
        eprintln!("{:?}:\n  {:?} -> {:?}", def_id, inputs, output);
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
