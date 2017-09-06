//! Type equivalence-class analysis.  The goal is to find groups of types annotations that must be
//! equal for the crate to typecheck.  Example:
//!
//!     fn f(x: i32, y: i32) {
//!         let a: i32 = x;
//!         let b: i32 = y;
//!     }
//!
//! Here the equivalence classes are {x, a} and {y, b}.  The annotations on `x` and `a` must match
//! for the program to typecheck, but the annotations on `x` and `y` do not need to match because
//! the two never interact.
//!
//! The analysis runs in two phases.  First, it constructs a "labeled type" for every type in the
//! AST.  The labeled type has the same structure as the `ty::Ty`, but has a unification key at
//! every type constructor.  Second, it walks over the AST unifying various types.  This includes
//! unifying the LHS type of an assignment with the RHS type, `typeof(&e)` with `&typeof(e)`, the
//! type of the expression `f` with `f`'s function signature, and so on.

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt;

use arena::DroplessArena;
use ena::unify::{UnificationTable, UnifyKey};
use rustc::hir;
use rustc::hir::*;
use rustc::hir::def_id::DefId;
use rustc::hir::intravisit::{self, Visitor, NestedVisitorMap};
use rustc::hir::itemlikevisit::{self, ItemLikeVisitor};
use rustc::hir::map::Node::*;
use rustc::ty::{self, TyCtxt, TypeckTables};
use rustc::ty::adjustment::Adjust;
use rustc::ty::subst::{self, Substs};
use rustc_data_structures::indexed_vec::IndexVec;
use syntax::abi::Abi;
use syntax::ast;
use syntax::ast::NodeId;
use syntax::codemap::Span;
use syntax::ptr::P;
use syntax::symbol::Symbol;

use analysis::labeled_ty::{LabeledTy, LabeledTyCtxt};
use type_map;
use util::HirDefExt;
use util::IntoSymbol;


#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct TyLabel(u32);

impl UnifyKey for TyLabel {
    type Value = ();

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        TyLabel(u)
    }

    fn tag() -> &'static str {
        "<tylabel>"
    }
}


type Label = Option<Cell<TyLabel>>;

type LTy<'tcx> = LabeledTy<'tcx, Label>;

#[derive(Clone, Copy, Debug)]
struct LFnSig<'tcx> {
    inputs: &'tcx [LTy<'tcx>],
    output: LTy<'tcx>,
    variadic: bool,
}


struct LTyTable<'tcx> {
    unif: RefCell<UnificationTable<TyLabel>>,
    lcx: LabeledTyCtxt<'tcx, Label>,
}

impl<'tcx> LTyTable<'tcx> {
    fn new(arena: &'tcx DroplessArena) -> LTyTable<'tcx> {
        LTyTable {
            unif: RefCell::new(UnificationTable::new()),
            lcx: LabeledTyCtxt::new(arena),
        }
    }


    fn label(&self, ty: ty::Ty<'tcx>) -> LTy<'tcx> {
        self.lcx.label(ty, &mut |_| Some(Cell::new(self.unif.borrow_mut().new_key(()))))
    }

    fn label_slice(&self, tys: &[ty::Ty<'tcx>]) -> &'tcx [LTy<'tcx>] {
        self.lcx.label_slice(tys, &mut |_| Some(Cell::new(self.unif.borrow_mut().new_key(()))))
    }

    fn label_sig(&self, sig: ty::FnSig<'tcx>) -> LFnSig<'tcx> {
        LFnSig {
            inputs: self.label_slice(sig.inputs()),
            output: self.label(sig.output()),
            variadic: sig.variadic,
        }
    }

    /// Produce a dummy `LTy` on which unification is a no-op.  In effect, every use of the dummy
    /// type behaves as if it were a fresh type.  Running `unify(dummy, a); unify(dummy, b);` does
    /// not result in the unification of `a` and `b`.
    fn non_unifiable(&self, ty: ty::Ty<'tcx>) -> LTy<'tcx> {
        self.lcx.label(ty, &mut |_| None)
    }

    fn non_unifiable_slice(&self, tys: &[ty::Ty<'tcx>]) -> &'tcx [LTy<'tcx>] {
        self.lcx.label_slice(tys, &mut |_| None)
    }

    fn non_unifiable_sig(&self, sig: ty::FnSig<'tcx>) -> LFnSig<'tcx> {
        LFnSig {
            inputs: self.non_unifiable_slice(sig.inputs()),
            output: self.non_unifiable(sig.output()),
            variadic: sig.variadic,
        }
    }


    fn subst(&self, lty: LTy<'tcx>, substs: &[LTy<'tcx>]) -> LTy<'tcx> {
        self.lcx.subst(lty, substs)
    }

    fn subst_slice(&self, ltys: &[LTy<'tcx>], substs: &[LTy<'tcx>]) -> &'tcx [LTy<'tcx>] {
        self.lcx.subst_slice(ltys, substs)
    }

    fn subst_sig(&self, sig: LFnSig<'tcx>, substs: &[LTy<'tcx>]) -> LFnSig<'tcx> {
        LFnSig {
            inputs: self.subst_slice(sig.inputs, substs),
            output: self.subst(sig.output, substs),
            variadic: sig.variadic,
        }
    }


    fn unify(&self, lty1: LTy<'tcx>, lty2: LTy<'tcx>) {
        if let (Some(cell1), Some(cell2)) = (lty1.label.as_ref(), lty2.label.as_ref()) {
            self.unif.borrow_mut().union(cell1.get(), cell2.get());
        }

        if lty1.args.len() == lty2.args.len() {
            self.unify_slices(lty1.args, lty2.args);
        }
    }

    fn unify_slices(&self, ltys1: &[LTy<'tcx>], ltys2: &[LTy<'tcx>]) {
        for (lty1, lty2) in ltys1.iter().zip(ltys2.iter()) {
            self.unify(lty1, lty2);
        }
    }
}



/// Walk over typechecking tables, building a labeled type for each expr and pattern.
struct ExprPatVisitor<'a, 'gcx: 'tcx, 'tcx: 'a> {
    tcx: TyCtxt<'a, 'gcx, 'tcx>,
    ltt: &'a LTyTable<'tcx>,

    unadjusted: HashMap<HirId, LTy<'tcx>>,
    adjusted: HashMap<HirId, LTy<'tcx>>,
    substs: HashMap<HirId, &'tcx [LTy<'tcx>]>,
}

impl<'a, 'gcx, 'tcx> ExprPatVisitor<'a, 'gcx, 'tcx> {
    fn handle_body(&mut self, body_id: BodyId) {
        let tables = self.tcx.body_tables(body_id);
        let def_id = match_or!([tables.local_id_root] Some(x) => x; return);

        for (&local_id, &ty) in tables.node_types().iter() {
            let id = HirId { owner: def_id.index, local_id: local_id };
            self.unadjusted.insert(id, self.ltt.label(ty));

            if let Some(adj) = tables.adjustments().get(id).and_then(|v| v.last()) {
                self.adjusted.insert(id, self.ltt.label(adj.target));
            }

            if let Some(substs) = tables.node_substs_opt(id) {
                let labeled = self.ltt.label_slice(&substs.types().collect::<Vec<_>>());
                self.substs.insert(id, labeled);
            }
        }
    }
}

impl<'a, 'gcx, 'tcx, 'hir> ItemLikeVisitor<'hir> for ExprPatVisitor<'a, 'gcx, 'tcx> {
    fn visit_item(&mut self, item: &'hir Item) {
        let body_id = match item.node {
            ItemStatic(_, _, body_id) => body_id,
            ItemConst(_, body_id) => body_id,
            ItemFn(_, _, _, _, _, body_id) => body_id,
            _ => return,
        };
        self.handle_body(body_id);
    }

    fn visit_trait_item(&mut self, item: &'hir TraitItem) {
        let body_id = match item.node {
            TraitItemKind::Const(_, Some(body_id)) => body_id,
            TraitItemKind::Method(_, TraitMethod::Provided(body_id)) => body_id,
            _ => return,
        };
        self.handle_body(body_id);
    }

    fn visit_impl_item(&mut self, item: &'hir ImplItem) {
        let body_id = match item.node {
            ImplItemKind::Const(_, body_id) => body_id,
            ImplItemKind::Method(_, body_id) => body_id,
            _ => return,
        };
        self.handle_body(body_id);
    }
}



struct LabelTysSource<'a, 'hir: 'a, 'gcx: 'tcx, 'tcx: 'a> {
    hir_map: &'a hir::map::Map<'hir>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,
    ltt: &'a LTyTable<'tcx>,
}

impl<'a, 'hir, 'gcx, 'tcx> LabelTysSource<'a, 'hir, 'gcx, 'tcx> {
    fn get_tables(&self, id: NodeId) -> &'gcx TypeckTables<'gcx> {
        let parent = self.hir_map.get_parent(id);
        let parent_body = self.hir_map.body_owned_by(parent);
        self.tcx.body_tables(parent_body)
    }

    fn get_substs(&self, id: NodeId) -> Option<&'tcx Substs<'tcx>> {
        let hir_id = self.hir_map.node_to_hir_id(id);
        self.get_tables(id).node_substs_opt(hir_id)
    }

    fn node_lty(&self, id: NodeId) -> LTy<'tcx> {
        let tables = self.get_tables(id);
        let hir_id = self.hir_map.node_to_hir_id(id);
        let ty = tables.node_id_to_type(hir_id);
        self.ltt.label(ty)
    }
}

impl<'a, 'hir, 'gcx, 'tcx> type_map::TypeSource for LabelTysSource<'a, 'hir, 'gcx, 'tcx> {
    type Type = LTy<'tcx>;
    type Signature = LFnSig<'tcx>;

    fn expr_type(&mut self, e: &ast::Expr) -> Option<LTy<'tcx>> {
        Some(self.node_lty(e.id))
    }

    fn pat_type(&mut self, p: &ast::Pat) -> Option<LTy<'tcx>> {
        Some(self.node_lty(p.id))
    }

    fn def_type(&mut self, did: DefId) -> Option<LTy<'tcx>> {
        let ty = self.tcx.type_of(did);
        Some(self.ltt.label(ty))
    }

    fn fn_sig(&mut self, did: DefId) -> Option<LFnSig<'tcx>> {
        let sig = self.tcx.fn_sig(did);
        Some(self.ltt.label_sig(sig.0))
    }

    fn closure_sig(&mut self, did: DefId) -> Option<LFnSig<'tcx>> {
        self.fn_sig(did).map(|sig| {
            // The returned signature has the arguments wrapped in a tuple
            LFnSig {
                inputs: sig.inputs[0].args,
                .. sig
            }
        })
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

fn label_tys<'a, 'gcx, 'tcx>(hir_map: &hir::map::Map,
                             tcx: TyCtxt<'a, 'gcx, 'tcx>,
                             ltt: &'a LTyTable<'tcx>,
                             krate: &ast::Crate)
                             -> HashMap<NodeId, LTy<'tcx>> {
    let mut ty_nodes = HashMap::new();
    let source = LabelTysSource {
        hir_map: hir_map,
        tcx: tcx,
        ltt: ltt,
    };
    type_map::map_types(hir_map, source, krate, |_, ast_ty, lty| {
        // Note that HIR `Ty` nodes don't have `HirId`s, so we index everything by the old `NodeId`
        // instead.
        ty_nodes.insert(ast_ty.id, lty);
    });
    ty_nodes
}



fn prim_tys<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                            ltt: &'a LTyTable<'tcx>)
                            -> HashMap<&'static str, LTy<'tcx>> {
    let mut map = HashMap::new();

    map.insert("bool", ltt.label(tcx.mk_bool()));
    map.insert("()", ltt.label(tcx.mk_nil()));
    map.insert("usize", ltt.label(tcx.mk_mach_uint(ast::UintTy::Us)));

    map
}



struct UnifyVisitor<'a, 'hir: 'a, 'gcx: 'tcx, 'tcx: 'a> {
    hir_map: &'a hir::map::Map<'hir>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,
    ltt: &'a LTyTable<'tcx>,

    unadjusted_nodes: &'a HashMap<HirId, LTy<'tcx>>,
    nodes: &'a HashMap<HirId, LTy<'tcx>>,
    node_substs: &'a HashMap<HirId, &'tcx [LTy<'tcx>]>,
    ty_nodes: &'a HashMap<NodeId, LTy<'tcx>>,
    prims: &'a HashMap<&'static str, LTy<'tcx>>,


    // Unfortunately, there is no central index of all/most `DefId`s and their types, like there is
    // for exprs and patterns.  And we can't visit all defs because some are pulled in from other
    // crates.  Since we can't precompute the `LTy` for every def, we have to keep this cache and
    // add defs to it as we encounter them.
    defs: RefCell<HashMap<DefId, LTy<'tcx>>>,

    /// Cache of signatures of function/method definitions.  This always contains the raw,
    /// un-substituted signature.
    def_sigs: RefCell<HashMap<DefId, LFnSig<'tcx>>>,
}

impl<'a, 'hir, 'gcx, 'tcx> UnifyVisitor<'a, 'hir, 'gcx, 'tcx> {
    fn expr_lty(&self, e: &Expr) -> LTy<'tcx> {
        self.nodes.get(&e.hir_id)
            .or_else(|| self.unadjusted_nodes.get(&e.hir_id))
            .unwrap_or_else(|| panic!("expr_lty: no lty for {:?} @ {:?}",
                                      e, self.tcx.sess.codemap().span_to_string(e.span)))
    }

    fn unadjusted_expr_lty(&self, e: &Expr) -> LTy<'tcx> {
        self.unadjusted_nodes.get(&e.hir_id)
            .unwrap_or_else(|| panic!("unadjusted_expr_lty: no unadjusted lty for {:?} @ {:?}",
                                      e, self.tcx.sess.codemap().span_to_string(e.span)))
    }

    fn opt_unadjusted_expr_lty(&self, e: &Expr) -> Option<LTy<'tcx>> {
        self.unadjusted_nodes.get(&e.hir_id).map(|&x| x)
    }

    fn block_lty(&self, b: &Block) -> LTy<'tcx> {
        match b.expr {
            Some(ref e) => self.expr_lty(e),
            None => self.prim_lty("()"),
        }
    }

    fn pat_lty(&self, p: &Pat) -> LTy<'tcx> {
        self.unadjusted_nodes.get(&p.hir_id)
            .unwrap_or_else(|| panic!("pat_lty: no lty for {:?} @ {:?}",
                                      p, self.tcx.sess.codemap().span_to_string(p.span)))
    }

    fn ty_lty(&self, t: &Ty) -> LTy<'tcx> {
        self.ty_nodes.get(&t.id)
            .unwrap_or_else(|| panic!("ty_lty: no lty for {:?} @ {:?}",
                                      t, self.tcx.sess.codemap().span_to_string(t.span)))
    }

    fn prim_lty(&self, name: &'static str) -> LTy<'tcx> {
        self.prims.get(&name)
            .unwrap_or_else(|| panic!("prim_lty: no such prim {:?}", name))
    }


    fn compute_def_lty(&self, id: DefId) -> LTy<'tcx> {
        match self.hir_map.get_if_local(id) {
            Some(NodeBinding(p)) => {
                return self.pat_lty(p);
            },
            _ => {},
        }

        self.ltt.label(self.tcx.type_of(id))
    }

    fn def_lty(&self, id: DefId) -> LTy<'tcx> {
        *self.defs.borrow_mut().entry(id)
            .or_insert_with(|| self.compute_def_lty(id))
    }


    fn compute_def_sig(&self, id: DefId) -> LFnSig<'tcx> {
        let sig = self.tcx.fn_sig(id);
        let is_extern = match sig.0.abi {
            Abi::Rust | Abi::RustIntrinsic | Abi::RustCall => false,
            _ => true,
        };

        if !is_extern {
            self.ltt.label_sig(sig.0)
        } else {
            self.ltt.non_unifiable_sig(sig.0)
        }
    }

    fn def_sig(&self, id: DefId) -> LFnSig<'tcx> {
        *self.def_sigs.borrow_mut().entry(id)
            .or_insert_with(|| self.compute_def_sig(id))
    }

    fn fn_num_inputs(&self, lty: LTy<'tcx>) -> usize {
        use rustc::ty::TypeVariants::*;
        match lty.ty.sty {
            TyFnDef(id, _) => self.def_sig(id).inputs.len(),
            TyFnPtr(_) => lty.args.len() - 1,
            _ => panic!("fn_num_inputs: not a fn type"),
        }
    }

    /// Get the input types out of a `FnPtr` or `FnDef` `LTy`.
    fn fn_input(&self, lty: LTy<'tcx>, idx: usize) -> LTy<'tcx> {
        use rustc::ty::TypeVariants::*;
        match lty.ty.sty {
            TyFnDef(id, _) => {
                // For a `TyFnDef`, retrieve the `LFnSig` for the given `DefId` and apply the
                // labeled substs recorded in `LTy.args`.
                let sig = self.def_sig(id);
                self.ltt.subst(sig.inputs[idx], &lty.args)
            },
            TyFnPtr(_) => {
                // For a `TyFnPtr`, `lty.args` records the labeled input and output types.
                &lty.args[idx]
            },
            _ => panic!("fn_input: not a fn type"),
        }
    }

    /// Get the output type out of a `FnPtr` or `FnDef` `LTy`.
    fn fn_output(&self, lty: LTy<'tcx>) -> LTy<'tcx> {
        use rustc::ty::TypeVariants::*;
        match lty.ty.sty {
            TyFnDef(id, _) => {
                let sig = self.def_sig(id);
                self.ltt.subst(sig.output, &lty.args)
            },
            TyFnPtr(_) => {
                &lty.args[lty.args.len() - 1]
            },
            _ => panic!("fn_output: not a fn type"),
        }
    }

    fn fn_is_variadic(&self, lty: LTy<'tcx>) -> bool {
        use rustc::ty::TypeVariants::*;
        match lty.ty.sty {
            TyFnDef(id, _) => {
                self.def_sig(id).variadic
            },
            TyFnPtr(ty_sig) => {
                ty_sig.0.variadic
            },
            _ => panic!("fn_is_variadic: not a fn type"),
        }
    }


    fn get_tables(&self, id: NodeId) -> &'gcx TypeckTables<'gcx> {
        let parent = self.hir_map.get_parent(id);
        let parent_body = self.hir_map.body_owned_by(parent);
        self.tcx.body_tables(parent_body)
    }

    fn method_sig(&self, e: &Expr) -> LFnSig<'tcx> {
        let def_id = self.get_tables(e.id).type_dependent_defs()[e.hir_id].def_id();
        let sig = self.def_sig(def_id);
        let substs = self.node_substs.get(&e.hir_id).map_or_else(|| &[] as &[_], |x| x);
        self.ltt.subst_sig(sig, substs)
    }


    fn field_lty(&self, struct_ty: LTy<'tcx>, name: Symbol) -> LTy<'tcx> {
        let adt = match struct_ty.ty.sty {
            ty::TypeVariants::TyAdt(ref adt, _) => adt,
            _ => panic!("field_lty: not a struct ty: {:?}", struct_ty),
        };
        let variant = adt.struct_variant();
        for field in &variant.fields {
            if field.name == name {
                let base = self.def_lty(field.did);
                return self.ltt.subst(base, &struct_ty.args);
            }
        }
        panic!("field_lty: no field `{}` in {:?}", name, struct_ty);
    }
}

impl<'a, 'hir, 'gcx, 'tcx> Visitor<'hir> for UnifyVisitor<'a, 'hir, 'gcx, 'tcx> {
    fn nested_visit_map<'this>(&'this mut self) -> NestedVisitorMap<'this, 'hir> {
        NestedVisitorMap::OnlyBodies(self.hir_map)
    }

    fn visit_expr(&mut self, e: &'hir Expr) {
        let rty = match self.opt_unadjusted_expr_lty(e) {
            Some(x) => x,
            None => {
                // This is a bit of a hack.  TyCtxt doesn't put entries in node_types for the
                // length expressions in fixed-length arrays, so we can't obtain the unadjusted LTy
                // for such exprs.  But given the existence of `const fn`s, we really ought to be
                // looking at array length exprs.  Instead of figuring out where to get the type
                // info for them, we just skip the length exprs entirely - along with anything else
                // that may be absent from node_types.  (Hopefully that's not very much.)
                intravisit::walk_expr(self, e);
                return;
            },
        };

        match e.node {
            ExprBox(ref e) => {
                self.ltt.unify(rty.args[0], self.expr_lty(e));
            },

            ExprArray(ref es) => {
                for e in es {
                    self.ltt.unify(rty.args[0], self.expr_lty(e));
                }
            },

            ExprCall(ref func, ref args) => {
                let func_lty = self.expr_lty(func);

                fn is_closure(ty: ty::Ty) -> bool {
                    if let ty::TypeVariants::TyClosure(..) = ty.sty {
                        true
                    } else {
                        false
                    }
                }
                if is_closure(func_lty.ty) ||
                   (func_lty.args.len() > 0 && is_closure(func_lty.args[0].ty)) {
                    intravisit::walk_expr(self, e);
                    return;
                }

                let args =
                    if !self.fn_is_variadic(func_lty) { args }
                    else { &args[.. self.fn_num_inputs(func_lty)] };
                for (i, arg) in args.iter().enumerate() {
                    self.ltt.unify(self.fn_input(func_lty, i), self.expr_lty(arg));
                }
                self.ltt.unify(rty, self.fn_output(func_lty));
            },

            ExprMethodCall(_, _, ref args) => {
                let sig = self.method_sig(e);
                for (i, arg) in args.iter().enumerate() {
                    self.ltt.unify(sig.inputs[i], self.expr_lty(arg));
                }
                self.ltt.unify(rty, sig.output);
            },

            ExprTup(ref es) => {
                for (expected, e) in rty.args.iter().zip(es.iter()) {
                    self.ltt.unify(expected, self.expr_lty(e));
                }
            },

            ExprBinary(ref op, ref a, ref b) => {
                match op.node {
                    BiAdd | BiSub | BiMul | BiDiv | BiRem |
                    BiBitXor | BiBitAnd | BiBitOr |
                    BiShl | BiShr => {
                        self.ltt.unify(rty, self.expr_lty(a));
                        self.ltt.unify(rty, self.expr_lty(b));
                    },
                    BiAnd | BiOr => {
                        self.ltt.unify(rty, self.prim_lty("bool"));
                        self.ltt.unify(self.expr_lty(a), self.prim_lty("bool"));
                        self.ltt.unify(self.expr_lty(b), self.prim_lty("bool"));
                    },
                    BiEq | BiLt | BiLe | BiNe | BiGe | BiGt => {
                        self.ltt.unify(rty, self.prim_lty("bool"));
                        self.ltt.unify(self.expr_lty(a), self.expr_lty(b));
                    },
                }
            },

            ExprUnary(op, ref a) => {
                match op {
                    UnDeref => self.ltt.unify(rty, self.expr_lty(a).args[0]),
                    UnNot => self.ltt.unify(rty, self.expr_lty(a)),
                    UnNeg => self.ltt.unify(rty, self.expr_lty(a)),
                }
            },

            ExprLit(..) => {},  // Nothing to unify

            ExprCast(_, ref ty) => {
                self.ltt.unify(rty, self.ty_lty(ty));
                // Ignore the expr type, since it has no connection to `rty`.
            },

            ExprType(ref e, ref ty) => {
                self.ltt.unify(rty, self.expr_lty(e));
                self.ltt.unify(rty, self.ty_lty(ty));
            },

            ExprIf(ref cond, ref e_true, ref e_false) => {
                self.ltt.unify(self.prim_lty("bool"), self.expr_lty(cond));
                self.ltt.unify(rty, self.expr_lty(e_true));
                self.ltt.unify(rty, e_false.as_ref().map_or_else(|| self.prim_lty("()"),
                                                                 |e| self.expr_lty(e)));
            },

            ExprWhile(ref cond, ref body, _) => {
                self.ltt.unify(self.prim_lty("bool"), self.expr_lty(cond));
                self.ltt.unify(self.prim_lty("()"), self.block_lty(body));
                self.ltt.unify(rty, self.prim_lty("()"));
            },

            ExprLoop(..) => {}, // TODO

            ExprMatch(..) => {}, // TODO

            ExprClosure(..) => {}, // TODO

            ExprBlock(ref b) => {
                self.ltt.unify(rty, self.block_lty(b));
            },

            ExprAssign(ref lhs, ref rhs) => {
                self.ltt.unify(self.expr_lty(lhs), self.expr_lty(rhs));
                self.ltt.unify(rty, self.prim_lty("()"));
            },

            ExprAssignOp(..) => {}, // TODO

            ExprField(ref e, ref field) => {
                self.ltt.unify(rty, self.field_lty(self.expr_lty(e), field.node));
            },

            ExprTupField(ref e, ref idx) => {}, // TODO

            ExprIndex(ref arr, ref idx) => {}, // TODO

            ExprPath(ref path) => {
                // TODO: many more subcases need handling here
                match *path {
                    QPath::Resolved(_, ref path) => {
                        if let Some(def_id) = path.def.opt_def_id() {
                            self.ltt.unify(rty, self.def_lty(def_id));
                        }
                    },
                    _ => {},
                }
            },

            ExprAddrOf(_, ref e) => {
                self.ltt.unify(rty.args[0], self.expr_lty(e));
            },

            // break/continue/return all have type `!`, which unifies with everything.
            ExprBreak(_, ref result) => {
                // TODO: handle result == Some(x) case
            },

            ExprAgain(_) => {},

            ExprRet(ref result) => {
                // TODO: handle result == Some(x) case
            },

            ExprYield(ref result) => {
                // TODO: handle result == Some(x) case
            },

            ExprInlineAsm(..) => {},

            ExprStruct(_, ref fields, ref base) => {
                for field in fields {
                    self.ltt.unify(self.field_lty(rty, field.name.node),
                                   self.expr_lty(&field.expr));
                }

                if let Some(ref base) = *base {
                    self.ltt.unify(rty, self.expr_lty(base));
                }
            },

            ExprRepeat(ref e, _) => {
                self.ltt.unify(rty.args[0], self.expr_lty(e));
            },
        }

        if let Some(adjs) = self.get_tables(e.id).adjustments().get(e.hir_id) {
            // Relate the unadjusted and adjusted types for this expr by stepping through the
            // intermediate adjustments one by one.
            let mut prev_ty = rty;
            for (i, adj) in adjs.iter().enumerate() {
                let rty =
                    if i < adjs.len() - 1 { self.ltt.label(adj.target) }
                    // Shortcut: instead of unifying the last adjustment's target type with the
                    // adjusted expr type, we use the adjusted expr type itself in place of the
                    // last target type.
                    else { self.expr_lty(e) };

                match adj.kind {
                    Adjust::NeverToAny => {},   // prev and result tys are unrelated
                    Adjust::ReifyFnPointer => {}, // TODO - need to unify the fn sigs
                    Adjust::UnsafeFnPointer => {
                        // prev and result ty shapes should be the same, only change is the
                        // "unsafe" tag on the function pointer.
                        self.ltt.unify(rty, prev_ty);
                    },
                    Adjust::ClosureFnPointer => {}, // unsupported
                    Adjust::MutToConstPointer => {
                        // Only the mutability tag changes
                        self.ltt.unify(rty, prev_ty);
                    },
                    Adjust::Deref(None) => {
                        self.ltt.unify(rty, prev_ty.args[0]);
                    },
                    Adjust::Deref(Some(_)) => {}, // TODO (overloaded deref case)
                    Adjust::Borrow(_) => {
                        // The AutoBorrow argument indicates whether we're going to a `&` or `*`
                        // pointer, and whether it's `mut` or `const`.  In all cases, the shape of
                        // rty is the same.
                        self.ltt.unify(rty.args[0], prev_ty);
                    },
                    Adjust::Unsize => {}, // TODO
                }

                prev_ty = rty;
            }
        }

        intravisit::walk_expr(self, e);
    }

    fn visit_pat(&mut self, p: &'hir Pat) {
        let rty = self.pat_lty(p);

        match p.node {
            PatKind::Wild => {},

            PatKind::Binding(_, def_id, _, ref opt_pat) => {
                self.ltt.unify(rty, self.def_lty(def_id));
                if let Some(ref p) = *opt_pat {
                    self.ltt.unify(rty, self.pat_lty(p));
                }
            },

            PatKind::Struct(..) => {}, // TODO

            PatKind::TupleStruct(..) => {}, // TODO

            PatKind::Path(..) => {}, // TODO

            PatKind::Tuple(ref ps, None) => {
                for (expected, p) in rty.args.iter().zip(ps.iter()) {
                    self.ltt.unify(expected, self.pat_lty(p));
                }
            },
            PatKind::Tuple(ref pats, Some(dotdot_idx)) => {}, // TODO

            PatKind::Box(ref p) => {
                self.ltt.unify(rty.args[0], self.pat_lty(p));
            },

            PatKind::Ref(ref p, _) => {
                self.ltt.unify(rty.args[0], self.pat_lty(p));
            },

            PatKind::Lit(_) => {},  // Nothing to unify

            PatKind::Range(..) => {}, // TODO

            PatKind::Slice(..) => {}, // TODO
        }

        intravisit::walk_pat(self, p);
    }

    fn visit_local(&mut self, l: &'hir Local) {
        if let Some(ref ty) = l.ty {
            self.ltt.unify(self.pat_lty(&l.pat), self.ty_lty(ty));
        }

        if let Some(ref e) = l.init {
            self.ltt.unify(self.pat_lty(&l.pat), self.expr_lty(e));
        }

        intravisit::walk_local(self, l);
    }

    fn visit_fn(&mut self,
                kind: intravisit::FnKind<'hir>,
                decl: &'hir FnDecl,
                body_id: BodyId,
                span: Span,
                id: NodeId) {
        if let intravisit::FnKind::Closure(..) = kind {
            return;
        }

        let body = self.hir_map.body(body_id);
        let def_id = self.hir_map.local_def_id(id);
        let sig = self.def_sig(def_id);

        for (i, ast_ty) in decl.inputs.iter().enumerate() {
            let lty = self.ty_lty(ast_ty);
            self.ltt.unify(lty, self.pat_lty(&body.arguments[i].pat));
            self.ltt.unify(lty, sig.inputs[i]);
        }

        let out_lty = match decl.output {
            FunctionRetTy::Return(ref ty) => self.ty_lty(ty),
            FunctionRetTy::DefaultReturn(_) => self.prim_lty("()"),
        };
        self.ltt.unify(out_lty, self.expr_lty(&body.value));
        self.ltt.unify(out_lty, sig.output);

        intravisit::walk_fn(self, kind, decl, body_id, span, id);
    }

    fn visit_struct_field(&mut self, field: &'hir StructField) {
        let def_id = self.hir_map.local_def_id(field.id);
        self.ltt.unify(self.ty_lty(&field.ty), self.def_lty(def_id));
        intravisit::walk_struct_field(self, field);
    }

    fn visit_foreign_item(&mut self, i: &'hir ForeignItem) {
        let def_id = self.hir_map.local_def_id(i.id);
        match i.node {
            ForeignItemFn(ref decl, _, _) => {
                let sig = self.def_sig(def_id);

                for (i, ast_ty) in decl.inputs.iter().enumerate() {
                    let lty = self.ty_lty(ast_ty);
                    self.ltt.unify(lty, sig.inputs[i]);
                }

                let out_lty = match decl.output {
                    FunctionRetTy::Return(ref ty) => self.ty_lty(ty),
                    FunctionRetTy::DefaultReturn(_) => self.prim_lty("()"),
                };
                self.ltt.unify(out_lty, sig.output);
            },

            ForeignItemStatic(ref ty, _) => {
                self.ltt.unify(self.ty_lty(ty), self.def_lty(def_id));
            },
        }

        intravisit::walk_foreign_item(self, i);
    }
}



pub fn analyze<'a, 'gcx, 'tcx>(hir_map: &hir::map::Map,
                               tcx: TyCtxt<'a, 'gcx, 'tcx>,
                               tcx_arena: &'tcx DroplessArena,
                               krate: &ast::Crate) -> HashMap<NodeId, u32> {
    let ltt = LTyTable::new(tcx_arena);

    // Collect labeled expr/pat types from the TypeckTables of each item.
    let mut v = ExprPatVisitor {
        tcx: tcx,
        ltt: &ltt,
        unadjusted: HashMap::new(),
        adjusted: HashMap::new(),
        substs: HashMap::new(),
    };
    hir_map.krate().visit_all_item_likes(&mut v);
    let ExprPatVisitor {
        unadjusted: unadjusted_nodes,
        adjusted: nodes,
        substs: node_substs,
        ..
    } = v;

    // Construct labeled types for each `ast::Ty` in the program.
    let ty_nodes = label_tys(hir_map, tcx, &ltt, krate);

    let prims = prim_tys(tcx, &ltt);

    let mut v = UnifyVisitor {
        hir_map: hir_map,
        tcx: tcx,
        ltt: &ltt,

        unadjusted_nodes: &unadjusted_nodes,
        nodes: &nodes,
        node_substs: &node_substs,
        ty_nodes: &ty_nodes,
        prims: &prims,
        defs: RefCell::new(HashMap::new()),
        def_sigs: RefCell::new(HashMap::new()),
    };
    hir_map.krate().visit_all_item_likes(&mut v.as_deep_visitor());

    ty_nodes.iter()
        .filter_map(
            |(&id, &lty)| lty.label.as_ref().map(
                |cell| {
                    let root = ltt.unif.borrow_mut().find(cell.get());
                    (id, root.index())
                }))
        .collect()
}
