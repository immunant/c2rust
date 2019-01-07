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
//!
//! Note that this analysis doesn't try to reason about traits.  Changing the type of an expression
//! can have all sorts of non-trivial effects on the program, such as changing which trait is used
//! at a method call (since the new type might implement a different type with the same method
//! name).  In this code, we assume the callee is fixed at every method call site.
//!
//! The handling of type aliases (including associated types) is also not very sophisticated.  The
//! analysis doesn't expand aliases before unification, so it never unifies the RHS type of an
//! alias with any other types.
//!
//! Other features currently not handled:
//!
//!  * Closures
//!  * Operator overloading
//!  * Many specific expr variants, expr adjustments, and pattern variants.  See `UnifyVisitor::visit_expr` and related functions for details.
//!
//! (The best fix may be to switch to analyzing MIR, as it has many fewer cases.  But that requires
//! mapping `ast::Ty` annotations down to MIR, which is likely nontrivial.)

use std::cell::RefCell;
use std::collections::HashMap;

use arena::SyncDroplessArena;
use ena::unify::{UnificationTable, UnifyKey, InPlace};
use rustc::hir;
use rustc::hir::*;
use rustc::hir::def_id::DefId;
use rustc::hir::intravisit::{self, Visitor, NestedVisitorMap};
use rustc::hir::itemlikevisit::ItemLikeVisitor;
use rustc::ty::{self, TyCtxt, TypeckTables};
use rustc::ty::adjustment::Adjust;
// use syntax::abi::Abi;
use rustc_target::spec::abi::Abi;
use syntax::ast;
use syntax::ast::NodeId;
use syntax::source_map::Span;
use syntax::symbol::Symbol;

use analysis::labeled_ty::{LabeledTy, LabeledTyCtxt};
use type_map;


/// Unification key for types.
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


/// Labels used on types.  If the `Option` is `None`, then there's nothing to unify at this node
/// (see `LTyTable::non_unifiable`).
type Label = Option<TyLabel>;

/// A `Ty` where every node is labeled with a unification key.
type LTy<'tcx> = LabeledTy<'tcx, Label>;

/// A `FnSig` where every node is labeled with a unification key.
#[derive(Clone, Copy, Debug)]
struct LFnSig<'tcx> {
    inputs: &'tcx [LTy<'tcx>],
    output: LTy<'tcx>,
    variadic: bool,
}


/// A table for tracking labeled types and their unifications.
struct LTyTable<'tcx> {
    unif: RefCell<UnificationTable<InPlace<TyLabel>>>,
    lcx: LabeledTyCtxt<'tcx, Label>,
}

impl<'tcx> LTyTable<'tcx> {
    fn new(arena: &'tcx SyncDroplessArena) -> LTyTable<'tcx> {
        LTyTable {
            unif: RefCell::new(UnificationTable::new()),
            lcx: LabeledTyCtxt::new(arena),
        }
    }


    /// Label a `Ty` with fresh unification keys.
    fn label(&self, ty: ty::Ty<'tcx>) -> LTy<'tcx> {
        self.lcx.label(ty, &mut |_| Some(self.unif.borrow_mut().new_key(())))
    }

    fn label_slice(&self, tys: &[ty::Ty<'tcx>]) -> &'tcx [LTy<'tcx>] {
        self.lcx.label_slice(tys, &mut |_| Some(self.unif.borrow_mut().new_key(())))
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


    /// Unify two types, including any type arguments they may have.
    fn unify(&self, lty1: LTy<'tcx>, lty2: LTy<'tcx>) {
        if let (Some(l1), Some(l2)) = (lty1.label, lty2.label) {
            self.unif.borrow_mut().union(l1, l2);
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
struct ExprPatVisitor<'a, 'tcx: 'a> {
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    ltt: &'a LTyTable<'tcx>,

    /// The labeled unadjusted type, for every node that has a type.
    unadjusted: HashMap<HirId, LTy<'tcx>>,
    /// The labeled adjusted type, for every node that has adjustments.
    adjusted: HashMap<HirId, LTy<'tcx>>,
    /// The labeled substitutions, for every node where type substitutions were applied.
    substs: HashMap<HirId, &'tcx [LTy<'tcx>]>,
}

impl<'a, 'tcx> ExprPatVisitor<'a, 'tcx> {
    /// Process the type tables for a single body.
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

impl<'a, 'hir> ItemLikeVisitor<'hir> for ExprPatVisitor<'a, 'hir> {
    // Visit every itemlike with a BodyId, and call `handle_body` on each.

    fn visit_item(&mut self, item: &'hir Item) {
        let body_id = match item.node {
            ItemKind::Static(_, _, body_id) => body_id,
            ItemKind::Const(_, body_id) => body_id,
            ItemKind::Fn(_, _, _, body_id) => body_id,
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



/// `type_map::TypeSource` for getting `TyCtxt` results.  Used when collecting the labeled types
/// for `ast::Ty` nodes.
struct LabelTysSource<'a, 'tcx: 'a> {
    hir_map: &'a hir::map::Map<'tcx>,
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    ltt: &'a LTyTable<'tcx>,
}

impl<'a, 'tcx> LabelTysSource<'a, 'tcx> {
    fn get_tables(&self, id: NodeId) -> &'tcx TypeckTables<'tcx> {
        let parent = self.hir_map.get_parent(id);
        let parent_body = self.hir_map.body_owned_by(parent);
        self.tcx.body_tables(parent_body)
    }

    fn node_lty(&self, id: NodeId) -> LTy<'tcx> {
        let tables = self.get_tables(id);
        let hir_id = self.hir_map.node_to_hir_id(id);
        let ty = tables.node_id_to_type(hir_id);
        self.ltt.label(ty)
    }
}

impl<'a, 'tcx> type_map::TypeSource for LabelTysSource<'a, 'tcx> {
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
        Some(self.ltt.label_sig(*sig.skip_binder()))
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

/// Label the `ty::Ty` for every `ast::Ty` in the crate.
fn label_tys<'a, 'tcx>(hir_map: &hir::map::Map<'tcx>,
                       tcx: TyCtxt<'a, 'tcx, 'tcx>,
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



/// Build a map of primitive types used by specific language features, such as `bool`'s usage in
/// `if` and `while`.
fn prim_tys<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>,
                      ltt: &'a LTyTable<'tcx>)
                      -> HashMap<&'static str, LTy<'tcx>> {
    let mut map = HashMap::new();

    map.insert("bool", ltt.label(tcx.mk_bool()));
    map.insert("()", ltt.label(tcx.mk_unit()));
    map.insert("usize", ltt.label(tcx.mk_mach_uint(ast::UintTy::Usize)));

    map
}



/// Walk over the HIR, unifying types as equality constraints are discovered.
struct UnifyVisitor<'a, 'tcx: 'a> {
    hir_map: &'a hir::map::Map<'tcx>,
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    ltt: &'a LTyTable<'tcx>,

    // These are the tables generated by the visitors and functions defined above.
    unadjusted_nodes: &'a HashMap<HirId, LTy<'tcx>>,
    nodes: &'a HashMap<HirId, LTy<'tcx>>,
    node_substs: &'a HashMap<HirId, &'tcx [LTy<'tcx>]>,
    ty_nodes: &'a HashMap<NodeId, LTy<'tcx>>,
    prims: &'a HashMap<&'static str, LTy<'tcx>>,

    /// Cache of labeled types for each definition.
    ///
    /// Unfortunately, there is no central index of all/most `DefId`s and their types, like there
    /// is for exprs and patterns.  And we can't easily visit all defs because some are pulled in
    /// from other crates.  Since we can't precompute the `LTy` for every def, we have to keep this
    /// cache and add defs to it as we encounter them.
    defs: RefCell<HashMap<DefId, LTy<'tcx>>>,

    /// Cache of labeled signatures of function/method definitions.  For generic functions, this
    /// always contains the unsubstituted (polymorphic) signature.
    def_sigs: RefCell<HashMap<DefId, LFnSig<'tcx>>>,
}

impl<'a, 'tcx> UnifyVisitor<'a, 'tcx> {
    // Helpers for looking up labeled types in the various precomputed tables.

    fn node_lty(&self, id: NodeId) -> LTy<'tcx> {
        let tables = self.get_tables(id);
        let hir_id = self.hir_map.node_to_hir_id(id);
        let ty = tables.node_id_to_type(hir_id);
        self.ltt.label(ty)
    }

    fn expr_lty(&self, e: &Expr) -> LTy<'tcx> {
        self.nodes.get(&e.hir_id)
            .or_else(|| self.unadjusted_nodes.get(&e.hir_id))
            .unwrap_or_else(|| panic!("expr_lty: no lty for {:?} @ {:?}",
                                      e, self.tcx.sess.source_map().span_to_string(e.span)))
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
                                      p, self.tcx.sess.source_map().span_to_string(p.span)))
    }

    fn ty_lty(&self, t: &Ty) -> LTy<'tcx> {
        self.ty_nodes.get(&t.id)
            .unwrap_or_else(|| panic!("ty_lty: no lty for {:?} @ {:?}",
                                      t, self.tcx.sess.source_map().span_to_string(t.span)))
    }

    fn prim_lty(&self, name: &'static str) -> LTy<'tcx> {
        self.prims.get(&name)
            .unwrap_or_else(|| panic!("prim_lty: no such prim {:?}", name))
    }


    // Functions for accessing the def ty/sig caches

    fn compute_def_lty(&self, id: DefId) -> LTy<'tcx> {
        match self.hir_map.get_if_local(id) {
            Some(Node::Binding(p)) => {
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
        let is_extern = match sig.skip_binder().abi {
            Abi::Rust | Abi::RustIntrinsic | Abi::RustCall => false,
            _ => true,
        };

        if !is_extern {
            self.ltt.label_sig(*sig.skip_binder())
        } else {
            self.ltt.non_unifiable_sig(*sig.skip_binder())
        }
    }

    fn def_sig(&self, id: DefId) -> LFnSig<'tcx> {
        *self.def_sigs.borrow_mut().entry(id)
            .or_insert_with(|| self.compute_def_sig(id))
    }


    // Helpers for extracting information from function types.

    fn fn_num_inputs(&self, lty: LTy<'tcx>) -> usize {
        use rustc::ty::TyKind::*;
        match lty.ty.sty {
            FnDef(id, _) => self.def_sig(id).inputs.len(),
            FnPtr(_) => lty.args.len() - 1,
            // TODO: Handle Closure.  This should be similar to FnDef, but the substs are a bit
            // more complicated.
            _ => panic!("fn_num_inputs: not a fn type"),
        }
    }

    /// Get the input types out of a `FnPtr` or `FnDef` `LTy`.
    fn fn_input(&self, lty: LTy<'tcx>, idx: usize) -> LTy<'tcx> {
        use rustc::ty::TyKind::*;
        match lty.ty.sty {
            FnDef(id, _) => {
                // For a `FnDef`, retrieve the `LFnSig` for the given `DefId` and apply the
                // labeled substs recorded in `LTy.args`.
                let sig = self.def_sig(id);
                self.ltt.subst(sig.inputs[idx], &lty.args)
            },
            FnPtr(_) => {
                // For a `FnPtr`, `lty.args` records the labeled input and output types.
                &lty.args[idx]
            },
            // TODO: Closure
            _ => panic!("fn_input: not a fn type"),
        }
    }

    /// Get the output type out of a `FnPtr` or `FnDef` `LTy`.
    fn fn_output(&self, lty: LTy<'tcx>) -> LTy<'tcx> {
        use rustc::ty::TyKind::*;
        match lty.ty.sty {
            FnDef(id, _) => {
                let sig = self.def_sig(id);
                self.ltt.subst(sig.output, &lty.args)
            },
            FnPtr(_) => {
                &lty.args[lty.args.len() - 1]
            },
            // TODO: Closure
            _ => panic!("fn_output: not a fn type"),
        }
    }

    fn fn_is_variadic(&self, lty: LTy<'tcx>) -> bool {
        use rustc::ty::TyKind::*;
        match lty.ty.sty {
            FnDef(id, _) => {
                self.def_sig(id).variadic
            },
            FnPtr(ty_sig) => {
                ty_sig.skip_binder().variadic
            },
            // TODO: Closure
            _ => panic!("fn_is_variadic: not a fn type"),
        }
    }


    fn get_tables(&self, id: NodeId) -> &'tcx TypeckTables<'tcx> {
        let parent = self.hir_map.get_parent(id);
        let parent_body = self.hir_map.body_owned_by(parent);
        self.tcx.body_tables(parent_body)
    }

    /// Get the signature of the method being called by an expression.  This includes substituting
    /// in the type arguments, if the method is generic.
    fn method_sig(&self, e: &Expr) -> LFnSig<'tcx> {
        let def_id = self.get_tables(e.id).type_dependent_defs()[e.hir_id].def_id();
        let sig = self.def_sig(def_id);
        let substs = self.node_substs.get(&e.hir_id).map_or_else(|| &[] as &[_], |x| x);
        self.ltt.subst_sig(sig, substs)
    }


    /// Get the labeled type of a field.  For generic structs, this returns the type after
    /// substitution, using the type arguments from `struct_ty`.
    fn field_lty(&self, struct_ty: LTy<'tcx>, name: Symbol) -> LTy<'tcx> {
        let adt = match struct_ty.ty.sty {
            ty::TyKind::Adt(ref adt, _) => adt,
            _ => panic!("field_lty: not a struct ty: {:?}", struct_ty),
        };
        let variant = adt.non_enum_variant();
        for field in &variant.fields {
            if field.ident.name == name {
                let base = self.def_lty(field.did);
                return self.ltt.subst(base, &struct_ty.args);
            }
        }
        panic!("field_lty: no field `{}` in {:?}", name, struct_ty);
    }
}

impl<'a, 'hir> Visitor<'hir> for UnifyVisitor<'a, 'hir> {
    fn nested_visit_map<'this>(&'this mut self) -> NestedVisitorMap<'this, 'hir> {
        NestedVisitorMap::OnlyBodies(self.hir_map)
    }

    fn visit_expr(&mut self, e: &'hir Expr) {
        use rustc::hir::BinOpKind::*;

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

        // TODO: Support operator overloading.  I think this can be detected by checking for a
        // `type_dependent_defs` entry on a non-`MethodCall` node.  (Alternative: rewrite this
        // whole analysis to run over MIR.  At that level, operator-overload method calls are fully
        // explicit.)

        match e.node {
            ExprKind::Box(ref e) => {
                self.ltt.unify(rty.args[0], self.expr_lty(e));
            },

            ExprKind::Array(ref es) => {
                for e in es {
                    self.ltt.unify(rty.args[0], self.expr_lty(e));
                }
            },

            ExprKind::Call(ref func, ref args) => {
                let func_lty = self.expr_lty(func);

                fn is_closure(ty: ty::Ty) -> bool {
                    if let ty::TyKind::Closure(..) = ty.sty {
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

            ExprKind::MethodCall(_, _, ref args) => {
                let sig = self.method_sig(e);
                for (i, arg) in args.iter().enumerate() {
                    self.ltt.unify(sig.inputs[i], self.expr_lty(arg));
                }
                self.ltt.unify(rty, sig.output);
            },

            ExprKind::Tup(ref es) => {
                for (expected, e) in rty.args.iter().zip(es.iter()) {
                    self.ltt.unify(expected, self.expr_lty(e));
                }
            },

            ExprKind::Binary(ref op, ref a, ref b) => {
                match op.node {
                    Add | Sub | Mul | Div | Rem |
                    BitXor | BitAnd | BitOr |
                    Shl | Shr => {
                        self.ltt.unify(rty, self.expr_lty(a));
                        self.ltt.unify(rty, self.expr_lty(b));
                    },
                    And | Or => {
                        self.ltt.unify(rty, self.prim_lty("bool"));
                        self.ltt.unify(self.expr_lty(a), self.prim_lty("bool"));
                        self.ltt.unify(self.expr_lty(b), self.prim_lty("bool"));
                    },
                    Eq | Lt | Le | Ne | Ge | Gt => {
                        self.ltt.unify(rty, self.prim_lty("bool"));
                        self.ltt.unify(self.expr_lty(a), self.expr_lty(b));
                    },
                }
            },

            ExprKind::Unary(op, ref a) => {
                match op {
                    UnDeref => self.ltt.unify(rty, self.expr_lty(a).args[0]),
                    UnNot => self.ltt.unify(rty, self.expr_lty(a)),
                    UnNeg => self.ltt.unify(rty, self.expr_lty(a)),
                }
            },

            ExprKind::Lit(..) => {},  // Nothing to unify

            ExprKind::Cast(_, ref ty) => {
                self.ltt.unify(rty, self.ty_lty(ty));
                // Ignore the expr type, since it has no connection to `rty`.
            },

            ExprKind::Type(ref e, ref ty) => {
                self.ltt.unify(rty, self.expr_lty(e));
                self.ltt.unify(rty, self.ty_lty(ty));
            },

            ExprKind::If(ref cond, ref e_true, ref e_false) => {
                self.ltt.unify(self.prim_lty("bool"), self.expr_lty(cond));
                self.ltt.unify(rty, self.expr_lty(e_true));
                self.ltt.unify(rty, e_false.as_ref().map_or_else(|| self.prim_lty("()"),
                                                                 |e| self.expr_lty(e)));
            },

            ExprKind::While(ref cond, ref body, _) => {
                self.ltt.unify(self.prim_lty("bool"), self.expr_lty(cond));
                self.ltt.unify(self.prim_lty("()"), self.block_lty(body));
                self.ltt.unify(rty, self.prim_lty("()"));
            },

            ExprKind::Loop(..) => {}, // TODO

            ExprKind::Match(..) => {}, // TODO

            ExprKind::Closure(..) => {}, // TODO

            ExprKind::Block(ref b, _) => {
                self.ltt.unify(rty, self.block_lty(b));
            },

            ExprKind::Assign(ref lhs, ref rhs) => {
                self.ltt.unify(self.expr_lty(lhs), self.expr_lty(rhs));
                self.ltt.unify(rty, self.prim_lty("()"));
            },

            ExprKind::AssignOp(..) => {}, // TODO

            ExprKind::Field(ref e, ref field) => { // TODO: tuples
                self.ltt.unify(rty, self.field_lty(self.expr_lty(e), field.name));
            },

            ExprKind::Index(ref _arr, ref _idx) => {}, // TODO

            ExprKind::Path(ref path) => {
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

            ExprKind::AddrOf(_, ref e) => {
                self.ltt.unify(rty.args[0], self.expr_lty(e));
            },

            // break/continue/return all have type `!`, which unifies with everything.
            ExprKind::Break(ref _dest, ref _result) => {
                // TODO: handle result == Some(x) case (unify the target `ExprKind::Loop`'s type with the
                // result expression type)
            },

            ExprKind::Continue(_) => {},

            ExprKind::Ret(ref _result) => {
                // TODO: handle result == Some(x) case (unify the result type with the current
                // function's return type)
            },

            ExprKind::Yield(ref _result) => {
                // TODO: handle result == Some(x) case
            },

            ExprKind::InlineAsm(..) => {},

            ExprKind::Struct(_, ref fields, ref base) => {
                for field in fields {
                    self.ltt.unify(self.field_lty(rty, field.ident.name),
                                   self.expr_lty(&field.expr));
                }

                if let Some(ref base) = *base {
                    self.ltt.unify(rty, self.expr_lty(base));
                }
            },

            ExprKind::Repeat(ref e, _) => {
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

            PatKind::Binding(_, node_id, _, ref opt_pat) => {

                let lty = self.node_lty(node_id);
                self.ltt.unify(rty, lty);
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
            PatKind::Tuple(ref _ps, Some(_dotdot_idx)) => {}, // TODO

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
        // The results of `def_sig` and `def_lty` are produced by calling `tcx.fn_sig` /
        // `tcx.type_of` and giving the results fresh labels, so they initially have no connection
        // to other representations of the item signature / type.  One of the reasons we need to
        // visit functions and other items is to unify the `def_sig` / `def_lty` types with the
        // types associated with the defs' `ast::Ty` annotations.

        // Unify argument type annotations with the types of the corresponding patterns, and with
        // the argument types that appear in the `DefId` signature.
        for (i, ast_ty) in decl.inputs.iter().enumerate() {
            let lty = self.ty_lty(ast_ty);
            self.ltt.unify(lty, self.pat_lty(&body.arguments[i].pat));
            self.ltt.unify(lty, sig.inputs[i]);
        }

        // Unify the return type annotation with the body expr type and the signature return type.
        let out_lty = match decl.output {
            FunctionRetTy::Return(ref ty) => self.ty_lty(ty),
            FunctionRetTy::DefaultReturn(_) => self.prim_lty("()"),
        };
        self.ltt.unify(out_lty, self.expr_lty(&body.value));
        self.ltt.unify(out_lty, sig.output);

        intravisit::walk_fn(self, kind, decl, body_id, span, id);
    }

    fn visit_struct_field(&mut self, field: &'hir StructField) {
        // Unify the field's type annotation with the definition type.
        let def_id = self.hir_map.local_def_id(field.id);
        self.ltt.unify(self.ty_lty(&field.ty), self.def_lty(def_id));
        intravisit::walk_struct_field(self, field);
    }

    fn visit_foreign_item(&mut self, i: &'hir ForeignItem) {
        let def_id = self.hir_map.local_def_id(i.id);
        match i.node {
            ForeignItemKind::Fn(ref decl, _, _) => {
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

            ForeignItemKind::Static(ref ty, _) => {
                self.ltt.unify(self.ty_lty(ty), self.def_lty(def_id));
            },

            ForeignItemKind::Type => { },
        }

        intravisit::walk_foreign_item(self, i);
    }

    // TODO: handle const and non-foreign static items.  These should be similar to the
    // `ForeignItemKind::Static` case.
}



/// Run the analysis, producing a map from `ast::Ty` `NodeId`s to an equivalence class number.
pub fn analyze<'a, 'tcx>(hir_map: &hir::map::Map<'tcx>,
                         tcx: TyCtxt<'a, 'tcx, 'tcx>,
                         tcx_arena: &'tcx SyncDroplessArena,
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

    // Construct labeled types for primitive operations.
    let prims = prim_tys(tcx, &ltt);


    // Run the unification pass.
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


    // For all `ast::Ty` nodes, build a map with the `NodeId` and the raw label of the root of its
    // equivalence class.
    ty_nodes.iter()
        .filter_map(
            |(&id, &lty)| lty.label.map(|l| {
                let root = ltt.unif.borrow_mut().find(l);
                (id, root.index())
            }))
        .collect()
}
