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

use std::collections::{HashMap, HashSet};
use std::fmt;

use ena::unify::{UnificationTable, UnifyKey};
use rustc::hir;
use rustc::hir::*;
use rustc::hir::def_id::DefId;
use rustc::hir::map::Node::*;
use rustc::hir::itemlikevisit::{self, ItemLikeVisitor};
use rustc::ty::{self, TyCtxt};
use rustc_data_structures::indexed_vec::IndexVec;
use syntax::ast;
use syntax::ast::NodeId;
use syntax::symbol::Symbol;

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


/// Representation of the skeleton of a type.  The `Symbol`s are only for debugging.
#[derive(Clone, PartialEq, Eq)]
enum TySkel {
    Unsupported(Symbol),
    Prim(Symbol),
    Constr(Symbol, Vec<LTy>),
}

#[derive(Clone, PartialEq, Eq)]
struct LTy {
    label: TyLabel,
    ty: TySkel,
}

impl fmt::Debug for TySkel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TySkel::Unsupported(s) => write!(f, "ERR({})", s),
            TySkel::Prim(s) => write!(f, "{}", s),
            TySkel::Constr(s, ref args) => write!(f, "{}<{:?}>", s, args),
        }
    }
}

impl fmt::Debug for LTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{:?}", self.label.0, self.ty)
    }
}

impl LTy {
    fn canonicalize(&mut self, unif: &mut UnificationTable<TyLabel>) -> &Self {
        self.label = unif.find(self.label);
        match self.ty {
            TySkel::Unsupported(_) => {},
            TySkel::Prim(_) => {},
            TySkel::Constr(_, ref mut args) => {
                for arg in args {
                    arg.canonicalize(unif);
                }
            },
        }
        self
    }

    fn args(&self) -> &[LTy] {
        match self.ty {
            TySkel::Constr(_, ref args) => args,
            _ => &[],
        }
    }
}



struct Ctxt<'a, 'hir: 'a, 'gcx: 'tcx, 'tcx: 'a> {
    hir_map: &'a hir::map::Map<'hir>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,

    unif: UnificationTable<TyLabel>,

    /// Types corresponding to `Ty` nodes in the AST.  Our main goal is to sort these into
    /// equivalence classes.
    ty_nodes: HashMap<NodeId, LTy>,

    /// Entries in `prims` represent the "definitions" of primitive types.  These appear in the
    /// signatures of primitive operations like slice indexing.  For example, the equivalence class
    /// containing `prims["usize"]` consists of all the types that must be exactly `usize` (likely
    /// because they were used as indices).
    prims: HashMap<&'static str, LTy>,

    /// Types of various (monomorphic) definitions.  Includes statics, non-generic fns, and local
    /// variables.
    defs: HashMap<DefId, LTy>,
}

impl<'a, 'hir, 'gcx, 'tcx> Ctxt<'a, 'hir, 'gcx, 'tcx> {
    fn new(hir_map: &'a hir::map::Map<'hir>,
           tcx: TyCtxt<'a, 'gcx, 'tcx>) -> Ctxt<'a, 'hir, 'gcx, 'tcx> {
        Ctxt {
            hir_map: hir_map,
            tcx: tcx,

            unif: UnificationTable::new(),

            ty_nodes: HashMap::new(),
            prims: HashMap::new(),
            defs: HashMap::new(),
        }
    }

    fn unify(&mut self, ty1: &LTy, ty2: &LTy) {
        match (&ty1.ty, &ty2.ty) {
            (&TySkel::Unsupported(_), _) |
            (_, &TySkel::Unsupported(_)) => {
                // Ignore all unification involving unsupported types.
                return;
            },

            (&TySkel::Prim(sym1),
             &TySkel::Prim(sym2)) => {
                if sym1 != sym2 {
                    warn!("unifying unequal primitive types: {}, {}", sym1, sym2);
                }
            },

            (&TySkel::Constr(sym1, ref args1),
             &TySkel::Constr(sym2, ref args2)) => {
                if sym1 != sym2 || args1.len() != args2.len() {
                    warn!("unifying unequal constructed types: {}({}), {}({})",
                          sym1, args1.len(), sym2, args2.len());
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    self.unify(arg1, arg2);
                }
            },

            (_, _) => {
                warn!("unifying unequal types: {:?}, {:?}", ty1.ty, ty2.ty);
            },
        }

        self.unif.union(ty1.label, ty2.label);
    }

    fn mk_unsupported(&mut self, name: &str) -> LTy {
        let label = self.unif.new_key(());
        LTy {
            label: label,
            ty: TySkel::Unsupported(name.into_symbol()),
        }
    }

    fn mk_prim(&mut self, name: &str) -> LTy {
        let label = self.unif.new_key(());
        LTy {
            label: label,
            ty: TySkel::Prim(name.into_symbol()),
        }
    }

    fn mk_constr(&mut self, name: &str, args: Vec<LTy>) -> LTy {
        let label = self.unif.new_key(());
        LTy {
            label: label,
            ty: TySkel::Constr(name.into_symbol(), args),
        }
    }


    fn typeck_node_lty(&mut self, id: NodeId) -> LTy {
        let parent = self.hir_map.get_parent(id);
        let body = self.hir_map.body_owned_by(parent);
        let tables = self.tcx.body_tables(body);
        let ty = tables.node_id_to_type(id);
        let lty = self.label_ty(ty);
        lty
    }

    fn label_ty(&mut self, ty: ty::Ty) -> LTy {
        use rustc::ty::TypeVariants::*;
        match ty.sty {
            TyBool => self.mk_prim("bool"),
            TyChar => self.mk_prim("char"),
            TyInt(ity) => self.mk_prim(ity.ty_to_string()),
            TyUint(uty) => self.mk_prim(uty.ty_to_string()),
            TyFloat(fty) => self.mk_prim(fty.ty_to_string()),
            TyAdt(def, substs) => self.mk_unsupported("adt"),
            TyStr => self.mk_prim("str"),
            TyArray(ty, _) => {
                let arg = self.label_ty(ty);
                self.mk_constr("[_; _]", vec![arg])
            },
            TySlice(ty) => {
                let arg = self.label_ty(ty);
                self.mk_constr("[_]", vec![arg])
            },
            TyRawPtr(mty) => {
                let mutbl = mty.mutbl == hir::Mutability::MutMutable;
                let arg = self.label_ty(mty.ty);
                self.mk_constr(if mutbl { "*mut" } else { "*const" }, vec![arg])
            },
            TyRef(_, mty) => {
                let mutbl = mty.mutbl == hir::Mutability::MutMutable;
                let arg = self.label_ty(mty.ty);
                self.mk_constr(if mutbl { "&mut" } else { "&" }, vec![arg])
            },
            TyFnDef(def_id, substs) => self.mk_unsupported("fndef"),
            TyFnPtr(sig) => {
                let args = sig.0.inputs_and_output.iter().map(|ty| self.label_ty(ty)).collect();
                self.mk_constr("fn", args)
            },
            TyDynamic(_, _) => self.mk_unsupported("dynamic"),
            TyClosure(_, _) => self.mk_unsupported("closure"),
            TyNever => self.mk_unsupported("never"),
            TyTuple(args, _) => {
                let args = args.iter().map(|ty| self.label_ty(ty)).collect();
                self.mk_constr("tuple", args)
            },
            TyProjection(_) => self.mk_unsupported("projection"),
            TyAnon(_, _) => self.mk_unsupported("anon"),
            TyParam(_) => self.mk_unsupported("param"),
            TyInfer(_) => self.mk_unsupported("infer"),
            TyError => self.mk_unsupported("error"),
        }
    }


    fn prim_lty(&mut self, name: &'static str) -> LTy {
        if let Some(lty) = self.prims.get(&name) {
            return lty.clone();
        }

        let lty = self.mk_prim(name);
        self.prims.insert(name, lty.clone());
        lty
    }

    fn get_def_lty(&mut self, id: DefId) -> LTy {
        if let Some(node_id) = self.hir_map.as_local_node_id(id) {
            match self.hir_map.get(node_id) {
                NodeLocal(ref pat) => {
                    return self.typeck_node_lty(pat.id);
                },
                _ => {},
            }
        }

        let def_ty = self.tcx.type_of(id);
        self.label_ty(def_ty)
    }

    fn def_lty(&mut self, id: DefId) -> LTy {
        if let Some(lty) = self.defs.get(&id) {
            return lty.clone();
        }

        let lty = self.get_def_lty(id);
        self.defs.insert(id, lty.clone());
        lty
    }

    fn build_ty_lty(&mut self, ty: &Ty) -> LTy {
        match ty.node {
            TySlice(ref ty) => {
                let item_lty = self.ty_lty(ty);
                self.mk_constr("[_]", vec![item_lty])
            },
            TyArray(ref ty, _) => {
                let item_lty = self.ty_lty(ty);
                self.mk_constr("[_; _]", vec![item_lty])
            },
            TyPtr(ref mty) => {
                let ctor = if mty.mutbl == Mutability::MutMutable { "*mut" } else { "*const" };
                let lty = self.ty_lty(&mty.ty);
                self.mk_constr(ctor, vec![lty])
            },
            TyRptr(_, ref mty) => {
                let ctor = if mty.mutbl == Mutability::MutMutable { "&mut" } else { "&" };
                let lty = self.ty_lty(&mty.ty);
                self.mk_constr(ctor, vec![lty])
            },
            TyBareFn(ref fn_ty) => {
                let mut ltys = Vec::with_capacity(fn_ty.decl.inputs.len() + 1);
                for ty in &fn_ty.decl.inputs {
                    ltys.push(self.ty_lty(ty));
                }
                ltys.push(match fn_ty.decl.output {
                    FunctionRetTy::DefaultReturn(_) => self.mk_prim("()"),
                    FunctionRetTy::Return(ref ty) => self.ty_lty(ty),
                });
                self.mk_constr("fn", ltys)
            },
            TyNever => self.mk_prim("!"),
            TyTup(ref tys) => {
                let ltys = tys.iter().map(|ty| self.ty_lty(ty)).collect();
                self.mk_constr("()", ltys)
            },
            TyPath(ref path) => {
                let seg = match *path {
                    QPath::Resolved(_, ref p) => p.segments.last().unwrap(),
                    QPath::TypeRelative(_, ref seg) => seg,
                };
                let args = match seg.parameters {
                    PathParameters::AngleBracketedParameters(ref abpd) => {
                        abpd.types.iter().map(|ty| self.build_ty_lty(ty)).collect()
                    },
                    PathParameters::ParenthesizedParameters(ref ppd) => {
                        let mut args = ppd.inputs.iter()
                            .map(|ty| self.build_ty_lty(ty)).collect::<Vec<_>>();
                        if let Some(ref ty) = ppd.output {
                            args.push(self.build_ty_lty(ty));
                        }
                        args
                    },
                };
                self.mk_constr(&seg.name.as_str(), args)
            },

            TyTraitObject(..) => self.mk_unsupported("TraitObject"),
            TyImplTrait(..) => self.mk_unsupported("ImplTrait"),
            TyTypeof(..) => self.mk_unsupported("ImplTrait"),
            TyInfer => self.mk_prim("infer"),
            TyErr => self.mk_unsupported("Err"),
        }
    }

    fn ty_lty(&mut self, ty: &Ty) -> LTy {
        if let Some(lty) = self.ty_nodes.get(&ty.id) {
            return lty.clone();
        }

        let lty = self.build_ty_lty(ty);
        self.ty_nodes.insert(ty.id, lty.clone());
        lty
    }

    fn block_lty(&mut self, b: &Block) -> LTy {
        for s in &b.stmts {
            self.walk_stmt(s);
        }
        if let Some(ref e) = b.expr {
            self.expr_lty(e)
        } else {
            self.prim_lty("()")
        }
    }

    fn walk_stmt(&mut self, s: &Stmt) {
        match s.node {
            StmtDecl(ref d, _) => { self.walk_decl(d); },
            StmtExpr(ref e, _) => { self.expr_lty(e); },
            StmtSemi(ref e, _) => { self.expr_lty(e); },
        }
    }

    fn walk_decl(&mut self, d: &Decl) {
        match d.node {
            DeclLocal(ref l) => self.walk_local(l),
            DeclItem(_) => {},
        }
    }

    fn walk_local(&mut self, l: &Local) {
        let pat_lty = self.pat_lty(&l.pat);

        if let Some(ref ty) = l.ty {
            let ty_lty = self.ty_lty(ty);
            self.unify(&pat_lty, &ty_lty);
        }

        if let Some(ref e) = l.init {
            let expr_lty = self.expr_lty(e);
            self.unify(&pat_lty, &expr_lty);
        }
    }

    fn expr_lty(&mut self, e: &Expr) -> LTy {
        // Get the true expression type from `tcx`.
        let tcx_lty = self.typeck_node_lty(e.id);

        // Unify with related types to propagate label equivalence.
        match e.node {
            ExprBox(ref e) => {
                let lty = self.expr_lty(e);
                self.unify(&lty, &tcx_lty.args()[0]);
            },

            ExprArray(ref es) => {
                let expected = &tcx_lty.args()[0];
                for e in es {
                    let lty = self.expr_lty(e);
                    self.unify(&lty, expected);
                }
            },

            ExprCall(ref func, ref args) => {
                let mut ltys = Vec::with_capacity(args.len() + 1);
                ltys.extend(args.iter().map(|e| self.expr_lty(e)));
                ltys.push(tcx_lty.clone());
                let expected = self.mk_constr("fn", ltys);

                let func_lty = self.expr_lty(func);

                eprintln!("function call");
                eprintln!("  func = {:?}", func_lty);
                eprintln!("  exp. = {:?}", expected);
                self.unify(&func_lty, &expected);
            },

            ExprMethodCall(..) => {}, // TODO

            ExprTup(ref es) => {
                for (e, expected) in es.iter().zip(tcx_lty.args().iter()) {
                    let lty = self.expr_lty(e);
                    self.unify(&lty, expected);
                }
            },

            ExprBinary(..) => {}, // TODO

            ExprUnary(op, ref a) => {
                let a_lty = self.expr_lty(a);
                match op {
                    UnDeref => self.unify(&a_lty.args()[0], &tcx_lty),
                    UnNot => self.unify(&a_lty, &tcx_lty),
                    UnNeg => self.unify(&a_lty, &tcx_lty),
                }
            },

            ExprLit(..) => {},  // Nothing to unify

            ExprCast(_, ref ty) => {
                let lty = self.ty_lty(ty);
                self.unify(&lty, &tcx_lty);
                // Ignore the expr type, since it has no connection to `ty` or `tcx_lty`.
            },

            ExprType(ref e, ref ty) => {
                // Both the type of the expr and the annotation type should match the `tcx` result.
                let lty1 = self.expr_lty(e);
                let lty2 = self.ty_lty(ty);
                self.unify(&lty1, &tcx_lty);
                self.unify(&lty2, &tcx_lty);
            },

            ExprIf(ref cond, ref e_true, ref e_false) => {
                let cond_lty = self.expr_lty(cond);
                let cond_expect = self.prim_lty("bool");
                self.unify(&cond_lty, &cond_expect);

                // Both branches should produce results of the expected type
                let lty1 = self.expr_lty(e_true);
                let lty2 = match e_false.as_ref() {
                    Some(e) => self.expr_lty(e),
                    None => self.prim_lty("()"),
                };
                self.unify(&lty1, &tcx_lty);
                self.unify(&lty2, &tcx_lty);
            },

            ExprWhile(ref cond, ref body, _) => {
                let cond_lty = self.expr_lty(cond);
                let cond_expect = self.prim_lty("bool");
                self.unify(&cond_lty, &cond_expect);

                // The body should produce a result of type `()`.
                let body_lty = self.block_lty(body);
                let unit = self.prim_lty("()");
                self.unify(&body_lty, &unit);
                self.unify(&tcx_lty, &unit);
            },

            ExprLoop(..) => {}, // TODO

            ExprMatch(..) => {}, // TODO

            ExprClosure(..) => {}, // TODO

            ExprBlock(ref b) => {
                let lty = self.block_lty(b);
                self.unify(&lty, &tcx_lty);
            },

            ExprAssign(ref lhs, ref rhs) => {
                let lhs_lty = self.expr_lty(lhs);
                let rhs_lty = self.expr_lty(rhs);
                self.unify(&lhs_lty, &rhs_lty);

                let unit = self.prim_lty("()");
                self.unify(&tcx_lty, &unit);
            },

            ExprAssignOp(..) => {}, // TODO

            ExprField(..) => {},

            ExprTupField(ref e, ref idx) => {}, // TODO

            ExprIndex(ref arr, ref idx) => {}, // TODO

            ExprPath(ref path) => {
                match *path {
                    QPath::Resolved(_, ref path) => {
                        if let Some(def_id) = path.def.opt_def_id() {
                            let def_lty = self.def_lty(def_id);
                            self.unify(&def_lty, &tcx_lty);
                        }
                    },
                    _ => {},
                }
            },

            ExprAddrOf(_, ref e) => {
                let lty = self.expr_lty(e);
                self.unify(&lty, &tcx_lty.args()[0]);
            },

            // break/continue/return all have type `!`, which unifies with everything.
            ExprBreak(_, ref result) => {
                // TODO: handle result == Some(x) case
            },

            ExprAgain(_) => {},

            ExprRet(ref result) => {
                // TODO: handle result == Some(x) case
            },

            ExprInlineAsm(..) => {},

            ExprStruct(..) => {},

            ExprRepeat(ref e, _) => {
                let lty = self.expr_lty(e);
                self.unify(&lty, &tcx_lty.args()[0]);
            },
        }

        tcx_lty
    }

    fn pat_lty(&mut self, p: &Pat) -> LTy {
        // Get the true pattern type from `tcx`.
        let tcx_lty = self.typeck_node_lty(p.id);

        // Unify with related types to propagate label equivalence.
        match p.node {
            PatKind::Wild => {},

            PatKind::Binding(_, def_id, _, ref opt_pat) => {
                let def_lty = self.def_lty(def_id);
                self.unify(&def_lty, &tcx_lty);

                if let Some(ref pat) = *opt_pat {
                    let lty = self.pat_lty(pat);
                    self.unify(&lty, &tcx_lty);
                }
            },

            PatKind::Struct(..) => {}, // TODO

            PatKind::TupleStruct(..) => {}, // TODO

            PatKind::Path(..) => {}, // TODO

            PatKind::Tuple(ref pats, None) => {
                for (pat, expect_lty) in pats.iter().zip(tcx_lty.args().iter()) {
                    let lty = self.pat_lty(pat);
                    self.unify(&lty, expect_lty);
                }
            },
            PatKind::Tuple(ref pats, Some(dotdot_idx)) => {}, // TODO

            PatKind::Box(ref pat) => {
                let lty = self.pat_lty(pat);
                self.unify(&lty, &tcx_lty.args()[0]);
            },

            PatKind::Ref(ref pat, _) => {
                let lty = self.pat_lty(pat);
                self.unify(&lty, &tcx_lty.args()[0]);
            },

            PatKind::Lit(_) => {},  // Nothing to unify

            PatKind::Range(..) => {}, // TODO

            PatKind::Slice(..) => {}, // TODO
        }

        tcx_lty
    }

    fn walk_fn_body(&mut self, id: BodyId, decl: &FnDecl) {
        let body = self.hir_map.body(id);

        for (arg, ty) in body.arguments.iter().zip(decl.inputs.iter()) {
            let pat_lty = self.pat_lty(&arg.pat);
            let ty_lty = self.ty_lty(ty);
            self.unify(&pat_lty, &ty_lty);
        }

        let ret_lty = match decl.output {
            FunctionRetTy::DefaultReturn(_) => self.prim_lty("()"),
            FunctionRetTy::Return(ref ty) => self.ty_lty(ty),
        };
        let body_lty = self.expr_lty(&body.value);
        self.unify(&body_lty, &ret_lty);



        let tables = self.tcx.body_tables(id);
        let mut keys = tables.node_types.keys().map(|&x| x).collect::<Vec<_>>();
        keys.sort();
        eprintln!("node types for fn body:");
        for id in keys {
            let ty = tables.node_types[&id];
            if let Some(node) = self.hir_map.find(id) {
                eprintln!("  {:?} {:?}: {:?}", id, node, ty);
            } else {
                eprintln!("  {:?} <not found>: {:?}", id, ty);
            }
        }
    }

    fn debug(&mut self) {
        let mut keys = self.ty_nodes.keys().map(|&x| x).collect::<Vec<_>>();
        keys.sort();

        eprintln!("ty_nodes:");
        for id in keys {
            let node = self.hir_map.find(id);
            let lty = self.ty_nodes.get_mut(&id).unwrap();
            lty.canonicalize(&mut self.unif);
            eprintln!("  {:?} ({:?}): {:?}", id, node, lty);
        }
    }
}


struct UnificationVisitor<'a, 'hir: 'a, 'gcx: 'tcx, 'tcx: 'a> {
    ctxt: Ctxt<'a, 'hir, 'gcx, 'tcx>,
}

impl<'a, 'hir, 'gcx, 'tcx> ItemLikeVisitor<'hir> for UnificationVisitor<'a, 'hir, 'gcx, 'tcx> {
    fn visit_item(&mut self, item: &'hir Item) {
        match item.node {
            ItemFn(ref decl, _, _, _, _, body_id) => self.ctxt.walk_fn_body(body_id, decl),
            _ => {},
        }
    }

    fn visit_trait_item(&mut self, item: &'hir TraitItem) {
        match item.node {
            TraitItemKind::Method(ref sig, TraitMethod::Provided(body_id)) =>
                self.ctxt.walk_fn_body(body_id, &sig.decl),
            _ => {},
        }
    }

    fn visit_impl_item(&mut self, item: &'hir ImplItem) {
        match item.node {
            ImplItemKind::Method(ref sig, body_id) => self.ctxt.walk_fn_body(body_id, &sig.decl),
            _ => {},
        }
    }
}


pub fn analyze(hir_map: &hir::map::Map, tcx: &TyCtxt) -> HashMap<NodeId, u32> {
    let mut v = UnificationVisitor {
        ctxt: Ctxt::new(hir_map, *tcx),
    };
    hir_map.krate().visit_all_item_likes(&mut v);
    v.ctxt.debug();

    v.ctxt.ty_nodes.iter()
        .map(|(&id, lty)| (id, lty.label.index()))
        .collect()
}
