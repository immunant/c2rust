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

use std::collections::{HashMap, HashSet};
use std::fmt;

use ena::unify::{UnificationTable, UnifyKey};
use rustc::hir;
use rustc::hir::*;
use rustc::hir::def_id::DefId;
use rustc::hir::intravisit::{self, Visitor, NestedVisitorMap};
use rustc::hir::itemlikevisit::{self, ItemLikeVisitor};
use rustc::hir::map::Node::*;
use rustc::ty::{self, TyCtxt, TypeckTables};
use rustc::ty::subst::{self, Substs};
use rustc_data_structures::indexed_vec::IndexVec;
use syntax::ast;
use syntax::ast::NodeId;
use syntax::ptr::P;
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


#[derive(Clone, PartialEq, Eq, Debug)]
struct LTy<'tcx> {
    ty: ty::Ty<'tcx>,
    label: TyLabel,
    args: Box<[LTy<'tcx>]>,
}

impl<'tcx> LTy<'tcx> {
    fn canonicalize(&mut self, unif: &mut UnificationTable<TyLabel>) -> &Self {
        self.label = unif.find(self.label);
        for arg in self.args.iter_mut() {
            arg.canonicalize(unif);
        }
        self
    }
}


struct LTyTable {
    unif: UnificationTable<TyLabel>,
}

impl LTyTable {
    fn new() -> LTyTable {
        LTyTable {
            unif: UnificationTable::new(),
        }
    }

    fn mk<'tcx>(&mut self, ty: ty::Ty<'tcx>, args: Vec<LTy<'tcx>>) -> LTy<'tcx> {
        let label = self.unif.new_key(());
        LTy {
            ty: ty,
            label: label,
            args: args.into_boxed_slice(),
        }
    }


    fn label<'tcx>(&mut self, ty: ty::Ty<'tcx>) -> LTy<'tcx> {
        use rustc::ty::TypeVariants::*;
        match ty.sty {
            // Types with no arguments
            TyBool |
            TyChar |
            TyInt(_) |
            TyUint(_) |
            TyFloat(_) |
            TyStr |
            TyNever => self.mk(ty, vec![]),

            // Types that aren't actually supported by this analysis
            TyFnDef(..) |
            TyFnPtr(..) |
            TyDynamic(..) |
            TyClosure(..) |
            TyProjection(..) |
            TyAnon(..) |
            TyParam(..) |
            TyInfer(..) |
            TyError => self.mk(ty, vec![]),

            // Types with arguments
            TyAdt(_, substs) => {
                let args = substs.iter().filter_map(|s| s.as_type())
                    .map(|t| self.label(t)).collect();
                self.mk(ty, args)
            },
            TyArray(elem, _) => {
                let args = vec![self.label(elem)];
                self.mk(ty, args)
            },
            TySlice(elem) => {
                let args = vec![self.label(elem)];
                self.mk(ty, args)
            },
            TyRawPtr(mty) => {
                let args = vec![self.label(mty.ty)];
                self.mk(ty, args)
            },
            TyRef(_, mty) => {
                let args = vec![self.label(mty.ty)];
                self.mk(ty, args)
            },
            TyTuple(ref elems, _) => {
                let args = elems.iter().map(|ty| self.label(ty)).collect();
                self.mk(ty, args)
            },
        }
    }
}



/// Walk over typechecking tables, building a labeled type for each expr and pattern.
struct ExprPatVisitor<'t, 'a, 'gcx: 'tcx, 'tcx: 'a> {
    tcx: TyCtxt<'a, 'gcx, 'tcx>,
    ltt: &'t mut LTyTable,
    unadjusted: HashMap<NodeId, LTy<'tcx>>,
    adjusted: HashMap<NodeId, LTy<'tcx>>,
}

impl<'t, 'a, 'gcx, 'tcx> ExprPatVisitor<'t, 'a, 'gcx, 'tcx> {
    fn handle_body(&mut self, body_id: BodyId) {
        let tables = self.tcx.body_tables(body_id);
        for (&id, &ty) in tables.node_types.iter() {
            self.unadjusted.insert(id, self.ltt.label(ty));

            if let Some(adj) = tables.adjustments.get(&id).and_then(|v| v.last()) {
                self.ltt.label(adj.target);
            }
        }
    }
}

impl<'t, 'a, 'gcx, 'tcx, 'hir> ItemLikeVisitor<'hir> for ExprPatVisitor<'t, 'a, 'gcx, 'tcx> {
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

fn label_nodes<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>, ltt: &mut LTyTable, krate: &Crate)
                               -> (HashMap<NodeId, LTy<'tcx>>, HashMap<NodeId, LTy<'tcx>>) {
    let mut v = ExprPatVisitor {
        tcx: tcx,
        ltt: ltt,
        unadjusted: HashMap::new(),
        adjusted: HashMap::new(),
    };
    krate.visit_all_item_likes(&mut v);
    (v.unadjusted, v.adjusted)
}



// Unfortunately, there is no central index of all/most `DefId`s and their types, like there is for
// exprs and patterns.  And we can't visit all defs because some are pulled in from other crates.
// Instead, we have to keep this cache and add defs to it as we encounter them.
struct DefCache<'tcx> {
    map: HashMap<DefId, LTy<'tcx>>,
}

impl<'tcx> DefCache<'tcx> {
    fn new() -> DefCache<'tcx> {
        DefCache {
            map: HashMap::new(),
        }
    }

    fn get<'a, 'gcx>(&mut self,
                     tcx: TyCtxt<'a, 'gcx, 'tcx>,
                     ltt: &mut LTyTable,
                     def_id: DefId) -> &LTy<'tcx> {
        if !self.map.contains_key(&def_id) {
            let lty = ltt.label(tcx.type_of(def_id));
            self.map.insert(def_id, lty);
        }
        &self.map[&def_id]
    }
}



/// Construct `LTy`s for all `hir::Ty` nodes in the AST.
struct TyVisitor<'t, 'a, 'hir: 'a, 'gcx: 'tcx, 'tcx: 'a> {
    hir_map: &'a hir::map::Map<'hir>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,
    ltt: &'t mut LTyTable,
    ty_nodes: HashMap<NodeId, LTy<'tcx>>,
}

impl<'t, 'a, 'hir, 'gcx, 'tcx> TyVisitor<'t, 'a, 'hir, 'gcx, 'tcx> {
    fn handle_ty(&mut self, ty: &Ty, tcx_ty: ty::Ty<'tcx>) {
        let lty = self.ltt.label(tcx_ty);
        eprintln!("HANDLE: {:?} => {:?}", ty, tcx_ty);
        self.record_ty(ty, &lty);
    }

    fn record_ty(&mut self, ty: &Ty, lty: &LTy<'tcx>) {
        use rustc::ty::TypeVariants::*;
        match (&ty.node, &lty.ty.sty) {
            (&Ty_::TySlice(ref elem), &TySlice(..)) => 
                self.record_ty(elem, &lty.args[0]),
            (&Ty_::TyArray(ref elem, _), &TyArray(..)) => 
                self.record_ty(elem, &lty.args[0]),
            (&Ty_::TyPtr(ref mty), &TyRawPtr(..)) =>
                self.record_ty(&mty.ty, &lty.args[0]),
            (&Ty_::TyRptr(_, ref mty), &TyRef(..)) =>
                self.record_ty(&mty.ty, &lty.args[0]),
            (&Ty_::TyBareFn(ref fn_ty), &TyFnPtr(..)) => {}, // unsupported
            (&Ty_::TyNever, &TyNever) => {},
            (&Ty_::TyTup(ref elems), &TyTuple(..)) =>
                self.record_ty_list(elems, &lty.args),
            (&Ty_::TyPath(ref qpath), _) => {
                // TyPath could resolve to absolutely anything, since resolution includes expanding
                // type aliases.  So this case gets special handling.
                self.record_path_ty(qpath, lty);
            },
            (&Ty_::TyTraitObject(..), &TyDynamic(..)) => {}, // unsupported
            (&Ty_::TyImplTrait(..), &TyAnon(..)) => {}, // unsupported
            // No case for TyTypeof - it can't be written in source programs currently
            // TyInfer has no sub-`hir::Ty`s to work on.
            (&Ty_::TyInfer, _) => {},
            (&Ty_::TyErr, _) => {},
            (_, _) => {
                panic!("unsupported hir::Ty/ty::Ty combination:\n  hir: {:?}\n  ty: {:?}",
                       ty, lty.ty);
            },
        }
        self.ty_nodes.insert(ty.id, lty.clone());
    }

    fn record_ty_list(&mut self, tys: &[P<Ty>], ltys: &[LTy<'tcx>]) {
        assert!(tys.len() == ltys.len());
        for (ty, lty) in tys.iter().zip(ltys.iter()) {
            self.record_ty(ty, lty);
        }
    }

    fn record_path_params(&mut self, params: &PathParameters, ltys: &[LTy<'tcx>]) {
        match *params {
            PathParameters::AngleBracketedParameters(ref abpd) => {
                self.record_ty_list(&abpd.types, ltys);
            },
            PathParameters::ParenthesizedParameters(ref ppd) => {
                if let Some(ref output) = ppd.output {
                    self.record_ty_list(&ppd.inputs, &ltys[.. ltys.len() - 1]);
                    self.record_ty(output, ltys.last().unwrap());
                } else {
                    self.record_ty_list(&ppd.inputs, ltys);
                }
            },
        }
    }

    fn record_path_ty(&mut self, qpath: &QPath, lty: &LTy<'tcx>) {
        use rustc::hir::def::Def;
        use rustc::ty::TypeVariants::*;

        let path = match *qpath {
            QPath::Resolved(None, ref p) => p,
            _ => {
                // The path is relative to a type or trait.  That means it must be an associated
                // type of some trait.  We don't handle those currently.
                return;
            },
        };

        let last_seg = path.segments.last().unwrap();

        match (&path.def, &lty.ty.sty) {
            (&Def::Struct(_), &TyAdt(..)) |
            (&Def::Union(_), &TyAdt(..)) |
            (&Def::Enum(_), &TyAdt(..)) =>
                self.record_path_params(&last_seg.parameters, &lty.args),

            (&Def::Trait(_), &TyDynamic(..)) => {}, // unsupported

            (&Def::TyAlias(_), _) => {}, // unsupported

            (&Def::PrimTy(_), &TyBool) |
            (&Def::PrimTy(_), &TyChar) |
            (&Def::PrimTy(_), &TyInt(_)) |
            (&Def::PrimTy(_), &TyUint(_)) |
            (&Def::PrimTy(_), &TyFloat(_)) |
            (&Def::PrimTy(_), &TyStr) => {},

            (&Def::TyParam(_), &TyParam(_)) => {}, // unsupported

            (&Def::SelfTy(_, _), _) => {}, // unsupported

            (&Def::Err, _) => {}, // unsupported

            // These are in the type namespace (according to comments in `enum Def`), but it should
            // be impossible to encounter them here.
            (&Def::Mod(_), _) |
            (&Def::Variant(_), _) |
            (&Def::AssociatedTy(_), _) => {
                panic!("impossible: ty path refers to {:?}", path.def);
            },

            (&Def::Fn(_), _) |
            (&Def::Const(_), _) |
            (&Def::Static(_, _), _) |
            (&Def::StructCtor(_, _), _) |
            (&Def::VariantCtor(_, _), _) |
            (&Def::Method(_), _) |
            (&Def::AssociatedConst(_), _) |
            (&Def::Local(_), _) |
            (&Def::Upvar(_, _, _), _) |
            (&Def::Label(_), _) |
            (&Def::Macro(_, _), _) |
            (&Def::GlobalAsm(_), _) => {
                panic!("impossible: ty path refers to non-type {:?}", path.def);
            },

            (d, t) => {
                panic!("unexpected def/ty combination\n  def = {:?}\n  ty = {:?}", d, t);
            },
        }
    }

    fn get_tables(&self, id: NodeId) -> &'gcx TypeckTables<'gcx> {
        let parent = self.hir_map.get_parent(id);
        let parent_body = self.hir_map.body_owned_by(parent);
        self.tcx.body_tables(parent_body)
    }

    fn get_substs(&self, id: NodeId) -> Option<&'tcx Substs<'tcx>> {
        self.get_tables(id).node_substs.get(&id).map(|&x| x)
    }

    fn handle_node_ty(&mut self, ty: &Ty, id: NodeId) {
        let tables = self.get_tables(id);
        let tcx_ty = tables.node_id_to_type(id);
        self.handle_ty(ty, tcx_ty);
    }

    fn handle_body_tys(&mut self, arg_tys: &[P<Ty>], ret_ty: Option<&Ty>, body_id: BodyId) {
        let body = self.hir_map.body(body_id);
        let tables = self.tcx.body_tables(body_id);

        assert!(arg_tys.len() == body.arguments.len());
        for (ty, arg) in arg_tys.iter().zip(body.arguments.iter()) {
            self.handle_ty(ty, tables.node_id_to_type(arg.id));
        }
        if let Some(ret_ty) = ret_ty {
            self.handle_ty(ret_ty, tables.node_id_to_type(body.value.id));
        }
    }

    fn handle_def_ty(&mut self, ty: &Ty, id: NodeId) {
        let def_id = self.hir_map.local_def_id(id);
        let tcx_ty = self.tcx.type_of(def_id);
        self.handle_ty(ty, tcx_ty);
    }

    fn handle_path_params<I: Iterator<Item=ty::Ty<'tcx>>>(&mut self,
                                                          seg: &PathParameters,
                                                          iter: &mut I) {
        match *seg {
            PathParameters::AngleBracketedParameters(ref abpd) => {
                eprintln!("angle seg {:?}: inferred? {}", seg, abpd.infer_types);
                for ty in &abpd.types {
                    self.handle_ty(ty, iter.next().unwrap());
                }
            },
            PathParameters::ParenthesizedParameters(ref ppd) => {
                for input in &ppd.inputs {
                    self.handle_ty(input, iter.next().unwrap());
                }
                if let Some(ref output) = ppd.output {
                    self.handle_ty(output, iter.next().unwrap());
                }
            },
        }
    }
}

impl<'t, 'a, 'hir, 'gcx, 'tcx> Visitor<'hir> for TyVisitor<'t, 'a, 'hir, 'gcx, 'tcx> {
    fn nested_visit_map<'this>(&'this mut self) -> NestedVisitorMap<'this, 'hir> {
        NestedVisitorMap::OnlyBodies(self.hir_map)
    }

    // There are several places we can encounter `Ty` nodes, and each one has a different way of
    // obtaining the corresponding `LTy`.

    fn visit_expr(&mut self, e: &'hir Expr) {
        match e.node {
            ExprPath(ref qpath) => {
                if let Some(substs) = self.get_substs(e.id) {
                    // TODO: needs to handle cases with ABPD.infer_types == true.
                    // It's not clear how to get the number of elements of `substs` to consume in
                    // those cases.
                    /*
                    // This case gets a little hairy.  `hir::Ty`s can appear in several different
                    // places inside a `QPath`, but for typechecking they all get stored in a
                    // single linear `[ty::Ty]`.
                    eprintln!(" ** SUBSTS: {:?} (for {:?})", substs, qpath);
                    let mut substs = substs.iter().filter_map(|s| s.as_type());

                    match *qpath {
                        QPath::Resolved(ref self_ty, ref path) => {
                            if let Some(ref self_ty) = *self_ty {
                                self.handle_ty(self_ty, substs.next().unwrap());
                            }
                            for seg in &path.segments {
                                self.handle_path_params(&seg.parameters, &mut substs);
                            }
                        },
                        QPath::TypeRelative(ref base_ty, ref seg) => {
                            self.handle_ty(base_ty, substs.next().unwrap());
                            self.handle_path_params(&seg.parameters, &mut substs);
                        },
                    }

                    assert!(substs.next().is_none());
                    */
                }
            },
            //ExprMethodCall(_, ref seg) => {
            //},
            ExprCast(_, ref ty) => self.handle_node_ty(ty, e.id),
            ExprType(_, ref ty) => self.handle_node_ty(ty, e.id),
            _ => {},
        }
        intravisit::walk_expr(self, e);
    }

    fn visit_local(&mut self, l: &'hir Local) {
        if let Some(ref ty) = l.ty {
            self.handle_node_ty(ty, l.pat.id);
        }
        intravisit::walk_local(self, l);
    }

    fn visit_item(&mut self, i: &'hir Item) {
        match i.node {
            ItemStatic(ref ty, _, body_id) =>
                self.handle_body_tys(&[], Some(ty), body_id),
            ItemConst(ref ty, body_id) =>
                self.handle_body_tys(&[], Some(ty), body_id),
            ItemFn(ref decl, _, _, _, _, body_id) => {
                let ret_ty = match decl.output {
                    FunctionRetTy::DefaultReturn(_) => None,
                    FunctionRetTy::Return(ref ty) => Some(ty as &Ty),
                };
                self.handle_body_tys(&decl.inputs, ret_ty, body_id);
            },
            ItemTy(ref ty, _) =>
                self.handle_def_ty(ty, i.id),
            // ItemEnum, ItemStruct, and ItemUnion are all handled by `visit_struct_field`.
            ItemImpl(_, _, _, _, _, ref ty, _) =>
                self.handle_def_ty(ty, i.id),
            _ => {},
        }

        intravisit::walk_item(self, i);
    }

    fn visit_struct_field(&mut self, field: &'hir StructField) {
        self.handle_def_ty(&field.ty, field.id);
        intravisit::walk_struct_field(self, field);
    }

    // TODO: impl items
    // TODO: trait items
}

fn label_tys<'a, 'gcx, 'tcx>(hir_map: &hir::map::Map,
                             tcx: TyCtxt<'a, 'gcx, 'tcx>,
                             ltt: &mut LTyTable)
                             -> HashMap<NodeId, LTy<'tcx>> {
    let mut v = TyVisitor {
        hir_map: hir_map,
        tcx: tcx,
        ltt: ltt,
        ty_nodes: HashMap::new(),
    };
    hir_map.krate().visit_all_item_likes(&mut v.as_deep_visitor());
    v.ty_nodes
}




/*


struct Ctxt<'a, 'hir: 'a, 'gcx: 'tcx, 'tcx: 'a> {
    hir_map: &'a hir::map::Map<'hir>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,

    unif: UnificationTable<TyLabel>,

    /// Unadjusted types assigned to `Expr` and `Pat` nodes in the AST.  These are simply labeled
    /// versions of the computed types obtained from `TypeckTables`.
    unadjusted_nodes: HashMap<NodeId, LTy>,

    /// Final adjusted type of each `Expr` and `Pat` in the AST.  The adjusted type should be used
    /// when obtaining the type of a subexpression.
    nodes: HashMap<NodeId, LTy>,

    /// Types corresponding to `Ty` nodes in the AST.  Our main goal is to sort these into
    /// equivalence classes.  These are kept separate from `nodes` both because they are relevant
    /// to the output and also because `TypeckTables` don't contain computed types for `Ty` nodes.
    ty_nodes: HashMap<NodeId, LTy>,

    /// Primitive types used in built-in operations.  If a type is unified with `prims["bool"]`,
    /// then the type must be exactly `bool` for the program to typecheck - most likely, a value
    /// with that annotation is used as an `if` condition.
    prims: HashMap<&'static str, LTy>,
}

impl<'a, 'hir, 'gcx, 'tcx> Ctxt<'a, 'hir, 'gcx, 'tcx> {
    fn new(hir_map: &'a hir::map::Map<'hir>,
           tcx: TyCtxt<'a, 'gcx, 'tcx>) -> Ctxt<'a, 'hir, 'gcx, 'tcx> {
        Ctxt {
            hir_map: hir_map,
            tcx: tcx,

            unif: UnificationTable::new(),

            unadjusted_nodes: HashMap::new(),
            nodes: HashMap::new(),
            ty_nodes: HashMap::new(),
            prims: HashMap::new(),
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

*/

pub fn analyze(hir_map: &hir::map::Map, tcx: &TyCtxt) -> HashMap<NodeId, u32> {
    let tcx = *tcx;
    let mut ltt = LTyTable::new();
    let (unadjusted_nodes, nodes) = label_nodes(tcx, &mut ltt, hir_map.krate());
    eprintln!("got {} unadjusted, {} adjusted", unadjusted_nodes.len(), nodes.len());
    let mut def_cache = DefCache::new();
    let ty_nodes = label_tys(hir_map, tcx, &mut ltt);
    eprintln!("got {} tys", ty_nodes.len());
    eprintln!("got {} cached defs", def_cache.map.len());
    let mut keys = def_cache.map.keys().map(|&x| x).collect::<Vec<_>>();
    keys.sort();
    for did in keys {
        eprintln!("  {:?}: {:?}", did, def_cache.map[&did]); 
    }

    HashMap::new()
}
