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

use arena::TypedArena;
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
use syntax::ast;
use syntax::ast::NodeId;
use syntax::codemap::Span;
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


#[derive(Clone, PartialEq, Eq)]
struct LTyS<'lcx, 'tcx: 'lcx> {
    ty: ty::Ty<'tcx>,
    label: Cell<TyLabel>,
    args: &'lcx [LTy<'lcx, 'tcx>],
}

type LTy<'lcx, 'tcx> = &'lcx LTyS<'lcx, 'tcx>;

impl<'lcx, 'tcx> LTyS<'lcx, 'tcx> {
    fn arg(&self, idx: usize) -> LTy<'lcx, 'tcx> {
        self.args[idx]
    }

    fn canonicalize(&self, ltt: &LTyTable<'lcx, 'tcx>) -> &Self {
        let label = self.label.get();
        let new_label = ltt.unif.borrow_mut().find(label);
        self.label.set(new_label);

        for arg in self.args.iter() {
            arg.canonicalize(ltt);
        }

        self
    }
}

impl<'lcx, 'tcx> fmt::Debug for LTyS<'lcx, 'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}#{:?}{:?}", self.label.get().index(), self.ty, self.args)
    }
}

#[derive(Clone, Copy, Debug)]
struct LFnSig<'lcx, 'tcx: 'lcx> {
    inputs: &'lcx [LTy<'lcx, 'tcx>],
    output: LTy<'lcx, 'tcx>,
    variadic: bool,
}


struct LTyTable<'lcx, 'tcx: 'lcx> {
    unif: RefCell<UnificationTable<TyLabel>>,
    arena: &'lcx TypedArena<LTyS<'lcx, 'tcx>>,
    ref_arena: &'lcx TypedArena<LTy<'lcx, 'tcx>>,
}

impl<'lcx, 'tcx> LTyTable<'lcx, 'tcx> {
    fn new(arena: &'lcx TypedArena<LTyS<'lcx, 'tcx>>,
           ref_arena: &'lcx TypedArena<LTy<'lcx, 'tcx>>) -> LTyTable<'lcx, 'tcx> {
        LTyTable {
            unif: RefCell::new(UnificationTable::new()),
            arena: arena,
            ref_arena: ref_arena,
        }
    }

    fn mk_slice(&self, ltys: &[LTy<'lcx, 'tcx>]) -> &'lcx [LTy<'lcx, 'tcx>] {
        if ltys.len() == 0 {
            return &[];
        }
        self.ref_arena.alloc_slice(ltys)
    }

    fn mk(&self, ty: ty::Ty<'tcx>, args: &[LTy<'lcx, 'tcx>]) -> LTy<'lcx, 'tcx> {
        let label = self.unif.borrow_mut().new_key(());
        self.arena.alloc(LTyS {
            ty: ty,
            label: Cell::new(label),
            args: self.mk_slice(args),
        })
    }


    fn label(&self, ty: ty::Ty<'tcx>) -> LTy<'lcx, 'tcx> {
        use rustc::ty::TypeVariants::*;
        match ty.sty {
            // Types with no arguments
            TyBool |
            TyChar |
            TyInt(_) |
            TyUint(_) |
            TyFloat(_) |
            TyStr |
            TyNever => self.mk(ty, &[]),

            // Types that aren't actually supported by this analysis
            TyDynamic(..) |
            TyClosure(..) |
            TyProjection(..) |
            TyAnon(..) |
            TyParam(..) |
            TyInfer(..) |
            TyError => self.mk(ty, &[]),

            // Types with arguments
            TyAdt(_, substs) => {
                let args = substs.iter().filter_map(|s| s.as_type())
                    .map(|t| self.label(t)).collect::<Vec<_>>();
                self.mk(ty, &args)
            },
            TyArray(elem, _) => {
                self.mk(ty, &[self.label(elem)])
            },
            TySlice(elem) => {
                self.mk(ty, &[self.label(elem)])
            },
            TyRawPtr(mty) => {
                self.mk(ty, &[self.label(mty.ty)])
            },
            TyRef(_, mty) => {
                self.mk(ty, &[self.label(mty.ty)])
            },
            TyFnDef(_, substs) => {
                let args = substs.types().map(|ty| self.label(ty)).collect::<Vec<_>>();
                self.mk(ty, &args)
            },
            TyFnPtr(ref sig) => {
                let args = sig.0.inputs_and_output.iter()
                    .map(|ty| self.label(ty)).collect::<Vec<_>>();
                self.mk(ty, &args)
            },
            TyTuple(ref elems, _) => {
                let args = elems.iter().map(|ty| self.label(ty)).collect::<Vec<_>>();
                self.mk(ty, &args)
            },
        }
    }

    fn label_slice(&self, tys: &[ty::Ty<'tcx>]) -> &'lcx [LTy<'lcx, 'tcx>] {
        self.mk_slice(&tys.iter().map(|ty| self.label(ty)).collect::<Vec<_>>())
    }

    fn label_sig(&self, sig: ty::FnSig<'tcx>) -> LFnSig<'lcx, 'tcx> {
        LFnSig {
            inputs: self.label_slice(sig.inputs()),
            output: self.label(sig.output()),
            variadic: sig.variadic,
        }
    }


    fn subst(&self, lty: LTy<'lcx, 'tcx>, substs: &[LTy<'lcx, 'tcx>]) -> LTy<'lcx, 'tcx> {
        match lty.ty.sty {
            ty::TypeVariants::TyParam(ref tp) => {
                substs[tp.idx as usize]
            },
            _ => {
                self.arena.alloc(LTyS {
                    ty: lty.ty,
                    label: lty.label.clone(),
                    args: self.subst_slice(lty.args, substs),
                })
            },
        }
    }

    fn subst_slice(&self, ltys: &[LTy<'lcx, 'tcx>], substs: &[LTy<'lcx, 'tcx>]) -> &'lcx [LTy<'lcx, 'tcx>] {
        self.mk_slice(&ltys.iter().map(|lty| self.subst(lty, substs)).collect::<Vec<_>>())
    }

    fn subst_sig(&self, sig: LFnSig<'lcx, 'tcx>, substs: &[LTy<'lcx, 'tcx>]) -> LFnSig<'lcx, 'tcx> {
        LFnSig {
            inputs: self.subst_slice(sig.inputs, substs),
            output: self.subst(sig.output, substs),
            variadic: sig.variadic,
        }
    }


    fn unify(&self, lty1: LTy<'lcx, 'tcx>, lty2: LTy<'lcx, 'tcx>) {
        self.unif.borrow_mut().union(lty1.label.get(), lty2.label.get());

        if lty1.args.len() == lty2.args.len() {
            self.unify_slices(lty1.args, lty2.args);
        }
    }

    fn unify_slices(&self, ltys1: &[LTy<'lcx, 'tcx>], ltys2: &[LTy<'lcx, 'tcx>]) {
        for (lty1, lty2) in ltys1.iter().zip(ltys2.iter()) {
            self.unify(lty1, lty2);
        }
    }
}



/// Walk over typechecking tables, building a labeled type for each expr and pattern.
struct ExprPatVisitor<'a, 'lcx: 'a, 'gcx: 'tcx, 'tcx: 'lcx> {
    tcx: TyCtxt<'a, 'gcx, 'tcx>,
    ltt: &'a LTyTable<'lcx, 'tcx>,

    unadjusted: HashMap<NodeId, LTy<'lcx, 'tcx>>,
    adjusted: HashMap<NodeId, LTy<'lcx, 'tcx>>,
    substs: HashMap<NodeId, &'lcx [LTy<'lcx, 'tcx>]>,
}

impl<'a, 'lcx, 'gcx, 'tcx> ExprPatVisitor<'a, 'lcx, 'gcx, 'tcx> {
    fn handle_body(&mut self, body_id: BodyId) {
        let tables = self.tcx.body_tables(body_id);
        for (&id, &ty) in tables.node_types.iter() {
            self.unadjusted.insert(id, self.ltt.label(ty));

            if let Some(adj) = tables.adjustments.get(&id).and_then(|v| v.last()) {
                self.adjusted.insert(id, self.ltt.label(adj.target));
            }
        }

        for (&id, &substs) in tables.node_substs.iter() {
            let labeled = self.ltt.label_slice(&substs.types().collect::<Vec<_>>());
            self.substs.insert(id, labeled);
        }
    }
}

impl<'a, 'lcx, 'gcx, 'tcx, 'hir> ItemLikeVisitor<'hir> for ExprPatVisitor<'a, 'lcx, 'gcx, 'tcx> {
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




/// Construct `LTy`s for all `hir::Ty` nodes in the AST.
struct TyVisitor<'a, 'lcx: 'a, 'hir: 'a, 'gcx: 'tcx, 'tcx: 'lcx> {
    hir_map: &'a hir::map::Map<'hir>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,
    ltt: &'a LTyTable<'lcx, 'tcx>,

    ty_nodes: HashMap<NodeId, LTy<'lcx, 'tcx>>,
}

impl<'a, 'lcx, 'hir, 'gcx, 'tcx> TyVisitor<'a, 'lcx, 'hir, 'gcx, 'tcx> {
    fn handle_ty(&mut self, ty: &Ty, tcx_ty: ty::Ty<'tcx>) {
        let lty = self.ltt.label(tcx_ty);
        self.record_ty(ty, &lty);
    }

    fn record_ty(&mut self, ty: &Ty, lty: &LTy<'lcx, 'tcx>) {
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
            (&Ty_::TyBareFn(ref fn_ty), &TyFnPtr(..)) => {
                self.record_ty_list(&fn_ty.decl.inputs, &lty.args[.. lty.args.len() - 1]);
                match fn_ty.decl.output {
                    FunctionRetTy::DefaultReturn(_) => {},
                    FunctionRetTy::Return(ref ty) =>
                        self.record_ty(ty, &lty.args[lty.args.len() - 1]),
                }
            },
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

    fn record_ty_list(&mut self, tys: &[P<Ty>], ltys: &[LTy<'lcx, 'tcx>]) {
        assert!(tys.len() == ltys.len());
        for (ty, lty) in tys.iter().zip(ltys.iter()) {
            self.record_ty(ty, lty);
        }
    }

    fn record_path_params(&mut self, params: &PathParameters, ltys: &[LTy<'lcx, 'tcx>]) {
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

    fn record_path_ty(&mut self, qpath: &QPath, lty: &LTy<'lcx, 'tcx>) {
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

            // These are not in the type namespace
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

            // We got a `Def` in the type namespace together with an unrecognized `ty::Ty` variant.
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
            self.handle_ty(ret_ty, tables.expr_ty_adjusted(&body.value));
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

impl<'a, 'lcx, 'hir, 'gcx, 'tcx> Visitor<'hir> for TyVisitor<'a, 'lcx, 'hir, 'gcx, 'tcx> {
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

            // We don't handle closure types well, but we still need to label the types of their
            // args and returns so the UnifyVisitor doesn't crash.
            ExprClosure(_, ref decl, body_id, _) => {
                let ret_ty = match decl.output {
                    FunctionRetTy::DefaultReturn(_) => None,
                    FunctionRetTy::Return(ref ty) => Some(ty as &Ty),
                };
                self.handle_body_tys(&decl.inputs, ret_ty, body_id);
            },

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

    fn visit_impl_item(&mut self, i: &'hir ImplItem) {
        match i.node {
            ImplItemKind::Const(ref ty, body_id) =>
                self.handle_body_tys(&[], Some(ty), body_id),
            ImplItemKind::Method(ref sig, body_id) => {
                let ret_ty = match sig.decl.output {
                    FunctionRetTy::DefaultReturn(_) => None,
                    FunctionRetTy::Return(ref ty) => Some(ty as &Ty),
                };
                self.handle_body_tys(&sig.decl.inputs, ret_ty, body_id);
            },
            ImplItemKind::Type(ref ty) =>
                self.handle_def_ty(ty, i.id),
        }

        intravisit::walk_impl_item(self, i);
    }

    // TODO: trait items
    // TODO: foreign items
}

fn label_tys<'a, 'lcx, 'gcx, 'tcx>(hir_map: &hir::map::Map,
                                   tcx: TyCtxt<'a, 'gcx, 'tcx>,
                                   ltt: &'a LTyTable<'lcx, 'tcx>)
                                   -> HashMap<NodeId, LTy<'lcx, 'tcx>> {
    let mut v = TyVisitor {
        hir_map: hir_map,
        tcx: tcx,
        ltt: ltt,

        ty_nodes: HashMap::new(),
    };
    hir_map.krate().visit_all_item_likes(&mut v.as_deep_visitor());
    v.ty_nodes
}



fn prim_tys<'a, 'lcx, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                                  ltt: &'a LTyTable<'lcx, 'tcx>)
                                  -> HashMap<&'static str, LTy<'lcx, 'tcx>> {
    let mut map = HashMap::new();

    map.insert("bool", ltt.label(tcx.mk_bool()));
    map.insert("()", ltt.label(tcx.mk_nil()));
    map.insert("usize", ltt.label(tcx.mk_mach_uint(ast::UintTy::Us)));

    map
}



struct UnifyVisitor<'a, 'lcx: 'a, 'hir: 'a, 'gcx: 'tcx, 'tcx: 'lcx> {
    hir_map: &'a hir::map::Map<'hir>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,
    ltt: &'a LTyTable<'lcx, 'tcx>,

    unadjusted_nodes: &'a HashMap<NodeId, LTy<'lcx, 'tcx>>,
    nodes: &'a HashMap<NodeId, LTy<'lcx, 'tcx>>,
    node_substs: &'a HashMap<NodeId, &'lcx [LTy<'lcx, 'tcx>]>,
    ty_nodes: &'a HashMap<NodeId, LTy<'lcx, 'tcx>>,
    prims: &'a HashMap<&'static str, LTy<'lcx, 'tcx>>,


    // Unfortunately, there is no central index of all/most `DefId`s and their types, like there is
    // for exprs and patterns.  And we can't visit all defs because some are pulled in from other
    // crates.  Since we can't precompute the `LTy` for every def, we have to keep this cache and
    // add defs to it as we encounter them.
    defs: RefCell<HashMap<DefId, LTy<'lcx, 'tcx>>>,

    /// Cache of signatures of function/method definitions.  This always contains the raw,
    /// un-substituted signature.
    def_sigs: RefCell<HashMap<DefId, LFnSig<'lcx, 'tcx>>>,
}

impl<'a, 'lcx, 'hir, 'gcx, 'tcx> UnifyVisitor<'a, 'lcx, 'hir, 'gcx, 'tcx> {
    fn expr_lty(&self, e: &Expr) -> LTy<'lcx, 'tcx> {
        self.nodes.get(&e.id)
            .or_else(|| self.unadjusted_nodes.get(&e.id))
            .unwrap_or_else(|| panic!("expr_lty: no lty for {:?} @ {:?}",
                                      e, self.tcx.sess.codemap().span_to_string(e.span)))
    }

    fn unadjusted_expr_lty(&self, e: &Expr) -> LTy<'lcx, 'tcx> {
        self.unadjusted_nodes.get(&e.id)
            .unwrap_or_else(|| panic!("unadjusted_expr_lty: no unadjusted lty for {:?} @ {:?}",
                                      e, self.tcx.sess.codemap().span_to_string(e.span)))
    }

    fn opt_unadjusted_expr_lty(&self, e: &Expr) -> Option<LTy<'lcx, 'tcx>> {
        self.unadjusted_nodes.get(&e.id).map(|&x| x)
    }

    fn block_lty(&self, b: &Block) -> LTy<'lcx, 'tcx> {
        match b.expr {
            Some(ref e) => self.expr_lty(e),
            None => self.prim_lty("()"),
        }
    }

    fn pat_lty(&self, p: &Pat) -> LTy<'lcx, 'tcx> {
        self.unadjusted_nodes.get(&p.id)
            .unwrap_or_else(|| panic!("pat_lty: no lty for {:?} @ {:?}",
                                      p, self.tcx.sess.codemap().span_to_string(p.span)))
    }

    fn ty_lty(&self, t: &Ty) -> LTy<'lcx, 'tcx> {
        self.ty_nodes.get(&t.id)
            .unwrap_or_else(|| panic!("ty_lty: no lty for {:?} @ {:?}",
                                      t, self.tcx.sess.codemap().span_to_string(t.span)))
    }

    fn prim_lty(&self, name: &'static str) -> LTy<'lcx, 'tcx> {
        self.prims.get(&name)
            .unwrap_or_else(|| panic!("prim_lty: no such prim {:?}", name))
    }


    fn compute_def_lty(&self, id: DefId) -> LTy<'lcx, 'tcx> {
        match self.hir_map.get_if_local(id) {
            Some(NodeLocal(p)) => {
                return self.pat_lty(p);
            },
            _ => {},
        }

        self.ltt.label(self.tcx.type_of(id))
    }

    fn def_lty(&self, id: DefId) -> LTy<'lcx, 'tcx> {
        *self.defs.borrow_mut().entry(id)
            .or_insert_with(|| self.compute_def_lty(id))
    }


    fn compute_def_sig(&self, id: DefId) -> LFnSig<'lcx, 'tcx> {
        let sig = self.tcx.fn_sig(id);
        self.ltt.label_sig(sig.0)
    }

    fn def_sig(&self, id: DefId) -> LFnSig<'lcx, 'tcx> {
        *self.def_sigs.borrow_mut().entry(id)
            .or_insert_with(|| self.compute_def_sig(id))
    }

    fn fn_num_inputs(&self, lty: LTy<'lcx, 'tcx>) -> usize {
        use rustc::ty::TypeVariants::*;
        match lty.ty.sty {
            TyFnDef(id, _) => self.def_sig(id).inputs.len(),
            TyFnPtr(_) => lty.args.len() - 1,
            _ => panic!("fn_num_inputs: not a fn type"),
        }
    }

    /// Get the input types out of a `FnPtr` or `FnDef` `LTy`.
    fn fn_input(&self, lty: LTy<'lcx, 'tcx>, idx: usize) -> LTy<'lcx, 'tcx> {
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
    fn fn_output(&self, lty: LTy<'lcx, 'tcx>) -> LTy<'lcx, 'tcx> {
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

    fn fn_is_variadic(&self, lty: LTy<'lcx, 'tcx>) -> bool {
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

    fn method_sig(&self, e: &Expr) -> LFnSig<'lcx, 'tcx> {
        let def_id = self.get_tables(e.id).type_dependent_defs[&e.id].def_id();
        let sig = self.def_sig(def_id);
        let substs = self.node_substs.get(&e.id).map_or_else(|| &[] as &[_], |x| x);
        self.ltt.subst_sig(sig, substs)
    }


    fn field_lty(&self, struct_ty: LTy<'lcx, 'tcx>, name: Symbol) -> LTy<'lcx, 'tcx> {
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

impl<'a, 'lcx, 'hir, 'gcx, 'tcx> Visitor<'hir> for UnifyVisitor<'a, 'lcx, 'hir, 'gcx, 'tcx> {
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
                self.ltt.unify(rty.arg(0), self.expr_lty(e));
            },

            ExprArray(ref es) => {
                for e in es {
                    self.ltt.unify(rty.arg(0), self.expr_lty(e));
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

            ExprBinary(..) => {}, // TODO

            ExprUnary(op, ref a) => {
                match op {
                    UnDeref => self.ltt.unify(rty, self.expr_lty(a).arg(0)),
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
                self.ltt.unify(rty.arg(0), self.expr_lty(e));
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
                self.ltt.unify(rty.arg(0), self.expr_lty(e));
            },
        }

        if let Some(adjs) = self.get_tables(e.id).adjustments.get(&e.id) {
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
                        self.ltt.unify(rty, prev_ty.arg(0));
                    },
                    Adjust::Deref(Some(_)) => {}, // TODO (overloaded deref case)
                    Adjust::Borrow(_) => {
                        // The AutoBorrow argument indicates whether we're going to a `&` or `*`
                        // pointer, and whether it's `mut` or `const`.  In all cases, the shape of
                        // rty is the same.
                        self.ltt.unify(rty.arg(0), prev_ty);
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
                self.ltt.unify(rty.arg(0), self.pat_lty(p));
            },

            PatKind::Ref(ref p, _) => {
                self.ltt.unify(rty.arg(0), self.pat_lty(p));
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
}



pub fn analyze(hir_map: &hir::map::Map, tcx: TyCtxt) -> HashMap<NodeId, u32> {
    // Allocate in a single `let` so that `arena` and `sig_arena` have identical lifetimes.
    let (arena, sig_arena) = (TypedArena::new(), TypedArena::new());
    let ltt = LTyTable::new(&arena, &sig_arena);

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
    let ty_nodes = label_tys(hir_map, tcx, &ltt);

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
        .map(|(&id, &lty)| (id, lty.canonicalize(&ltt).label.get().index()))
        .collect()
}
