//! Helper definitions for constructing a mapping between `ast::Ty`s and some higher-level
//! representation (usually `ty::Ty` or `LabeledTy`).

use std::fmt::Debug;

use rustc::hir;
use rustc::hir::def_id::DefId;
use rustc::ty;
use syntax::ast::*;
use syntax::visit::{self, Visitor};

use crate::context::HirMap;

/// Provider of a higher-level type representation.
///
/// Methods can return `None` under any circumstances to indicate that the provider can't find a
/// type for the node.  In that case, `map_types` will simply skip visiting the corresponding node.
#[allow(unused)]
pub trait TypeSource {
    type Type: Type;
    type Signature: Signature<Self::Type>;

    fn expr_type(&mut self, e: &Expr) -> Option<Self::Type> {
        None
    }
    fn pat_type(&mut self, p: &Pat) -> Option<Self::Type> {
        None
    }
    fn def_type(&mut self, did: DefId) -> Option<Self::Type> {
        None
    }
    fn fn_sig(&mut self, did: DefId) -> Option<Self::Signature> {
        None
    }
    fn closure_sig(&mut self, did: DefId) -> Option<Self::Signature> {
        self.fn_sig(did)
    }
}

pub trait Signature<T>: Debug {
    fn num_inputs(&self) -> usize;
    fn input(&self, idx: usize) -> T;
    fn output(&self) -> T;
}

pub trait Type: Copy + Debug {
    fn sty(&self) -> &ty::TyKind;
    fn num_args(&self) -> usize;
    fn arg(&self, idx: usize) -> Self;
}

pub struct TypeMapVisitor<'a, 'tcx: 'a, S, F> {
    hir_map: &'a hir::map::Map<'tcx>,
    source: S,
    callback: F,
}

impl<'a, 'tcx, S, F> TypeMapVisitor<'a, 'tcx, S, F>
where
    S: TypeSource,
    F: FnMut(&mut S, &Ty, S::Type),
{
    /// Record a matching `S::Type` and `ast::Ty`.  If the two representations have matching
    /// shapes, this method recurses into their corresponding subtrees and records those as well.
    /// (The structures may not match if the `ast::Ty` refers to a type alias which has been
    /// expanded, for example - then `ast_ty` looks like `Alias` while `ty` is `Foo<Bar, Baz>`.)
    fn record_ty(&mut self, ty: S::Type, ast_ty: &Ty) {
        use rustc::ty::TyKind::*;

        (self.callback)(&mut self.source, ast_ty, ty);

        match (&ast_ty.kind, ty.sty()) {
            (&TyKind::Slice(ref elem), &Slice(..)) => self.record_ty(ty.arg(0), elem),
            (&TyKind::Array(ref elem, _), &Array(..)) => self.record_ty(ty.arg(0), elem),
            (&TyKind::Ptr(ref mty), &RawPtr(..)) => self.record_ty(ty.arg(0), &mty.ty),
            (&TyKind::Rptr(_, ref mty), &Ref(..)) => self.record_ty(ty.arg(0), &mty.ty),
            (&TyKind::Ptr(ref mty), &Ref(..)) => self.record_ty(ty.arg(0), &mty.ty),
            (&TyKind::BareFn(ref fn_ty), &FnPtr(..)) => {
                assert!(ty.num_args() == fn_ty.decl.inputs.len() + 1);
                for (i, arg) in fn_ty.decl.inputs.iter().enumerate() {
                    self.record_ty(ty.arg(i), &arg.ty);
                }
                self.record_function_ret_ty(ty.arg(fn_ty.decl.inputs.len()), &fn_ty.decl.output);
            }
            (&TyKind::Never, &Never) => {}
            (&TyKind::Tup(ref elems), &Tuple(..)) => {
                for (i, ast_ty) in elems.iter().enumerate() {
                    self.record_ty(ty.arg(i), ast_ty);
                }
            }
            (&TyKind::Path(ref qself, ref path), _) => {
                // TyKind::Path could resolve to absolutely anything, since resolution includes
                // expanding type aliases.  So this case gets special handling.
                self.record_path_ty(ty, qself.as_ref(), path);
            }
            (&TyKind::TraitObject(..), &Dynamic(..)) => {} // TODO
            // `Paren` should never appear, but just in case...
            (&TyKind::Paren(ref ast_ty), _) => self.record_ty(ty, ast_ty),
            // No case for TyTypeof - it can't be written in source programs currently

            // These cases have no internal structure to recurse on.
            (&TyKind::Infer, _) => {}
            (&TyKind::ImplicitSelf, _) => {}
            (&TyKind::Mac(_), _) => {}
            (TyKind::CVarArgs, Adt(..)) => {}

            (_, _) => {
                panic!(
                    "unsupported AST/resolved type combination:\
                     \n  ast: {:?}\
                     \n  resolved: {:?}",
                    ast_ty, ty
                );
            }
        }
    }

    fn record_function_ret_ty(&mut self, ty: S::Type, output: &FunctionRetTy) {
        match *output {
            FunctionRetTy::Default(_) => {}
            FunctionRetTy::Ty(ref ast_ty) => self.record_ty(ty, ast_ty),
        }
    }

    fn record_fn_decl(&mut self, sig: S::Signature, decl: &FnDecl) {
        let is_variadic = decl.c_variadic();

        if is_variadic {
            assert_eq!(sig.num_inputs(), decl.inputs.len() - 1);
        } else {
            assert_eq!(sig.num_inputs(), decl.inputs.len());
        }

        for (i, arg) in decl.inputs.iter().enumerate() {
            if is_variadic && i == decl.inputs.len() - 1 {
                continue;
            }

            self.record_ty(sig.input(i), &arg.ty);
        }
        self.record_function_ret_ty(sig.output(), &decl.output);
    }

    fn record_path_ty(&mut self, _ty: S::Type, _qself: Option<&QSelf>, _path: &Path) {
        // TODO: See comments below for reasons why `Path`-related cases are difficult to handle.
    }
}

impl<'ast, 'a, 'tcx, S, F> Visitor<'ast> for TypeMapVisitor<'a, 'tcx, S, F>
where
    S: TypeSource,
    F: FnMut(&mut S, &Ty, S::Type),
{
    // There are several places we can encounter `Ty` nodes, and each one has a different way of
    // obtaining the corresponding `LTy`.

    fn visit_expr(&mut self, e: &'ast Expr) {
        match e.kind {
            ExprKind::Cast(_, ref ast_ty) => {
                if let Some(ty) = self.source.expr_type(e) {
                    self.record_ty(ty, ast_ty);
                }
            }

            ExprKind::Type(_, ref ast_ty) => {
                if let Some(ty) = self.source.expr_type(e) {
                    self.record_ty(ty, ast_ty);
                }
            }

            ExprKind::Closure(_, _, _, ref decl, _, _) => {
                let def_id = self.hir_map.local_def_id_from_node_id(e.id);
                if let Some(sig) = self.source.closure_sig(def_id) {
                    self.record_fn_decl(sig, decl);
                }
            }

            ExprKind::Path(ref _qself, ref _path) => {
                // TODO: Handle `ast::Ty`s appearing inside path segments' `parameters` field.
                // In cases where `parameters` is `Some`, the expr type should be `Adt`,
                // `FnDef`, or some other type with `substs`.  The `parameters` correspond to the
                // `substs`, though the specific relationship is non-obvious.  The easy case is a
                // path expr `T::<Foo>::f::<Bar>` with substs `[Foo, Bar]`.  The harder cases are
                // those where some of the types are omitted - `T::<Foo>::f`, `T::f::<Bar>`, and
                // `T::f` may all have substs `[Foo, Bar]` (based on inference results), and it's
                // not obvious how many of the `substs` correspond to each position in the path.
            }

            ExprKind::Struct(ref _path, _, _) => {
                // TODO: Another case like `ExprKind::Path` - the path in the `Struct` can have
                // type parameters given explicitly.
            }

            _ => {}
        }

        visit::walk_expr(self, e);
    }

    fn visit_local(&mut self, l: &'ast Local) {
        if let Some(ref ast_ty) = l.ty {
            if let Some(ty) = self.source.pat_type(&l.pat) {
                self.record_ty(ty, ast_ty);
            }
            // TODO: If pat_type returns None, we may be able to recover by recursing on the Pat
            // and the Ty (for example, when both are tuples, as in `let (x, y): (T, U)`).  This
            // would help with use from MIR, where we can easily obtain types for individual
            // locals, but not for pats.
        }

        visit::walk_local(self, l);
    }

    fn visit_item(&mut self, i: &'ast Item) {
        let def_id = self.hir_map.local_def_id_from_node_id(i.id);
        match i.kind {
            ItemKind::Static(ref ast_ty, _, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            }

            ItemKind::Const(ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            }

            ItemKind::Fn(ref decl, _, _, _) => {
                if let Some(sig) = self.source.fn_sig(def_id) {
                    self.record_fn_decl(sig, decl);
                }
            }

            ItemKind::TyAlias(ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            }

            // Enum/Struct/Union are handled by `visit_struct_field`.
            ItemKind::Impl(_, _, _, _, _, ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            }

            _ => {}
        }

        visit::walk_item(self, i);
    }

    fn visit_struct_field(&mut self, f: &'ast StructField) {
        let def_id = self.hir_map.local_def_id_from_node_id(f.id);
        if let Some(ty) = self.source.def_type(def_id) {
            self.record_ty(ty, &f.ty);
        }

        visit::walk_struct_field(self, f);
    }

    fn visit_impl_item(&mut self, i: &'ast ImplItem) {
        let def_id = self.hir_map.local_def_id_from_node_id(i.id);
        match i.kind {
            ImplItemKind::Const(ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            }

            ImplItemKind::Method(ref method_sig, _) => {
                if let Some(sig) = self.source.fn_sig(def_id) {
                    self.record_fn_decl(sig, &method_sig.decl);
                }
            }

            ImplItemKind::TyAlias(ref ast_ty) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            }

            _ => {}
        }

        visit::walk_impl_item(self, i);
    }

    fn visit_trait_item(&mut self, i: &'ast TraitItem) {
        let def_id = self.hir_map.local_def_id_from_node_id(i.id);
        match i.kind {
            TraitItemKind::Const(ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            }

            TraitItemKind::Method(ref method_sig, _) => {
                if let Some(sig) = self.source.fn_sig(def_id) {
                    self.record_fn_decl(sig, &method_sig.decl);
                }
            }

            TraitItemKind::Type(_, ref opt_ast_ty) => {
                if let Some(ref ast_ty) = *opt_ast_ty {
                    if let Some(ty) = self.source.def_type(def_id) {
                        self.record_ty(ty, ast_ty);
                    }
                }
            }

            _ => {}
        }

        visit::walk_trait_item(self, i);
    }

    fn visit_foreign_item(&mut self, i: &'ast ForeignItem) {
        let def_id = self.hir_map.local_def_id_from_node_id(i.id);
        match i.kind {
            ForeignItemKind::Fn(ref decl, _) => {
                if let Some(sig) = self.source.fn_sig(def_id) {
                    self.record_fn_decl(sig, &decl);
                }
            }

            ForeignItemKind::Static(ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            }
            ForeignItemKind::Ty => {}
            ForeignItemKind::Macro(..) => {}
        }

        visit::walk_foreign_item(self, i);
    }
}

/// Try to match up `ast::Ty` nodes in the source with higher-level type representations provided
/// by `source`.  The callback will be passed matching pairs of AST-level and higher-level type
/// representations.
pub fn map_types<'a, 'tcx, S, F>(
    hir_map: &HirMap<'a, 'tcx>,
    source: S,
    krate: &Crate,
    callback: F,
) where
    S: TypeSource,
    F: FnMut(&mut S, &Ty, S::Type),
{
    let mut v = TypeMapVisitor {
        hir_map,
        source,
        callback,
    };
    visit::walk_crate(&mut v, krate);
}
