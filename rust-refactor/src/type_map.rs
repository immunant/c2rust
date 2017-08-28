/// Helper definitions for constructing a mapping between `ast::Ty`s and some higher-level
/// representation (usually `ty::Ty` or `LabeledTy`).

use std::fmt::Debug;

use rustc::hir;
use rustc::hir::def_id::DefId;
use rustc::ty;
use syntax::ast::*;
use syntax::visit::{self, Visitor, FnKind};


pub trait TypeSource {
    type Type: Type;
    type Signature: Signature<Self::Type>;

    fn expr_type(&mut self, e: &Expr) -> Option<Self::Type> { None }
    fn pat_type(&mut self, p: &Pat) -> Option<Self::Type> { None }
    fn def_type(&mut self, did: DefId) -> Option<Self::Type> { None }
    fn fn_sig(&mut self, did: DefId) -> Option<Self::Signature> { None }
    fn closure_sig(&mut self, did: DefId) -> Option<Self::Signature> { self.fn_sig(did) }
}

pub trait Signature<T>: Debug {
    fn num_inputs(&self) -> usize;
    fn input(&self, idx: usize) -> T;
    fn output(&self) -> T;
}

pub trait Type: Copy + Debug {
    fn sty(&self) -> &ty::TypeVariants;
    fn num_args(&self) -> usize;
    fn arg(&self, idx: usize) -> Self;
}


pub struct TypeMapVisitor<'a, 'hir: 'a, S, F> {
    hir_map: &'a hir::map::Map<'hir>,
    source: S,
    callback: F,
}

impl<'a, 'hir, S, F> TypeMapVisitor<'a, 'hir, S, F>
        where S: TypeSource,
              F: FnMut(&mut S, &Ty, S::Type) {
    fn record_ty(&mut self, ty: S::Type, ast_ty: &Ty) {
        use rustc::ty::TypeVariants::*;

        (self.callback)(&mut self.source, ast_ty, ty);

        match (&ast_ty.node, ty.sty()) {
            (&TyKind::Slice(ref elem), &TySlice(..)) => 
                self.record_ty(ty.arg(0), elem),
            (&TyKind::Array(ref elem, _), &TyArray(..)) => 
                self.record_ty(ty.arg(0), elem),
            (&TyKind::Ptr(ref mty), &TyRawPtr(..)) =>
                self.record_ty(ty.arg(0), &mty.ty),
            (&TyKind::Rptr(_, ref mty), &TyRef(..)) =>
                self.record_ty(ty.arg(0), &mty.ty),
            (&TyKind::BareFn(ref fn_ty), &TyFnPtr(..)) => {
                assert!(ty.num_args() == fn_ty.decl.inputs.len() + 1);
                for (i, arg) in fn_ty.decl.inputs.iter().enumerate() {
                    self.record_ty(ty.arg(i), &arg.ty);
                }
                self.record_function_ret_ty(ty.arg(fn_ty.decl.inputs.len()), &fn_ty.decl.output);
            },
            (&TyKind::Never, &TyNever) => {},
            (&TyKind::Tup(ref elems), &TyTuple(..)) => {
                for (i, ast_ty) in elems.iter().enumerate() {
                    self.record_ty(ty.arg(i), ast_ty);
                }
            },
            (&TyKind::Path(ref qself, ref path), _) => {
                // TyKind::Path could resolve to absolutely anything, since resolution includes
                // expanding type aliases.  So this case gets special handling.
                self.record_path_ty(ty, qself.as_ref(), path);
            },
            (&TyKind::TraitObject(..), &TyDynamic(..)) => {}, // TODO
            (&TyKind::ImplTrait(..), &TyAnon(..)) => {}, // TODO
            // `Paren` should never appear, but just in case...
            (&TyKind::Paren(ref ast_ty), _) => self.record_ty(ty, ast_ty),
            // No case for TyTypeof - it can't be written in source programs currently

            // These cases have no internal structure to recurse on.
            (&TyKind::Infer, _) => {},
            (&TyKind::ImplicitSelf, _) => {},
            (&TyKind::Mac(_), _) => {},

            (_, _) => {
                panic!("unsupported AST/resolved type combination:\
                        \n  ast: {:?}\
                        \n  resolved: {:?}",
                       ast_ty, ty);
            },
        }
    }

    fn record_function_ret_ty(&mut self, ty: S::Type, output: &FunctionRetTy) {
        match *output {
            FunctionRetTy::Default(_) => {},
            FunctionRetTy::Ty(ref ast_ty) => self.record_ty(ty, ast_ty),
        }
    }

    fn record_fn_decl(&mut self, sig: S::Signature, decl: &FnDecl) {
        assert!(sig.num_inputs() == decl.inputs.len());
        for (i, arg) in decl.inputs.iter().enumerate() {
            self.record_ty(sig.input(i), &arg.ty);
        }
        self.record_function_ret_ty(sig.output(), &decl.output);
    }

    fn record_path_ty(&mut self, ty: S::Type, qself: Option<&QSelf>, path: &Path) {
        // TODO: this is yet another Path case, which makes it a bit difficult to handle.
    }
}

impl<'ast, 'a, 'hir, S, F> Visitor<'ast> for TypeMapVisitor<'a, 'hir, S, F>
        where S: TypeSource,
              F: FnMut(&mut S, &Ty, S::Type) {

    // There are several places we can encounter `Ty` nodes, and each one has a different way of
    // obtaining the corresponding `LTy`.
    
    fn visit_expr(&mut self, e: &'ast Expr) {
        match e.node {
            ExprKind::Cast(_, ref ast_ty) => {
                if let Some(ty) = self.source.expr_type(e) {
                    self.record_ty(ty, ast_ty);
                }
            },

            ExprKind::Type(_, ref ast_ty) => {
                if let Some(ty) = self.source.expr_type(e) {
                    self.record_ty(ty, ast_ty);
                }
            },

            ExprKind::Closure(_, ref decl, _, _) => {
                let def_id = self.hir_map.local_def_id(e.id);
                if let Some(sig) = self.source.closure_sig(def_id) {
                    self.record_fn_decl(sig, decl);
                }
            },

            ExprKind::Path(ref qself, ref path) => {
                // TODO: this case is hard
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
            },

            ExprKind::Struct(ref path, _, _) => {
                // TODO: another path case
            },

            _ => {},
        }

        visit::walk_expr(self, e);
    }

    fn visit_local(&mut self, l: &'ast Local) {
        if let Some(ref ast_ty) = l.ty {
            if let Some(ty) = self.source.pat_type(&l.pat) {
                self.record_ty(ty, ast_ty);
            }
            // TODO: if pat_type returns None, we may be able to recover by recursing on the Pat
            // and the Ty.  This would help with use from MIR, where we can easily obtain types for
            // individual locals, but not for pats.
        }

        visit::walk_local(self, l);
    }

    fn visit_item(&mut self, i: &'ast Item) {
        let def_id = self.hir_map.local_def_id(i.id);
        match i.node {
            ItemKind::Static(ref ast_ty, _, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            },

            ItemKind::Const(ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            },

            ItemKind::Fn(ref decl, _, _, _, _, _) => {
                if let Some(sig) = self.source.fn_sig(def_id) {
                    self.record_fn_decl(sig, decl);
                }
            },

            ItemKind::Ty(ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            },

            // Enum/Struct/Union are handled by `visit_struct_field`.

            ItemKind::Impl(_, _, _, _, _, ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            },

            _ => {},
        }

        visit::walk_item(self, i);
    }

    fn visit_struct_field(&mut self, f: &'ast StructField) {
        let def_id = self.hir_map.local_def_id(f.id);
        if let Some(ty) = self.source.def_type(def_id) {
            self.record_ty(ty, &f.ty);
        }

        visit::walk_struct_field(self, f);
    }

    fn visit_impl_item(&mut self, i: &'ast ImplItem) {
        let def_id = self.hir_map.local_def_id(i.id);
        match i.node {
            ImplItemKind::Const(ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            },

            ImplItemKind::Method(ref method_sig, _) => {
                if let Some(sig) = self.source.fn_sig(def_id) {
                    self.record_fn_decl(sig, &method_sig.decl);
                }
            },

            ImplItemKind::Type(ref ast_ty) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            },

            _ => {},
        }

        visit::walk_impl_item(self, i);
    }

    fn visit_trait_item(&mut self, i: &'ast TraitItem) {
        let def_id = self.hir_map.local_def_id(i.id);
        match i.node {
            TraitItemKind::Const(ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            },

            TraitItemKind::Method(ref method_sig, _) => {
                if let Some(sig) = self.source.fn_sig(def_id) {
                    self.record_fn_decl(sig, &method_sig.decl);
                }
            },

            TraitItemKind::Type(_, ref opt_ast_ty) => {
                if let Some(ref ast_ty) = *opt_ast_ty {
                    if let Some(ty) = self.source.def_type(def_id) {
                        self.record_ty(ty, ast_ty);
                    }
                }
            },

            _ => {},
        }

        visit::walk_trait_item(self, i);
    }

    fn visit_foreign_item(&mut self, i: &'ast ForeignItem) {
        let def_id = self.hir_map.local_def_id(i.id);
        match i.node {
            ForeignItemKind::Fn(ref decl, _) => {
                if let Some(sig) = self.source.fn_sig(def_id) {
                    self.record_fn_decl(sig, &decl);
                }
            },

            ForeignItemKind::Static(ref ast_ty, _) => {
                if let Some(ty) = self.source.def_type(def_id) {
                    self.record_ty(ty, ast_ty);
                }
            },

            _ => {},
        }

        visit::walk_foreign_item(self, i);
    }
}

pub fn map_types<'a, 'hir, S, F>(hir_map: &'a hir::map::Map<'hir>,
                                 source: S,
                                 krate: &Crate,
                                 callback: F)
        where S: TypeSource,
              F: FnMut(&mut S, &Ty, S::Type) {
    let mut v = TypeMapVisitor {
        hir_map: hir_map,
        source: source,
        callback: callback,
    };
    visit::walk_crate(&mut v, krate);
}
