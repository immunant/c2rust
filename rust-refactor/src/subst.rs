use rustc::hir::map::Node;
use rustc::hir::def_id::DefId;
use syntax::ast::{Ident, Path, Expr, ExprKind, Pat, Ty, TyKind, Stmt, Item, ImplItem};
use syntax::ast::Mac;
use syntax::fold::{self, Folder};
use syntax::parse::parser::Parser;
use syntax::parse::token::Token;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::tokenstream::ThinTokenStream;
use syntax::util::small_vector::SmallVector;

use api::DriverCtxtExt;
use bindings::Bindings;
use command::CommandState;
use driver;
use fold::Fold;
use make_ast::mk;
use util;
use util::IntoSymbol;
use util::PatternSymbol;
use util::Lone;


struct SubstFolder<'a, 'hir: 'a, 'gcx: 'tcx + 'a, 'tcx: 'a> {
    st: &'a CommandState,
    cx: &'a driver::Ctxt<'a, 'hir, 'gcx, 'tcx>,
    bindings: &'a Bindings,
}

impl<'a, 'hir, 'gcx, 'tcx> SubstFolder<'a, 'hir, 'gcx, 'tcx> {
    fn named_marked_def_id(&self, name: Symbol, label: Symbol) -> Option<DefId> {
        let mut found = None;

        for &(node_id, node_label) in self.st.marks().iter() {
            if node_label != label {
                continue;
            }

            let node = match_or!([self.cx.hir_map().find(node_id)] Some(x) => x;
                                 continue);
            let node_name = match node {
                Node::NodeItem(i) => i.name,
                Node::NodeForeignItem(i) => i.name,
                Node::NodeTraitItem(i) => i.name,
                Node::NodeImplItem(i) => i.name,
                _ => continue,
            };
            if node_name != name {
                continue;
            }

            assert!(found.is_none(),
                    "found multiple nodes with name `{}` and label `{}`", name, label);
            found = Some(self.cx.node_def_id(node_id));
        }

        found
    }

    fn get_def_path(&self, tts: &ThinTokenStream) -> Path {
        let mut p = Parser::new(&self.cx.session().parse_sess,
                                tts.clone().into(),
                                None, false, false);
        let name = p.parse_ident().unwrap().name;
        let label =
            if p.eat(&Token::Comma) {
                p.parse_ident().unwrap().name
            } else {
                "target".into_symbol()
            };

        if let Some(path) = self.bindings.get_def_path(name, label) {
            return path.clone();
        }

        if let Some(def_id) = self.named_marked_def_id(name, label) {
            return self.cx.def_path(def_id);
        }

        panic!("found no definition with name `{}` and label `{}`", name, label);
    }
}

impl<'a, 'hir, 'gcx, 'tcx> Folder for SubstFolder<'a, 'hir, 'gcx, 'tcx> {
    fn fold_ident(&mut self, i: Ident) -> Ident {
        // The `Ident` case is a bit different from the others.  If `fold_stmt` finds a non-`Stmt`
        // in `self.bindings`, it can ignore the problem and hope `fold_expr` or `fold_ident` will
        // find an `Expr`/`Ident` for the symbol later on.  If `fold_ident` fails, there is no
        // lower-level construct to try.  So we report an error if a binding exists at this point
        // but is not an `Ident`.

        if let Some(sym) = i.pattern_symbol() {
            if let Some(ident) = self.bindings.get_ident(sym) {
                return ident.clone();
            } else if let Some(ty) = self.bindings.get_type(sym) {
                panic!("binding {:?} (of type {:?}) has wrong type for hole", sym, ty);
            }
            // Otherwise, fall through
        }
        fold::noop_fold_ident(i, self)
    }

    fn fold_path(&mut self, p: Path) -> Path {
        if let Some(path) = p.pattern_symbol().and_then(|sym| self.bindings.get_path(sym)) {
            path.clone()
        } else {
            fold::noop_fold_path(p, self)
        }
    }

    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        if let Some(expr) = e.pattern_symbol().and_then(|sym| self.bindings.get_expr(sym)) {
            return expr.clone();
        }

        if let ExprKind::Mac(ref mac) = e.node {
            match &util::macro_name(&mac).as_str() as &str {
                "def" => {
                    let path = self.get_def_path(&mac.node.tts);
                    return mk().path_expr(path);
                },
                _ => {},
            }
        }

        e.map(|e| fold::noop_fold_expr(e, self))
    }

    fn fold_pat(&mut self, p: P<Pat>) -> P<Pat> {
        if let Some(pat) = p.pattern_symbol().and_then(|sym| self.bindings.get_pat(sym)) {
            pat.clone()
        } else {
            fold::noop_fold_pat(p, self)
        }
    }

    fn fold_ty(&mut self, ty: P<Ty>) -> P<Ty> {
        if let Some(ty) = ty.pattern_symbol().and_then(|sym| self.bindings.get_ty(sym)) {
            return ty.clone();
        }

        if let TyKind::Mac(ref mac) = ty.node {
            match &util::macro_name(&mac).as_str() as &str {
                "def" => {
                    let path = self.get_def_path(&mac.node.tts);
                    return mk().path_ty(path);
                },
                _ => {},
            }
        }

        fold::noop_fold_ty(ty, self)
    }

    fn fold_stmt(&mut self, s: Stmt) -> SmallVector<Stmt> {
        if let Some(stmt) = s.pattern_symbol().and_then(|sym| self.bindings.get_stmt(sym)) {
            SmallVector::one(stmt.clone())
        } else if let Some(stmts) = s.pattern_symbol()
                .and_then(|sym| self.bindings.get_multi_stmt(sym)) {
            SmallVector::many(stmts.clone())
        } else {
            fold::noop_fold_stmt(s, self)
        }
    }

    fn fold_item(&mut self, i: P<Item>) -> SmallVector<P<Item>> {
        if let Some(item) = i.pattern_symbol().and_then(|sym| self.bindings.get_item(sym)) {
            SmallVector::one(item.clone())
        } else {
            fold::noop_fold_item(i, self)
        }
    }

    fn fold_mac(&mut self, mac: Mac) -> Mac {
        fold::noop_fold_mac(mac, self)
    }
}


pub trait Subst {
    fn subst(self, st: &CommandState, cx: &driver::Ctxt, bindings: &Bindings) -> Self;
}

macro_rules! subst_impl {
    ($ty:ty, $fold_func:ident) => {
        impl Subst for $ty {
            fn subst(self, st: &CommandState, cx: &driver::Ctxt, bindings: &Bindings) -> Self {
                let mut f = SubstFolder {
                    st: st,
                    cx: cx,
                    bindings: bindings,
                };
                let result = self.fold(&mut f);
                result.lone()
            }
        }
    };
}

macro_rules! multi_subst_impl {
    ($ty:ty, $fold_func:ident) => {
        impl Subst for Vec<$ty> {
            fn subst(self, st: &CommandState, cx: &driver::Ctxt, bindings: &Bindings) -> Self {
                let mut f = SubstFolder {
                    st: st,
                    cx: cx,
                    bindings: bindings,
                };
                let mut results = Vec::with_capacity(self.len());
                for x in self {
                    results.extend_from_slice(&x.fold(&mut f));
                }
                results
            }
        }
    };
}

subst_impl!(Ident, fold_ident);
subst_impl!(P<Expr>, fold_expr);
subst_impl!(P<Pat>, fold_pat);
subst_impl!(P<Ty>, fold_ty);
subst_impl!(Stmt, fold_stmt);
subst_impl!(P<Item>, fold_item);
subst_impl!(ImplItem, fold_impl_item);

multi_subst_impl!(Stmt, fold_stmt);
multi_subst_impl!(P<Item>, fold_item);
multi_subst_impl!(ImplItem, fold_impl_item);
