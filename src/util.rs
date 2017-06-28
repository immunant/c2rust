use syntax::ast::*;
use syntax::symbol::Symbol;
use syntax::tokenstream::{TokenStream, ThinTokenStream};
use syntax::util::small_vector::SmallVector;


// Helper functions for extracting the `Symbol` from a pattern AST.

pub fn ident_sym(i: &Ident) -> Option<Symbol> {
    Some(i.name)
}

pub fn path_sym(p: &Path) -> Option<Symbol> {
    if p.segments.len() != 1 {
        return None;
    }
    let seg = &p.segments[0];
    if seg.parameters.is_some() {
        return None;
    }
    ident_sym(&seg.identifier)
}

pub fn expr_sym(e: &Expr) -> Option<Symbol> {
    let path = match e.node {
        ExprKind::Path(None, ref p) => p,
        _ => return None,
    };
    path_sym(path)
}

pub fn stmt_sym(s: &Stmt) -> Option<Symbol> {
    let e = match s.node {
        StmtKind::Semi(ref e) => e,
        _ => return None,
    };
    expr_sym(&e)
}

pub fn pat_sym(p: &Pat) -> Option<Symbol> {
    let i = match p.node {
        PatKind::Ident(BindingMode::ByValue(Mutability::Immutable),
                       ref i, None) => i,
        _ => return None,
    };
    ident_sym(&i.node)
}

pub fn ty_sym(t: &Ty) -> Option<Symbol> {
    let path = match t.node {
        TyKind::Path(None, ref p) => p,
        _ => return None,
    };
    path_sym(path)
}

pub fn mac_sym(m: &Mac) -> Option<Symbol> {
    if m.node.tts != ThinTokenStream::from(TokenStream::empty()) {
        return None;
    }
    path_sym(&m.node.path)
}

pub fn item_sym(i: &Item) -> Option<Symbol> {
    let m = match i.node {
        ItemKind::Mac(ref m) => m,
        _ => return None,
    };
    mac_sym(m)
}

pub fn impl_item_sym(i: &ImplItem) -> Option<Symbol> {
    let m = match i.node {
        ImplItemKind::Macro(ref m) => m,
        _ => return None,
    };
    mac_sym(m)
}

pub fn trait_item_sym(i: &TraitItem) -> Option<Symbol> {
    let m = match i.node {
        TraitItemKind::Macro(ref m) => m,
        _ => return None,
    };
    mac_sym(m)
}


pub trait Lone<T> {
    fn lone(self) -> T;
}

impl<T> Lone<T> for T {
    fn lone(self) -> T {
        self
    }
}

impl<T> Lone<T> for SmallVector<T> {
    fn lone(mut self) -> T {
        assert!(self.len() == 1);
        self.pop().unwrap()
    }
}
