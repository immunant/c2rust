use rustc::hir::def::Def;
use rustc::hir::def_id::DefId;
use syntax::ast::*;
use syntax::codemap::{CodeMap, Span};
use syntax::symbol::Symbol;
use syntax::tokenstream::{TokenStream, ThinTokenStream};
use syntax::util::small_vector::SmallVector;


// Helper functions for extracting the `Symbol` from a pattern AST.

pub trait AsSymbol {
    fn as_symbol(&self) -> Option<Symbol>;
}

impl AsSymbol for Ident {
    fn as_symbol(&self) -> Option<Symbol> {
        Some(self.name)
    }
}

impl AsSymbol for Path {
    fn as_symbol(&self) -> Option<Symbol> {
        if self.segments.len() != 1 {
            return None;
        }
        let seg = &self.segments[0];
        if seg.parameters.is_some() {
            return None;
        }
        seg.identifier.as_symbol()
    }
}

impl AsSymbol for Expr {
    fn as_symbol(&self) -> Option<Symbol> {
        match self.node {
            ExprKind::Path(None, ref p) => p.as_symbol(),
            _ => None,
        }
    }
}

impl AsSymbol for Stmt {
    fn as_symbol(&self) -> Option<Symbol> {
        match self.node {
            StmtKind::Semi(ref e) => e.as_symbol(),
            _ => None,
        }
    }
}

impl AsSymbol for Pat {
    fn as_symbol(&self) -> Option<Symbol> {
        match self.node {
            PatKind::Ident(BindingMode::ByValue(Mutability::Immutable),
                           ref i, None) => i.node.as_symbol(),
            _ => None,
        }
    }
}

impl AsSymbol for Ty {
    fn as_symbol(&self) -> Option<Symbol> {
        match self.node {
            TyKind::Path(None, ref p) => p.as_symbol(),
            _ => None,
        }
    }
}

impl AsSymbol for Mac {
    fn as_symbol(&self) -> Option<Symbol> {
        if self.node.tts != ThinTokenStream::from(TokenStream::empty()) {
            return None;
        }
        self.node.path.as_symbol()
    }
}

impl AsSymbol for Item {
    fn as_symbol(&self) -> Option<Symbol> {
        match self.node {
            ItemKind::Mac(ref m) => m.as_symbol(),
            _ => None,
        }
    }
}

impl AsSymbol for ImplItem {
    fn as_symbol(&self) -> Option<Symbol> {
        match self.node {
            ImplItemKind::Macro(ref m) => m.as_symbol(),
            _ => None,
        }
    }
}

impl AsSymbol for TraitItem {
    fn as_symbol(&self) -> Option<Symbol> {
        match self.node {
            TraitItemKind::Macro(ref m) => m.as_symbol(),
            _ => None,
        }
    }
}


pub trait Lone<T> {
    fn lone(self) -> T;
}

impl<T> Lone<T> for T {
    fn lone(self) -> T {
        self
    }
}

impl<T> Lone<T> for Vec<T> {
    fn lone(mut self) -> T {
        assert!(self.len() == 1);
        self.pop().unwrap()
    }
}

impl<T> Lone<T> for SmallVector<T> {
    fn lone(mut self) -> T {
        assert!(self.len() == 1);
        self.pop().unwrap()
    }
}


pub fn with_span_text<F: FnOnce(&str)>(cm: &CodeMap, span: Span, callback: F) -> bool {
    let lo = cm.lookup_byte_offset(span.lo);
    let hi = cm.lookup_byte_offset(span.hi);
    let file_src = match lo.fm.src.as_ref() {
        Some(x) => x,
        None => return false,
    };
    let node_src = &file_src[lo.pos.0 as usize .. hi.pos.0 as usize];
    callback(node_src);
    true
}


pub fn extended_span(mut s: Span, attrs: &[Attribute]) -> Span {
    // Extend `s` backward to cover all the attrs
    for attr in attrs {
        // Not sure these checks are exactly right, but it seems to work for now.
        if attr.span.ctxt == s.ctxt && attr.span.lo < s.lo {
            s.lo = attr.span.lo;
        }
    }
    s
}


pub trait HirDefExt {
    fn opt_def_id(&self) -> Option<DefId>;
}

impl HirDefExt for Def {
    fn opt_def_id(&self) -> Option<DefId> {
        match *self {
            Def::Mod(did) |
            Def::Struct(did) |
            Def::Union(did) |
            Def::Enum(did) |
            Def::Variant(did) |
            Def::Trait(did) |
            Def::TyAlias(did) |
            Def::AssociatedTy(did) |
            Def::TyParam(did) |
            Def::Fn(did) |
            Def::Const(did) |
            Def::Static(did, _) |
            Def::StructCtor(did, _) |
            Def::VariantCtor(did, _) |
            Def::Method(did) |
            Def::AssociatedConst(did) |
            Def::Local(did) |
            Def::Upvar(did, _, _) |
            Def::Macro(did, _) |
            Def::GlobalAsm(did) => Some(did),

            Def::PrimTy(_) |
            Def::SelfTy(_, _) |
            Def::Label(_) |
            Def::Err => None
        }
    }
}


/// Conversion of string-like values into interned `Symbol`s.
pub trait IntoSymbol {
    fn into_symbol(self) -> Symbol;
}

impl IntoSymbol for Symbol {
    fn into_symbol(self) -> Symbol {
        self
    }
}

impl<'a> IntoSymbol for &'a str {
    fn into_symbol(self) -> Symbol {
        Symbol::intern(self)
    }
}

impl IntoSymbol for String {
    fn into_symbol(self) -> Symbol {
        Symbol::intern(&self)
    }
}

impl<'a> IntoSymbol for &'a String {
    fn into_symbol(self) -> Symbol {
        <&str as IntoSymbol>::into_symbol(self)
    }
}
