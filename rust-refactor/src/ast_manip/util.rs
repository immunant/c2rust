//! Miscellaneous utility functions.
use syntax::ast::*;
use syntax::codemap::{CodeMap, Span};
use syntax::symbol::Symbol;
use syntax::tokenstream::{TokenStream, ThinTokenStream};


/// Extract the symbol from a pattern-like AST.
pub trait PatternSymbol {
    fn pattern_symbol(&self) -> Option<Symbol>;
}

impl PatternSymbol for Ident {
    fn pattern_symbol(&self) -> Option<Symbol> {
        Some(self.name)
    }
}

impl PatternSymbol for Path {
    fn pattern_symbol(&self) -> Option<Symbol> {
        if self.segments.len() != 1 {
            return None;
        }
        let seg = &self.segments[0];
        if seg.parameters.is_some() {
            return None;
        }
        seg.identifier.pattern_symbol()
    }
}

impl PatternSymbol for Expr {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.node {
            ExprKind::Path(None, ref p) => p.pattern_symbol(),
            _ => None,
        }
    }
}

impl PatternSymbol for Stmt {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.node {
            StmtKind::Semi(ref e) => e.pattern_symbol(),
            _ => None,
        }
    }
}

impl PatternSymbol for Pat {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.node {
            PatKind::Ident(BindingMode::ByValue(Mutability::Immutable),
                           ref i, None) => i.node.pattern_symbol(),
            _ => None,
        }
    }
}

impl PatternSymbol for Ty {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.node {
            TyKind::Path(None, ref p) => p.pattern_symbol(),
            _ => None,
        }
    }
}

impl PatternSymbol for Mac {
    fn pattern_symbol(&self) -> Option<Symbol> {
        if self.node.tts != ThinTokenStream::from(TokenStream::empty()) {
            return None;
        }
        self.node.path.pattern_symbol()
    }
}

impl PatternSymbol for Item {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.node {
            ItemKind::Mac(ref m) => m.pattern_symbol(),
            _ => None,
        }
    }
}

impl PatternSymbol for ImplItem {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.node {
            ImplItemKind::Macro(ref m) => m.pattern_symbol(),
            _ => None,
        }
    }
}

impl PatternSymbol for TraitItem {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.node {
            TraitItemKind::Macro(ref m) => m.pattern_symbol(),
            _ => None,
        }
    }
}


/// Get the text of a span, and pass it to a callback.  Returns `false` if the span text isn't
/// available.
pub fn with_span_text<F: FnOnce(&str)>(cm: &CodeMap, span: Span, callback: F) -> bool {
    let lo = cm.lookup_byte_offset(span.lo());
    let hi = cm.lookup_byte_offset(span.hi());
    let file_src = match lo.fm.src.as_ref() {
        Some(x) => x,
        None => return false,
    };
    let node_src = &file_src[lo.pos.0 as usize .. hi.pos.0 as usize];
    callback(node_src);
    true
}


/// Extend a node span to cover its attributes.  (By default, item spans cover only the item body,
/// not the preceding attrs.)
pub fn extended_span(mut s: Span, attrs: &[Attribute]) -> Span {
    // Extend `s` backward to cover all the attrs
    for attr in attrs {
        // Not sure these checks are exactly right, but it seems to work for now.
        if attr.span.ctxt() == s.ctxt() && attr.span.lo() < s.lo() {
            s.lo = attr.span.lo();
        }
    }
    s
}


/// Get the name of a macro invocation.
pub fn macro_name(mac: &Mac) -> Name {
    let p = &mac.node.path;
    p.segments.last().unwrap().identifier.name
}
