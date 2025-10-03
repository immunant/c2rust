//! Miscellaneous utility functions.
use rustc_ast::ptr::P;
use rustc_ast::*;
use rustc_hir::def::{self, Namespace, Res};
use rustc_span::source_map::{SourceMap, Span, DUMMY_SP};
use rustc_span::sym;
use rustc_span::symbol::{kw, Ident, Symbol};
use smallvec::smallvec;
use smallvec::SmallVec;

use crate::expect;

use super::AstEquiv;

/// Extract the symbol from a pattern-like AST.
pub trait PatternSymbol {
    fn pattern_symbol(&self) -> Option<Symbol>;
}

impl PatternSymbol for Ident {
    fn pattern_symbol(&self) -> Option<Symbol> {
        Some(self.name)
    }
}

impl PatternSymbol for Lit {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.kind {
            // FIXME: can this conflict with regular Err literals???
            LitKind::Err(ref sym) => Some(*sym),
            _ => None,
        }
    }
}

impl PatternSymbol for Label {
    fn pattern_symbol(&self) -> Option<Symbol> {
        self.ident.pattern_symbol()
    }
}

impl PatternSymbol for Path {
    fn pattern_symbol(&self) -> Option<Symbol> {
        if self.segments.len() != 1 {
            return None;
        }
        let seg = &self.segments[0];
        if seg.args.is_some() {
            return None;
        }
        seg.ident.pattern_symbol()
    }
}

impl PatternSymbol for Expr {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.kind {
            ExprKind::Path(None, ref p) => p.pattern_symbol(),
            _ => None,
        }
    }
}

impl PatternSymbol for Stmt {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.kind {
            StmtKind::Semi(ref e) => e.pattern_symbol(),
            _ => None,
        }
    }
}

impl PatternSymbol for Pat {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.kind {
            PatKind::Ident(BindingMode::ByValue(Mutability::Not), ref i, None) => {
                i.pattern_symbol()
            }
            _ => None,
        }
    }
}

impl PatternSymbol for Ty {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.kind {
            TyKind::Path(None, ref p) => p.pattern_symbol(),
            _ => None,
        }
    }
}

impl PatternSymbol for MacCall {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match &*self.args {
            MacArgs::Empty => self.path.pattern_symbol(),
            _ => None,
        }
    }
}

impl PatternSymbol for Item {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.kind {
            ItemKind::MacCall(ref m) => m.pattern_symbol(),
            _ => None,
        }
    }
}

impl PatternSymbol for AssocItem {
    fn pattern_symbol(&self) -> Option<Symbol> {
        match self.kind {
            AssocItemKind::MacCall(ref m) => m.pattern_symbol(),
            _ => None,
        }
    }
}

pub fn is_c2rust_attr(attr: &Attribute, name: &str) -> bool {
    if let AttrKind::Normal(item, _) = &attr.kind {
        item.path.segments.len() == 2
            && item.path.segments[0].ident.as_str() == "c2rust"
            && item.path.segments[1].ident.as_str() == name
    } else {
        false
    }
}

/// Get the text of a span, and pass it to a callback.  Returns `false` if the span text isn't
/// available.
pub fn with_span_text<F: FnOnce(&str)>(cm: &SourceMap, span: Span, callback: F) -> bool {
    let lo = cm.lookup_byte_offset(span.lo());
    let hi = cm.lookup_byte_offset(span.hi());
    let file_src = match lo.sf.src.as_ref() {
        Some(x) => x,
        None => return false,
    };
    let node_src = &file_src[lo.pos.0 as usize..hi.pos.0 as usize];
    callback(node_src);
    true
}

/// Extend a node span to cover its attributes.  (By default, item spans cover only the item body,
/// not the preceding attrs.)
pub fn extend_span_attrs(mut s: Span, attrs: &[Attribute]) -> Span {
    // Extend `s` backward to cover all the attrs
    for attr in attrs {
        // Not sure these checks are exactly right, but it seems to work for now.
        if attr.span != DUMMY_SP && attr.span.ctxt() == s.ctxt() && attr.span.lo() < s.lo() {
            s = s.with_lo(attr.span.lo());
        }
    }
    s
}

/// Get the name of a macro invocation.
pub fn macro_name(mac: &MacCall) -> Symbol {
    let p = &mac.path;
    p.segments.last().unwrap().ident.name
}

/// Retrieve the list of Idents defined by the given UseTree
pub fn use_idents(tree: &UseTree) -> Vec<Ident> {
    match &tree.kind {
        UseTreeKind::Simple(..) => vec![tree.ident()],
        UseTreeKind::Glob => unimplemented!(),
        UseTreeKind::Nested(children) => children
            .iter()
            .flat_map(|(tree, _)| use_idents(tree))
            .collect(),
    }
}

/// Helper function to recursively split nested uses into simple ones
fn split_uses_impl(
    mut item: P<Item>,
    mut path: Path,
    id: NodeId,
    tree: UseTree,
    out: &mut SmallVec<[P<Item>; 1]>,
) {
    path.segments.extend_from_slice(&tree.prefix.segments);
    match tree.kind {
        UseTreeKind::Simple(..) | UseTreeKind::Glob => {
            item.id = id;
            item.kind = ItemKind::Use(UseTree {
                prefix: path,
                ..tree
            });
            out.push(item);
        }
        UseTreeKind::Nested(children) => {
            for (u, id) in children.into_iter() {
                split_uses_impl(item.clone(), path.clone(), id, u, out);
            }
        }
    }
}

/// Split a use statement which may have nesting into one or more simple use
/// statements without nesting.
pub fn split_uses(item: P<Item>) -> SmallVec<[P<Item>; 1]> {
    let use_tree = expect!([&item.kind] ItemKind::Use(u) => u.clone());
    let mut out = smallvec![];
    let initial_path = Path {
        span: use_tree.prefix.span,
        segments: vec![],
        tokens: None,
    };
    let id = item.id;
    split_uses_impl(item, initial_path, id, use_tree, &mut out);
    out
}

/// Is a path relative to the current module?
pub fn is_relative_path(path: &Path) -> bool {
    !path.segments.is_empty()
        && (path.segments[0].ident.name == kw::SelfLower
            || path.segments[0].ident.name == kw::Super)
}

/// Return the namespace the given Def is defined in. Does not yet handle the
/// macro namespace.
pub fn namespace<T>(res: &def::Res<T>) -> Option<Namespace> {
    use rustc_hir::def::DefKind::*;
    match res {
        Res::Def(kind, _) => match kind {
            Mod | Struct | Union | Enum | Variant | Trait | TyAlias | ForeignTy | TraitAlias
            | AssocTy | TyParam => Some(Namespace::TypeNS),
            Fn | Const | ConstParam | Static(_) | Ctor(..) | AssocFn | AssocConst => {
                Some(Namespace::ValueNS)
            }
            Macro(..) => Some(Namespace::MacroNS),

            ExternCrate | Use | ForeignMod | AnonConst | InlineConst | OpaqueTy | Field
            | LifetimeParam | GlobalAsm | Impl | Closure | Generator => None,
        },

        Res::PrimTy(..) | Res::SelfTy { .. } | Res::ToolMod => Some(Namespace::TypeNS),

        Res::SelfCtor(..) | Res::Local(..) => Some(Namespace::ValueNS),

        Res::NonMacroAttr(..) => Some(Namespace::MacroNS),

        Res::Err => None,
    }
}

/// Select the wider of the two given visibilities
pub fn join_visibility(vis1: &VisibilityKind, vis2: &VisibilityKind) -> VisibilityKind {
    use rustc_ast::VisibilityKind::*;
    match (vis1, vis2) {
        (Public, _) | (_, Public) => Public,
        (Restricted { path, .. }, _) if path.eq(&kw::Crate) => vis1.clone(),
        (_, Restricted { path, .. }) if path.eq(&kw::Crate) => vis2.clone(),
        (Restricted { path: path1, .. }, Restricted { path: path2, .. }) => {
            if path1.ast_equiv(&path2) {
                vis1.clone()
            } else {
                Restricted {
                    path: P(Path::from_ident(Ident::new(kw::Crate, DUMMY_SP))),
                    id: DUMMY_NODE_ID,
                }
            }
        }
        (Restricted { .. }, Inherited) => vis1.clone(),
        (Inherited, Restricted { .. }) => vis2.clone(),
        _ => Inherited,
    }
}

/// Is this item visible outside its translation unit?
pub fn is_exported(item: &Item) -> bool {
    match &item.kind {
        // Values are only visible outside their translation unit if marked with
        // no mangle or an explicit symbol name
        ItemKind::Static(..) | ItemKind::Const(..) | ItemKind::Fn(..) => {
            item.attrs.iter().any(is_export_attr)
        }

        // Types are visible if pub
        _ => item.vis.kind.is_pub(),
    }
}

pub fn is_export_attr(attr: &Attribute) -> bool {
    attr.has_name(sym::no_mangle) || attr.has_name(sym::export_name)
}

/// Are the idents of the segments of `path` equivalent to the list of idents
pub fn path_eq<T: AsRef<str>>(path: &Path, idents: &[T]) -> bool {
    path.segments.len() == idents.len()
        && path
            .segments
            .iter()
            .zip(idents)
            .all(|(p, i)| p.ident.as_str() == i.as_ref())
}
