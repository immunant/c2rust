//! Miscellaneous utility functions.
use rustc::hir::def::{Def, Namespace};
use smallvec::SmallVec;
use syntax::ast::*;
use syntax::ptr::P;
use syntax::source_map::{SourceMap, Span, DUMMY_SP};
use syntax::symbol::{keywords, Symbol};
use syntax::tokenstream::TokenStream;
use syntax_pos::{BytePos, Pos};

use super::{AstEquiv, Comment, CommentStyle};

/// Extract the symbol from a pattern-like AST.
pub trait PatternSymbol {
    fn pattern_symbol(&self) -> Option<Symbol>;
}

impl PatternSymbol for Ident {
    fn pattern_symbol(&self) -> Option<Symbol> {
        Some(self.name)
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
            PatKind::Ident(BindingMode::ByValue(Mutability::Immutable), ref i, None) => {
                i.pattern_symbol()
            }
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
        if self.node.tts != TokenStream::empty() {
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

/// Extend a node span to cover comments around it.
pub fn extend_span_comments(mut span: Span, comments: &[Comment], sources: &SourceMap) -> Span {
    if comments.is_empty() {
        return span;
    }

    debug!("Extending span comments for {:?} for comments: {:?}", span, comments);

    let mut before = vec![];
    let mut after = vec![];
    for comment in comments {
        match comment.style {
            CommentStyle::Isolated => {
                before.push(comment);
            }

            CommentStyle::Trailing => {
                after.push(comment);
            }

            _ => unimplemented!("Mixed and BlankLine comment styles are not implemented"),
        }
    }

    before.sort_by_key(|c| c.pos);
    after.sort_by_key(|c| c.pos);

    before.reverse();

    for comment in &before {
        let comment_span = span.shrink_to_lo().with_lo(comment.pos);
        let source = sources.span_to_snippet(comment_span).unwrap();
        let matches = source.lines().zip(&comment.lines).all(|(src_line, comment_line)| {
            src_line.trim() == comment_line.trim()
        });
        if matches {
            let mut comment_pos = comment.pos;

            // Extend to previous newline because this is an isolated comment
            let comment_begin = sources.lookup_byte_offset(comment.pos);
            let mut extend_comment_pos = |src: &str| {
                if let Some(newline_index) = src[..comment_begin.pos.to_usize()].rfind('\n') {
                    comment_pos = BytePos::from_usize(newline_index) + comment_begin.sf.start_pos;
                }
            };
            if let Some(ref src) = comment_begin.sf.src {
                extend_comment_pos(src);
            } else if let Some(src) = comment_begin.sf.external_src.borrow().get_source() {
                extend_comment_pos(src);
            }

            span = span.with_lo(comment_pos);
        } else {
            debug!("comment {:?} did not match source {:?}", comment, source);
            break;
        }
    }

    for comment in &after {
        for comment_line in &comment.lines {
            let line_end = if comment_line.starts_with("//") {
                BytePos::from_usize(span.hi().to_usize() + comment_line.len() + 1)
            } else {
                BytePos::from_usize(span.hi().to_usize() + comment_line.len())
            };
            let line_span = span.shrink_to_hi().with_hi(line_end);
            let src_line = sources.span_to_snippet(line_span).unwrap();
            if comment_line.trim() == src_line.trim() {
                span = span.with_hi(line_end);
            } else {
                // We need to break out of processing any after comments because
                // a line didn't match.
                debug!("comment {:?} did not match line {:?}", comment_line, src_line);
                return span;
            }
        }
    }

    span
}

/// Get the name of a macro invocation.
pub fn macro_name(mac: &Mac) -> Name {
    let p = &mac.node.path;
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
            item.node = ItemKind::Use(P(UseTree {
                prefix: path,
                ..tree
            }));
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
    let use_tree = expect!([&item.node] ItemKind::Use(u) => u)
        .clone()
        .into_inner();
    let mut out = smallvec![];
    let initial_path = Path {
        span: use_tree.prefix.span,
        segments: vec![],
    };
    let id = item.id;
    split_uses_impl(item, initial_path, id, use_tree, &mut out);
    out
}

/// Is a path relative to the current module?
pub fn is_relative_path(path: &Path) -> bool {
    !path.segments.is_empty()
        && (path.segments[0].ident.name == keywords::SelfLower.name()
            || path.segments[0].ident.name == keywords::Super.name())
}

/// Return the namespace the given Def is defined in. Does not yet handle the
/// macro namespace.
pub fn namespace(def: &Def) -> Option<Namespace> {
    use rustc::hir::def::Def::*;
    match def {
        Mod(..)
        | Struct(..)
        | Union(..)
        | Enum(..)
        | Variant(..)
        | Trait(..)
        | Existential(..)
        | TyAlias(..)
        | ForeignTy(..)
        | TraitAlias(..)
        | AssociatedTy(..)
        | AssociatedExistential(..)
        | PrimTy(..)
        | TyParam(..)
        | SelfTy(..)
        | ToolMod => Some(Namespace::TypeNS),

        Fn(..) | Const(..) | Static(..) | SelfCtor(..) | Method(..) | AssociatedConst(..)
        | Local(..) | Upvar(..) | Label(..) => Some(Namespace::ValueNS),

        _ => None,
    }
}

/// Select the wider of the two given visibilities
pub fn join_visibility(vis1: &VisibilityKind, vis2: &VisibilityKind) -> VisibilityKind {
    use syntax::ast::CrateSugar::PubCrate;
    use syntax::ast::VisibilityKind::*;
    match (vis1, vis2) {
        (Public, _) | (_, Public) => Public,
        (Crate(_), _) | (_, Crate(_)) => Crate(PubCrate),
        (Restricted { path: path1, .. }, Restricted { path: path2, .. }) => {
            if path1.ast_equiv(&path2) {
                vis1.clone()
            } else {
                Crate(PubCrate)
            }
        }
        (Restricted { .. }, Inherited) => vis1.clone(),
        (Inherited, Restricted { .. }) => vis2.clone(),
        _ => Inherited,
    }
}
