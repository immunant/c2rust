//! This modules handles accumulating / re-arranging comments for the Rust AST.
//!
//! The only way we have found to have comments inserted into the pretty-printed Rust output is via
//! a comment vector. The Rust pretty-printer accepts a vector of comments and, before printing
//! any AST node, it dumps out the prefix of comments whose position is less than the span of the
//! AST node.
//!
//! The logic for creating/storing a comment vector is in `CommentStore`. For example, if you want
//! to add a comment to a match arm:
//!
//! ```rust
//!   let sp: Span = cmmt_store.add_comment_lines(vec!["Some comment on an arm"]);
//!   let arm = mk().span(sp).arm(pats, None, body);
//!   ...
//! ```
//!
//! Right before printing the output, it is a good idea to use the `CommentTraverser` to make sure
//! that the comment vector is in the right order. That just means doing something like this:
//!
//! ```rust
//!   let trav: CommentTraverser = cmmt_store.into_comment_traverser();
//!   let updated_module: Mod = trav.traverse_mod(module);
//!   let updated_cmmt_store = trav.into_comment_store();
//! ```

use syntax_pos::BytePos;
use syntax_pos::hygiene::SyntaxContext;
use syntax::codemap::{DUMMY_SP, Span};
use syntax::parse::lexer::comments;
use std::collections::BTreeMap;
use syntax::ast::*;
use rust_ast::traverse;

pub struct CommentStore {
    /// The `Span` keys do _not_ correspond to the comment position. Instead, they refer to the
    /// `Span` of whatever is associated with the comment.
    output_comments: BTreeMap<Span, comments::Comment>,

    /// Monotonically increasing source of new byte positions.
    span_source: u32,
}

impl CommentStore {

    pub fn new() -> Self {
        CommentStore {
            output_comments: BTreeMap::new(),
            span_source: 0,
        }
    }

    pub fn into_comment_traverser(self) -> CommentTraverser {
        CommentTraverser {
            old_comments: self.output_comments,
            store: CommentStore::new(),
        }
    }

    /// Convert the comment context into the accumulated (and ordered) `libsyntax` comments.
    pub fn into_comments(self) -> Vec<comments::Comment> {
        self.output_comments.into_iter().map(|(_, v)| v).collect()
    }

    /// Add a `Comment` at the current position, then return the `Span` that should be given to
    /// something we want associated with this comment.
    pub fn add_comment(&mut self, mut cmmt: comments::Comment) -> Span {
        self.span_source += 1;
        cmmt.pos = BytePos(self.span_source);

        self.span_source += 1;
        let sp = Span::new(
            BytePos(self.span_source),
            BytePos(self.span_source),
            SyntaxContext::empty(),
        );

        self.output_comments.insert(sp, cmmt);
        sp
    }

    /// Add a comment at the current position, then return the `Span` that should be given to
    /// something we want associated with this comment.
    pub fn add_comment_lines(&mut self, lines: Vec<String>) -> Span {
        let lines: Vec<String> = lines
            .into_iter()
            .map(|mut comment| {
                if comment.starts_with("//!") || comment.starts_with("///") ||
                    comment.starts_with("/**") || comment.starts_with("/*!") {
                    comment.insert(2,' ');
                }
                comment
            })
            .collect();

        if lines.is_empty() {
            DUMMY_SP
        } else {
            self.add_comment(
                comments::Comment {
                    style: comments::CommentStyle::Isolated,
                    lines: lines,
                    pos: BytePos(0), // overwritten in `add_comment`
                }
            )
        }
    }
}


pub struct CommentTraverser {
    old_comments: BTreeMap<Span, comments::Comment>,
    store: CommentStore
}
impl CommentTraverser {
    fn reinsert_comment_at(&mut self, sp: Span) -> Span {
        if let Some(cmmt) = self.old_comments.remove(&sp) {
            self.store.add_comment(cmmt)
        } else {
            DUMMY_SP
        }
    }

    /// Turn the traverser back into a `CommentStore`.
    pub fn into_comment_store(self) -> CommentStore {
//        assert!(old_comments.is_empty());
        self.store
    }
}

impl traverse::Traverse for CommentTraverser {

    fn traverse_stmt(&mut self, mut s: Stmt) -> Stmt {
        s.span = self.reinsert_comment_at(s.span);
        traverse::traverse_stmt_def(self, s)
    }

    fn traverse_expr(&mut self, mut e: Expr) -> Expr {
        e.span = self.reinsert_comment_at(e.span);
        traverse::traverse_expr_def(self, e)
    }

    fn traverse_trait_item(&mut self, mut ti: TraitItem) -> TraitItem {
        ti.span = self.reinsert_comment_at(ti.span);
        traverse::traverse_trait_item_def(self, ti)
    }

    fn traverse_impl_item(&mut self, mut ii: ImplItem) -> ImplItem {
        ii.span = self.reinsert_comment_at(ii.span);
        traverse::traverse_impl_item_def(self, ii)
    }

    fn traverse_block(&mut self, mut b: Block) -> Block {
        b.span = self.reinsert_comment_at(b.span);
        traverse::traverse_block_def(self, b)
    }

    fn traverse_local(&mut self, mut l: Local) -> Local {
        l.span = self.reinsert_comment_at(l.span);
        traverse::traverse_local_def(self, l)
    }

    fn traverse_field(&mut self, mut f: Field) -> Field {
        f.span = self.reinsert_comment_at(f.span);
        traverse::traverse_field_def(self, f)
    }

    fn traverse_item(&mut self, mut i: Item) -> Item {
        i.span = self.reinsert_comment_at(i.span);
        traverse::traverse_item_def(self, i)
    }

    fn traverse_foreign_item(&mut self, mut i: ForeignItem) -> ForeignItem {
        i.span = self.reinsert_comment_at(i.span);
        i
    }
}