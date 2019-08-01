//! This module handles accumulating / re-arranging comments for the Rust AST.
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
//!
//! Comments can currently be attached and printed in the following positions
//! (see pprust.rs for current details):
//!
//! Before the following AST elements:
//! - Lit
//! - Attribute
//! - Ty
//! - ForeignItem
//! - Item
//! - Variant
//! - Field
//! - TraitItem
//! - ImplItem
//! - Stmt
//! - Block
//! - Path
//! - Arm
//!
//! Trailing comments can be printed after the following elements hi pos, but on
//! the same line:
//! - Stmt
//! - Comma separated Expr (struct initializer, tuples literals, etc.)
//!
//! Before the close of a Block

use crate::rust_ast::{pos_to_span, traverse};
use itertools::Itertools;
use smallvec::{smallvec, SmallVec};
use std::collections::BTreeMap;
use syntax::ast::*;
use syntax::parse::lexer::comments;
use syntax_pos::BytePos;

pub struct CommentStore {
    /// The `BytePos` keys do _not_ correspond to the comment position. Instead, they refer to the
    /// `BytePos` of whatever is associated with the comment.
    output_comments: BTreeMap<BytePos, SmallVec<[comments::Comment; 1]>>,

    /// Monotonically increasing source of new byte positions.
    current_position: u32,
}

impl CommentStore {
    pub fn new() -> Self {
        CommentStore {
            output_comments: BTreeMap::new(),
            current_position: 0,
        }
    }

    pub fn into_comment_traverser(self) -> CommentTraverser {
        CommentTraverser {
            old_comments: self.output_comments,
            old_to_new_pos: BTreeMap::new(),
            store: CommentStore::new(),
        }
    }

    /// Convert the comment context into the accumulated (and ordered) `libsyntax` comments.
    pub fn into_comments(self) -> Vec<comments::Comment> {
        self.output_comments.into_iter().map(|(_, v)| v).flatten().collect()
    }

    /// Add comments at the specified position, then return the `BytePos` that
    /// should be given to something we want associated with this comment. If
    /// `pos` is None, or a comment is not found in the given position, use the
    /// current position instead.
    fn insert_comments(&mut self, mut new_comments: SmallVec<[comments::Comment; 1]>, pos: Option<BytePos>) -> BytePos {
        if let Some(pos) = pos {
            if let Some(comments) = self.output_comments.get_mut(&pos) {
                comments.extend(new_comments);
                return pos;
            }
        }

        // This line is not necessary. All it does is prevent the confusing
        // situation where comments have exactly the same position as some AST
        // node to which they are _not_ related.
        self.current_position += 1;

        // The position of isolated comments have to be LESS than the span of
        // the AST node it annotates.
        for cmmt in &mut new_comments {
            if let comments::CommentStyle::Isolated = cmmt.style {
                cmmt.pos = BytePos(self.current_position);
                self.current_position += 1;
            }
        }

        let new_pos = BytePos(self.current_position);

        // The position of trailing comments have to be GREATER than the span of
        // the AST node it follows.
        for cmmt in &mut new_comments {
            if let comments::CommentStyle::Trailing = cmmt.style {
                self.current_position += 1;
                cmmt.pos = BytePos(self.current_position);
            }
        }

        self.output_comments.insert(new_pos, new_comments);
        new_pos
    }

    /// Add an isolated comment at the current position, then return the `Span`
    /// that should be given to something we want associated with this comment.
    pub fn add_comments(&mut self, lines: &[String]) -> Option<BytePos> {
        self.extend_existing_comments(lines, None, comments::CommentStyle::Isolated)
    }

    /// Add a comment at the specified position, then return the `BytePos` that
    /// should be given to something we want associated with this comment. If
    /// pos is None, use the current position.
    pub fn extend_existing_comments(
        &mut self,
        lines: &[String],
        pos: Option<BytePos>,
        style: comments::CommentStyle,
    ) -> Option<BytePos> {
        fn translate_comment(comment: &String) -> String {
            comment
                .lines()
                .map(|line: &str| {
                    let mut line = line.to_owned();
                    if line.starts_with("//!")
                        || line.starts_with("///")
                        || line.starts_with("/**")
                        || line.starts_with("/*!")
                    {
                        line.insert(2, ' ');
                    };
                    line
                })
                .join("\n")
        }

        let lines: Vec<String> = lines.into_iter().map(translate_comment).collect();

        if lines.is_empty() {
            None
        } else {
            let new_comment = comments::Comment {
                style,
                lines: lines,
                pos: BytePos(0), // overwritten in `add_comment`
            };
            Some(self.insert_comments(smallvec![new_comment], pos))
        }
    }

    /// Move comments associated with `old` to `new`.
    pub fn move_comments(&mut self, old: BytePos, new: BytePos) {
        if old == new {
            return;
        }
        if let Some(comments) = self.output_comments.remove(&old) {
            self.output_comments.get_mut(&new).unwrap().extend(comments);
        }
    }
}

pub struct CommentTraverser {
    old_comments: BTreeMap<BytePos, SmallVec<[comments::Comment; 1]>>,
    old_to_new_pos: BTreeMap<BytePos, BytePos>,
    store: CommentStore,
}
impl CommentTraverser {
    fn reinsert_comment_at(&mut self, sp: BytePos) -> Option<BytePos> {
        if let Some(cmmts) = self.old_comments.remove(&sp) {
            let new_pos = self.store.insert_comments(cmmts, None);
            self.old_to_new_pos.insert(sp, new_pos);
            Some(new_pos)
        } else if let Some(new_pos) = self.old_to_new_pos.get(&sp) {
            Some(*new_pos)
        } else {
            None
        }
    }

    /// Turn the traverser back into a `CommentStore`.
    pub fn into_comment_store(self) -> CommentStore {
        //        assert!(old_comments.is_empty());
        self.store
    }
}

macro_rules! reinsert_and_traverse {
    ($fn:ident, $ty:ty, $traverse:path) => {
        fn $fn(&mut self, mut x: $ty) -> $ty {
            let orig = x.span.data();
            x.span = pos_to_span(self.reinsert_comment_at(orig.lo).unwrap_or(BytePos(0)));
            x = $traverse(self, x);
            if orig.lo != orig.hi {
                if let Some(new_hi) = self.reinsert_comment_at(orig.hi) {
                    x.span = x.span.with_hi(new_hi);
                }
            }
            x
        }
    };
}

impl traverse::Traversal for CommentTraverser {
    reinsert_and_traverse!(traverse_stmt, Stmt, traverse::traverse_stmt_def);
    reinsert_and_traverse!(traverse_expr, Expr, traverse::traverse_expr_def);
    reinsert_and_traverse!(traverse_trait_item, TraitItem, traverse::traverse_trait_item_def);
    reinsert_and_traverse!(traverse_impl_item, ImplItem, traverse::traverse_impl_item_def);
    reinsert_and_traverse!(traverse_block, Block, traverse::traverse_block_def);
    reinsert_and_traverse!(traverse_local, Local, traverse::traverse_local_def);
    reinsert_and_traverse!(traverse_field, Field, traverse::traverse_field_def);
    reinsert_and_traverse!(traverse_item, Item, traverse::traverse_item_def);

    fn traverse_foreign_item(&mut self, mut i: ForeignItem) -> ForeignItem {
        i.span = pos_to_span(self.reinsert_comment_at(i.span.lo()).unwrap_or(BytePos(0)));
        i
    }
}
