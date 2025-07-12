//! This module handles accumulating / re-arranging comments for the Rust AST.
//!
//! Comments are stored as attributes on the AST node immediately following the comment.
//!
//! Comments can currently be attached and printed in the following positions
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
//! Comments cannot currently be attached before the close of a Block, or after all Items in a File.

use crate::rust_ast::{pos_to_span, set_span::SetSpan, traverse, BytePos, SpanExt};
use c2rust_ast_printer::pprust::comments;
use itertools::Itertools;
use log::warn;
use proc_macro2::{Span, TokenStream};
use smallvec::{smallvec, SmallVec};
use std::collections::BTreeMap;
use std::default::Default;
use syn::__private::ToTokens;
use syn::spanned::Spanned as _;
use syn::*;

#[derive(Default)]
pub struct CommentStore {
    /// The `BytePos` keys do _not_ correspond to the comment position. Instead, they refer to the
    /// `BytePos` of whatever is associated with the comment.
    output_comments: BTreeMap<BytePos, SmallVec<[comments::Comment; 1]>>,

    /// Monotonically increasing source of new byte positions.
    current_position: u32,
}

impl CommentStore {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn into_comment_traverser(self) -> CommentTraverser {
        CommentTraverser {
            old_comments: self.output_comments,
            ..Default::default()
        }
    }

    /// Convert the comment context into the accumulated (and ordered) `libsyntax` comments.
    pub fn into_comments(self) -> Vec<comments::Comment> {
        self.output_comments
            .into_iter()
            .flat_map(|(_, v)| v)
            .collect()
    }

    /// Add comments at the specified position, then return the `BytePos` that
    /// should be given to something we want associated with this comment. If
    /// `pos` is None, or a comment is not found in the given position, use the
    /// current position instead.
    fn insert_comments(
        &mut self,
        mut new_comments: SmallVec<[comments::Comment; 1]>,
        pos: Option<BytePos>,
    ) -> BytePos {
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
            //if let comments::CommentStyle::Isolated = cmmt.style {
            cmmt.pos = BytePos(self.current_position);
            self.current_position += 1;
            //}
        }

        let new_pos = BytePos(self.current_position);

        // The position of trailing comments have to be GREATER than the span of
        // the AST node it follows.
        /*for cmmt in &mut new_comments {
            if let comments::CommentStyle::Trailing = cmmt.style {
                self.current_position += 1;
                cmmt.pos = BytePos(self.current_position);
            }
        }*/

        self.output_comments.insert(new_pos, new_comments);
        new_pos
    }

    /// Add an isolated comment at the current position, then return the `Span`
    /// that should be given to something we want associated with this comment.
    pub fn add_comments(&mut self, lines: &[String]) -> Option<BytePos> {
        self.extend_existing_comments(lines, None) //, comments::CommentStyle::Isolated)
    }

    /// Add a comment at the specified position, then return the `BytePos` that
    /// should be given to something we want associated with this comment. If
    /// pos is None, use the current position.
    pub fn extend_existing_comments(
        &mut self,
        lines: &[String],
        pos: Option<BytePos>,
        //style: comments::CommentStyle,
    ) -> Option<BytePos> {
        fn translate_comment(comment: &str) -> String {
            comment
                .lines()
                .map(|line: &str| {
                    let mut line = line.to_owned();
                    let begin = line.trim_start();
                    if begin.starts_with("//!") || begin.starts_with("///") {
                        let begin_loc = line.len() - begin.len();
                        line.insert(2 + begin_loc, ' ');
                    };
                    line
                })
                .join("\n")
                .replace("/**", "/* *")
                .replace("/*!", "/* !")
        }

        let lines: Vec<String> = lines
            .iter()
            .map(|comment| translate_comment(comment))
            .collect();

        if lines.is_empty() {
            None
        } else {
            let new_comment = comments::Comment {
                //style,
                lines,
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
            self.output_comments
                .entry(new)
                .or_default()
                .extend(comments);
        }
    }

    /// Move any comments attached to the end of `span` to its beginning and
    /// return a new span for those comments.
    pub fn move_comments_to_begin(&mut self, span: Span) -> Span {
        let span = if span.lo() != span.hi() {
            if span.lo() == BytePos(0) {
                span.shrink_to_hi()
            } else {
                let new_comments = match self.output_comments.remove(&span.hi()) {
                    Some(nc) => nc,
                    None => {
                        warn!(
                            "Expected comments attached to the high end of span {:?}",
                            span
                        );
                        return span;
                    }
                };
                self.output_comments
                    .entry(span.lo())
                    .or_default()
                    .extend(new_comments);
                span.shrink_to_lo()
            }
        } else {
            span
        };

        /*// All comments attached to this span should become isolated.
        if let Some(comments) = self.output_comments.get_mut(&span.lo()) {
            for comment in comments {
                comment.style = comments::CommentStyle::Isolated;
            }
        }*/

        span
    }
}

#[derive(Default)]
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
        } else {
            self.old_to_new_pos.get(&sp).copied()
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
            let (lo, hi) = x.span().inner();
            x.set_span(pos_to_span(
                self.reinsert_comment_at(BytePos(lo)).unwrap_or(BytePos(0)),
            ));
            x = $traverse(self, x);
            if lo != hi {
                if let Some(new_hi) = self.reinsert_comment_at(BytePos(hi)) {
                    x.set_span(x.span().with_hi(new_hi));
                }
            }
            x
        }
    };
}

impl traverse::Traversal for CommentTraverser {
    reinsert_and_traverse!(traverse_stmt, Stmt, traverse::traverse_stmt_def);
    reinsert_and_traverse!(traverse_expr, Expr, traverse::traverse_expr_def);
    reinsert_and_traverse!(
        traverse_trait_item,
        TraitItem,
        traverse::traverse_trait_item_def
    );
    reinsert_and_traverse!(
        traverse_impl_item,
        ImplItem,
        traverse::traverse_impl_item_def
    );
    reinsert_and_traverse!(traverse_block, Block, traverse::traverse_block_def);
    reinsert_and_traverse!(traverse_local, Local, traverse::traverse_local_def);
    reinsert_and_traverse!(traverse_field, FieldValue, traverse::traverse_field_def);
    reinsert_and_traverse!(traverse_item, Item, traverse::traverse_item_def);

    fn traverse_foreign_item(&mut self, mut i: ForeignItem) -> ForeignItem {
        i.set_span(pos_to_span(
            self.reinsert_comment_at(i.span().lo())
                .unwrap_or(BytePos(0)),
        ));
        i
    }
}

pub fn insert_comment_attrs(attrs: &mut Vec<Attribute>, new_comments: SmallVec<[&str; 1]>) {
    attrs.reserve(new_comments.len());
    let eq: syn::Token![=] = Default::default();
    fn make_comment_path() -> Path {
        let mut segments = punctuated::Punctuated::new();
        segments.push(PathSegment {
            ident: Ident::new("comment", Span::call_site()),
            arguments: PathArguments::None,
        });
        Path {
            leading_colon: None,
            segments,
        }
    }

    for c in new_comments {
        let lit = Lit::new(proc_macro2::Literal::string(c));
        let mut tokens = TokenStream::new();
        eq.to_tokens(&mut tokens);
        lit.to_tokens(&mut tokens);
        let attr = Attribute {
            pound_token: Default::default(),
            style: AttrStyle::Inner(Default::default()),
            bracket_token: Default::default(),
            meta: Meta::Path(make_comment_path()),
        };
        attrs.push(attr);
    }
}
