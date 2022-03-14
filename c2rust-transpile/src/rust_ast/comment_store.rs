//! This module handles accumulating / re-arranging comments for the Rust AST.
//!
//! Comments are stored as attributes on the AST node immediately following the comment.
//!
//! ```rust
//!   insert_comments(&mut expr.attrs, vec!["Some comment on an arm"]);
//! ```
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

use crate::rust_ast::{pos_to_span, traverse, SpanExt, DUMMY_SP, BytePos, set_span::SetSpan};
use itertools::Itertools;
use smallvec::{smallvec, SmallVec};
use std::collections::BTreeMap;
use std::default::Default;
use syn::spanned::Spanned as _;
use syn::__private::ToTokens;
use proc_macro2::{Span, TokenStream};
use syn::*;
use c2rust_ast_printer::pprust::comments;

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
        self.extend_existing_comments(lines, None)//, comments::CommentStyle::Isolated)
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
        fn translate_comment(comment: &String) -> String {
            comment
                .lines()
                .map(|line: &str| {
                    let mut line = line.to_owned();
                    let begin = line.trim_start();
                    if begin.starts_with("//!")
                        || begin.starts_with("///")
                    {
                        let begin_loc = line.len() - begin.len();
                        line.insert(2+begin_loc, ' ');
                    };
                    line
                })
                .join("\n")
                .replace("/**", "/* *")
                .replace("/*!", "/* !")
        }

        let lines: Vec<String> = lines.into_iter().map(translate_comment).collect();

        if lines.is_empty() {
            None
        } else {
            let new_comment = comments::Comment {
                //style,
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
            self.output_comments.entry(new).or_insert(SmallVec::new()).extend(comments);
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
                        warn!("Expected comments attached to the high end of span {:?}", span);
                        return span
                    },
                };
                self.output_comments.entry(span.lo()).or_insert(SmallVec::new()).extend(new_comments);
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
            let (lo, hi) = x.span().inner();
            x.set_span(pos_to_span(self.reinsert_comment_at(BytePos(lo)).unwrap_or(BytePos(0))));
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
    reinsert_and_traverse!(traverse_trait_item, TraitItem, traverse::traverse_trait_item_def);
    reinsert_and_traverse!(traverse_impl_item, ImplItem, traverse::traverse_impl_item_def);
    reinsert_and_traverse!(traverse_block, Block, traverse::traverse_block_def);
    reinsert_and_traverse!(traverse_local, Local, traverse::traverse_local_def);
    reinsert_and_traverse!(traverse_field, FieldValue, traverse::traverse_field_def);
    reinsert_and_traverse!(traverse_item, Item, traverse::traverse_item_def);

    fn traverse_foreign_item(&mut self, mut i: ForeignItem) -> ForeignItem {
        i.set_span(pos_to_span(self.reinsert_comment_at(i.span().lo()).unwrap_or(BytePos(0))));
        i
    }
}

macro_rules! with_fn_name {
    ($fn_name:tt, $what:ident, $What:ident) => {
        fn $fn_name (&mut self, node: &mut $What) {
            // Delegate to the default impl to visit nested expressions.
            let node_span = node.span();
            let joined = node_span.join(self.span);
            let tokens = node.into_token_stream();
            let contains = joined.map(|j| j.eq(&node_span)).unwrap_or(false);
            /* if the node's span is not changed by joining it with the span to find,
            then the span to find lies within the node's span and we should recurse */
            if contains {
                visit_mut::$fn_name(self, node);

                if let Some(comments) = self.comments.take() {
                    insert_comment_attrs(&mut node.attrs, comments);
                }
            }
        }
    }
}

macro_rules! traverse_bake_comments_for_node {
    ($what:ident, $What:ident, $visit_what_mut:ident) => {
        with_fn_name!($visit_what_mut, $what, $What);
    }
}



struct UnbakedComment<'a> {
    comments: Option<&'a [&'a str]>,
    span: Span,
}

use syn::visit_mut::VisitMut;
impl<'a> VisitMut for UnbakedComment<'a> {
    traverse_bake_comments_for_node!(variant, Variant, visit_variant_mut);
    traverse_bake_comments_for_node!(field, Field, visit_field_mut);
    traverse_bake_comments_for_node!(field_value, FieldValue, visit_field_value_mut);
    traverse_bake_comments_for_node!(derive_input, DeriveInput, visit_derive_input_mut);
    traverse_bake_comments_for_node!(expr_array, ExprArray, visit_expr_array_mut);
    traverse_bake_comments_for_node!(expr_assign, ExprAssign, visit_expr_assign_mut);
    traverse_bake_comments_for_node!(expr_assign_op, ExprAssignOp, visit_expr_assign_op_mut);
    traverse_bake_comments_for_node!(expr_async, ExprAsync, visit_expr_async_mut);
    traverse_bake_comments_for_node!(expr_await, ExprAwait, visit_expr_await_mut);
    traverse_bake_comments_for_node!(expr_binary, ExprBinary, visit_expr_binary_mut);
    traverse_bake_comments_for_node!(expr_block, ExprBlock, visit_expr_block_mut);
    traverse_bake_comments_for_node!(expr_box, ExprBox, visit_expr_box_mut);
    traverse_bake_comments_for_node!(expr_break, ExprBreak, visit_expr_break_mut);
    traverse_bake_comments_for_node!(expr_call, ExprCall, visit_expr_call_mut);
    traverse_bake_comments_for_node!(expr_cast, ExprCast, visit_expr_cast_mut);
    traverse_bake_comments_for_node!(expr_closure, ExprClosure, visit_expr_closure_mut);
    traverse_bake_comments_for_node!(expr_continue, ExprContinue, visit_expr_continue_mut);
    traverse_bake_comments_for_node!(expr_field, ExprField, visit_expr_field_mut);
    traverse_bake_comments_for_node!(expr_for_loop, ExprForLoop, visit_expr_for_loop_mut);
    traverse_bake_comments_for_node!(expr_group, ExprGroup, visit_expr_group_mut);
    traverse_bake_comments_for_node!(expr_if, ExprIf, visit_expr_if_mut);
    traverse_bake_comments_for_node!(expr_index, ExprIndex, visit_expr_index_mut);
    traverse_bake_comments_for_node!(expr_let, ExprLet, visit_expr_let_mut);
    traverse_bake_comments_for_node!(expr_lit, ExprLit, visit_expr_lit_mut);
    traverse_bake_comments_for_node!(expr_loop, ExprLoop, visit_expr_loop_mut);
    traverse_bake_comments_for_node!(expr_macro, ExprMacro, visit_expr_macro_mut);
    traverse_bake_comments_for_node!(expr_match, ExprMatch, visit_expr_match_mut);
    traverse_bake_comments_for_node!(expr_method_call, ExprMethodCall, visit_expr_method_call_mut);
    traverse_bake_comments_for_node!(expr_paren, ExprParen, visit_expr_paren_mut);
    traverse_bake_comments_for_node!(expr_path, ExprPath, visit_expr_path_mut);
    traverse_bake_comments_for_node!(expr_range, ExprRange, visit_expr_range_mut);
    traverse_bake_comments_for_node!(expr_reference, ExprReference, visit_expr_reference_mut);
    traverse_bake_comments_for_node!(expr_repeat, ExprRepeat, visit_expr_repeat_mut);
    traverse_bake_comments_for_node!(expr_return, ExprReturn, visit_expr_return_mut);
    traverse_bake_comments_for_node!(expr_struct, ExprStruct, visit_expr_struct_mut);
    traverse_bake_comments_for_node!(expr_try, ExprTry, visit_expr_try_mut);
    traverse_bake_comments_for_node!(expr_try_block, ExprTryBlock, visit_expr_try_block_mut);
    traverse_bake_comments_for_node!(expr_tuple, ExprTuple, visit_expr_tuple_mut);
    traverse_bake_comments_for_node!(expr_type, ExprType, visit_expr_type_mut);
    traverse_bake_comments_for_node!(expr_unary, ExprUnary, visit_expr_unary_mut);
    traverse_bake_comments_for_node!(expr_unsafe, ExprUnsafe, visit_expr_unsafe_mut);
    traverse_bake_comments_for_node!(expr_while, ExprWhile, visit_expr_while_mut);
    traverse_bake_comments_for_node!(expr_yield, ExprYield, visit_expr_yield_mut);
    traverse_bake_comments_for_node!(arm, Arm, visit_arm_mut);
    traverse_bake_comments_for_node!(type_param, TypeParam, visit_type_param_mut);
    traverse_bake_comments_for_node!(lifetime_def, LifetimeDef, visit_lifetime_def_mut);
    traverse_bake_comments_for_node!(const_param, ConstParam, visit_const_param_mut);
    traverse_bake_comments_for_node!(item_const, ItemConst, visit_item_const_mut);
    traverse_bake_comments_for_node!(item_enum, ItemEnum, visit_item_enum_mut);
    traverse_bake_comments_for_node!(item_extern_crate, ItemExternCrate, visit_item_extern_crate_mut);
    traverse_bake_comments_for_node!(item_fn, ItemFn, visit_item_fn_mut);
    traverse_bake_comments_for_node!(item_foreign_mod, ItemForeignMod, visit_item_foreign_mod_mut);
    traverse_bake_comments_for_node!(item_impl, ItemImpl, visit_item_impl_mut);
    traverse_bake_comments_for_node!(item_macro, ItemMacro, visit_item_macro_mut);
    traverse_bake_comments_for_node!(item_macro2, ItemMacro2, visit_item_macro2_mut);
    traverse_bake_comments_for_node!(item_mod, ItemMod, visit_item_mod_mut);
    traverse_bake_comments_for_node!(item_static, ItemStatic, visit_item_static_mut);
    traverse_bake_comments_for_node!(item_struct, ItemStruct, visit_item_struct_mut);
    traverse_bake_comments_for_node!(item_trait, ItemTrait, visit_item_trait_mut);
    traverse_bake_comments_for_node!(item_trait_alias, ItemTraitAlias, visit_item_trait_alias_mut);
    traverse_bake_comments_for_node!(item_type, ItemType, visit_item_type_mut);
    traverse_bake_comments_for_node!(item_union, ItemUnion, visit_item_union_mut);
    traverse_bake_comments_for_node!(item_use, ItemUse, visit_item_use_mut);
    traverse_bake_comments_for_node!(foreign_item_fn, ForeignItemFn, visit_foreign_item_fn_mut);
    traverse_bake_comments_for_node!(foreign_item_static, ForeignItemStatic, visit_foreign_item_static_mut);
    traverse_bake_comments_for_node!(foreign_item_type, ForeignItemType, visit_foreign_item_type_mut);
    traverse_bake_comments_for_node!(foreign_item_macro, ForeignItemMacro, visit_foreign_item_macro_mut);
    traverse_bake_comments_for_node!(trait_item_const, TraitItemConst, visit_trait_item_const_mut);
    traverse_bake_comments_for_node!(trait_item_method, TraitItemMethod, visit_trait_item_method_mut);
    traverse_bake_comments_for_node!(trait_item_type, TraitItemType, visit_trait_item_type_mut);
    traverse_bake_comments_for_node!(trait_item_macro, TraitItemMacro, visit_trait_item_macro_mut);
    traverse_bake_comments_for_node!(impl_item_const, ImplItemConst, visit_impl_item_const_mut);
    traverse_bake_comments_for_node!(impl_item_method, ImplItemMethod, visit_impl_item_method_mut);
    traverse_bake_comments_for_node!(impl_item_type, ImplItemType, visit_impl_item_type_mut);
    traverse_bake_comments_for_node!(impl_item_macro, ImplItemMacro, visit_impl_item_macro_mut);
    traverse_bake_comments_for_node!(receiver, Receiver, visit_receiver_mut);
    traverse_bake_comments_for_node!(file, File, visit_file_mut);
    traverse_bake_comments_for_node!(local, Local, visit_local_mut);
    traverse_bake_comments_for_node!(bare_fn_arg, BareFnArg, visit_bare_fn_arg_mut);
    traverse_bake_comments_for_node!(variadic, Variadic, visit_variadic_mut);
    traverse_bake_comments_for_node!(pat_box, PatBox, visit_pat_box_mut);
    traverse_bake_comments_for_node!(pat_ident, PatIdent, visit_pat_ident_mut);
    traverse_bake_comments_for_node!(pat_lit, PatLit, visit_pat_lit_mut);
    traverse_bake_comments_for_node!(pat_macro, PatMacro, visit_pat_macro_mut);
    traverse_bake_comments_for_node!(pat_or, PatOr, visit_pat_or_mut);
    traverse_bake_comments_for_node!(pat_path, PatPath, visit_pat_path_mut);
    traverse_bake_comments_for_node!(pat_range, PatRange, visit_pat_range_mut);
    traverse_bake_comments_for_node!(pat_reference, PatReference, visit_pat_reference_mut);
    traverse_bake_comments_for_node!(pat_rest, PatRest, visit_pat_rest_mut);
    traverse_bake_comments_for_node!(pat_slice, PatSlice, visit_pat_slice_mut);
    traverse_bake_comments_for_node!(pat_struct, PatStruct, visit_pat_struct_mut);
    traverse_bake_comments_for_node!(pat_tuple, PatTuple, visit_pat_tuple_mut);
    traverse_bake_comments_for_node!(pat_tuple_struct, PatTupleStruct, visit_pat_tuple_struct_mut);
    traverse_bake_comments_for_node!(pat_type, PatType, visit_pat_type_mut);
    traverse_bake_comments_for_node!(pat_wild, PatWild, visit_pat_wild_mut);
    traverse_bake_comments_for_node!(field_pat, FieldPat, visit_field_pat_mut);
}

fn insert_comment_attrs(attrs: &mut Vec<Attribute>, new_comments: &[&str]) {
    attrs.reserve(new_comments.len());
    let eq: syn::Token![=] = Default::default();
    fn make_comment_path() -> Path {
        let mut segments = punctuated::Punctuated::new();
        segments.push(PathSegment {
            ident: Ident::new("comment", DUMMY_SP),
            arguments: PathArguments::None,
        });
        Path {
            leading_colon: None,
            segments,
        }
    }

    for c in new_comments {
        let lit = Lit::new(proc_macro2::Literal::string(&*c));
        let mut tokens = TokenStream::new();
        eq.to_tokens(&mut tokens);
        lit.to_tokens(&mut tokens);
        let attr = Attribute {
            pound_token: Default::default(),
            style: AttrStyle::Inner(Default::default()),
            bracket_token: Default::default(),
            path: make_comment_path(),
            tokens,
        };
        attrs.push(attr);
    }
}

pub fn bake_comment_into_file(file: &mut File, comment_lines: &[&str], loc: Span) {
    let mut unbaked = UnbakedComment {
        comments: Some(comment_lines),
        span: loc,
    };
    debug!("inserting comment '{:?}'", comment_lines);
    unbaked.visit_file_mut(file);
    if unbaked.comments.is_some() {
        debug!("failed to insert comment '{:?}'", comment_lines);
    }
}