//! Handles collapsing of macros, turning `std::io::_print(...)` into `println!(...)` and so on.
//!
//! This also computes edges to add to `NodeMap`.  Most of these are identity edges (mapping each
//! node's `x.id -> x.id`), but in cases where nonterminals get merged we do something a bit more
//! interesting (see `token_rewrite_map` for details).  Edges for different categories of nodes get
//! added in different places: macro invocations in `CollapseMacros`, macro arguments in
//! `token_rewrite_map`, and nodes outside of macros in `ReplaceTokens`.
use crate::ast_builder::mk;
use log::{debug, trace, warn};
use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_ast::token::{Nonterminal, Token, TokenKind};
use rustc_ast::tokenstream::{self, Spacing, TokenStream, TokenTree};
use rustc_ast::*;
use rustc_data_structures::sync::Lrc;
use rustc_span::source_map::{BytePos, Span};
use rustc_span::sym;
use smallvec::smallvec;
use smallvec::SmallVec;
use std::collections::{BTreeMap, HashMap, HashSet};

use super::mac_table::{InvocId, InvocKind, MacTable};
use super::nt_match::{self, NtMatch};
use super::root_callsite_span;

use crate::ast_manip::{AstEquiv, ListNodeIds, MutVisit};
use crate::expect;

#[derive(Clone, Debug)]
struct RewriteItem {
    invoc_id: InvocId,
    span: Span,
    nt: Nonterminal,
}

/// Folder that replaces macro-generated nodes with their original macro invocations.  For example,
/// it turns `std::io::_print(...)` back into `println!(...)`.  It also (1) records a `RewriteItem`
/// for each nonterminal detected in the macro expansion, so that edits inside the expansion can be
/// converted to token-stream rewrites on the collapsed macro, and (2) records ID matches between
/// the transformed and collapsed versions of the macro.
struct CollapseMacros<'a> {
    mac_table: &'a MacTable<'a>,

    /// Used to keep track of which invocations we've seen, inside contexts where a macro might
    /// expand to multiple nodes.  The first node for each invocation gets replaced with the macro;
    /// the rest get discarded.
    seen_invocs: HashSet<InvocId>,

    token_rewrites: Vec<RewriteItem>,

    matched_ids: &'a mut Vec<(NodeId, NodeId)>,
}

impl<'a> CollapseMacros<'a> {
    fn collect_token_rewrites<T: NtMatch + ::std::fmt::Debug>(
        &mut self,
        invoc_id: InvocId,
        old: &T,
        new: &T,
    ) {
        trace!(
            "(invoc {:?}) record nts for {:?} -> {:?}",
            invoc_id,
            old,
            new
        );
        for (span, nt) in nt_match::match_nonterminals(old, new) {
            trace!(
                "  got {} at {:?}",
                rustc_ast_pretty::pprust::token_to_string(&Token {
                    kind: TokenKind::Interpolated(Lrc::new(nt.clone())),
                    span,
                }),
                span,
            );
            self.token_rewrites.push(RewriteItem { invoc_id, span, nt });
        }
    }

    fn record_matched_ids(&mut self, old: NodeId, new: NodeId) {
        self.matched_ids.push((old, new));
    }
}

impl<'a> MutVisitor for CollapseMacros<'a> {
    fn visit_expr(&mut self, e: &mut P<Expr>) {
        if let Some(info) = self.mac_table.get(e.id) {
            if let InvocKind::Mac(mac) = info.invoc {
                let old = info.expanded.as_expr().unwrap_or_else(|| {
                    panic!(
                        "replaced {:?} with {:?} which is a different type?",
                        e, info.expanded,
                    )
                });
                self.collect_token_rewrites(info.id, old, &e as &Expr);
                let new_e = mk().id(e.id).span(root_callsite_span(e.span)).mac_expr(mac);
                trace!("collapse: {:?} -> {:?}", e, new_e);
                self.record_matched_ids(e.id, new_e.id);
                *e = new_e;
            } else {
                warn!("bad macro kind for expr: {:?}", info.invoc);
            }
        }
        mut_visit::noop_visit_expr(e, self)
    }

    fn visit_pat(&mut self, p: &mut P<Pat>) {
        if let Some(info) = self.mac_table.get(p.id) {
            if let InvocKind::Mac(mac) = info.invoc {
                let old = info.expanded.as_pat().unwrap_or_else(|| {
                    panic!(
                        "replaced {:?} with {:?} which is a different type?",
                        p, info.expanded,
                    )
                });
                self.collect_token_rewrites(info.id, old, &p as &Pat);
                let new_p = mk().id(p.id).span(root_callsite_span(p.span)).mac_pat(mac);
                trace!("collapse: {:?} -> {:?}", p, new_p);
                self.record_matched_ids(p.id, new_p.id);
                *p = new_p;
            } else {
                warn!("bad macro kind for pat: {:?}", info.invoc);
            }
        }
        mut_visit::noop_visit_pat(p, self)
    }

    fn visit_ty(&mut self, t: &mut P<Ty>) {
        if let Some(info) = self.mac_table.get(t.id) {
            if let InvocKind::Mac(mac) = info.invoc {
                let old = info.expanded.as_ty().unwrap_or_else(|| {
                    panic!(
                        "replaced {:?} with {:?} which is a different type?",
                        t, info.expanded,
                    )
                });
                self.collect_token_rewrites(info.id, old, &t as &Ty);
                let new_t = mk().id(t.id).span(root_callsite_span(t.span)).mac_ty(mac);
                trace!("collapse: {:?} -> {:?}", t, new_t);
                self.record_matched_ids(t.id, new_t.id);
                *t = new_t;
            } else {
                warn!("bad macro kind for ty: {:?}", info.invoc);
            }
        }
        mut_visit::noop_visit_ty(t, self)
    }

    fn flat_map_stmt(&mut self, mut s: Stmt) -> SmallVec<[Stmt; 1]> {
        if let Some(info) = self.mac_table.get(s.id) {
            match info.invoc {
                InvocKind::Mac(mac) => {
                    let old = info.expanded.as_stmt().unwrap_or_else(|| {
                        panic!(
                            "replaced {:?} with {:?} which is a different type?",
                            s, info.expanded,
                        )
                    });
                    self.collect_token_rewrites(info.id, old, &s as &Stmt);

                    if !self.seen_invocs.contains(&info.id) {
                        self.seen_invocs.insert(info.id);
                        let new_s = mk().id(s.id).span(root_callsite_span(s.span)).mac_stmt(mac);
                        self.record_matched_ids(s.id, new_s.id);
                        trace!("collapse: {:?} -> {:?}", s, new_s);
                        s = new_s;
                    } else {
                        trace!("collapse (duplicate): {:?} -> /**/", s);
                        return smallvec![];
                    }
                }
                InvocKind::Attrs(attrs) => {
                    trace!("Attrs: return original: {:?}", s);
                    match &mut s.kind {
                        StmtKind::Local(l) => {
                            let mut new_attrs = l.attrs.to_vec();
                            restore_attrs(&mut new_attrs, attrs);
                            l.attrs = new_attrs.into();
                        }
                        StmtKind::Item(i) => restore_attrs(&mut i.attrs, attrs),
                        StmtKind::Expr(e) | StmtKind::Semi(e) => {
                            let mut new_attrs = e.attrs.to_vec();
                            restore_attrs(&mut new_attrs, attrs);
                            e.attrs = new_attrs.into();
                        }
                        StmtKind::MacCall(..) => {}
                        StmtKind::Empty => {}
                    }
                    self.record_matched_ids(s.id, s.id);
                }
                InvocKind::Derive(_parent_invoc_id) => {
                    trace!("Derive: drop (generated): {:?} -> /**/", s);
                    return smallvec![];
                }
            }
        }
        mut_visit::noop_flat_map_stmt(s, self)
    }

    fn flat_map_item(&mut self, mut i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        if let Some(info) = self.mac_table.get(i.id) {
            match info.invoc {
                InvocKind::Mac(mac) => {
                    let old = info.expanded.as_item().unwrap_or_else(|| {
                        panic!(
                            "replaced {:?} with {:?} which is a different type?",
                            i, info.expanded,
                        )
                    });
                    self.collect_token_rewrites(info.id, old, &i as &Item);

                    if !self.seen_invocs.contains(&info.id) {
                        self.seen_invocs.insert(info.id);
                        let new_i = mk().id(i.id).span(root_callsite_span(i.span)).mac_item(mac);
                        trace!("collapse: {:?} -> {:?}", i, new_i);
                        self.record_matched_ids(i.id, new_i.id);
                        i = new_i;
                    } else {
                        trace!("collapse (duplicate): {:?} -> /**/", i);
                        return smallvec![];
                    }
                }
                InvocKind::Attrs(attrs) => {
                    trace!("Attrs: return original: {:?}", i);
                    restore_attrs(&mut i.attrs, attrs);
                    self.record_matched_ids(i.id, i.id);
                }
                InvocKind::Derive(_parent_invoc_id) => {
                    trace!("Derive: drop (generated): {:?} -> /**/", i);
                    return smallvec![];
                }
            }
        }
        mut_visit::noop_flat_map_item(i, self)
    }

    fn flat_map_impl_item(&mut self, ii: P<AssocItem>) -> SmallVec<[P<AssocItem>; 1]> {
        if let Some(info) = self.mac_table.get(ii.id) {
            if let InvocKind::Mac(mac) = info.invoc {
                let old = info.expanded.as_assoc_item().unwrap_or_else(|| {
                    panic!(
                        "replaced {:?} with {:?} which is a different type?",
                        ii, info.expanded,
                    )
                });
                self.collect_token_rewrites(info.id, old, &ii as &AssocItem);

                if !self.seen_invocs.contains(&info.id) {
                    self.seen_invocs.insert(info.id);
                    let new_ii = mk()
                        .id(ii.id)
                        .span(root_callsite_span(ii.span))
                        .mac_impl_item(mac);
                    trace!("collapse: {:?} -> {:?}", ii, new_ii);
                    self.record_matched_ids(ii.id, new_ii.id);
                    return smallvec![new_ii];
                } else {
                    trace!("collapse (duplicate): {:?} -> /**/", ii);
                    return smallvec![];
                }
            } else {
                warn!("bad macro kind for assoc item: {:?}", info.invoc);
            }
        }
        mut_visit::noop_flat_map_assoc_item(ii, self)
    }

    fn flat_map_trait_item(&mut self, ti: P<AssocItem>) -> SmallVec<[P<AssocItem>; 1]> {
        if let Some(info) = self.mac_table.get(ti.id) {
            if let InvocKind::Mac(mac) = info.invoc {
                let old = info.expanded.as_assoc_item().unwrap_or_else(|| {
                    panic!(
                        "replaced {:?} with {:?} which is a different type?",
                        ti, info.expanded,
                    )
                });
                self.collect_token_rewrites(info.id, old, &ti as &AssocItem);

                if !self.seen_invocs.contains(&info.id) {
                    self.seen_invocs.insert(info.id);
                    let new_ti = mk()
                        .id(ti.id)
                        .span(root_callsite_span(ti.span))
                        .mac_trait_item(mac);
                    trace!("collapse: {:?} -> {:?}", ti, new_ti);
                    self.record_matched_ids(ti.id, new_ti.id);
                    return smallvec![new_ti];
                } else {
                    trace!("collapse (duplicate): {:?} -> /**/", ti);
                    return smallvec![];
                }
            } else {
                warn!("bad macro kind for trait item: {:?}", info.invoc);
            }
        }
        mut_visit::noop_flat_map_assoc_item(ti, self)
    }

    fn flat_map_foreign_item(&mut self, fi: P<ForeignItem>) -> SmallVec<[P<ForeignItem>; 1]> {
        if let Some(info) = self.mac_table.get(fi.id) {
            if let InvocKind::Mac(mac) = info.invoc {
                let old = info.expanded.as_foreign_item().unwrap_or_else(|| {
                    panic!(
                        "replaced {:?} with {:?} which is a different type?",
                        fi, info.expanded,
                    )
                });
                self.collect_token_rewrites(info.id, old, &fi as &ForeignItem);

                if !self.seen_invocs.contains(&info.id) {
                    self.seen_invocs.insert(info.id);
                    let new_fi = mk()
                        .id(fi.id)
                        .span(root_callsite_span(fi.span))
                        .mac_foreign_item(mac);
                    trace!("collapse: {:?} -> {:?}", fi, new_fi);
                    self.record_matched_ids(fi.id, new_fi.id);
                    return smallvec![new_fi];
                } else {
                    trace!("collapse (duplicate): {:?} -> /**/", fi);
                    return smallvec![];
                }
            } else {
                warn!("bad macro kind for trait item: {:?}", info.invoc);
            }
        }
        mut_visit::noop_flat_map_foreign_item(fi, self)
    }

    fn visit_mac_call(&mut self, mac: &mut MacCall) {
        mut_visit::noop_visit_mac(mac, self)
    }
}

/// Undo changes to attributes that occurred during macro expansion.  `old` is unexpanded and `new`
/// is transformed/collapsed.  Since additional attributes could have been added/removed by the
/// transform, we can't just copy `old.attrs`.  Instead, we look through `old.attrs` for attributes
/// with known effects (such as `#[cfg]`, which removes itself when the condition is met) and tries
/// to reverse those specific effects on `new.attrs`.
fn restore_attrs(new_attrs: &mut Vec<Attribute>, old_attrs: &[Attribute]) {
    // If the original item had a `#[derive]` attr, transfer it to the new one.
    // TODO: handle multiple instances of `#[derive]`
    // TODO: try to keep attrs in the same order
    if let Some(attr) = crate::util::find_by_name(old_attrs, sym::derive) {
        if !crate::util::contains_name(&new_attrs, sym::derive) {
            new_attrs.push(attr.clone());
        }
    }

    if let Some(attr) = crate::util::find_by_name(old_attrs, sym::cfg) {
        if !crate::util::contains_name(&new_attrs, sym::cfg) {
            new_attrs.push(attr.clone());
        }
    }

    new_attrs.retain(|attr| {
        // TODO: don't erase user-written #[structural_match] attrs
        // (It can be written explicitly, but is also inserted by #[derive(Eq)].)
        !attr.has_name(sym::structural_match)
    });
}

fn spans_overlap(sp1: Span, sp2: Span) -> bool {
    sp1.lo() < sp2.hi() && sp2.lo() < sp1.hi()
}

/// Index the `RewriteItem`s collected by the `CollapseMacros` folder by start position, so they
/// can be looked up efficiently during token rewriting.
///
/// As a side effect, this also updates `matched_ids` for all nonterminals in `vec`.  Normally each
/// nonterminal gets its IDs mapped to themselves (recursively), because we don't generally
/// renumber nodes during collapsing (i.e., the "old" and "new" NodeId namespaces are essentially
/// identical).  But in cases where two identical nonterminals get merged (example below), the
/// second instance gets discarded, and all its IDs get mapped to the IDs of the first instance.
///
/// ## Nonterminal merging example
///
///  1. Unexpanded: `some_macro!(2 + 2)` - The original token string is `2` `+` `2`.
///  2. Expanded: `(2 + 2)#1 * (2 + 2)#2` - The same tokens are used to produce two different nodes.
///     These nodes have different `NodeId`s (the `#1` and `#2` markers), but both have the same
///     `Span`, that of the original token string.
///  3. Transformed: `4#1 * 4#2` - `nt_match` matches each `4` to the corresponding expanded `2 + 2`.
///  4. Collapsed: `some_macro!(4)` - This is the interesting part.  The token string `2` `+` `2`
///     should be replaced with a nonterminal `4#1` for the first occurrence, but it should also be
///     replaced by a different `4#2` for the second occurrence, differing only in `NodeId`.  This
///     is where merging happens: we replace the tokens only with `4#1`, discarding the `4#2`
///     replacement, but update the ID map with both mappings `1 -> 1` and `2 -> 1`.  This way, the
///     `4#1` in the collapsed AST inherits all the marks and other annotations that applied to
///     either the `4#1` or the `4#2` copy.
fn token_rewrite_map(
    vec: Vec<RewriteItem>,
    matched_ids: &mut Vec<(NodeId, NodeId)>,
) -> BTreeMap<BytePos, RewriteItem> {
    // Map from `span.lo()` to the full RewriteItem.  Invariant: all spans in `map` are
    // nonoverlapping and nonempty.  This means we can check if a new span overlaps any old span by
    // examining only the entries just before and just after the new span's `lo()` position.
    let mut map: BTreeMap<BytePos, RewriteItem> = BTreeMap::new();

    for item in vec {
        if item.span.lo() == item.span.hi() {
            warn!("macro rewrite has empty span {:?}", item.span);
            continue;
        }

        let key = item.span.lo();
        if let Some((_, before)) = map.range(..key).next_back() {
            if spans_overlap(item.span, before.span) {
                warn!(
                    "macro rewrite at {:?} overlaps rewrite at {:?}",
                    item.span, before.span
                );
                continue;
            }
        }
        if let Some((_, after)) = map.range(key..).next() {
            if item.span == after.span && AstEquiv::ast_equiv(&item.nt, &after.nt) {
                // Both rewrites replace the same old tokens with the same new AST, so we can just
                // drop the second one.

                // `item` in the old/transformed AST is now known by `after`'s ID in the
                // new/collapsed AST.
                matched_ids.extend(
                    item.nt
                        .list_node_ids()
                        .into_iter()
                        .zip(after.nt.list_node_ids().into_iter()),
                );
                continue;
            } else if spans_overlap(item.span, after.span) {
                warn!(
                    "macro rewrite at {:?} overlaps rewrite at {:?}",
                    item.span, after.span
                );
                continue;
            }
        }

        // `item` in the old/transformed AST keeps the same ID in the new/collapsed AST.
        matched_ids.extend(item.nt.list_node_ids().into_iter().map(|x| (x, x)));
        map.insert(key, item);
    }

    map
}

fn rewrite_tokens(
    invoc_id: InvocId,
    tts: tokenstream::Cursor,
    rewrites: &mut BTreeMap<BytePos, RewriteItem>,
) -> TokenStream {
    let mut new_tts = Vec::new();
    let mut ignore_until = None;

    for tt in tts {
        if let Some(end) = ignore_until {
            if tt.span().lo() >= end {
                // Previous ignore is now over.
                ignore_until = None;
            } else {
                continue;
            }
        }

        if let Some(item) = rewrites.remove(&tt.span().lo()) {
            assert!(item.invoc_id == invoc_id);
            new_tts.push(TokenTree::Token(
                Token {
                    kind: TokenKind::Interpolated(Lrc::new(item.nt)),
                    span: item.span,
                },
                Spacing::Alone,
            ));
            ignore_until = Some(item.span.hi());
            continue;
        }

        match tt {
            TokenTree::Token(t, sp) => {
                new_tts.push(TokenTree::Token(t, sp));
            }
            TokenTree::Delimited(sp, delim, tts) => {
                new_tts.push(TokenTree::Delimited(
                    sp,
                    delim,
                    rewrite_tokens(invoc_id, tts.into_trees(), rewrites).into(),
                ));
            }
        }
    }

    new_tts.into_iter().collect()
}

fn convert_token_rewrites(
    rewrite_vec: Vec<RewriteItem>,
    mac_table: &MacTable,
    matched_ids: &mut Vec<(NodeId, NodeId)>,
) -> HashMap<InvocId, P<MacArgs>> {
    let mut rewrite_map = token_rewrite_map(rewrite_vec, matched_ids);
    let invoc_ids = rewrite_map
        .values()
        .map(|r| r.invoc_id)
        .collect::<HashSet<_>>();
    invoc_ids
        .into_iter()
        .filter_map(|invoc_id| {
            let invoc = mac_table
                .get_invoc(invoc_id)
                .expect("recorded token rewrites for nonexistent invocation?");
            if let InvocKind::Mac(mac) = invoc {
                let old_tts = mac.args.inner_tokens();
                let new_tts = rewrite_tokens(invoc_id, old_tts.into_trees(), &mut rewrite_map);
                let mut new_args = mac.args.clone();
                match *new_args {
                    MacArgs::Delimited(.., ref mut tokens) => *tokens = new_tts,
                    MacArgs::Eq(..) => todo!("Rewrite attribute macros"),
                    _ => {}
                }
                Some((invoc_id, new_args))
            } else {
                None
            }
        })
        .collect()
}

/// MutVisitor for updating the `TokenStream`s of macro invocations.  This is where we actually copy
/// the rewritten token streams produced by `convert_token_rewrites` into the AST.
///
/// As a side effect, this updates `matched_ids` with identity edges (`x.id -> x.id`) for any
/// remaining nodes that were unaffected by the collapsing.
struct ReplaceTokens<'a> {
    mac_table: &'a MacTable<'a>,
    new_args: HashMap<InvocId, P<MacArgs>>,
    matched_ids: &'a mut Vec<(NodeId, NodeId)>,
}

impl<'a> MutVisitor for ReplaceTokens<'a> {
    fn visit_expr(&mut self, e: &mut P<Expr>) {
        if let Some(invoc_id) = self.mac_table.get(e.id).map(|m| m.id) {
            if let Some(new_args) = self.new_args.get(&invoc_id).cloned() {
                // NB: Don't walk, so we never run `self.new_id` on `e.id`.  matched_ids entries
                // for macro invocations get handled by the CollapseMacros pass.
                expect!([e.kind] ExprKind::MacCall(ref mut mac) => mac.args = new_args);
            }
        }
        mut_visit::noop_visit_expr(e, self)
    }

    fn visit_pat(&mut self, p: &mut P<Pat>) {
        if let Some(invoc_id) = self.mac_table.get(p.id).map(|m| m.id) {
            if let Some(new_args) = self.new_args.get(&invoc_id).cloned() {
                expect!([p.kind] PatKind::MacCall(ref mut mac) => mac.args = new_args);
            }
        }
        mut_visit::noop_visit_pat(p, self)
    }

    fn visit_ty(&mut self, t: &mut P<Ty>) {
        if let Some(invoc_id) = self.mac_table.get(t.id).map(|m| m.id) {
            if let Some(new_args) = self.new_args.get(&invoc_id).cloned() {
                expect!([t.kind] TyKind::MacCall(ref mut mac) => mac.args = new_args);
            }
        }
        mut_visit::noop_visit_ty(t, self)
    }

    fn flat_map_stmt(&mut self, mut s: Stmt) -> SmallVec<[Stmt; 1]> {
        if let Some(invoc_id) = self.mac_table.get(s.id).map(|m| m.id) {
            if let Some(new_args) = self.new_args.get(&invoc_id).cloned() {
                expect!([s.kind] StmtKind::MacCall(ref mut mac) => mac.mac.args = new_args);
                return smallvec![s];
            }
        }
        mut_visit::noop_flat_map_stmt(s, self)
    }

    fn flat_map_item(&mut self, mut i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        if let Some(invoc_id) = self.mac_table.get(i.id).map(|m| m.id) {
            if let Some(new_args) = self.new_args.get(&invoc_id).cloned() {
                expect!([i.kind] ItemKind::MacCall(ref mut mac) => mac.args = new_args);
                return smallvec![i];
            }
        }
        mut_visit::noop_flat_map_item(i, self)
    }

    fn flat_map_impl_item(&mut self, ii: P<AssocItem>) -> SmallVec<[P<AssocItem>; 1]> {
        if let Some(invoc_id) = self.mac_table.get(ii.id).map(|m| m.id) {
            if let Some(new_args) = self.new_args.get(&invoc_id).cloned() {
                let mut ii = ii;
                expect!([ii.kind] AssocItemKind::MacCall(ref mut mac) => mac.args = new_args);
                return smallvec![ii];
            }
        }
        mut_visit::noop_flat_map_assoc_item(ii, self)
    }

    fn flat_map_trait_item(&mut self, ti: P<AssocItem>) -> SmallVec<[P<AssocItem>; 1]> {
        if let Some(invoc_id) = self.mac_table.get(ti.id).map(|m| m.id) {
            if let Some(new_args) = self.new_args.get(&invoc_id).cloned() {
                let mut ti = ti;
                expect!([ti.kind] AssocItemKind::MacCall(ref mut mac) => mac.args = new_args);
                return smallvec![ti];
            }
        }
        mut_visit::noop_flat_map_assoc_item(ti, self)
    }

    fn flat_map_foreign_item(&mut self, fi: P<ForeignItem>) -> SmallVec<[P<ForeignItem>; 1]> {
        if let Some(invoc_id) = self.mac_table.get(fi.id).map(|m| m.id) {
            if let Some(new_args) = self.new_args.get(&invoc_id).cloned() {
                let mut fi = fi;
                expect!([fi.kind] ForeignItemKind::MacCall(ref mut mac) => mac.args = new_args);
                return smallvec![fi];
            }
        }
        mut_visit::noop_flat_map_foreign_item(fi, self)
    }

    fn visit_mac_call(&mut self, mac: &mut MacCall) {
        mut_visit::noop_visit_mac(mac, self)
    }

    fn visit_id(&mut self, i: &mut NodeId) {
        self.matched_ids.push((*i, *i));
    }
}

pub fn collapse_macros(krate: &mut Crate, mac_table: &MacTable) -> Vec<(NodeId, NodeId)> {
    let mut matched_ids = Vec::new();

    let token_rewrites: Vec<RewriteItem>;
    {
        let mut collapse_macros = CollapseMacros {
            mac_table,
            seen_invocs: HashSet::new(),
            token_rewrites: Vec::new(),
            matched_ids: &mut matched_ids,
        };
        krate.visit(&mut collapse_macros);
        token_rewrites = collapse_macros.token_rewrites;
    }

    let new_args = convert_token_rewrites(token_rewrites, mac_table, &mut matched_ids);
    for (k, v) in &new_args {
        debug!(
            "new tokens for {:?} = {:?}",
            k,
            rustc_ast_pretty::pprust::tts_to_string(&v.inner_tokens())
        );
    }

    krate.visit(&mut ReplaceTokens {
        mac_table,
        new_args,
        matched_ids: &mut matched_ids,
    });

    matched_ids
}
