use log::trace;
use std::collections::HashMap;

use rustc_ast::token::{BinOpToken, CommentKind, Delimiter, Nonterminal, Token, TokenKind};
use rustc_ast::token::{Lit as TokenLit, LitKind as TokenLitKind};
use rustc_ast::tokenstream::{DelimSpan, LazyTokenStream, Spacing, TokenStream, TokenTree};
use rustc_ast::*;
use rustc_data_structures::thin_vec::ThinVec;
use rustc_span::source_map::{Span, SyntaxContext};
use rustc_span::symbol::Ident;
use rustc_target::spec::abi::Abi;

use rustc_ast::ptr::P;
use rustc_ast::visit::{AssocCtxt, Visitor};
use rustc_span::source_map::Spanned;
use rustc_span::{sym, Symbol};
use std::fmt::Debug;
use std::rc::Rc;

use crate::ast_manip::util::path_eq;
use crate::ast_manip::Visit;
use crate::ast_manip::{GetNodeId, GetSpan};

use super::root_callsite_span;

#[derive(Clone, Copy, Debug)]
pub enum MacNodeRef<'a> {
    Expr(&'a Expr),
    Pat(&'a Pat),
    Ty(&'a Ty),

    Item(&'a Item),
    AssocItem(&'a AssocItem),
    ForeignItem(&'a ForeignItem),

    Stmt(&'a Stmt),
}

impl<'a> MacNodeRef<'a> {
    pub fn id(&self) -> NodeId {
        match *self {
            MacNodeRef::Expr(x) => x.id,
            MacNodeRef::Pat(x) => x.id,
            MacNodeRef::Ty(x) => x.id,
            MacNodeRef::Item(x) => x.id,
            MacNodeRef::AssocItem(x) => x.id,
            MacNodeRef::ForeignItem(x) => x.id,
            MacNodeRef::Stmt(x) => x.id,
        }
    }
}

macro_rules! mac_node_ref_getters {
    ($($as_ty:ident($Ty:ident),)*) => {
        impl<'a> MacNodeRef<'a> {
            $(
                pub fn $as_ty(self) -> Option<&'a $Ty> {
                    match self {
                        MacNodeRef::$Ty(x) => Some(x),
                        _ => None,
                    }
                }
            )*
        }
    };
}

mac_node_ref_getters! {
    as_expr(Expr),
    as_pat(Pat),
    as_ty(Ty),
    as_item(Item),
    as_assoc_item(AssocItem),
    as_foreign_item(ForeignItem),
    as_stmt(Stmt),
}

pub trait AsMacNodeRef: Clone + Sized {
    fn as_mac_node_ref<'a>(&'a self) -> MacNodeRef<'a>;
    fn from_mac_node_ref<'a>(r: MacNodeRef<'a>) -> &'a Self;
    fn clone_from_mac_node_ref<'a>(r: MacNodeRef<'a>) -> Self {
        (*Self::from_mac_node_ref(r)).clone()
    }
}

macro_rules! as_mac_node_ref_impls {
    ($($Ty:ident,)*) => {
        $(
            impl AsMacNodeRef for $Ty {
                fn as_mac_node_ref<'a>(&'a self) -> MacNodeRef<'a> {
                    MacNodeRef::$Ty(self)
                }

                fn from_mac_node_ref<'a>(r: MacNodeRef<'a>) -> &'a Self {
                    match r {
                        MacNodeRef::$Ty(r) => r,
                        _ => panic!(concat!("bad MacNodeRef kind (expected ",
                                            stringify!($Ty), ")")),
                    }
                }
            }
        )*
    };
}

as_mac_node_ref_impls! {
    Expr, Pat, Ty,
    Item, AssocItem, ForeignItem,
    Stmt,
}

impl<T: AsMacNodeRef + ?Sized + 'static> AsMacNodeRef for P<T> {
    fn as_mac_node_ref<'a>(&'a self) -> MacNodeRef<'a> {
        <T as AsMacNodeRef>::as_mac_node_ref(self)
    }

    fn from_mac_node_ref<'a>(_r: MacNodeRef<'a>) -> &'a Self {
        panic!("can't get reference to P<T>")
    }

    fn clone_from_mac_node_ref<'a>(r: MacNodeRef<'a>) -> Self {
        P(<T as AsMacNodeRef>::clone_from_mac_node_ref(r))
    }
}

impl<'a> Visit for MacNodeRef<'a> {
    fn visit<'b, V: Visitor<'b>>(&'b self, v: &mut V) {
        match *self {
            MacNodeRef::Expr(x) => v.visit_expr(x),
            MacNodeRef::Pat(x) => v.visit_pat(x),
            MacNodeRef::Ty(x) => v.visit_ty(x),
            MacNodeRef::Item(x) => v.visit_item(x),
            // TODO: do callees care if this is actually an AssocCtxt::Trait?
            MacNodeRef::AssocItem(x) => v.visit_assoc_item(x, AssocCtxt::Impl),
            MacNodeRef::ForeignItem(x) => v.visit_foreign_item(x),
            MacNodeRef::Stmt(x) => v.visit_stmt(x),
        }
    }
}

/// Unique identifier of a macro invocation.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct InvocId(pub u32);

#[derive(Clone, Copy, Debug)]
pub enum InvocKind<'ast> {
    Mac(&'ast MacCall),
    Attrs(&'ast [Attribute]),
    /// This is the generated item part of a `#[derive]`'s output.  The `InvocId` points to the
    /// originating `Attrs`.
    Derive(InvocId),
}

struct MacInfoRaw<'ast> {
    id: InvocId,
    expanded: MacNodeRef<'ast>,
}

pub struct MacInfo<'ast> {
    pub id: InvocId,
    pub invoc: InvocKind<'ast>,
    pub expanded: MacNodeRef<'ast>,
}

pub struct MacTable<'ast> {
    /// Maps expanded `NodeId` to macro invocation info.  Note that multiple nodes may map to the
    /// same invocation, when a macro produces multiple items/stmts.
    map: HashMap<NodeId, MacInfoRaw<'ast>>,

    /// For macro invocations that produced no items, this maps the unexpanded NodeId to the
    /// invocation ID.  `collapse::cfg` uses this to detect when nodes are removed due to `#[cfg]`
    /// attrs.
    pub empty_invocs: HashMap<NodeId, InvocId>,

    pub invoc_map: HashMap<InvocId, InvocKind<'ast>>,
}

impl<'ast> MacTable<'ast> {
    pub fn get(&self, id: NodeId) -> Option<MacInfo<'ast>> {
        self.map.get(&id).map(|raw| MacInfo {
            id: raw.id,
            invoc: self.invoc_map[&raw.id],
            expanded: raw.expanded,
        })
    }

    pub fn get_invoc(&self, id: InvocId) -> Option<InvocKind<'ast>> {
        self.invoc_map.get(&id).cloned()
    }

    pub fn invocations<'a>(&'a self) -> impl Iterator<Item = MacInfo<'ast>> + 'a {
        self.map.values().map(move |raw| MacInfo {
            id: raw.id,
            invoc: self.invoc_map[&raw.id],
            expanded: raw.expanded,
        })
    }
}

pub fn collect_macro_invocations<'ast>(
    unexpanded: &'ast Crate,
    expanded: &'ast Crate,
) -> (MacTable<'ast>, Vec<(NodeId, NodeId)>) {
    let mut ctxt = Ctxt::new();
    // Because `expanded` hasn't been transformed since it was macro expanded, we know that the
    // first `inj_count` items are the injected prelude and crate imports.
    let (crate_names, has_prelude) = super::injected_items(unexpanded);
    let inj_count = crate_names.len() + if has_prelude { 1 } else { 0 };
    collect_macros_seq(
        &unexpanded.items[..],
        &expanded.items[inj_count..],
        &mut ctxt,
    );
    (ctxt.table, ctxt.matched_node_ids)
}

struct Ctxt<'a> {
    table: MacTable<'a>,
    next_id: u32,
    matched_node_ids: Vec<(NodeId, NodeId)>,
}

impl<'a> Ctxt<'a> {
    fn new() -> Ctxt<'a> {
        Ctxt {
            table: MacTable {
                map: HashMap::new(),
                empty_invocs: HashMap::new(),
                invoc_map: HashMap::new(),
            },
            next_id: 0,
            matched_node_ids: Vec::new(),
        }
    }

    fn next_id(&mut self) -> InvocId {
        let id = self.next_id;
        self.next_id += 1;
        InvocId(id)
    }

    fn record_invoc(&mut self, invoc_kind: InvocKind<'a>) -> InvocId {
        let invoc_id = self.next_id();
        self.table.invoc_map.insert(invoc_id, invoc_kind);
        invoc_id
    }

    fn record_empty_invoc(&mut self, node_id: NodeId, invoc_id: InvocId) {
        self.table.empty_invocs.insert(node_id, invoc_id);
    }

    fn record_macro_with_id(&mut self, invoc_id: InvocId, expanded: MacNodeRef<'a>) {
        self.table.map.insert(
            expanded.id(),
            MacInfoRaw {
                id: invoc_id,
                expanded,
            },
        );
    }

    fn record_node_id_match(&mut self, old: NodeId, new: NodeId) {
        self.matched_node_ids.push((old, new));
    }

    fn record_one_macro(
        &mut self,
        old_id: NodeId,
        invoc_kind: InvocKind<'a>,
        expanded: MacNodeRef<'a>,
    ) {
        let invoc_id = self.record_invoc(invoc_kind);
        trace!(
            "new {:?} from macro {:?} - collect matching {:?}",
            invoc_id,
            old_id,
            expanded.id()
        );
        self.record_macro_with_id(invoc_id, expanded);
        self.record_node_id_match(old_id, expanded.id());
    }
}

fn is_macro_generated(sp: Span) -> bool {
    sp.source_callsite() != sp
}

fn collect_macros_seq<'a, T>(old_seq: &'a [T], new_seq: &'a [T], cx: &mut Ctxt<'a>)
where
    T: CollectMacros + MaybeInvoc + GetNodeId + GetSpan + AsMacNodeRef + Debug,
{
    let mut j = 0;

    for old in old_seq {
        if let Some(invoc) = old.as_invoc() {
            let invoc_id = cx.record_invoc(invoc);
            trace!(
                "new {:?} from macro {:?} at {:?}: {:?}",
                invoc_id,
                old.get_node_id(),
                old.get_span(),
                old,
            );

            let mut empty = true;
            while j < new_seq.len() {
                let new = &new_seq[j];

                if !old.get_span().contains(root_callsite_span(new.get_span())) {
                    // Reached a node that didn't come from this macro.
                    if empty {
                        cx.record_empty_invoc(old.get_node_id(), invoc_id);
                    }
                    break;
                }

                empty = false;

                trace!("  collect {:?} at {:?}", new.get_node_id(), new.get_span());

                // The node came from `invoc`, so consume and record the node.
                if let Some(child_invoc) = get_child_invoc(invoc, invoc_id, new.as_mac_node_ref()) {
                    let child_invoc_id = cx.record_invoc(child_invoc);
                    cx.record_macro_with_id(child_invoc_id, new.as_mac_node_ref());
                } else {
                    cx.record_macro_with_id(invoc_id, new.as_mac_node_ref());

                    // Recurse into children so they get added to the node map.
                    match invoc {
                        InvocKind::Attrs(..) => {
                            CollectMacros::collect_macros(old, new, cx);
                        }
                        _ => {}
                    }
                }
                cx.record_node_id_match(old.get_node_id(), new.get_node_id());
                j += 1;
            }

            if empty {
                cx.record_empty_invoc(old.get_node_id(), invoc_id);
            }
        } else {
            // For now, any time we see a node with a macro-generated span that wasn't eaten up by
            // the macro handling above, we assume it was created by a compiler plugin (such as
            // prelude injection or `#[derive]`), and ignore it.
            while j < new_seq.len() && is_macro_generated(new_seq[j].get_span()) {
                j += 1;
            }
            if j < new_seq.len() {
                CollectMacros::collect_macros(old, &new_seq[j], cx);
                j += 1;
            }
        }
    }

    assert!(
        j == new_seq.len(),
        "impossible: too many items in expanded sequence"
    );
}

fn get_child_invoc<'a>(
    invoc: InvocKind<'a>,
    id: InvocId,
    new: MacNodeRef<'a>,
) -> Option<InvocKind<'a>> {
    if is_derived(invoc, new) {
        Some(InvocKind::Derive(id))
    } else {
        None
    }
}

fn is_derived<'a>(invoc: InvocKind<'a>, new: MacNodeRef<'a>) -> bool {
    match invoc {
        InvocKind::Attrs(..) => {
            let attrs = match new {
                MacNodeRef::Item(i) => {
                    if is_structural_derive(i) {
                        return true;
                    }
                    Some(&i.attrs[..])
                }
                MacNodeRef::Stmt(s) => match &s.kind {
                    StmtKind::Local(l) => Some(&l.attrs[..]),
                    StmtKind::Item(i) => {
                        if is_structural_derive(i) {
                            return true;
                        }
                        Some(&i.attrs[..])
                    }
                    StmtKind::Expr(e) | StmtKind::Semi(e) => Some(&e.attrs[..]),
                    StmtKind::MacCall(..) => None,
                    StmtKind::Empty => None,
                },
                MacNodeRef::Expr(e) => Some(&e.attrs[..]),
                MacNodeRef::AssocItem(i) => Some(&i.attrs[..]),
                _ => None,
            };
            if let Some(attrs) = attrs {
                if crate::util::contains_name(attrs, sym::automatically_derived) {
                    return true;
                }
            }
        }
        _ => {}
    }
    false
}

fn is_structural_derive(i: &Item) -> bool {
    // This is a hack to work around the StructuralPartialEq impl
    // from derive(PartialEq) not being marked with an
    // automatically_derived attribute. TODO: remove this when
    // StructuralPartialEq is labeled with the right attribute.
    match &i.kind {
        ItemKind::Impl(box Impl {
            of_trait: Some(traitref),
            ..
        }) => {
            if path_eq(&traitref.path, &["$crate", "marker", "StructuralPartialEq"])
                || path_eq(&traitref.path, &["$crate", "marker", "StructuralEq"])
            {
                return true;
            }
        }
        _ => {}
    }
    false
}

trait CollectMacros {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>);
}

include!(concat!(env!("OUT_DIR"), "/mac_table_gen.inc.rs"));

impl<T: CollectMacros + ?Sized> CollectMacros for Rc<T> {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        <T as CollectMacros>::collect_macros(old, new, cx);
    }
}

impl<T: CollectMacros + ?Sized> CollectMacros for P<T> {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        <T as CollectMacros>::collect_macros(old, new, cx);
    }
}

impl<T: CollectMacros + ?Sized> CollectMacros for Box<T> {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        <T as CollectMacros>::collect_macros(old, new, cx);
    }
}

impl<T: CollectMacros> CollectMacros for Spanned<T> {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        <T as CollectMacros>::collect_macros(&old.node, &new.node, cx);
    }
}

impl<T: CollectMacros + Debug> CollectMacros for Option<T> {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        match (old, new) {
            (&Some(ref old), &Some(ref new)) => {
                <T as CollectMacros>::collect_macros(old, new, cx);
            }
            (&None, &None) => {}
            (_, _) => panic!(
                "mismatch between unexpanded and expanded ASTs \n  old: {:?}\n  new: {:?}",
                old, new
            ),
        }
    }
}

impl<A: CollectMacros, B: CollectMacros> CollectMacros for (A, B) {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        <A as CollectMacros>::collect_macros(&old.0, &new.0, cx);
        <B as CollectMacros>::collect_macros(&old.1, &new.1, cx);
    }
}

impl<A: CollectMacros, B: CollectMacros, C: CollectMacros> CollectMacros for (A, B, C) {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        <A as CollectMacros>::collect_macros(&old.0, &new.0, cx);
        <B as CollectMacros>::collect_macros(&old.1, &new.1, cx);
        <C as CollectMacros>::collect_macros(&old.2, &new.2, cx);
    }
}

impl<T: CollectMacros> CollectMacros for [T] {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        for (old_item, new_item) in old.iter().zip(new.iter()) {
            <T as CollectMacros>::collect_macros(old_item, new_item, cx);
        }
    }
}

impl<T: CollectMacros> CollectMacros for Vec<T> {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        <[T] as CollectMacros>::collect_macros(old, new, cx)
    }
}

impl<T: CollectMacros> CollectMacros for ThinVec<T> {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        <[T] as CollectMacros>::collect_macros(old, new, cx)
    }
}

// Custom impl for NodeIds.  We want to match up NodeIds between the unexpanded and expanded ASTs.
// This is not really part of MacTable construction, but we happen to also want NodeId matching
// every time we want a MacTable, and the two operations follow the same recursion scheme.
impl CollectMacros for NodeId {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        cx.record_node_id_match(*old, *new);
    }
}

fn has_macro_attr(attrs: &[Attribute]) -> bool {
    crate::util::contains_name(attrs, sym::derive)
        || crate::util::contains_name(attrs, sym::cfg)
        || crate::util::contains_name(attrs, sym::test)
}

trait MaybeInvoc {
    fn as_invoc(&self) -> Option<InvocKind>;
}

impl MaybeInvoc for Expr {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.kind {
            ExprKind::MacCall(ref mac) => Some(InvocKind::Mac(mac)),
            _ if has_macro_attr(&self.attrs) => Some(InvocKind::Attrs(&self.attrs)),
            _ => None,
        }
    }
}

impl MaybeInvoc for Pat {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.kind {
            PatKind::MacCall(ref mac) => Some(InvocKind::Mac(mac)),
            _ => None,
        }
    }
}

impl MaybeInvoc for Ty {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.kind {
            TyKind::MacCall(ref mac) => Some(InvocKind::Mac(mac)),
            _ => None,
        }
    }
}

impl MaybeInvoc for Item {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.kind {
            ItemKind::MacCall(ref mac) => Some(InvocKind::Mac(mac)),
            _ => {
                if has_macro_attr(&self.attrs) {
                    Some(InvocKind::Attrs(&self.attrs))
                } else {
                    None
                }
            }
        }
    }
}

impl MaybeInvoc for AssocItem {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.kind {
            AssocItemKind::MacCall(ref mac) => Some(InvocKind::Mac(mac)),
            _ => {
                if has_macro_attr(&self.attrs) {
                    Some(InvocKind::Attrs(&self.attrs))
                } else {
                    None
                }
            }
        }
    }
}

impl MaybeInvoc for ForeignItem {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.kind {
            ForeignItemKind::MacCall(ref mac) => Some(InvocKind::Mac(mac)),
            _ => None,
        }
    }
}

impl MaybeInvoc for Stmt {
    fn as_invoc(&self) -> Option<InvocKind> {
        match &self.kind {
            StmtKind::MacCall(mac) => Some(InvocKind::Mac(&mac.mac)),
            StmtKind::Local(l) if has_macro_attr(&l.attrs) => Some(InvocKind::Attrs(&l.attrs)),
            StmtKind::Item(i) if has_macro_attr(&i.attrs) => Some(InvocKind::Attrs(&i.attrs)),
            StmtKind::Expr(e) | StmtKind::Semi(e) if has_macro_attr(&e.attrs) => {
                Some(InvocKind::Attrs(&e.attrs))
            }
            _ => None,
        }
    }
}

impl<T: MaybeInvoc + ?Sized> MaybeInvoc for P<T> {
    fn as_invoc(&self) -> Option<InvocKind> {
        <T as MaybeInvoc>::as_invoc(self)
    }
}
