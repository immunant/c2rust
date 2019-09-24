use std::collections::HashMap;

use rustc_target::spec::abi::Abi;
use syntax::ast::*;
use syntax::parse::token::{DelimToken, Nonterminal, Token};
use syntax::parse::token::{Lit as TokenLit, LitKind as TokenLitKind};
use syntax::source_map::{Span, SyntaxContext};
use syntax::tokenstream::{DelimSpan, TokenStream, TokenTree};
use syntax::ThinVec;

use std::rc::Rc;
use syntax::attr;
use syntax::ptr::P;
use syntax::source_map::Spanned;
use syntax::visit::Visitor;
use syntax_pos::Symbol;

use crate::ast_manip::Visit;
use crate::ast_manip::{GetNodeId, GetSpan};

use super::root_callsite_span;

#[derive(Clone, Copy, Debug)]
pub enum MacNodeRef<'a> {
    Expr(&'a Expr),
    Pat(&'a Pat),
    Ty(&'a Ty),

    Item(&'a Item),
    ImplItem(&'a ImplItem),
    TraitItem(&'a TraitItem),
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
            MacNodeRef::ImplItem(x) => x.id,
            MacNodeRef::TraitItem(x) => x.id,
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
    as_impl_item(ImplItem),
    as_trait_item(TraitItem),
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
    Item, ImplItem, TraitItem, ForeignItem,
    Stmt,
}

impl<T: AsMacNodeRef + 'static> AsMacNodeRef for P<T> {
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
            MacNodeRef::ImplItem(x) => v.visit_impl_item(x),
            MacNodeRef::TraitItem(x) => v.visit_trait_item(x),
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
    Mac(&'ast Mac),
    ItemAttr(&'ast Item),
    /// This is the generated item part of a `#[derive]`'s output.  The `InvocId` points to the
    /// originating `ItemAttr`.
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
        &unexpanded.module.items[..],
        &expanded.module.items[inj_count..],
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
    T: CollectMacros + MaybeInvoc + GetNodeId + GetSpan + AsMacNodeRef,
{
    let mut j = 0;

    for old in old_seq {
        if let Some(invoc) = old.as_invoc() {
            let invoc_id = cx.record_invoc(invoc);
            trace!(
                "new {:?} from macro {:?} at {:?}",
                invoc_id,
                old.get_node_id(),
                old.get_span()
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
                    if let InvocKind::ItemAttr(_) = invoc {
                        CollectMacros::collect_macros(old, new, cx);
                    }
                }
                cx.record_node_id_match(old.get_node_id(), new.get_node_id());
                j += 1;
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
    match invoc {
        InvocKind::ItemAttr(..) => {
            if let MacNodeRef::Item(i) = new {
                if attr::contains_name(&i.attrs, Symbol::intern("automatically_derived")) {
                    return Some(InvocKind::Derive(id));
                }
            }
        }
        _ => {}
    }
    None
}

trait CollectMacros {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>);
}

include!(concat!(env!("OUT_DIR"), "/mac_table_gen.inc.rs"));

impl<T: CollectMacros> CollectMacros for Rc<T> {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        <T as CollectMacros>::collect_macros(old, new, cx);
    }
}

impl<T: CollectMacros> CollectMacros for P<T> {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        <T as CollectMacros>::collect_macros(old, new, cx);
    }
}

impl<T: CollectMacros> CollectMacros for Spanned<T> {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        <T as CollectMacros>::collect_macros(&old.node, &new.node, cx);
    }
}

impl<T: CollectMacros> CollectMacros for Option<T> {
    fn collect_macros<'a>(old: &'a Self, new: &'a Self, cx: &mut Ctxt<'a>) {
        match (old, new) {
            (&Some(ref old), &Some(ref new)) => {
                <T as CollectMacros>::collect_macros(old, new, cx);
            }
            (&None, &None) => {}
            (_, _) => panic!("mismatch between unexpanded and expanded ASTs"),
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

trait MaybeInvoc {
    fn as_invoc(&self) -> Option<InvocKind>;
}

impl MaybeInvoc for Expr {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.node {
            ExprKind::Mac(ref mac) => Some(InvocKind::Mac(mac)),
            _ => None,
        }
    }
}

impl MaybeInvoc for Pat {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.node {
            PatKind::Mac(ref mac) => Some(InvocKind::Mac(mac)),
            _ => None,
        }
    }
}

impl MaybeInvoc for Ty {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.node {
            TyKind::Mac(ref mac) => Some(InvocKind::Mac(mac)),
            _ => None,
        }
    }
}

impl MaybeInvoc for Item {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.node {
            ItemKind::Mac(ref mac) => Some(InvocKind::Mac(mac)),
            _ => {
                if attr::contains_name(&self.attrs, Symbol::intern("derive"))
                    || attr::contains_name(&self.attrs, Symbol::intern("cfg"))
                    || attr::contains_name(&self.attrs, Symbol::intern("test"))
                {
                    Some(InvocKind::ItemAttr(self))
                } else {
                    None
                }
            }
        }
    }
}

impl MaybeInvoc for ImplItem {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.node {
            ImplItemKind::Macro(ref mac) => Some(InvocKind::Mac(mac)),
            _ => None,
        }
    }
}

impl MaybeInvoc for TraitItem {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.node {
            TraitItemKind::Macro(ref mac) => Some(InvocKind::Mac(mac)),
            _ => None,
        }
    }
}

impl MaybeInvoc for ForeignItem {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.node {
            ForeignItemKind::Macro(ref mac) => Some(InvocKind::Mac(mac)),
            _ => None,
        }
    }
}

impl MaybeInvoc for Stmt {
    fn as_invoc(&self) -> Option<InvocKind> {
        match self.node {
            StmtKind::Mac(ref mac) => Some(InvocKind::Mac(&mac.0)),
            _ => None,
        }
    }
}

impl<T: MaybeInvoc> MaybeInvoc for P<T> {
    fn as_invoc(&self) -> Option<InvocKind> {
        <T as MaybeInvoc>::as_invoc(self)
    }
}
