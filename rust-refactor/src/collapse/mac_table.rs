use std::collections::HashMap;

use syntax::ast::*;
use syntax::codemap::{Span, SyntaxContext};
use syntax::parse::token::{Token, DelimToken, Nonterminal};
use syntax::tokenstream::{TokenTree, Delimited, TokenStream, ThinTokenStream};
use rustc_target::spec::abi::Abi;

use std::rc::Rc;
use syntax::ptr::P;
use syntax::codemap::Spanned;

use ast_manip::{GetNodeId, GetSpan};


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


trait AsMacNodeRef {
    fn as_mac_node_ref<'a>(&'a self) -> MacNodeRef<'a>;
}

macro_rules! as_mac_node_ref_impls {
    ($($Ty:ident,)*) => {
        $(
            impl AsMacNodeRef for $Ty {
                fn as_mac_node_ref<'a>(&'a self) -> MacNodeRef<'a> {
                    MacNodeRef::$Ty(self)
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

impl<T: AsMacNodeRef> AsMacNodeRef for P<T> {
    fn as_mac_node_ref<'a>(&'a self) -> MacNodeRef<'a> {
        <T as AsMacNodeRef>::as_mac_node_ref(self)
    }
}


/// Unique identifier of a macro invocation.  We can't use `NodeId`s for this because `Mac` nodes
/// exist only in the unexpanded AST, where all IDs are DUMMY, and we can't use `Mark`s because we
/// have no way to obtain the mark for a macro that expands to one of its arguments unmodified.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct InvocId(pub u32);

pub struct MacInfo<'ast> {
    pub id: InvocId,
    pub mac: &'ast Mac,
    pub expanded: MacNodeRef<'ast>,
}

pub struct MacTable<'ast> {
    /// Maps expanded `NodeId` to macro invocation info.  Note that multiple nodes may map to the
    /// same invocation, when a macro produces multiple items/stmts.
    pub map: HashMap<NodeId, MacInfo<'ast>>,

    pub invoc_map: HashMap<InvocId, &'ast Mac>,
}

impl<'ast> MacTable<'ast> {
    pub fn get(&self, id: NodeId) -> Option<&MacInfo<'ast>> {
        self.map.get(&id)
    }

    pub fn get_invoc(&self, id: InvocId) -> Option<&'ast Mac> {
        self.invoc_map.get(&id).cloned()
    }
}

pub fn collect_macro_invocations<'ast>(unexpanded: &'ast Crate,
                                       expanded: &'ast Crate) -> MacTable<'ast> {
    let mut ctxt = Ctxt::new();
    // FIXME properly detect injected prelude
    CollectMacros::collect_macros(&unexpanded.module.items as &[_], 
                                  &expanded.module.items[2..],
                                  &mut ctxt);
    ctxt.table
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

    fn record_macro_with_id(&mut self,
                            invoc_id: InvocId,
                            mac: &'a Mac,
                            expanded: MacNodeRef<'a>) {
        self.table.map.insert(expanded.id(), MacInfo { id: invoc_id, mac, expanded });
        self.table.invoc_map.insert(invoc_id, mac);
    }

    fn record_node_id_match(&mut self,
                            old: NodeId,
                            new: NodeId) {
        self.matched_node_ids.push((old, new));
    }
}

fn record_one_macro<'a>(mac: &'a Mac, expanded: MacNodeRef<'a>, cx: &mut Ctxt<'a>) {
    let invoc_id = cx.next_id();
    cx.record_macro_with_id(invoc_id, mac, expanded);
}

fn root_callsite_span(sp: Span) -> Span {
    let callsite = sp.source_callsite();
    if callsite == sp {
        sp
    } else {
        root_callsite_span(callsite)
    }
}

fn collect_macros_seq<'a, T>(old_seq: &'a [T], new_seq: &'a [T], cx: &mut Ctxt<'a>)
        where T: CollectMacros + MaybeMac + GetSpan + AsMacNodeRef {
    let mut j = 0;

    for (i, old) in old_seq.iter().enumerate() {
        if let Some(mac) = old.as_mac() {
            let invoc_id = cx.next_id();

            while j < new_seq.len() {
                let new = &new_seq[j];

                info!("checking item {} - is {:?} inside {:?}?", j,
                      root_callsite_span(new.get_span()), old.get_span());

                if !old.get_span().contains(root_callsite_span(new.get_span())) {
                    // Reached a node that didn't come from this macro.
                    break;
                }

                // The node came from `mac`, so consume and record the node.
                cx.record_macro_with_id(invoc_id, mac, new.as_mac_node_ref());
                j += 1;
            }
        } else {
            assert!(j < new_seq.len(),
                    "impossible: ran out of items in expanded sequence");
            CollectMacros::collect_macros(old, &new_seq[j], cx);
            j += 1;
        }
    }

    assert!(j == new_seq.len(),
            "impossible: too many items in expanded sequence");
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
            },
            (&None, &None) => {},
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


trait MaybeMac {
    fn as_mac(&self) -> Option<&Mac>;
}

impl MaybeMac for Expr {
    fn as_mac(&self) -> Option<&Mac> {
        match self.node {
            ExprKind::Mac(ref mac) => Some(mac),
            _ => None,
        }
    }
}

impl MaybeMac for Pat {
    fn as_mac(&self) -> Option<&Mac> {
        match self.node {
            PatKind::Mac(ref mac) => Some(mac),
            _ => None,
        }
    }
}

impl MaybeMac for Ty {
    fn as_mac(&self) -> Option<&Mac> {
        match self.node {
            TyKind::Mac(ref mac) => Some(mac),
            _ => None,
        }
    }
}

impl MaybeMac for Item {
    fn as_mac(&self) -> Option<&Mac> {
        match self.node {
            ItemKind::Mac(ref mac) => Some(mac),
            _ => None,
        }
    }
}

impl MaybeMac for ImplItem {
    fn as_mac(&self) -> Option<&Mac> {
        match self.node {
            ImplItemKind::Macro(ref mac) => Some(mac),
            _ => None,
        }
    }
}

impl MaybeMac for TraitItem {
    fn as_mac(&self) -> Option<&Mac> {
        match self.node {
            TraitItemKind::Macro(ref mac) => Some(mac),
            _ => None,
        }
    }
}

impl MaybeMac for ForeignItem {
    fn as_mac(&self) -> Option<&Mac> {
        match self.node {
            ForeignItemKind::Macro(ref mac) => Some(mac),
            _ => None,
        }
    }
}

impl MaybeMac for Stmt {
    fn as_mac(&self) -> Option<&Mac> {
        match self.node {
            StmtKind::Mac(ref mac) => Some(&mac.0),
            _ => None,
        }
    }
}

impl<T: MaybeMac> MaybeMac for P<T> {
    fn as_mac(&self) -> Option<&Mac> {
        <T as MaybeMac>::as_mac(self)
    }
}

