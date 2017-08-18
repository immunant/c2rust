use std::collections::{HashMap, HashSet};
use regex::Regex;
use syntax::ast::*;
use syntax::ptr::P;
use syntax::symbol::Symbol;

use command::CommandState;
use command::{Registry, DriverCommand};
use driver::{self, Phase};
use pick_node::NodeKind;
use util::IntoSymbol;

pub use self::filter::ItemLikeKind;

pub mod filter;
pub mod parse;
pub mod visitor;



#[derive(Clone, Debug)]
pub enum SelectOp {
    /// `marked(l)`: Select all nodes that are already marked with label `l`.
    Marked(Symbol),
    /// `mark(l)`: Add a mark with label `l` to all selected nodes.
    Mark(Symbol),
    /// `unmark(l)`: Remove any marks with label `l` from all selected nodes.
    Unmark(Symbol),

    /// `reset`: Clear the current selection.
    Reset,

    /// `crate`: Select the crate root.
    Crate,

    /// `child(f)`: Replace the current selection with the set of all nodes that are direct
    /// children of selected nodes and that match filter `f`.  
    ChildMatch(Filter),
    /// `desc(f)`: Replace the current selection with the set of all nodes that are descendants of
    /// selected nodes and that match filter `f`.
    DescMatch(Filter),

    /// `filter(f)`: Filter the set of selected nodes, keeping only nodes that match filter `f`.
    Filter(Filter),
}


#[derive(Clone, Debug)]
pub enum Filter {
    /// `kind(k)`: The node is of kind `k`.  See `pick_node::NodeKind` for a list of supported node
    /// kinds.  Shorthand: `k` is an alias for `kind(k)` if `k` is a valid node kind.
    Kind(NodeKind),
    /// `item_kind(k)`: The node is an itemlike of subkind `k`.  See `select::filter::ItemLikeKind`
    /// for a list of supported itemlike subkinds.  Shorthand: `k` is an alias for `item_kind(k)`
    /// if `k` is a valid itemlike subkind.
    ItemKind(ItemLikeKind),
    /// `pub`: The node's visibility is set to "public".  This implies the node must be item-like.
    Public,
    /// `name(re)`: The node's name matches regular expression `re`.
    Name(Regex),
    /// `path_prefix(n, p)`: The prefix of the node's path, obtained by removing the last `n`
    /// segments, is `p`.  Shorthand: `path(p)` is an alias for `path_prefix(0, p)`.
    PathPrefix(usize, Box<Path>),
    /// `has_attr(a)`: The node has an attribute named `a`.
    HasAttr(Symbol),
    /// `match_k(p)`: The node matches a pattern `p` of kind `k`, according to the `matcher`
    /// module.  This implies that the node kind must match the pattern kind.
    Matches(AnyPattern),
    /// `marked(l)`: The node is marked with label `l`.
    Marked(Symbol),

    /// `any_child(f)`: At least one direct child of the node matches filter `f`.
    AnyChild(Box<Filter>),
    /// `all_child(f)`: All direct children of the node match filter `f`.
    AllChild(Box<Filter>),

    /// `any_desc(f)`: At least one descendant of the node matches filter `f`.
    AnyDesc(Box<Filter>),
    /// `all_desc(f)`: All descendants of the node match filter `f`.
    AllDesc(Box<Filter>),

    /// `f1 && f2`: Filters `f1` and `f2` both hold on the node.
    And(Vec<Filter>),
    /// `f1 || f2`: At least one of filters `f1` and `f2` holds on the node.
    Or(Vec<Filter>),
    /// `!f`: Filter `f` does not hold on the node.
    Not(Box<Filter>),
}

#[derive(Clone, Debug)]
pub enum AnyPattern {
    Expr(P<Expr>),
    Ty(P<Ty>),
    Stmt(Stmt),
}


/// The `select` command runs a script to select and mark a set of nodes.
///
/// `select` scripts manipulate a set of nodes, called the "selection".  The selection starts out
/// empty, and various commands can add or remove nodes from the set.  When the script finishes,
/// all nodes in the selection will be marked with the given label.
pub fn run_select<S: IntoSymbol>(st: &CommandState,
                                 cx: &driver::Ctxt,
                                 ops: &[SelectOp],
                                 label: S) {
    let mut sel = HashSet::new();
    for op in ops {
        match *op {
            SelectOp::Marked(label) => {
                for &(id, mark_label) in st.marks().iter() {
                    if mark_label == label {
                        sel.insert(id);
                    }
                }
            },

            SelectOp::Mark(label) => {
                for &id in &sel {
                    st.add_mark(id, label);
                }
            },

            SelectOp::Unmark(label) => {
                for &id in &sel {
                    st.remove_mark(id, label);
                }
            },

            SelectOp::Reset => {
                sel = HashSet::new();
            },

            SelectOp::Crate => {
                sel.insert(CRATE_NODE_ID);
            },

            SelectOp::ChildMatch(ref filt) => {
                sel = visitor::matching_children(st, cx, &st.krate(), sel, filt);
            },

            SelectOp::DescMatch(ref filt) => {
                sel = visitor::matching_descendants(st, cx, &st.krate(), sel, filt);
            },

            SelectOp::Filter(ref filt) => {
                sel = visitor::filter(st, cx, &st.krate(), sel, filt);
            },
        }
    }

    let label = label.into_symbol();
    for id in sel {
        st.add_mark(id, label);
    }
}


pub fn register_commands(reg: &mut Registry) {
    reg.register("select", |args| {
        let label = (&args[0]).into_symbol();
        let ops_str = args[1].clone();
        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            let ops = parse::parse(cx.session(), &ops_str);
            eprintln!("running select: {:?} -> {}", ops, label);
            run_select(st, cx, &ops, label);
        }))
    });
}
