use std::collections::{HashMap, HashSet};
use regex::Regex;
use syntax::ast::*;
use syntax::ptr::P;
use syntax::symbol::Symbol;

use command::CommandState;
use command::{Registry, FuncCommand};
use driver::{self, Phase};
use pick_node::NodeKind;
use util::IntoSymbol;

pub use self::filter::ItemLikeKind;

pub mod filter;
pub mod parse;
pub mod visitor;



#[derive(Clone, Debug)]
pub enum SelectOp {
    /// Select all nodes that are already marked with a given label.
    Marked(Symbol),
    /// Add a mark with the given label to all selected nodes.
    Mark(Symbol),
    /// Remove any marks with the given label from all selected nodes.
    Unmark(Symbol),

    /// Clear the current selection.
    Reset,

    /// Select the crate root.
    Crate,

    /// Select all nodes that are direct children of selected nodes and that match the filter.
    ChildMatch(Filter),
    /// Select all nodes that are descendants of selected nodes and that match the filter.
    DescMatch(Filter),

    /// Filter the set of selected nodes, keeping only nodes that match the filter.
    Filter(Filter),
}


#[derive(Clone, Debug)]
pub enum Filter {
    /// The node is of the indicated kind.
    Kind(NodeKind),
    /// The node is an itemlike of the indicated subkind.
    ItemKind(ItemLikeKind),
    /// The node's visibility is set to "public".  This implies the node must be an item-like.
    Public,
    /// The node's name matches the given regular expression.
    Name(Regex),
    /// The node has an attribute with the given name.
    HasAttr(Symbol),
    /// The node matches a pattern, according to the `matcher` module.  This implies that the node
    /// kind must match the pattern kind.
    Matches(AnyPattern),
    /// The node is marked with the given label.
    Marked(Symbol),

    /// At least one direct child of the node matches the filter.
    AnyChild(Box<Filter>),
    /// All direct children of the node match the filter.
    AllChild(Box<Filter>),

    /// At least one descendant of the node matches the filter.
    AnyDesc(Box<Filter>),
    /// All descendants of the node match the filter.
    AllDesc(Box<Filter>),

    And(Vec<Filter>),
    Or(Vec<Filter>),
    Not(Box<Filter>),
}

#[derive(Clone, Debug)]
pub enum AnyPattern {
    Expr(P<Expr>),
    Ty(P<Ty>),
    Stmt(Stmt),
}


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
        Box::new(FuncCommand::new(Phase::Phase2, move |st, cx| {
            let ops = parse::parse(cx.session(), &ops_str);
            eprintln!("running select: {:?} -> {}", ops, label);
            run_select(st, cx, &ops, label);
        }))
    });
}
