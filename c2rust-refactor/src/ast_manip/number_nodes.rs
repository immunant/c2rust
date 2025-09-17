use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::{MacCall, NodeId, DUMMY_NODE_ID};
use std::cell::Cell;

use crate::ast_manip::MutVisit;

#[derive(Clone)]
pub struct NodeIdCounter(Cell<u32>);

impl NodeIdCounter {
    pub fn new(start: u32) -> NodeIdCounter {
        NodeIdCounter(Cell::new(start))
    }

    pub fn next(&self) -> NodeId {
        let id = NodeId::from_u32(self.0.get());
        self.0.set(self.0.get() + 1);
        id
    }
}

struct NumberNodes<'a> {
    counter: &'a NodeIdCounter,
}

impl<'a> MutVisitor for NumberNodes<'a> {
    fn visit_id(&mut self, i: &mut NodeId) {
        *i = self.counter.next()
    }

    fn visit_mac_call(&mut self, mac: &mut MacCall) {
        mut_visit::noop_visit_mac(mac, self)
    }
}

/// Assign new `NodeId`s to all nodes in `x`.
pub fn number_nodes<T: MutVisit>(x: &mut T) {
    // 0 is a valid node id.  DUMMY_NODE_ID is -1.
    number_nodes_with(x, &NodeIdCounter::new(0))
}

/// Assign new `NodeId`s to all nodes in `x`.
pub fn number_nodes_with<T: MutVisit>(x: &mut T, counter: &NodeIdCounter) {
    x.visit(&mut NumberNodes { counter })
}

struct ResetNodeIds;
impl MutVisitor for ResetNodeIds {
    fn visit_id(&mut self, i: &mut NodeId) {
        *i = DUMMY_NODE_ID;
    }

    fn visit_mac_call(&mut self, mac: &mut MacCall) {
        mut_visit::noop_visit_mac(mac, self)
    }
}

pub fn reset_node_ids<T: MutVisit>(x: &mut T) {
    x.visit(&mut ResetNodeIds)
}
