use std::cell::Cell;
use syntax::ast::{NodeId, Mac, DUMMY_NODE_ID};
use syntax::fold::{self, Folder};

use crate::ast_manip::Fold;


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

impl<'a> Folder for NumberNodes<'a> {
    fn new_id(&mut self, _i: NodeId) -> NodeId {
        self.counter.next()
    }

    fn fold_mac(&mut self, mac: Mac) -> Mac {
        fold::noop_fold_mac(mac, self)
    }
}

/// Assign new `NodeId`s to all nodes in `x`.
pub fn number_nodes<T: Fold>(x: T) -> <T as Fold>::Result {
    // 0 is a valid node id.  DUMMY_NODE_ID is -1.
    number_nodes_with(x, &NodeIdCounter::new(0))
}

/// Assign new `NodeId`s to all nodes in `x`.
pub fn number_nodes_with<T: Fold>(x: T, counter: &NodeIdCounter) -> <T as Fold>::Result {
    x.fold(&mut NumberNodes { counter })
}


struct ResetNodeIds;
impl Folder for ResetNodeIds {
    fn new_id(&mut self, _i: NodeId) -> NodeId { DUMMY_NODE_ID }

    fn fold_mac(&mut self, mac: Mac) -> Mac {
        fold::noop_fold_mac(mac, self)
    }
}

pub fn reset_node_ids<T: Fold>(x: T) -> <T as Fold>::Result {
    x.fold(&mut ResetNodeIds)
}
