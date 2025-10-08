//! Mappings between old and new `NodeId`s.  Also has some support for `AttrId`s.
use log::{trace, warn};
use rustc_ast::{AttrId, NodeId, DUMMY_NODE_ID};
use rustc_span::source_map::symbol::Symbol;
use std::collections::hash_map::Entry;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::mem;
use std::ops::Bound::Included;
use std::ops::Deref;

pub const DUMMY_ATTR_ID: AttrId = AttrId::MAX;

#[derive(Clone, Debug)]
pub struct NodeMap {
    /// Map from current NodeIds to old NodeIds.
    id_map: HashMap<NodeId, NodeId>,

    /// Edges from current IDs to new IDs.  `commit_nodes()` will merge this into `self.id_map`, so
    /// that the new IDs become current IDs.
    pending_edges: BTreeSet<(NodeId, NodeId)>,
}

impl NodeMap {
    pub fn new() -> NodeMap {
        NodeMap {
            id_map: HashMap::new(),

            pending_edges: BTreeSet::new(),
        }
    }

    pub fn into_inner(self) -> HashMap<NodeId, NodeId> {
        self.id_map
    }

    pub fn commit(&mut self) {
        let mut new_id_map = HashMap::new();
        trace!("committing edges");
        for (id2, id3) in mem::replace(&mut self.pending_edges, BTreeSet::new()) {
            if id2 == DUMMY_NODE_ID || id3 == DUMMY_NODE_ID {
                continue;
            }

            if let Some(&id1) = self.id_map.get(&id2) {
                trace!("  {:?} -> {:?} -> {:?}", id3, id2, id1);
                match new_id_map.entry(id3) {
                    Entry::Vacant(e) => {
                        e.insert(id1);
                    }
                    Entry::Occupied(mut e) => {
                        if *e.get() != id1 {
                            // This is bad - we have two *different* old IDs for the same new ID.
                            // Report a warning, and deterministically pick one as the winner.
                            let winner = if *e.get() < id1 { *e.get() } else { id1 };
                            warn!(
                                "new {:?} maps to both old {:?} and old {:?} - \
                                 picking {:?} as the winner",
                                id3,
                                *e.get(),
                                id1,
                                winner
                            );
                            *e.get_mut() = winner;
                        }
                        // Otherwise, both old IDs match - there's no conflict.
                    }
                }
            } else {
                trace!("  {:?} -> {:?} -> NOT FOUND", id3, id2);
            }
        }

        self.id_map = new_id_map;
    }

    /// Initialize by mapping every `NodeId` in `nodes` to itself.
    pub fn init<I: Iterator<Item = NodeId>>(&mut self, nodes: I) {
        for id in nodes {
            if id == DUMMY_NODE_ID {
                continue;
            }
            self.id_map.insert(id, id);
        }
    }

    /// Update the NodeId mapping using a list of `(old_id, new_id)` pairs.
    pub fn add_edges(&mut self, matched_ids: &[(NodeId, NodeId)]) {
        self.pending_edges.extend(matched_ids.iter().cloned());
    }

    pub fn add_edge(&mut self, id: NodeId, new_id: NodeId) {
        self.pending_edges.insert((id, new_id));
    }

    /// Save what we know about the origin of node `id`.  The origin can be tracked externally and
    /// restored later with `restore_id`.  This is useful when a node will be removed from the AST,
    /// but could be reinserted later on.
    pub fn save_origin(&self, id: NodeId) -> Option<NodeId> {
        self.id_map.get(&id).cloned()
    }

    /// Restore saved information about a node's origin.
    pub fn restore_origin(&mut self, id: NodeId, origin: Option<NodeId>) {
        if let Some(origin) = origin {
            self.id_map.insert(id, origin);
        }
    }

    /// Update mark NodeIds to account for the pending (not committed) NodeId changes.
    pub fn transfer_marks(&self, marks: &mut HashSet<(NodeId, Symbol)>) {
        let mut new_marks = HashSet::new();
        for &(old_id, label) in marks.iter() {
            let lo = (old_id, NodeId::from_u32(0));
            let hi = (old_id, NodeId::MAX);
            let mut empty = true;
            for &(_, new_id) in self.pending_edges.range((Included(&lo), Included(&hi))) {
                trace!("  {:?}: {:?} -> {:?}", label, old_id, new_id);
                new_marks.insert((new_id, label));
                empty = false;
            }
            if empty {
                trace!("  {:?}: {:?} -> DROPPED", label, old_id);
            }
        }
        *marks = new_marks;
    }

    /// Update keys of an arbitrary `HashMap` to account for the pending (not committed) NodeId
    /// changes.
    pub fn transfer_map<V: Clone>(&self, map: HashMap<NodeId, V>) -> HashMap<NodeId, V> {
        let mut new_map = HashMap::with_capacity(map.len());
        for (old_id, v) in map {
            let lo = (old_id, NodeId::from_u32(0));
            let hi = (old_id, NodeId::MAX);

            let mut new_ids = self
                .pending_edges
                .range((Included(&lo), Included(&hi)))
                .map(|&(_, new_id)| new_id)
                .peekable();

            // Avoid a clone if there's zero or one new IDs.
            while let Some(new_id) = new_ids.next() {
                if new_ids.peek().is_none() {
                    new_map.insert(new_id, v);
                    break;
                } else {
                    new_map.insert(new_id, v.clone());
                }
            }
        }
        new_map
    }

    pub fn transfer<'a>(&'a self, id: NodeId) -> impl Iterator<Item = NodeId> + 'a {
        let lo = (id, NodeId::from_u32(0));
        let hi = (id, NodeId::MAX);

        self.pending_edges
            .range((Included(&lo), Included(&hi)))
            .map(|&(_, new_id)| new_id)
    }
}

impl Deref for NodeMap {
    type Target = HashMap<NodeId, NodeId>;

    fn deref(&self) -> &Self::Target {
        &self.id_map
    }
}
