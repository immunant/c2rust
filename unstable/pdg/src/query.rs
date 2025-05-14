//! ## Future Plans for Queries
//! TODO(kkysen, aneksteind)
//!
//! It would be nice to test properties like "an object is create in `foo`,
//! and its object [`Graph`] contains a [`StoreAddr`] node".
//!
//! [`StoreAddr`]: NodeKind::StoreAddr.
//!
//! For example PDGs that we have already (manually) confirmed are correct,
//! we want to set up snapshot testing to make sure we don't introduce any regressions,
//! and be able to test if certain changes have any effect on the PDG output.
//! We are thinking about using [`insta`](https://insta.rs/) for this.

use linked_hash_set::LinkedHashSet;

use crate::graph::{Graph, NodeId, NodeKind};

impl Graph {
    /// Query an object [`Graph`] to determine which of its [`Node`]s (returned as [`NodeId`]s)
    /// need write permissions for future refactors into Rust references instead of raw pointers.
    ///
    /// This is calculated based on whether or not there is a path to a [`StoreAddr`] node,
    /// which is a write, from the current [`Node`] we are testing
    /// (in the same object [`Graph`], though there shouldn't be any paths out of an object [`Graph`] anyways).
    ///
    /// The way the PDG/[`Graph`]s is/are represented, it is actually easiest to work backwards from [`StoreAddr`] nodes
    /// and mark all ancestor nodes as needing write permissions.
    ///
    /// [`StoreAddr`]: NodeKind::StoreAddr
    /// [`Node`]: crate::graph::Node
    pub fn needs_write_permission(&self) -> impl Iterator<Item = NodeId> {
        let mut needs_write = LinkedHashSet::new();
        let mut not_needs_write = LinkedHashSet::new();
        for (node_id, node) in self.nodes.iter_enumerated().rev() {
            if !needs_write.contains(&node_id) && !not_needs_write.contains(&node_id) {
                if let NodeKind::StoreAddr = node.kind {
                    let mut cur = node_id;
                    loop {
                        needs_write.insert(cur);
                        let source = match self.nodes[cur].source {
                            None => break,
                            Some(source) => source,
                        };
                        cur = source;
                    }
                } else {
                    not_needs_write.insert(node_id);
                }
            }
        }
        needs_write.into_iter()
    }
}
