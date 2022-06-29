use linked_hash_set::LinkedHashSet;

use crate::graph::{Graph, NodeId, NodeKind};

impl Graph {
    /// it would be nice to test properties like "an object is created in foo,
    /// and its object graph contains a StoreAddr node".
    /// essentially, if Stuart thinks the lighttpd example PDG looks good,
    /// it would be nice to convert those mappings into test cases
    ///
    /// Wanted to give you some more info about the query. It should basically say for each node in each object graph 
    /// whether the node needs write permissions, based on whether or not there is a path to a StoreAddr node
    /// (within the same object graph, but there should be no paths out of the object graph anyway).
    /// The way the current representation of the PDG is, it's actually easiest to work backwards from StoreAddr nodes 
    /// and mark all ancestor nodes as needing write permissions (I've avoided changing the representation 
    /// and am aligning with Stuart's original as much as possible, but if there is a strong case for doing a forward search 
    /// then I think tweaking the representation should be fine). We can also query on a per-object basis 
    /// and only run the query for "the object that was created in fnode_init " for example . 
    /// Essentially, the meat of the query might be something like
    /// 
    /// ```psuedocode
    /// write_permissions_required:
    ///   needs_write = []
    ///   not_needs_write = []
    ///   for node in nodes.reversed():
    ///     if node not in needs_write and node not in not_needs_write
    ///       if node.type == StoreAddr:
    ///         cur = node
    ///         # walk ancestry
    ///         while let Some(parent) = cur.parent:
    ///             needs_write.add(parent)
    ///             cur = parent
    ///       else: not_needs_write.add(node)
    ///   return needs_write # or query_node in needs_write
    /// ```
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
