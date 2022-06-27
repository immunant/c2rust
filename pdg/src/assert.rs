// * it would be nice to test properties like "an object is created in foo,
// and its object graph contains a StoreAddr node".
// essentially, if Stuart thinks the lighttpd example PDG looks good,
// it would be nice to convert those mappings into test cases

// * in that lighttpd map example I have a field for source which is a NodeId type,
// but i'd like to confirm that the GraphId is always correct too.
// we can go over this one if you want because it'll involve a small tweak to the pdg code

// * this isn't implemented yet but when the graph deduplication is done,
//   it would be nice to have a test case that there are no duplicate objects

// * all head of objects (node lists) have a source of None

// Wanted to give you some more info about the query. It should basically say for each node in each object graph whether the node needs write permissions, based on whether or not there is a path to a StoreAddr node (within the same object graph, but there should be no paths out of the object graph anyway). The way the current representation of the PDG is, it's actually easiest to work backwards from StoreAddr nodes and mark all ancestor nodes as needing write permissions (I've avoided changing the representation and am aligning with Stuart's original as much as possible, but if there is a strong case for doing a forward search then I think tweaking the representation should be fine). We can also query on a per-object basis and only run the query for "the object that was created in fnode_init " for example . Essentially, the meat of the query might be something like
// write_permissions_required:
//   needs_write = []
//   not_needs_write = []
//   for node in nodes.reversed():
//     if node not in needs_write and node not in not_needs_write
//       if node.type == StoreAddr:
//         cur = node
//         # walk ancestry
//         while let Some(parent) = cur.parent:
//             needs_write.add(parent)
//             cur = parent
//       else: not_needs_write.add(node)
//   return needs_write # or query_node in needs_write

use crate::{graph::{Graph, Graphs}, util::Duplicates};

impl Graphs {
    /// Assert that a graph has no duplicate objects.
    ///
    /// This is not necessary, but helps minimize the graphs.
    /// Once when graph deduplication is implemented, we should implement and test this as well.
    pub fn assert_no_duplicates(&self) {
        Duplicates::find(&self.graphs).assert_empty();
    }
}

impl Graph {
    /// Assert that a graph's head has no source, as the head should be the root source.
    pub fn assert_head_has_no_source(&self) {
        todo!()
    }
}

impl Graphs {
    /// Assert [`Graph::assert_head_has_no_source`] for every [`Graph`].
    pub fn assert_heads_have_no_sources(&self) {
        for graph in &self.graphs {
            graph.assert_head_has_no_source();
        }
    }
}

impl Graphs {
    /// Assert all [`Graph`] tests.
    pub fn assert_all_tests(&self) {
        self.assert_no_duplicates();
        self.assert_heads_have_no_sources();
    }
}
