// TODO(kkysen, aneksteind)
// in that lighttpd map example I have a field for source which is a NodeId type,
// but i'd like to confirm that the GraphId is always correct too.
// we can go over this one if you want because it'll involve a small tweak to the pdg code

use crate::{
    graph::{Graph, Graphs},
    util::Duplicates,
};

impl Graphs {
    /// Assert that a graph has no duplicate objects.
    ///
    /// This is not necessary, but helps minimize the graphs.
    pub fn assert_no_duplicates(&self) {
        Duplicates::find(&self.graphs).assert_empty_display();
    }
}

impl Graph {
    /// Assert that a graph's head has no source, as the head should be the root source.
    pub fn assert_head_has_no_source(&self) {
        assert_eq!(self.nodes[0usize.into()].source, None);
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
