// TODO(kkysen, aneksteind)
// in that lighttpd map example I have a field for source which is a NodeId type,
// but i'd like to confirm that the GraphId is always correct too.
// we can go over this one if you want because it'll involve a small tweak to the pdg code

use itertools::Itertools;

use crate::{
    graph::{Graph, Graphs, IWithMetadata, WithMetadata},
    util::Duplicates,
};

impl Graphs {
    /// Assert that a graph has no duplicate objects.
    ///
    /// This is not necessary, but helps minimize the graphs.
    /// Once when graph deduplication is implemented, we should implement and test this as well.
    /// 
    /// This is the same as [`WithMetadata<'_, Graphs>::assert_no_duplicates`]
    /// except this only checks the deduplicated count is the same.
    pub fn assert_no_duplicates(&self) {
        let count = self.graphs.len();
        let deduplicated_count = self.graphs.iter().unique().count();
        assert_eq!(count, deduplicated_count);
    }
}

impl WithMetadata<'_, Graphs> {
    /// Assert that a graph has no duplicate objects.
    ///
    /// This is not necessary, but helps minimize the graphs.
    /// Once when graph deduplication is implemented, we should implement and test this as well.
    /// 
    /// This is the same as [`Graphs::assert_no_duplicates`]
    /// except this prints nice debug output.
    pub fn assert_no_duplicates(&self) {
        Duplicates::find(&self.inner.graphs)
            .assert_empty_with(|graph| graph.with_metadata(self.metadata).to_string());
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
    /// 
    /// This is the same as [`Graphs::assert_all_tests`]
    /// except it does not print nice debug output.
    // Used for debugging.
    #[allow(dead_code)]
    pub fn assert_all_tests(&self) {
        self.assert_no_duplicates();
        self.assert_heads_have_no_sources();
    }
}

impl WithMetadata<'_, Graphs> {
    /// Assert all [`Graph`] tests.
    /// 
    /// This is the same as [`Graphs::assert_all_tests`]
    /// except this prints nice debug output.
    // Used for debugging.
    #[allow(dead_code)]
    pub fn assert_all_tests(&self) {
        self.assert_no_duplicates();
        self.inner.assert_heads_have_no_sources();
    }
}
