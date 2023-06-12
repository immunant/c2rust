//! Defines the [`SpanIndex`] data structure for looking up MIR statements by span.
use rustc_span::Span;

/// A mapping from [`Span`]s to values of type `T`.  Allows looking up all items whose span is a
/// subset of some target span.
#[derive(Clone, Debug)]
pub struct SpanIndex<T> {
    v: Vec<(Span, T)>,
}

impl<T> SpanIndex<T> {
    /// Construct a new [`SpanIndex`], containing the items provided by the iterator `it`.
    pub fn new(it: impl IntoIterator<Item = (Span, T)>) -> SpanIndex<T> {
        let mut v = it.into_iter().collect::<Vec<_>>();
        v.sort_by_key(|(span, _)| span.lo());
        SpanIndex { v }
    }

    /// Iterate over items whose spans are entirely contained within `span`.  The order of the
    /// returned items is unspecified.
    pub fn _lookup(&self, span: Span) -> impl Iterator<Item = (Span, &T)> {
        let data = span.data();
        let start = self.v.partition_point(|(span, _)| span.lo() < data.lo);
        self.v[start..]
            .iter()
            // The list is sorted by `lo`, so once we move past `span.hi()`, we won't see any more
            // overlapping spans.
            .take_while(move |&&(s, _)| s.lo() < data.hi)
            // Skip any spans that extend beyond `span.hi()`.
            .filter(move |&&(s, _)| s.hi() <= data.hi)
            .map(|&(s, ref x): &(Span, T)| (s, x))
    }

    /// Iterate over items whose spans are exactly equal to `span`.  The order of the returned
    /// items is unspecified.
    pub fn lookup_exact(&self, span: Span) -> impl Iterator<Item = &T> {
        let data = span.data();
        let start = self.v.partition_point(|(s, _)| s.lo() < data.lo);
        self.v[start..]
            .iter()
            // The list is sorted by `lo`, so once we move past `self.lo`, we won't see it again.
            .take_while(move |&&(s, _)| s.lo() == data.lo)
            // Only return values for the requested span.  We might see other spans that cover only
            // a prefix of `span` or have a different `SyntaxContext`; we ignore the values
            // associated with those spans.
            .filter(move |&&(s, _)| s == span)
            .map(|&(_, ref t)| t)
    }
}
