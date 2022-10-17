//! Defines the [`SpanIndex`] data structure for looking up MIR statements by span.
use rustc_span::{BytePos, Span};
use std::slice;

/// A mapping from [`Span`]s to values of type `T`.  Allows looking up all items whose span is a
/// subset of some target span.
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
    pub fn lookup<'a>(&'a self, span: Span) -> RangeIter<'a, T> {
        let data = span.data();
        let start = self.v.partition_point(|(span, _)| span.lo() < data.lo);
        RangeIter {
            inner: self.v[start..].iter(),
            hi: data.hi,
        }
    }
}

pub struct RangeIter<'a, T> {
    inner: slice::Iter<'a, (Span, T)>,
    hi: BytePos,
}

impl<'a, T> Iterator for RangeIter<'a, T> {
    type Item = (Span, &'a T);

    fn next(&mut self) -> Option<(Span, &'a T)> {
        loop {
            let &(span, ref value) = self.inner.next()?;
            let data = span.data();
            if data.lo >= self.hi {
                // We've reached the end of the requested range.
                return None;
            }
            if data.hi > self.hi {
                // This span extends beyond the requseted range.
                continue;
            }
            return Some((span, value));
        }
    }
}
