//! Defines the [`SpanIndex`] data structure for looking up MIR statements by span.
use rustc_span::{BytePos, Span};
use std::slice;

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
    pub fn _lookup(&self, span: Span) -> RangeIter<T> {
        let data = span.data();
        let start = self.v.partition_point(|(span, _)| span.lo() < data.lo);
        RangeIter {
            inner: self.v[start..].iter(),
            hi: data.hi,
        }
    }

    /// Iterate over items whose spans are exactly equal to `span`.  The order of the returned
    /// items is unspecified.
    pub fn lookup_exact(&self, span: Span) -> ExactIter<T> {
        let data = span.data();
        let start = self.v.partition_point(|(span, _)| span.lo() < data.lo);
        ExactIter {
            inner: self.v[start..].iter(),
            lo: data.lo,
            span,
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
                // This span extends beyond the requested range.
                continue;
            }
            return Some((span, value));
        }
    }
}

pub struct ExactIter<'a, T> {
    inner: slice::Iter<'a, (Span, T)>,
    lo: BytePos,
    span: Span,
}

impl<'a, T> Iterator for ExactIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        loop {
            let &(span, ref value) = self.inner.next()?;
            let data = span.data();
            if data.lo != self.lo {
                // The list is sorted by `lo`, so once we move past `self.lo`, we won't see it
                // again.
                return None;
            }
            if span != self.span {
                // This is not the requested span.  Note we compare the entire span, not just `lo`
                // and `hi`, because spans with the same `lo` and `hi` can come from two different
                // macro expansions (thus having different `SyntaxContext`s).
                continue;
            }
            return Some(value);
        }
    }
}
