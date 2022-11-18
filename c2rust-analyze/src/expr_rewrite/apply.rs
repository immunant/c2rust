#![allow(dead_code)]
use crate::expr_rewrite::hir_op::Rewrite;
use rustc_span::{BytePos, Span};
use std::cmp::Reverse;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum RewriteError<S = Span> {
    /// The provided rewrite overlaps, but is not contained in, another rewrite.  `.0` is the span
    /// of the other rewrite.
    PartialOverlap(S),
    /// The provided rewrite conflicts with a different rewrite at the same span.
    Conflict,
    /// The provided rewrite affects code that would be discarded by a rewrite of a containing
    /// expression.  `.0` is the span of the containing expression, and `.1` is its rewrite.
    Discarded(S, Box<Rewrite<S>>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct RewriteTree<S = Span> {
    span: S,
    rw: Rewrite<S>,
    /// Child nodes, representing rewrites that are contained entirely within the current rewrite.
    /// Each child's `span` is contained within `self.span`, no child's `span` overlaps any other
    /// child's `span`, and children are sorted by position (`span.lo()`).
    children: Vec<RewriteTree<S>>,
}

trait SpanLike: Copy + Eq {
    fn lo(self) -> BytePos;
    fn hi(self) -> BytePos;
    fn contains(self, other: Self) -> bool;
    fn overlaps(self, other: Self) -> bool;
}

impl SpanLike for Span {
    fn lo(self) -> BytePos {
        self.lo()
    }
    fn hi(self) -> BytePos {
        self.hi()
    }
    fn contains(self, other: Self) -> bool {
        self.contains(other)
    }
    fn overlaps(self, other: Self) -> bool {
        self.overlaps(other)
    }
}

impl SpanLike for (u32, u32) {
    fn lo(self) -> BytePos {
        BytePos(self.0)
    }
    fn hi(self) -> BytePos {
        BytePos(self.1)
    }
    fn contains(self, other: Self) -> bool {
        self.0 <= other.0 && other.1 <= self.1
    }
    fn overlaps(self, other: Self) -> bool {
        self.0 < other.1 && other.0 < self.1
    }
}

impl<S: SpanLike> RewriteTree<S> {
    pub fn build(
        mut rws: Vec<(S, Rewrite<S>)>,
    ) -> (Vec<RewriteTree<S>>, Vec<(S, Rewrite<S>, RewriteError<S>)>) {
        // Sort by start position and then by decreasing length, so that each parent span comes
        // before all its children.
        rws.sort_by_key(|&(ref s, _)| (s.lo(), Reverse(s.hi() - s.lo())));

        // The stack contains a `RewriteTree` node for the most recently processed item from `rws`
        // along with all of its ancestors (nodes whose spans strictly contain its span).
        // `stack[0]` is the outermost ancestor, and `stack.last()` is the most recently pushed
        // node.  Nodes are "committed" once they are known to have no more children; when
        // committed, the node is moved into the `children` list of its parent or (if it has no
        // parent) into the output list `out`.
        let mut stack = Vec::<RewriteTree<S>>::new();
        let mut out = Vec::new();
        let mut errs = Vec::new();

        fn commit<S: SpanLike>(
            stack: &mut Vec<RewriteTree<S>>,
            out: &mut Vec<RewriteTree<S>>,
            rt: RewriteTree<S>,
        ) {
            if let Some(parent) = stack.last_mut() {
                debug_assert!(parent.span.contains(rt.span));
                debug_assert!(parent
                    .children
                    .last()
                    .map_or(true, |ch| ch.span.hi() <= rt.span.lo()),);
                parent.children.push(rt);
            } else {
                debug_assert!(out.last().map_or(true, |ch| ch.span.hi() <= rt.span.lo()),);
                out.push(rt);
            }
        }

        for (span, rw) in rws {
            let lo = span.lo();

            // Handle items with identical spans.
            //
            // If `rws` contains two items with the same span, they should be adjacent in the
            // sorted list.  If the first item of such a run was pushed onto the stack, we will
            // catch it here when processing the second item.  Since this case avoids pushing or
            // committing any items, all remaining items in the run will land here too.
            if let Some(other) = stack.last().filter(|other| other.span == span) {
                if other.rw != rw {
                    // This item has the same span as the previous one, but wants to perform a
                    // different rewrite.
                    errs.push((span, rw, RewriteError::Conflict));
                }
                continue;
            }

            // Commit all rewrites that come strictly before `span`.  This leaves only nodes that
            // at least partially overlap the current item.  These should normally be ancestors of
            // the current item on the stack, but it's also possible that the current item
            // erroneously partially overlaps a previous item.
            while stack.last().map_or(false, |rt| rt.span.hi() <= lo) {
                let rt = stack.pop().unwrap();
                commit(&mut stack, &mut out, rt);
            }

            // Check for error cases.
            let siblings = if let Some(parent) = stack.last() {
                debug_assert!(parent.span.overlaps(span));
                if !parent.span.contains(span) {
                    // `span` overlaps `parent.span`, but isn't contained within it.
                    errs.push((span, rw, RewriteError::PartialOverlap(parent.span)));
                    continue;
                }
                &parent.children
            } else {
                &out
            };

            if let Some(prev) = siblings.last() {
                debug_assert!(prev.span.lo() <= span.lo());
                if prev.span.overlaps(span) {
                    errs.push((span, rw, RewriteError::PartialOverlap(prev.span)));
                    continue;
                }
            }

            // TODO: check that all children are emitted somewhere in the `Rewrite`
            // (e.g. given the rewrite `f(x + y) -> x`, we can't also have a rewrite on `x + y`,
            // because that expression is discarded as part of the outer rewrite)
            //
            // Specifically: if there is a parent, check that either the current item's span is
            // contained in the parent span and the parent's rewrite contains `Rewrite::Identity`,
            // or the current item's span is contained in the span of some `Rewrite::Subexpr` in
            // the parent rewrite.

            // Push a new node onto the stack.
            stack.push(RewriteTree {
                span,
                rw,
                children: Vec::new(),
            });
        }

        while let Some(rt) = stack.pop() {
            commit(&mut stack, &mut out, rt);
        }

        (out, errs)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    type FakeSpan = (u32, u32);

    fn mk(start: u32, end: u32, i: usize) -> (FakeSpan, Rewrite<FakeSpan>) {
        let span = (start, end);
        let rw = mk_rewrite(i);
        (span, rw)
    }

    fn mk_rewrite(i: usize) -> Rewrite<FakeSpan> {
        Rewrite::Subexpr(i, (0, 0))
    }

    fn mk_rt(
        start: u32,
        end: u32,
        i: usize,
        children: Vec<RewriteTree<FakeSpan>>,
    ) -> RewriteTree<FakeSpan> {
        let span = (start, end);
        let rw = mk_rewrite(i);
        RewriteTree { span, rw, children }
    }

    #[test]
    fn rewrite_tree_nesting() {
        let (rts, errs) = RewriteTree::build(vec![mk(1, 2, 0), mk(3, 4, 1), mk(0, 5, 2)]);
        assert_eq!(errs, vec![]);
        assert_eq!(
            rts,
            vec![mk_rt(
                0,
                5,
                2,
                vec![mk_rt(1, 2, 0, vec![]), mk_rt(3, 4, 1, vec![]),]
            ),]
        );
    }

    #[test]
    fn rewrite_tree_identical() {
        let (rts, errs) = RewriteTree::build(vec![
            mk(1, 2, 0),
            mk(3, 4, 1),
            mk(0, 5, 2),
            mk(1, 2, 0),
            mk(3, 4, 1),
            mk(0, 5, 2),
        ]);
        assert_eq!(errs, vec![]);
        assert_eq!(
            rts,
            vec![mk_rt(
                0,
                5,
                2,
                vec![mk_rt(1, 2, 0, vec![]), mk_rt(3, 4, 1, vec![]),]
            ),]
        );
    }

    #[test]
    fn rewrite_tree_nonidentical() {
        let (rts, errs) = RewriteTree::build(vec![
            mk(1, 2, 0),
            mk(3, 4, 1),
            mk(0, 5, 2),
            mk(1, 2, 3),
            mk(3, 4, 4),
            mk(0, 5, 5),
        ]);
        assert_eq!(
            errs,
            vec![
                ((0, 5), mk_rewrite(5), RewriteError::Conflict),
                ((1, 2), mk_rewrite(3), RewriteError::Conflict),
                ((3, 4), mk_rewrite(4), RewriteError::Conflict),
            ]
        );
        assert_eq!(
            rts,
            vec![mk_rt(
                0,
                5,
                2,
                vec![mk_rt(1, 2, 0, vec![]), mk_rt(3, 4, 1, vec![]),]
            ),]
        );
    }

    #[test]
    fn rewrite_tree_overlap() {
        let (rts, errs) =
            RewriteTree::build(vec![mk(1, 3, 0), mk(3, 4, 1), mk(0, 5, 2), mk(2, 4, 3)]);
        assert_eq!(
            errs,
            vec![((2, 4), mk_rewrite(3), RewriteError::PartialOverlap((1, 3))),]
        );
        assert_eq!(
            rts,
            vec![mk_rt(
                0,
                5,
                2,
                vec![mk_rt(1, 3, 0, vec![]), mk_rt(3, 4, 1, vec![]),]
            ),]
        );
    }
}
