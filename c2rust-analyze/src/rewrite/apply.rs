use crate::rewrite::Rewrite;
use rustc_hir::Mutability;
use rustc_span::source_map::{FileName, SourceMap};
use rustc_span::{BytePos, SourceFile, Span, SyntaxContext};
use std::cmp::Reverse;
use std::collections::HashMap;

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
            // or the current item's span is contained in the span of some `Rewrite::Sub` in
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

/// Split `rts` into the portion before `span`, the portion overlapping `span`, and the portion
/// after `span`.  Nodes within `rts` should not overlap each other, and the list must be sorted by
/// span; otherwise, the result are unspecified.
fn partition_nodes<'a>(
    rts: &'a [RewriteTree],
    span: Span,
) -> (&'a [RewriteTree], &'a [RewriteTree], &'a [RewriteTree]) {
    let lo = span.lo();
    let hi = span.hi();

    // Collect nodes from the front of `rts` until we find one that overlaps or comes after `span`.
    let i = rts
        .iter()
        .position(|rt| rt.span.hi() > lo)
        .unwrap_or(rts.len());
    let (before, rest) = rts.split_at(i);

    // Collect nodes from the front of `rts` until we find one that comes strictly after `span`.
    let j = rest
        .iter()
        .position(|rt| rt.span.lo() >= hi)
        .unwrap_or(rest.len());
    let (overlap, after) = rest.split_at(j);

    (before, overlap, after)
}

struct Emitter<'a, F> {
    file: &'a SourceFile,
    emit: &'a mut F,
}

impl<'a, F: FnMut(&str)> Emitter<'a, F> {
    fn emit_str(&mut self, s: &str) {
        (self.emit)(s);
    }

    fn emit_parenthesized(&mut self, cond: bool, f: impl FnOnce(&mut Self)) {
        if cond {
            self.emit_str("(");
        }
        f(self);
        if cond {
            self.emit_str(")");
        }
    }

    fn emit_rewrite(
        &mut self,
        rw: &Rewrite,
        prec: usize,
        emit_expr: &mut impl FnMut(&mut Self),
        emit_subexpr: &mut impl FnMut(&mut Self, Span),
    ) {
        match *rw {
            Rewrite::Identity => self.emit_parenthesized(true, |slf| {
                emit_expr(slf);
            }),
            Rewrite::Sub(_, span) => self.emit_parenthesized(true, |slf| {
                emit_subexpr(slf, span);
            }),

            Rewrite::Ref(ref rw, mutbl) => self.emit_parenthesized(prec > 2, |slf| {
                match mutbl {
                    Mutability::Not => slf.emit_str("&"),
                    Mutability::Mut => slf.emit_str("&mut "),
                }
                slf.emit_rewrite(rw, 2, emit_expr, emit_subexpr);
            }),
            Rewrite::AddrOf(ref rw, mutbl) => {
                match mutbl {
                    Mutability::Not => self.emit_str("core::ptr::addr_of!"),
                    Mutability::Mut => self.emit_str("core::ptr::addr_of_mut!"),
                }
                self.emit_parenthesized(true, |slf| {
                    slf.emit_rewrite(rw, 0, emit_expr, emit_subexpr);
                });
            }
            Rewrite::Deref(ref rw) => self.emit_parenthesized(prec > 2, |slf| {
                slf.emit_str("*");
                slf.emit_rewrite(rw, 2, emit_expr, emit_subexpr);
            }),
            Rewrite::Index(ref arr, ref idx) => self.emit_parenthesized(prec > 3, |slf| {
                slf.emit_rewrite(arr, 3, emit_expr, emit_subexpr);
                slf.emit_str("[");
                slf.emit_rewrite(idx, 0, emit_expr, emit_subexpr);
                slf.emit_str("]");
            }),
            Rewrite::SliceTail(ref arr, ref idx) => self.emit_parenthesized(prec > 3, |slf| {
                slf.emit_rewrite(arr, 3, emit_expr, emit_subexpr);
                slf.emit_str("[");
                // Rather than figure out the right precedence for `..`, just force
                // parenthesization in this position.
                slf.emit_rewrite(idx, 999, emit_expr, emit_subexpr);
                slf.emit_str(" ..]");
            }),
            Rewrite::CastUsize(ref rw) => self.emit_parenthesized(prec > 1, |slf| {
                slf.emit_rewrite(rw, 1, emit_expr, emit_subexpr);
                slf.emit_str(" as usize");
            }),
            Rewrite::LitZero => {
                self.emit_str("0");
            }

            Rewrite::TyPtr(ref rw, mutbl) => {
                match mutbl {
                    Mutability::Not => self.emit_str("*const "),
                    Mutability::Mut => self.emit_str("*mut "),
                }
                self.emit_rewrite(rw, 0, emit_expr, emit_subexpr);
            }
            Rewrite::TyRef(ref rw, mutbl) => {
                match mutbl {
                    Mutability::Not => self.emit_str("&"),
                    Mutability::Mut => self.emit_str("&mut "),
                }
                self.emit_rewrite(rw, 0, emit_expr, emit_subexpr);
            }
            Rewrite::TyCtor(ref name, ref rws) => {
                self.emit_str(name);
                self.emit_str("<");
                for rw in rws {
                    self.emit_rewrite(rw, 0, emit_expr, emit_subexpr);
                }
                self.emit_str(">");
            }
        }
    }

    fn emit_bytes(&mut self, lo: BytePos, hi: BytePos) {
        assert!(
            self.file.start_pos <= lo && hi <= self.file.end_pos,
            "bytes {:?} .. {:?} are out of range for file {:?}",
            lo,
            hi,
            self.file.name
        );
        let src = self
            .file
            .src
            .as_ref()
            .unwrap_or_else(|| panic!("source is not available for file {:?}", self.file.name));
        self.emit_str(&src[lo.0 as usize..hi.0 as usize]);
    }

    fn emit_span_with_rewrites(&mut self, span: Span, rts: &[RewriteTree]) {
        let (_, overlap, _) = partition_nodes(rts, span);

        let mut pos = span.lo();
        for rt in overlap {
            // Every child node is contained by the span of some `Rewrite::Identity` or
            // `Rewrite::Sub` in its parent node.
            debug_assert!(span.contains(rt.span));

            self.emit_bytes(pos, rt.span.lo());
            self.emit_rewrite(
                &rt.rw,
                0,
                &mut |slf| slf.emit_span_with_rewrites(rt.span, &rt.children),
                &mut |slf, subexpr_span| slf.emit_span_with_rewrites(subexpr_span, &rt.children),
            );
            pos = rt.span.hi();
        }

        self.emit_bytes(pos, span.hi());
    }
}

/// Apply rewrites `rws` to the source files covered by their `Span`s.  Returns a map giving the
/// rewritten source code for each file that contains at least one rewritten `Span`.
pub fn apply_rewrites(
    source_map: &SourceMap,
    rws: Vec<(Span, Rewrite)>,
) -> HashMap<FileName, String> {
    let (rts, errs) = RewriteTree::build(rws);
    for (span, rw, err) in errs {
        eprintln!(
            "{:?}: warning: failed to apply rewrite {:?}: {:?}",
            span, rw, err
        );
    }

    let mut new_src = HashMap::new();
    let mut rts = &rts as &[RewriteTree<Span>];
    while rts.len() > 0 {
        let file = source_map.lookup_source_file(rts[0].span.lo());
        let idx = rts
            .iter()
            .position(|rt| rt.span.lo() >= file.end_pos)
            .unwrap_or(rts.len());
        assert!(idx > 0);
        let (file_rts, rest) = rts.split_at(idx);
        rts = rest;

        let mut buf = String::new();
        let mut emit = |s: &str| {
            buf.push_str(s);
        };
        let mut emitter = Emitter {
            file: &file,
            emit: &mut emit,
        };
        let file_span = Span::new(file.start_pos, file.end_pos, SyntaxContext::root(), None);
        emitter.emit_span_with_rewrites(file_span, file_rts);

        new_src.insert(file.name.clone(), buf);
    }

    new_src
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
        Rewrite::Sub(i, (0, 0))
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
