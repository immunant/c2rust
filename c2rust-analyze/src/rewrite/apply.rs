use crate::rewrite::Rewrite;
use log::warn;
use rustc_hir::Mutability;
use rustc_span::source_map::{FileName, SourceMap};
use rustc_span::{BytePos, SourceFile, Span, SyntaxContext};
use std::cmp::{self, Reverse};
use std::collections::HashMap;
use std::convert::Infallible;
use std::fmt;
use std::mem;

use super::LifetimeName;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum RewriteError<S = Span> {
    /// The provided rewrite overlaps, but is not contained in, another rewrite.  `.0` is the span
    /// of the other rewrite.
    PartialOverlap(S),
    /// The provided rewrite conflicts with a different rewrite at the same span.
    Conflict,
    /// The provided rewrite affects code that would be discarded by a rewrite of a containing
    /// expression.  `.0` is the span of the containing expression, and `.1` is its rewrite.
    _Discarded(S, Box<Rewrite<S>>),
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

/// This trait defines the subset of the [`Span`] API that we use in this module.  It's implemented
/// for `Span` and also for `FakeSpan`, which is a simple span type we use in tests to avoid
/// dealing with interner/`SourceMap` machinery.
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

impl<S: SpanLike> RewriteTree<S> {
    #[allow(clippy::type_complexity)]
    pub fn build(
        mut rws: Vec<(S, Rewrite<S>)>,
    ) -> (Vec<RewriteTree<S>>, Vec<(S, Rewrite<S>, RewriteError<S>)>) {
        // Sort by start position and then by decreasing length, so that each parent span comes
        // before all its children.
        rws.sort_by_key(|&(ref s, _)| (s.lo(), Reverse(s.hi() - s.lo())));

        // `stack` contains partially-built `RewriteTree`s, which might have more children that we
        // haven't seen yet.  Once we know that a node has no more children, we "commit" that node,
        // removing it from `stack` and adding it to the `children` list of its parent, or to `out`
        // if it has no parent.  The parent of each node `stack[i+1]` is the previous node
        // `stack[i]`; the node `stack[0]` has no parent.
        let mut stack = Vec::<RewriteTree<S>>::new();
        let mut out = Vec::new();
        let mut errs = Vec::new();

        fn commit<S: SpanLike>(
            stack: &mut [RewriteTree<S>],
            out: &mut Vec<RewriteTree<S>>,
            rt: RewriteTree<S>,
        ) {
            if let Some(parent) = stack.last_mut() {
                debug_assert!(parent.span.contains(rt.span));
                // Children must be committed in order and must be non-overlapping, so `rt.span`
                // should come after `ch.span`.
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
            // If `rws` contains two or more items with the same span, they will be adjacent in the
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

            // Commit all rewrites that come strictly before `span`.  We know each node before
            // `span` has no more children; if it did have more children, those children would also
            // be before `span` (as the child span must be contained within the parent span), so we
            // would have seen them already in the sort order of `rws`.
            //
            // This leaves the stack populated with only nodes that at least partially overlap the
            // current item.  These should normally be just the ancestors of the current item, but
            // it's also possible that the current item erroneously partially overlaps a previous
            // item.
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
            // the parent rewrite.  If this doesn't hold, then produce `RewriteError::Discarded`.

            // Push a new node onto the stack.
            stack.push(RewriteTree {
                span,
                rw,
                children: Vec::new(),
            });
        }

        // There are no more children, so we can commit all remaining items on the stack.  Each
        // item `stack[i+1]` is the last child (so far) of its parent `stack[i]`.
        while let Some(rt) = stack.pop() {
            commit(&mut stack, &mut out, rt);
        }

        (out, errs)
    }
}

/// Split `rts` into the portion before `span`, the portion overlapping `span`, and the portion
/// after `span`.  Nodes within `rts` should not overlap each other, and the list must be sorted by
/// span; otherwise, the result are unspecified.
fn partition_nodes(
    rts: &[RewriteTree],
    span: Span,
) -> (&[RewriteTree], &[RewriteTree], &[RewriteTree]) {
    // Check that `rts[i]` ends before `rts[i+1]` begins, which implies that the nodes in `rts` are
    // sorted and non-overlapping.
    debug_assert!(rts
        .iter()
        .zip(rts.iter().skip(1))
        .all(|(a, b)| a.span.hi() <= b.span.lo()));

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

pub trait Sink {
    type Error;
    const PARENTHESIZE_EXPRS: bool;
    fn emit_str(&mut self, s: &str) -> Result<(), Self::Error>;
    /// Emit a string from the original file into the output.  `s` is a contiguous substring of the
    /// original, and the start of `s` is on line `line`.  If the first character of `s` is `'\n'`,
    /// then it's the newline separating `line` from `line + 1`.
    fn emit_orig_str(&mut self, s: &str, line: usize) -> Result<(), Self::Error>;
    fn emit_fmt(&mut self, args: fmt::Arguments) -> Result<(), Self::Error>;
    fn emit_expr(&mut self) -> Result<(), Self::Error>;
    fn emit_sub(&mut self, idx: usize, span: Span) -> Result<(), Self::Error>;
    /// Emit the original text of a source span before any rewrites were applied.
    fn emit_span(&mut self, span: Span) -> Result<(), Self::Error>;
}

struct Emitter<'a, S> {
    sink: &'a mut S,
}

impl<S: Sink> Emitter<'_, S> {
    fn emit_str(&mut self, s: &str) -> Result<(), S::Error> {
        self.sink.emit_str(s)
    }
    fn emit_fmt(&mut self, args: fmt::Arguments) -> Result<(), S::Error> {
        self.sink.emit_fmt(args)
    }
    fn emit_expr(&mut self) -> Result<(), S::Error> {
        self.sink.emit_expr()
    }
    fn emit_sub(&mut self, idx: usize, span: Span) -> Result<(), S::Error> {
        self.sink.emit_sub(idx, span)
    }
    fn emit_span(&mut self, span: Span) -> Result<(), S::Error> {
        self.sink.emit_span(span)
    }

    fn emit_parenthesized(
        &mut self,
        cond: bool,
        f: impl FnOnce(&mut Self) -> Result<(), S::Error>,
    ) -> Result<(), S::Error> {
        if cond {
            self.emit_str("(")?;
        }
        f(self)?;
        if cond {
            self.emit_str(")")?;
        }
        Ok(())
    }

    /// Emit the text of `rw` into `self.sink`, using `Sink` methods to paste in the expression
    /// being rewritten or its subexpressions if needed.
    ///
    /// `prec` is the precedence of the surrounding context.  Each operator is assigned a
    /// precedence number, where a higher precedence number means the operator binds more tightly.
    /// For example, `a + b * c` parses as `a + (b * c)`, not `(a + b) * c`, because `*` binds more
    /// tightly than `+`; this means `*` will have a higher precedence number than `+`.  Nesting a
    /// lower-precedence operator inside a higher one requires parentheses, but nesting higher
    /// precedence inside lower does not.  For example, when emitting `y + z` in the context `x *
    /// _`, we must parenthesize because `+` has lower precedence than `*`, so the result is `x *
    /// (y + z)`.  But when emitting `y * z` in the context `x + _`, we don't need to parenthesize,
    /// and the result is `x + y * z`.
    ///
    /// Top-level calls to `emit` should normally use a `prec` of 0, meaning any operator can be
    /// used without parenthesization.  Recursive calls within `pretty` will use a different `prec`
    /// as appropriate for the context.
    fn emit(&mut self, rw: &Rewrite, prec: usize) -> Result<(), S::Error> {
        match *rw {
            Rewrite::Identity => {
                self.emit_parenthesized(S::PARENTHESIZE_EXPRS, |slf| slf.emit_expr())
            }
            Rewrite::Sub(idx, span) => {
                self.emit_parenthesized(S::PARENTHESIZE_EXPRS, |slf| slf.emit_sub(idx, span))
            }

            Rewrite::Text(ref s) => self.emit_str(s),
            Rewrite::Extract(span) => self.emit_span(span),

            Rewrite::Ref(ref rw, mutbl) => self.emit_parenthesized(prec > 2, |slf| {
                match mutbl {
                    Mutability::Not => slf.emit_str("&")?,
                    Mutability::Mut => slf.emit_str("&mut ")?,
                }
                slf.emit(rw, 2)
            }),
            Rewrite::AddrOf(ref rw, mutbl) => {
                match mutbl {
                    Mutability::Not => self.emit_str("core::ptr::addr_of!")?,
                    Mutability::Mut => self.emit_str("core::ptr::addr_of_mut!")?,
                }
                self.emit_parenthesized(true, |slf| slf.emit(rw, 0))
            }
            Rewrite::Deref(ref rw) => self.emit_parenthesized(prec > 2, |slf| {
                slf.emit_str("*")?;
                slf.emit(rw, 2)
            }),
            Rewrite::Index(ref arr, ref idx) => self.emit_parenthesized(prec > 3, |slf| {
                slf.emit(arr, 3)?;
                slf.emit_str("[")?;
                slf.emit(idx, 0)?;
                slf.emit_str("]")
            }),
            Rewrite::SliceRange(ref arr, ref idx1, ref idx2) => {
                self.emit_parenthesized(prec > 3, |slf| {
                    slf.emit(arr, 3)?;
                    slf.emit_str("[")?;
                    if let Some(idx1) = idx1.as_ref() {
                        // Rather than figure out the right precedence for `..`, just force
                        // parenthesization in this position.
                        slf.emit(idx1, 999)?;
                        slf.emit_str(" ")?;
                    }
                    slf.emit_str("..")?;
                    if let Some(idx2) = idx2.as_ref() {
                        slf.emit_str(" ")?;
                        slf.emit(idx2, 999)?;
                    }
                    slf.emit_str("]")
                })
            }
            Rewrite::Cast(ref rw, ref ty) => self.emit_parenthesized(prec > 1, |slf| {
                slf.emit(rw, 1)?;
                slf.emit_str(" as ")?;
                slf.emit(ty, 0)
            }),
            Rewrite::RemovedCast(ref rw) => self.emit(rw, prec),
            Rewrite::LitZero => self.emit_str("0"),

            Rewrite::Print(ref s) => self.emit_str(s),
            Rewrite::_TyGenericParams(ref rws) => {
                self.emit_str("<")?;
                for (index, rw) in rws.iter().enumerate() {
                    self.emit(rw, 0)?;
                    if index < rws.len() - 1 {
                        self.emit_str(",")?;
                    }
                }
                self.emit_str(">")
            }
            Rewrite::Call(ref func, ref arg_rws) => {
                self.emit_str(func)?;
                self.emit_parenthesized(true, |slf| {
                    for (index, rw) in arg_rws.iter().enumerate() {
                        slf.emit(rw, 0)?;
                        if index < arg_rws.len() - 1 {
                            slf.emit_str(",")?;
                        }
                    }
                    Ok(())
                })
            }
            Rewrite::MethodCall(ref method, ref receiver_rw, ref arg_rws) => {
                self.emit(receiver_rw, 3)?;
                self.emit_str(".")?;
                self.emit_str(method)?;
                self.emit_parenthesized(true, |slf| {
                    for (index, rw) in arg_rws.iter().enumerate() {
                        slf.emit(rw, 0)?;
                        if index < arg_rws.len() - 1 {
                            slf.emit_str(",")?;
                        }
                    }
                    Ok(())
                })
            }

            Rewrite::Block(ref stmts, ref expr) => {
                self.emit_str("{\n")?;
                for stmt in stmts {
                    self.emit_str("    ")?;
                    self.emit(stmt, 0)?;
                    self.emit_str(";\n")?;
                }
                if let Some(ref expr) = *expr {
                    self.emit_str("    ")?;
                    self.emit(expr, 0)?;
                    self.emit_str("\n")?;
                }
                self.emit_str("}")
            }

            Rewrite::Let(ref vars) => {
                self.emit_str("let (")?;
                for (ref name, _) in vars {
                    self.emit_str(name)?;
                    self.emit_str(", ")?;
                }
                self.emit_str(") = (")?;
                for (_, ref rw) in vars {
                    self.emit(rw, 0)?;
                    self.emit_str(", ")?;
                }
                self.emit_str(")")
            }

            Rewrite::Let1(ref name, ref rw) => {
                self.emit_str("let ")?;
                self.emit_str(name)?;
                self.emit_str(" = ")?;
                self.emit(rw, 0)
            }

            Rewrite::Closure1(ref name, ref rw) => {
                self.emit_str("|")?;
                self.emit_str(name)?;
                self.emit_str("| ")?;
                self.emit(rw, 0)
            }

            Rewrite::Match(ref expr, ref cases) => {
                self.emit_str("match ")?;
                self.emit(expr, 0)?;
                self.emit_str(" {\n")?;
                for &(ref pat, ref body) in cases {
                    self.emit_str("    ")?;
                    self.emit_str(pat)?;
                    self.emit_str(" => ")?;
                    self.emit(body, 0)?;
                    self.emit_str(",\n")?;
                }
                self.emit_str("}")
            }

            Rewrite::TyPtr(ref rw, mutbl) => {
                match mutbl {
                    Mutability::Not => self.emit_str("*const ")?,
                    Mutability::Mut => self.emit_str("*mut ")?,
                }
                self.emit(rw, 0)
            }
            Rewrite::TyRef(ref lifetime, ref rw, mutbl) => {
                self.emit_str("&")?;
                if let LifetimeName::Explicit(lt) = lifetime {
                    self.emit_str(lt)?;
                    self.emit_str(" ")?;
                }

                if let Mutability::Mut = mutbl {
                    self.emit_str("mut")?;
                    self.emit_str(" ")?;
                }

                self.emit(rw, 0)
            }
            Rewrite::TySlice(ref rw) => {
                self.emit_str("[")?;
                self.emit(rw, 0)?;
                self.emit_str("]")
            }
            Rewrite::TyCtor(ref name, ref rws) => {
                self.emit_str(name)?;
                self.emit_str("<")?;
                for (index, rw) in rws.iter().enumerate() {
                    self.emit(rw, 0)?;
                    if index < rws.len() - 1 {
                        self.emit_str(",")?;
                    }
                }
                self.emit_str(">")
            }

            Rewrite::StaticMut(mutbl, span) => {
                match mutbl {
                    Mutability::Not => self.emit_str("static ")?,
                    Mutability::Mut => self.emit_str("static mut ")?,
                }
                self.emit_sub(0, span)
            }

            Rewrite::DefineFn {
                ref name,
                ref arg_tys,
                ref return_ty,
                ref body,
            } => {
                self.emit_fmt(format_args!("\nunsafe fn {name}("))?;
                for (i, arg_ty) in arg_tys.iter().enumerate() {
                    if i > 0 {
                        self.emit_str(", ")?;
                    }
                    self.emit_fmt(format_args!("arg{i}: "))?;
                    self.emit(arg_ty, 0)?;
                }
                self.emit_str(")")?;
                if let Some(return_ty) = return_ty.as_ref() {
                    self.emit_str(" -> ")?;
                    self.emit(return_ty, 0)?;
                }
                self.emit_str(" {\n")?;

                self.emit_str("    ")?;
                self.emit(body, 0)?;
                self.emit_str("\n")?;

                self.emit_str("}\n")
            }

            Rewrite::FnArg(i) => self.emit_fmt(format_args!("arg{i}")),
        }
    }
}

pub fn emit_rewrite<S: Sink>(sink: &mut S, rw: &Rewrite) -> Result<(), S::Error> {
    Emitter { sink }.emit(rw, 0)
}

struct RewriteTreeSink<'a, F> {
    file: &'a SourceFile,
    emit: &'a mut F,
    rt: Option<&'a RewriteTree>,
}

impl<'a, F: FnMut(&str, Option<usize>)> RewriteTreeSink<'a, F> {
    fn new(file: &'a SourceFile, emit: &'a mut F) -> RewriteTreeSink<'a, F> {
        RewriteTreeSink {
            file,
            emit,
            rt: None,
        }
    }

    fn with_rt<R>(&mut self, rt: &'a RewriteTree, f: impl FnOnce(&mut Self) -> R) -> R {
        let old = mem::replace(&mut self.rt, Some(rt));
        let r = f(self);
        self.rt = old;
        r
    }

    fn emit_bytes(&mut self, lo: BytePos, hi: BytePos) -> Result<(), <Self as Sink>::Error> {
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
        // `lo` and `hi` are relative to the SourceMap within which various files' data is located,
        // so subtract the file's start to obtain indices within its data.
        let lo_in_file = lo - self.file.start_pos;
        let hi_in_file = hi - self.file.start_pos;
        let s = &src[lo_in_file.0 as usize..hi_in_file.0 as usize];
        if let Some(line) = self.file.lookup_line(lo) {
            self.emit_orig_str(s, line)
        } else {
            self.emit_str(s)
        }
    }

    fn emit_span_with_rewrites(
        &mut self,
        span: Span,
        rts: &'a [RewriteTree],
    ) -> Result<(), <Self as Sink>::Error> {
        let (_, overlap, _) = partition_nodes(rts, span);

        let mut pos = span.lo();
        for rt in overlap {
            // Every child node is contained by the span of some `Rewrite::Identity` or
            // `Rewrite::Sub` in its parent node.
            debug_assert!(span.contains(rt.span));

            self.emit_bytes(pos, rt.span.lo())?;
            self.with_rt(rt, |slf| emit_rewrite(slf, &rt.rw))?;
            pos = rt.span.hi();
        }

        self.emit_bytes(pos, span.hi())?;
        Ok(())
    }
}

impl<'a, F: FnMut(&str, Option<usize>)> Sink for RewriteTreeSink<'a, F> {
    type Error = Infallible;
    const PARENTHESIZE_EXPRS: bool = true;

    fn emit_str(&mut self, s: &str) -> Result<(), Self::Error> {
        (self.emit)(s, None);
        Ok(())
    }
    fn emit_orig_str(&mut self, s: &str, line: usize) -> Result<(), Self::Error> {
        (self.emit)(s, Some(line));
        Ok(())
    }
    fn emit_fmt(&mut self, args: fmt::Arguments) -> Result<(), Self::Error> {
        (self.emit)(&format!("{args}"), None);
        Ok(())
    }
    fn emit_expr(&mut self) -> Result<(), Self::Error> {
        let rt = self.rt.unwrap();
        self.emit_span_with_rewrites(rt.span, &rt.children)
    }
    fn emit_sub(&mut self, _idx: usize, span: Span) -> Result<(), Self::Error> {
        let rt = self.rt.unwrap();
        self.emit_span_with_rewrites(span, &rt.children)
    }
    fn emit_span(&mut self, span: Span) -> Result<(), Self::Error> {
        self.emit_bytes(span.lo(), span.hi())
    }
}

#[derive(Debug, Default)]
struct LineMapBuilder {
    /// Map from line indices in the original source code to line indices in `buf`.  For each input
    /// line, we give the index of the first output line containing some unmodified portion of the
    /// input line.
    v: Vec<Option<usize>>,
}

impl LineMapBuilder {
    /// Record that part of input line `i` is found in output line `j`.
    pub fn record(&mut self, i: usize, j: usize) {
        if i >= self.v.len() {
            self.v.resize(i + 1, None);
        }
        self.v[i] = Some(self.v[i].map_or(j, |old_j| cmp::min(j, old_j)));
    }

    pub fn finish(self) -> Vec<usize> {
        // If an input line is missing from the output, find the next non-missing input line and
        // use its output index instead.  This way, we always have somewhere to attach annotations
        // for any line.
        let mut out = Vec::with_capacity(self.v.len());
        for (i, j) in self.v.into_iter().enumerate() {
            if let Some(j) = j {
                // Reuse `j` to fill in for any previous `None` entries, then push a `j` for input
                // line `i` itself.
                out.resize(i + 1, j);
            }
            // Otherwise, do nothing.
        }
        out
    }
}

pub struct FileRewrite {
    /// The rewritten source code for this file.
    pub new_src: String,
    /// For each input line in the original source code, this gives the line number within
    /// `new_src` of the first output line that contains some part of the input line.
    pub line_map: Vec<usize>,
}

/// Apply rewrites `rws` to the source files covered by their `Span`s.  Returns a map giving the
/// rewritten source code for each file that contains at least one rewritten `Span`.
pub fn apply_rewrites(
    source_map: &SourceMap,
    rws: Vec<(Span, Rewrite)>,
) -> HashMap<FileName, FileRewrite> {
    let (rts, errs) = RewriteTree::build(rws);
    for (span, rw, err) in errs {
        warn!(
            "{:?}: warning: failed to apply rewrite {:?}: {:?}",
            span, rw, err
        );
    }

    let mut file_rewrites = HashMap::new();
    let mut rts = &rts as &[RewriteTree<Span>];
    while !rts.is_empty() {
        let file = source_map.lookup_source_file(rts[0].span.lo());
        let idx = rts
            .iter()
            .position(|rt| rt.span.lo() >= file.end_pos)
            .unwrap_or(rts.len());
        assert!(idx > 0);
        let (file_rts, rest) = rts.split_at(idx);
        rts = rest;

        let mut buf = String::new();
        // Number of newlines in `buf`.
        let mut buf_line = 0;
        let mut line_map = LineMapBuilder::default();
        let mut emit = |s: &str, line| {
            if let Some(mut line) = line {
                line_map.record(line, buf_line);
                for _ in s.matches('\n') {
                    line += 1;
                    buf_line += 1;
                    line_map.record(line, buf_line);
                }
            } else {
                buf_line += s.matches('\n').count();
            }
            buf.push_str(s);
        };

        let mut sink = RewriteTreeSink::new(&file, &mut emit);
        let file_span = Span::new(file.start_pos, file.end_pos, SyntaxContext::root(), None);
        sink.emit_span_with_rewrites(file_span, file_rts).unwrap();

        file_rewrites.insert(
            file.name.clone(),
            FileRewrite {
                new_src: buf,
                line_map: line_map.finish(),
            },
        );
    }

    file_rewrites
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
    struct FakeSpan {
        lo: u32,
        hi: u32,
    }

    impl SpanLike for FakeSpan {
        fn lo(self) -> BytePos {
            BytePos(self.lo)
        }
        fn hi(self) -> BytePos {
            BytePos(self.hi)
        }
        fn contains(self, other: Self) -> bool {
            self.lo <= other.lo && other.hi <= self.hi
        }
        fn overlaps(self, other: Self) -> bool {
            self.lo < other.hi && other.lo < self.hi
        }
    }

    fn mk_span(lo: u32, hi: u32) -> FakeSpan {
        FakeSpan { lo, hi }
    }

    fn mk(lo: u32, hi: u32, i: usize) -> (FakeSpan, Rewrite<FakeSpan>) {
        let span = FakeSpan { lo, hi };
        let rw = mk_rewrite(i);
        (span, rw)
    }

    fn mk_rewrite(i: usize) -> Rewrite<FakeSpan> {
        Rewrite::Sub(i, mk_span(0, 0))
    }

    fn mk_rt(
        lo: u32,
        hi: u32,
        i: usize,
        children: Vec<RewriteTree<FakeSpan>>,
    ) -> RewriteTree<FakeSpan> {
        let (span, rw) = mk(lo, hi, i);
        RewriteTree { span, rw, children }
    }

    /// Test `RewriteTree::build` with ranges that require nesting.
    #[test]
    fn rewrite_tree_nesting() {
        // `1..2` and `3..4` should be made into child nodes of `0..5`.
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

    /// Test `RewriteTree::build` with multiple identical rewrites on the same spans.
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

    /// Test `RewriteTree::build` with multiple conflicting rewrites on the same spans.
    #[test]
    fn rewrite_tree_nonidentical() {
        let (rts, errs) = RewriteTree::build(vec![
            // Trying to rewrite the span 1..2 to both Sub(0) and Sub(1) should produce a conflict
            // error.
            mk(1, 2, 0),
            mk(1, 2, 1),
            mk(3, 4, 2),
            mk(3, 4, 3),
            mk(0, 5, 4),
            mk(0, 5, 5),
        ]);
        assert_eq!(
            errs,
            vec![
                (mk_span(0, 5), mk_rewrite(5), RewriteError::Conflict),
                (mk_span(1, 2), mk_rewrite(1), RewriteError::Conflict),
                (mk_span(3, 4), mk_rewrite(3), RewriteError::Conflict),
            ]
        );
        assert_eq!(
            rts,
            vec![mk_rt(
                0,
                5,
                4,
                vec![mk_rt(1, 2, 0, vec![]), mk_rt(3, 4, 2, vec![]),]
            ),]
        );
    }

    /// Test `RewriteTree::build` with partially overlapping spans.
    #[test]
    fn rewrite_tree_overlap() {
        let (rts, errs) =
            RewriteTree::build(vec![mk(1, 3, 0), mk(3, 4, 1), mk(0, 5, 2), mk(2, 4, 3)]);
        assert_eq!(
            errs,
            vec![(
                mk_span(2, 4),
                mk_rewrite(3),
                RewriteError::PartialOverlap(mk_span(1, 3))
            ),]
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
