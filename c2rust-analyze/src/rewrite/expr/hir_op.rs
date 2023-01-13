use crate::rewrite::expr::mir_op::{self, MirRewrite};
use crate::rewrite::span_index::SpanIndex;
use crate::rewrite::Rewrite;
use rustc_hir as hir;
use rustc_hir::def::Res;
use rustc_hir::intravisit;
use rustc_middle::hir::nested_filter;
use rustc_middle::mir::{self, Body, Location};
use rustc_middle::ty::adjustment::{Adjust, AutoBorrow, PointerCast};
use rustc_middle::ty::{TyCtxt, TypeckResults};
use rustc_span::Span;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum SoleLocationError {
    NoMatch,
    MultiMatch(Location, Location),
}

struct HirRewriteVisitor<'a, 'tcx> {
    tcx: TyCtxt<'tcx>,
    typeck_results: &'tcx TypeckResults<'tcx>,
    mir: &'a Body<'tcx>,
    span_index: SpanIndex<Location>,
    rewrites: &'a HashMap<Location, Vec<MirRewrite>>,
    locations_visited: HashSet<Location>,
    hir_rewrites: Vec<(Span, Rewrite)>,
    /// When `true`, any `Expr` where rustc added an implicit adjustment will be rewritten to make
    /// that adjustment explicit.  Any node that emits a non-adjustment rewrite sets this flag when
    /// visiting its children.  This is important to ensure that implicit ref/deref operations are
    /// not simply discarded by our rewrites.
    ///
    /// For example, suppose we'd like to remove the `as_ptr` call from `arr.as_ptr()` to produce a
    /// safe reference `&[T]` instead of a raw pointer `*const T`.  Simply eliminating the call,
    /// leaving `arr`, is incorrect if `arr` has type `[T; 10]`.  In this case, rustc was adding an
    /// implicit `Ref` adjustment, as if the programmer had written `(&arr).as_ptr()`.  The correct
    /// rewriting of this code is therefore `&arr`, not `arr`.
    ///
    /// To get the right result, we rewrite in two steps.  First, we materialize the implicit `Ref`
    /// adjustment that rustc applies to `arr`, producing the expression `(&arr).as_ptr()`.
    /// Second, we remove the `as_ptr` call, leaving only `&arr`.
    ///
    /// However, we don't want to apply this `x.f()` to `(&x).f()` step on code that's already
    /// safe, since it's unnecessary there and makes the code harder to read.  Our solution is to
    /// only materialize adjustments within the children (and further descendants) of nodes that
    /// are already being rewritten for some other reason.
    materialize_adjustments: bool,
}

impl<'a, 'tcx> HirRewriteVisitor<'a, 'tcx> {
    /// If `set`, set `self.materialize_adjustments` to `true` while running the closure.  If `set`
    /// is `false`, `self.materialize_adjustments` is left unchanged (inherited from the parent).
    fn with_materialize_adjustments<R>(&mut self, set: bool, f: impl FnOnce(&mut Self) -> R) -> R {
        let old = self.materialize_adjustments;
        self.materialize_adjustments |= set;
        let r = f(self);
        self.materialize_adjustments = old;
        r
    }

    /// Find the sole `Location` where the provided filters match and the statement or terminator
    /// has a span exactly equal to `target_span`.  Returns `Err` if there is no such location or
    /// if there are multiple matching locations.
    fn find_sole_location_matching(
        &mut self,
        target_span: Span,
        mut filter_stmt: impl FnMut(&mir::Statement<'tcx>) -> bool,
        mut filter_term: impl FnMut(&mir::Terminator<'tcx>) -> bool,
    ) -> Result<Location, SoleLocationError> {
        let mut found_loc = None;
        for (span, &loc) in self.span_index.lookup(target_span) {
            if span != target_span {
                continue;
            }
            let matched = self
                .mir
                .stmt_at(loc)
                .either(|s| filter_stmt(s), |t| filter_term(t));
            if !matched {
                continue;
            }

            if let Some(found_loc) = found_loc {
                return Err(SoleLocationError::MultiMatch(found_loc, loc));
            }
            found_loc = Some(loc);
        }

        match found_loc {
            Some(x) => Ok(x),
            None => Err(SoleLocationError::NoMatch),
        }
    }

    fn find_optional_location_matching(
        &mut self,
        target_span: Span,
        filter_stmt: impl FnMut(&mir::Statement<'tcx>) -> bool,
        filter_term: impl FnMut(&mir::Terminator<'tcx>) -> bool,
    ) -> Result<Option<Location>, SoleLocationError> {
        match self.find_sole_location_matching(target_span, filter_stmt, filter_term) {
            Ok(x) => Ok(Some(x)),
            Err(SoleLocationError::NoMatch) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn find_primary_location(&mut self, ex: &'tcx hir::Expr<'tcx>) -> Option<Location> {
        let panic_sole_location_error = |err, desc| -> ! {
            match err {
                SoleLocationError::NoMatch => panic!("couldn't find {} for {:?}", desc, ex),
                SoleLocationError::MultiMatch(loc1, loc2) => panic!(
                    "expected to find only one {}, but got multiple:\n\
                        expr = {:?}\n\
                        first match = {:?}\n\
                        second match = {:?}",
                    desc,
                    ex,
                    self.mir.stmt_at(loc1),
                    self.mir.stmt_at(loc2),
                ),
            }
        };

        match ex.kind {
            hir::ExprKind::Call(..) | hir::ExprKind::MethodCall(..) => {
                // We expect to find exactly one `TerminatorKind::Call` whose span exactly matches
                // this `hir::Expr`.
                let call_loc = self
                    .find_sole_location_matching(
                        ex.span,
                        |_stmt| false,
                        |term| matches!(term.kind, mir::TerminatorKind::Call { .. }),
                    )
                    .unwrap_or_else(|err| panic_sole_location_error(err, "Call terminator"));
                Some(call_loc)
            }
            hir::ExprKind::Path(hir::QPath::Resolved(_, p)) if matches!(p.res, Res::Local(..)) => {
                // Currently we only handle cases where the local is copied into a temporary.  If
                // the local is used as-is in some other expression, this case will return `None`.
                let opt_assign_loc = self
                    .find_optional_location_matching(
                        ex.span,
                        |stmt| matches!(stmt.kind, mir::StatementKind::Assign(..)),
                        |_term| false,
                    )
                    .unwrap_or_else(|err| panic_sole_location_error(err, "Assign statement"));
                opt_assign_loc
            }
            hir::ExprKind::Field(..) => {
                // Currently we only handle the case where the value retrieved from the field is
                // stored into a temporary.  If it's stored into a local or some other place (e.g.
                // `let y = x.f;`, or `y = x.f;` alone), then this case will return `None`.
                //
                // Also, for chained field accesses, this only matches the outermost ones.  Code
                // like `x.0.0` translates into a single MIR statement with the span of the overall
                // expression.
                let opt_assign_loc = self
                    .find_optional_location_matching(
                        ex.span,
                        |stmt| matches!(stmt.kind, mir::StatementKind::Assign(..)),
                        |_term| false,
                    )
                    .unwrap_or_else(|err| panic_sole_location_error(err, "Assign statement"));
                opt_assign_loc
            }
            _ => {
                eprintln!("warning: find_primary_location: unsupported expr {:?}", ex);
                None
            }
        }
    }

    /// Get subexpression `idx` of `ex`.  Panics if the index is out of range for `ex`.  The
    /// precise meaning of the index depends on the expression kind.
    fn get_subexpr(&self, ex: &'tcx hir::Expr<'tcx>, idx: usize) -> Rewrite {
        use hir::ExprKind::*;
        let sub_ex = match (&ex.kind, idx) {
            (&Box(e), 0) => e,
            (&Array(es), i) => &es[i],
            (&Call(_, args), i) => &args[i],
            (&MethodCall(_, args, _), i) => &args[i],
            (&Tup(es), i) => &es[i],
            (&Binary(_, x, _), 0) => x,
            (&Binary(_, _, y), 1) => y,
            (&Unary(_, x), 0) => x,
            (&Cast(e, _), 0) => e,
            (&Type(e, _), 0) => e,
            (&DropTemps(e), 0) => e,
            (&If(cond, _, _), 0) => cond,
            (&If(_, then, _), 1) => then,
            (&If(_, _, Some(else_)), 2) => else_,
            (&Match(e, _, _), 0) => e,
            (&Assign(l, _, _), 0) => l,
            (&Assign(_, r, _), 1) => r,
            (&AssignOp(_, l, _), 0) => l,
            (&AssignOp(_, _, r), 1) => r,
            (&Field(e, _), 0) => e,
            (&Index(arr, _), 0) => arr,
            (&Index(_, e_idx), 1) => e_idx,
            (&AddrOf(_, _, e), 0) => e,
            (&Break(_, Some(e)), 0) => e,
            (&Ret(Some(e)), 0) => e,
            (&Struct(_, flds, base), i) => {
                if i == flds.len() {
                    base.unwrap()
                } else {
                    flds[i].expr
                }
            }
            (&Repeat(e, _), 0) => e,
            (&Yield(e, _), 0) => e,
            _ => panic!("bad subexpression index {} for {:?}", idx, ex),
        };
        Rewrite::Sub(idx, sub_ex.span)
    }
}

impl<'a, 'tcx> intravisit::Visitor<'tcx> for HirRewriteVisitor<'a, 'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_expr(&mut self, ex: &'tcx hir::Expr<'tcx>) {
        let mut hir_rw = Rewrite::Identity;

        if let Some(loc) = self.find_primary_location(ex) {
            self.locations_visited.insert(loc);
            let rws = self.rewrites.get(&loc).map_or(&[] as &[_], |v| v);
            for rw in rws {
                match rw.kind {
                    mir_op::RewriteKind::OffsetSlice { mutbl } => {
                        assert!(matches!(hir_rw, Rewrite::Identity));
                        //assert_eq!(num_args, 2);
                        // `p.offset(i)` -> `&p[i as usize ..]`
                        let arr = self.get_subexpr(ex, 0);
                        let idx = Rewrite::CastUsize(Box::new(self.get_subexpr(ex, 1)));
                        let elem = Rewrite::SliceTail(Box::new(arr), Box::new(idx));
                        hir_rw = Rewrite::Ref(Box::new(elem), mutbl_from_bool(mutbl));
                    }

                    mir_op::RewriteKind::SliceFirst { mutbl } => {
                        // `p` -> `&p[0]`
                        let arr = hir_rw;
                        let idx = Rewrite::LitZero;
                        let elem = Rewrite::Index(Box::new(arr), Box::new(idx));
                        hir_rw = Rewrite::Ref(Box::new(elem), mutbl_from_bool(mutbl));
                    }

                    mir_op::RewriteKind::MutToImm => {
                        // `p` -> `&*p`
                        let place = Rewrite::Deref(Box::new(hir_rw));
                        hir_rw = Rewrite::Ref(Box::new(place), hir::Mutability::Not);
                    }

                    mir_op::RewriteKind::RemoveAsPtr => {
                        assert!(matches!(hir_rw, Rewrite::Identity));
                        //assert_eq!(num_args, 1);
                        hir_rw = self.get_subexpr(ex, 0);
                    }
                }
            }
        }

        // Emit rewrites on subexpressions first.
        let applied_mir_rewrite = !matches!(hir_rw, Rewrite::Identity);
        self.with_materialize_adjustments(applied_mir_rewrite, |this| {
            intravisit::walk_expr(this, ex);
        });

        // Materialize adjustments if requested by an ancestor.
        if self.materialize_adjustments {
            let adjusts = self.typeck_results.expr_adjustments(ex);
            assert!(
                adjusts.is_empty() || !applied_mir_rewrite,
                "combining adjustments ({:?}) with rewrite ({:?}) is NYI",
                adjusts,
                hir_rw,
            );
            for adj in adjusts {
                match adj.kind {
                    Adjust::NeverToAny => {
                        // Should work fine with no explicit cast.
                    }
                    Adjust::Deref(_) => {
                        hir_rw = Rewrite::Deref(Box::new(hir_rw));
                    }
                    Adjust::Borrow(AutoBorrow::Ref(_, mutbl)) => {
                        hir_rw = Rewrite::Ref(Box::new(hir_rw), mutbl.into());
                    }
                    Adjust::Borrow(AutoBorrow::RawPtr(mutbl)) => {
                        hir_rw = Rewrite::AddrOf(Box::new(hir_rw), mutbl.into());
                    }
                    Adjust::Pointer(PointerCast::Unsize) => {
                        // TODO: figure out what kind of unsize this is and insert an explicit cast
                        // (e.g. `&x` -> `&x as &[_]`).  In many cases this should work without a cast.
                    }
                    Adjust::Pointer(cast) => todo!("Adjust::Pointer({:?})", cast),
                }
            }
        }

        // Emit the rewrite, if it's nontrivial.
        if !matches!(hir_rw, Rewrite::Identity) {
            eprintln!(
                "rewrite {:?} at {:?} (materialize? {})",
                hir_rw, ex.span, self.materialize_adjustments
            );
            self.hir_rewrites.push((ex.span, hir_rw));
        }
    }
}

fn mutbl_from_bool(m: bool) -> hir::Mutability {
    if m {
        hir::Mutability::Mut
    } else {
        hir::Mutability::Not
    }
}

pub fn gen_hir_rewrites<'tcx>(
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    hir_body_id: hir::BodyId,
    rewrites: &HashMap<Location, Vec<MirRewrite>>,
) -> Vec<(Span, Rewrite)> {
    // Build `span_index`, which maps `Span`s to MIR `Locations`.
    let mut span_index_items = Vec::new();
    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        for (i, stmt) in bb_data.statements.iter().enumerate() {
            let loc = Location {
                block: bb,
                statement_index: i,
            };
            span_index_items.push((stmt.source_info.span, loc));
        }

        let loc = Location {
            block: bb,
            statement_index: bb_data.statements.len(),
        };
        span_index_items.push((bb_data.terminator().source_info.span, loc));
    }

    let span_index = SpanIndex::new(span_index_items);

    // Run the visitor.
    let typeck_results = tcx.typeck_body(hir_body_id);
    let hir = tcx.hir().body(hir_body_id);

    let mut v = HirRewriteVisitor {
        tcx,
        typeck_results,
        mir,
        span_index,
        rewrites,
        locations_visited: HashSet::new(),
        hir_rewrites: Vec::new(),
        materialize_adjustments: false,
    };
    intravisit::Visitor::visit_body(&mut v, hir);

    // Check that all locations with rewrites were visited at least once.
    let mut locs = rewrites.keys().cloned().collect::<Vec<_>>();
    locs.sort();
    let mut found_unvisited_loc = false;
    for loc in locs {
        if v.locations_visited.contains(&loc) {
            continue;
        }
        let rws = &rewrites[&loc];
        if rws.len() == 0 {
            continue;
        }

        found_unvisited_loc = true;
        eprintln!("error: location {:?} has rewrites but wasn't visited", loc);
        eprintln!("  stmt = {:?}", mir.stmt_at(loc));
        eprintln!(
            "  span = {:?}",
            mir.stmt_at(loc)
                .either(|s| s.source_info.span, |t| t.source_info.span)
        );
        eprintln!("  rewrites = {:?}", rws);
    }
    assert!(!found_unvisited_loc);

    v.hir_rewrites
}
