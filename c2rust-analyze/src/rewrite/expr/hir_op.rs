use crate::panic_detail;
use crate::rewrite::expr::mir_op::{self, MirRewrite};
use crate::rewrite::span_index::SpanIndex;
use crate::rewrite::{build_span_index, Rewrite, SoleLocationError};
use assert_matches::assert_matches;
use hir::{ExprKind, UnOp};
use rustc_hir as hir;
use rustc_hir::def::{Namespace, Res};
use rustc_hir::intravisit;
use rustc_middle::hir::nested_filter;
use rustc_middle::mir::{self, Body, Location};
use rustc_middle::ty::adjustment::{Adjust, Adjustment, AutoBorrow, PointerCast};
use rustc_middle::ty::print::{FmtPrinter, Print};
use rustc_middle::ty::{TyCtxt, TypeckResults};
use rustc_span::Span;
use std::collections::{HashMap, HashSet};

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

    /// Find all `Location`s where the provided filters match and the statement or terminator
    /// has a span exactly equal to `target_span`.
    fn find_all_locations_matching(
        &mut self,
        target_span: Span,
        mut filter_stmt: impl FnMut(&mir::Statement<'tcx>) -> bool,
        mut filter_term: impl FnMut(&mir::Terminator<'tcx>) -> bool,
    ) -> Vec<Location> {
        let mut found_locs = Vec::new();
        for (span, &loc) in self.span_index.lookup(target_span) {
            if span != target_span {
                continue;
            }
            let matched = self
                .mir
                .stmt_at(loc)
                .either(&mut filter_stmt, &mut filter_term);
            if !matched {
                continue;
            }

            found_locs.push(loc);
        }

        found_locs
    }

    /// Find the sole `Location` where the provided filters match and the statement or terminator
    /// has a span exactly equal to `target_span`. Returns `Err` if there is no such location or
    /// if there are multiple matching locations.
    fn find_sole_location_matching(
        &mut self,
        target_span: Span,
        filter_stmt: impl FnMut(&mir::Statement<'tcx>) -> bool,
        filter_term: impl FnMut(&mir::Terminator<'tcx>) -> bool,
    ) -> Result<Location, SoleLocationError> {
        let found_locs = self.find_all_locations_matching(target_span, filter_stmt, filter_term);

        match found_locs.len() {
            0 => Err(SoleLocationError::NoMatch),
            1 => Ok(found_locs[0]),
            _ => Err(SoleLocationError::MultiMatch(found_locs)),
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

    fn find_locations(&mut self, ex: &'tcx hir::Expr<'tcx>) -> Vec<Location> {
        let panic_location_error = |err, desc| -> ! {
            match err {
                SoleLocationError::NoMatch => panic!("couldn't find {} for {:?}", desc, ex),
                SoleLocationError::MultiMatch(locations) => {
                    let all_matches = locations
                        .iter()
                        .map(|loc| self.mir.stmt_at(*loc))
                        .collect::<Vec<_>>();

                    panic!(
                        "expected to find only one {}, but got multiple:\n\
                         expr = {:?}\n\
                         matches = {:?}",
                        desc, ex, all_matches,
                    )
                }
            }
        };

        let mut locations = Vec::new();

        let mut push_assign_loc = |span: Span| match self.find_optional_location_matching(
            span,
            |stmt| matches!(stmt.kind, mir::StatementKind::Assign(..)),
            |_term| false,
        ) {
            Ok(Some(assign_loc)) => locations.push(assign_loc),
            Ok(None) => {}
            Err(err) => panic_location_error(err, "Assign statement"),
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
                    .unwrap_or_else(|err| panic_location_error(err, "Call terminator"));
                locations.push(call_loc)
            }
            hir::ExprKind::Path(hir::QPath::Resolved(_, p)) if matches!(p.res, Res::Local(..)) => {
                // Currently we only handle cases where the local is copied into a temporary.  If
                // the local is used as-is in some other expression, this case will return `None`.
                push_assign_loc(ex.span)
            }
            hir::ExprKind::Unary(UnOp::Deref, ..)
            | hir::ExprKind::Lit(..)
            | hir::ExprKind::Field(..) => {
                // For `hir::ExprKind::Field` we currently we only handle the case where the value
                // retrieved from the field is stored into a temporary.  If it's stored into a
                // local or some other place (e.g. `let y = x.f;`, or `y = x.f;` alone), then
                // this case will return `None`.
                //
                // Also, for chained field accesses, this only matches the outermost ones.  Code
                // like `x.0.0` translates into a single MIR statement with the span of the overall
                // expression.
                push_assign_loc(ex.span)
            }
            hir::ExprKind::AddrOf(..) => {
                locations.extend(
                    self.find_all_locations_matching(
                        ex.span,
                        |stmt| matches!(stmt.kind, mir::StatementKind::Assign(..)),
                        |_term| false,
                    )
                    .iter(),
                );
            }
            hir::ExprKind::Assign(..) => {
                let assign_loc = self
                    .find_sole_location_matching(
                        ex.span,
                        |stmt| matches!(stmt.kind, mir::StatementKind::Assign(..)),
                        |_term| false,
                    )
                    .unwrap_or_else(|err| panic_location_error(err, "Assignment statement"));
                locations.push(assign_loc)
            }
            _ => eprintln!("warning: find_primary_location: unsupported expr {:?}", ex),
        }

        locations
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
        let _g = panic_detail::set_current_span(ex.span);
        let mut hir_rw = Rewrite::Identity;

        // This span will be used to apply the actual rewrite.
        // To prevent attempts to rewrite macros in their definition
        // location (e.g. `std::ptr::addr_of`) we instead specify that
        // the rewrite should occur at the callsite
        let callsite_span = ex.span.source_callsite();

        let locs = self.find_locations(ex);
        for loc in &locs {
            self.locations_visited.insert(*loc);
        }

        let all_rws_unflattened: Vec<_> = locs
            .iter()
            .map(|loc| {
                self.rewrites
                    .get(loc)
                    .map(|rws| rws.iter().map(|rw| &rw.kind).collect::<Vec<_>>())
                    .unwrap_or_else(Vec::new)
            })
            .collect();

        let all_rws_unflattened = all_rws_unflattened
            .iter()
            .map(|v| v.as_slice())
            .collect::<Vec<_>>();

        let rewrite_from_mir_rws = |rw: &mir_op::RewriteKind, hir_rw: Rewrite| -> Rewrite {
            match rw {
                mir_op::RewriteKind::OffsetSlice { mutbl } => {
                    // `p.offset(i)` -> `&p[i as usize ..]`
                    let arr = self.get_subexpr(ex, 0);
                    let idx = Rewrite::Cast(Box::new(self.get_subexpr(ex, 1)), "usize".to_owned());
                    let elem = Rewrite::SliceTail(Box::new(arr), Box::new(idx));
                    Rewrite::Ref(Box::new(elem), mutbl_from_bool(*mutbl))
                }

                mir_op::RewriteKind::SliceFirst { mutbl } => {
                    // `p` -> `&p[0]`
                    let arr = hir_rw;
                    let idx = Rewrite::LitZero;
                    let elem = Rewrite::Index(Box::new(arr), Box::new(idx));
                    Rewrite::Ref(Box::new(elem), mutbl_from_bool(*mutbl))
                }

                mir_op::RewriteKind::MutToImm => {
                    // `p` -> `&*p`
                    let place = Rewrite::Deref(Box::new(hir_rw));
                    Rewrite::Ref(Box::new(place), hir::Mutability::Not)
                }

                mir_op::RewriteKind::RemoveAsPtr => {
                    // `slice.as_ptr()` -> `slice`
                    assert!(matches!(hir_rw, Rewrite::Identity));
                    self.get_subexpr(ex, 0)
                }

                mir_op::RewriteKind::RawToRef { mutbl } => {
                    // &raw _ to &_ or &raw mut _ to &mut _
                    Rewrite::Ref(Box::new(self.get_subexpr(ex, 0)), mutbl_from_bool(*mutbl))
                }

                mir_op::RewriteKind::CellNew => {
                    // `x` to `Cell::new(x)`
                    Rewrite::Call("std::cell::Cell::new".to_string(), vec![Rewrite::Identity])
                }

                mir_op::RewriteKind::CellGet => {
                    // `*x` to `Cell::get(x)`
                    Rewrite::MethodCall(
                        "get".to_string(),
                        Box::new(self.get_subexpr(ex, 0)),
                        vec![],
                    )
                }

                mir_op::RewriteKind::CellSet => {
                    // `*x` to `Cell::set(x)`
                    let deref_lhs = assert_matches!(ex.kind, ExprKind::Assign(lhs, ..) => lhs);
                    let lhs = self.get_subexpr(deref_lhs, 0);
                    let rhs = self.get_subexpr(ex, 1);
                    Rewrite::MethodCall("set".to_string(), Box::new(lhs), vec![rhs])
                }
            }
        };

        let is_addr_of_expansion = || {
            let rvalues: Vec<_> = locs
                .into_iter()
                .map(|loc| {
                    self.mir
                        .stmt_at(loc)
                        .left()
                        .and_then(|s| s.kind.as_assign().cloned().map(|(_, rv)| rv))
                })
                .collect();
            use mir::Rvalue;
            callsite_span != ex.span
                && matches!(
                    rvalues[..],
                    [Some(Rvalue::Ref(..)), Some(Rvalue::AddressOf(..))]
                )
        };

        use mir_op::RewriteKind::*;
        if !all_rws_unflattened.is_empty() {
            hir_rw = match &all_rws_unflattened[..] {
                [[], [rw @ RawToRef { .. }]]
                    if is_addr_of_expansion() || callsite_span == ex.span =>
                {
                    rewrite_from_mir_rws(rw, hir_rw)
                }
                [rws] => rws
                    .iter()
                    .fold(hir_rw, |acc, rw| rewrite_from_mir_rws(rw, acc)),
                rwss if rwss.iter().all(|rws| rws.is_empty()) => hir_rw,
                _ => panic!(
                    "unsupported MIR rewrites {:?} for HIR expr: {:?}",
                    all_rws_unflattened, ex
                ),
            };
        }

        // Emit rewrites on subexpressions first.
        let applied_mir_rewrite = !matches!(hir_rw, Rewrite::Identity);
        self.with_materialize_adjustments(applied_mir_rewrite, |this| {
            intravisit::walk_expr(this, ex);
        });

        // Materialize adjustments if requested by an ancestor.
        if self.materialize_adjustments {
            let adjusts = self.typeck_results.expr_adjustments(ex);
            hir_rw = materialize_adjustments(self.tcx, adjusts, hir_rw);
        }

        // Emit the rewrite, if it's nontrivial.
        if !matches!(hir_rw, Rewrite::Identity) {
            eprintln!(
                "rewrite {:?} at {:?} (materialize? {})",
                hir_rw, callsite_span, self.materialize_adjustments
            );
            self.hir_rewrites.push((callsite_span, hir_rw));
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

fn apply_identity_adjustment<'tcx>(
    tcx: TyCtxt<'tcx>,
    adjustment: &Adjustment<'tcx>,
    rw: Rewrite,
) -> Rewrite {
    match adjustment.kind {
        Adjust::NeverToAny => rw,
        Adjust::Deref(_) => Rewrite::Deref(Box::new(rw)),
        Adjust::Borrow(AutoBorrow::Ref(_, mutbl)) => Rewrite::Ref(Box::new(rw), mutbl.into()),
        Adjust::Borrow(AutoBorrow::RawPtr(mutbl)) => Rewrite::AddrOf(Box::new(rw), mutbl),
        Adjust::Pointer(PointerCast::Unsize) => {
            let ty = adjustment.target;
            let printer = FmtPrinter::new(tcx, Namespace::TypeNS);
            let s = ty.print(printer).unwrap().into_buffer();
            Rewrite::Cast(Box::new(rw), s)
        }
        Adjust::Pointer(cast) => todo!("Adjust::Pointer({:?})", cast),
    }
}

fn materialize_adjustments<'tcx>(
    tcx: TyCtxt<'tcx>,
    adjustments: &[Adjustment<'tcx>],
    hir_rw: Rewrite,
) -> Rewrite {
    let adj_kinds: Vec<&_> = adjustments.iter().map(|a| &a.kind).collect();
    match (hir_rw, &adj_kinds[..]) {
        (Rewrite::Identity, []) => Rewrite::Identity,
        (Rewrite::Identity, _) => {
            let mut hir_rw = Rewrite::Identity;
            for adj in adjustments {
                hir_rw = apply_identity_adjustment(tcx, adj, hir_rw);
            }
            hir_rw
        }
        (rw @ Rewrite::Ref(..), &[Adjust::Deref(..), Adjust::Borrow(..)]) => rw,
        (rw, &[]) => rw,
        (rw, adjs) => panic!("rewrite {rw:?} and materializations {adjs:?} NYI"),
    }
}

pub fn gen_hir_rewrites<'tcx>(
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    hir_body_id: hir::BodyId,
    rewrites: &HashMap<Location, Vec<MirRewrite>>,
) -> Vec<(Span, Rewrite)> {
    // Build `span_index`, which maps `Span`s to MIR `Locations`.
    let span_index = build_span_index(mir);

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
        if rws.is_empty() {
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
