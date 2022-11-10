use crate::expr_rewrite::mir_op::{self, MirRewrite};
use crate::expr_rewrite::span_index::SpanIndex;
use rustc_hir as hir;
use rustc_hir::def::Res;
use rustc_hir::intravisit;
use rustc_middle::hir::nested_filter;
use rustc_middle::mir::{self, Body, Location};
use rustc_middle::ty::adjustment::{Adjust, AutoBorrow, PointerCast};
use rustc_middle::ty::{TyCtxt, TypeckResults};
use rustc_span::Span;
use std::collections::HashMap;

#[derive(Debug)]
enum Rewrite {
    /// Take the original expression unchanged.
    Identity,
    /// Extract the subexpression at the given index.
    Subexpr(usize),
    /// `&e`, `&mut e`
    Ref(Box<Rewrite>, hir::Mutability),
    /// `core::ptr::addr_of!(e)`, `core::ptr::addr_of_mut!(e)`
    AddrOf(Box<Rewrite>, hir::Mutability),
    /// `*e`
    Deref(Box<Rewrite>),
    /// `arr[idx]`
    Index(Box<Rewrite>, Box<Rewrite>),
    /// `arr[idx..]`
    SliceTail(Box<Rewrite>, Box<Rewrite>),
    /// `e as usize`
    CastUsize(Box<Rewrite>),
    /// The integer literal `0`.
    LitZero,
}

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
}

impl<'a, 'tcx> HirRewriteVisitor<'a, 'tcx> {
    /// Find the sole `Location` where the provided filters match and the statement or terminator
    /// has a span exactly equal to `target_span`.  Panics if there is no such location or if there
    /// are multiple matching locations.
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
}

impl<'a, 'tcx> intravisit::Visitor<'tcx> for HirRewriteVisitor<'a, 'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_expr(&mut self, ex: &'tcx hir::Expr<'tcx>) {
        // Emit rewrites on subexpressions first.
        intravisit::walk_expr(self, ex);

        let mut hir_rw = Rewrite::Identity;

        if let Some(loc) = self.find_primary_location(ex) {
            let rws = self.rewrites.get(&loc).map_or(&[] as &[_], |v| v);
            for rw in rws {
                match rw.kind {
                    mir_op::RewriteKind::OffsetSlice { mutbl } => {
                        assert!(matches!(hir_rw, Rewrite::Identity));
                        //assert_eq!(num_args, 2);
                        // `p.offset(i)` -> `&p[i as usize ..]`
                        let arr = Rewrite::Subexpr(0);
                        let idx = Rewrite::CastUsize(Box::new(Rewrite::Subexpr(1)));
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
                        hir_rw = Rewrite::Subexpr(0);
                    }
                }
            }
        }

        // Materialize adjustments.
        let adjusts = self.typeck_results.expr_adjustments(ex);
        assert!(
            adjusts.is_empty() || matches!(hir_rw, Rewrite::Identity),
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

        eprintln!("rewrite {:?} at {:?}", hir_rw, ex.span);
    }
}

fn mutbl_from_bool(m: bool) -> hir::Mutability {
    if m {
        hir::Mutability::Mut
    } else {
        hir::Mutability::Not
    }
}

pub fn test_visitor<'tcx>(
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    hir_body_id: hir::BodyId,
    rewrites: &HashMap<Location, Vec<MirRewrite>>,
) {
    eprintln!("\n === test mir visitor ===");
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
    let typeck_results = tcx.typeck_body(hir_body_id);
    let hir = tcx.hir().body(hir_body_id);

    let mut v = HirRewriteVisitor {
        tcx,
        typeck_results,
        mir,
        span_index,
        rewrites,
    };
    intravisit::Visitor::visit_body(&mut v, hir);

    // FIXME: make sure every mir_op rewrite was processed at least once
}
