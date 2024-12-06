use crate::borrowck::atoms::{AllFacts, AtomMaps, Loan, Path, SubPoint};
use log::debug;
use rustc_middle::mir::visit::{
    MutatingUseContext, NonMutatingUseContext, NonUseContext, PlaceContext, Visitor,
};
use rustc_middle::mir::{
    Body, BorrowKind, Local, Location, Place, ProjectionElem, Statement, StatementKind,
};
use rustc_middle::ty::{List, TyCtxt};
use std::cmp;
use std::collections::HashMap;

// From `rustc_borrowck/src/def_use.rs`, licensed MIT/Apache2
#[derive(Eq, PartialEq, Clone, Debug)]
pub enum DefUse {
    Def,
    Use,
    Drop,
}

// From `rustc_borrowck/src/def_use.rs`, licensed MIT/Apache2
pub fn categorize(context: PlaceContext) -> Option<DefUse> {
    match context {
        ///////////////////////////////////////////////////////////////////////////
        // DEFS

        PlaceContext::MutatingUse(MutatingUseContext::Store) |

        // We let Call define the result in both the success and
        // unwind cases. This is not really correct, however it
        // does not seem to be observable due to the way that we
        // generate MIR. To do things properly, we would apply
        // the def in call only to the input from the success
        // path and not the unwind path. -nmatsakis
        PlaceContext::MutatingUse(MutatingUseContext::Call) |
        PlaceContext::MutatingUse(MutatingUseContext::AsmOutput) |
        PlaceContext::MutatingUse(MutatingUseContext::Yield) => Some(DefUse::Def),

        // Storage live and storage dead aren't proper defines, but we can ignore
        // values that come before them.
        //
        // C2Rust: For borrowchecking purposes, we ignore `StorageLive` and `StorageDead`.  In the
        // original version of this function, they're considered to be `DefUse::Def`s, but this
        // approach creates a problem for code like this:
        //
        // ```
        // let q = {
        //   let p = &mut ...;
        //   p
        // };
        // *q = 1;
        // ```
        //
        // The MIR for this has an assignment like `q = p`, followed by `StorageDead(p)`.  We
        // interpret the assignment as a reborrow of `p`, and if `StorageDead(p)` was considered a
        // `Def`, we would invalidate the loan at the end of the block when `StorageDead` "writes"
        // to `p`.  However, this code is perfectly valid, and omitting `loan_invalidated_at` for
        // `StorageLive` and `StorageDead` appears to be consistent with `rustc -Z nll-facts`
        // output (tested on `tests/filecheck/move_mut.rs`).
        PlaceContext::NonUse(NonUseContext::StorageLive) |
        PlaceContext::NonUse(NonUseContext::StorageDead) => None,

        ///////////////////////////////////////////////////////////////////////////
        // REGULAR USES
        //
        // These are uses that occur *outside* of a drop. For the
        // purposes of NLL, these are special in that **all** the
        // lifetimes appearing in the variable must be live for each regular use.

        PlaceContext::NonMutatingUse(NonMutatingUseContext::Projection) |
        PlaceContext::MutatingUse(MutatingUseContext::Projection) |

        // Borrows only consider their local used at the point of the borrow.
        // This won't affect the results since we use this analysis for generators
        // and we only care about the result at suspension points. Borrows cannot
        // cross suspension points so this behavior is unproblematic.
        PlaceContext::MutatingUse(MutatingUseContext::Borrow) |
        PlaceContext::NonMutatingUse(NonMutatingUseContext::SharedBorrow) |
        PlaceContext::NonMutatingUse(NonMutatingUseContext::ShallowBorrow) |
        PlaceContext::NonMutatingUse(NonMutatingUseContext::UniqueBorrow) |

        PlaceContext::MutatingUse(MutatingUseContext::AddressOf) |
        PlaceContext::NonMutatingUse(NonMutatingUseContext::AddressOf) |
        PlaceContext::NonMutatingUse(NonMutatingUseContext::Inspect) |
        PlaceContext::NonMutatingUse(NonMutatingUseContext::Copy) |
        PlaceContext::NonMutatingUse(NonMutatingUseContext::Move) |
        PlaceContext::NonUse(NonUseContext::AscribeUserTy) |
        PlaceContext::MutatingUse(MutatingUseContext::Retag) =>
            Some(DefUse::Use),

        ///////////////////////////////////////////////////////////////////////////
        // DROP USES
        //
        // These are uses that occur in a DROP (a MIR drop, not a
        // call to `std::mem::drop()`). For the purposes of NLL,
        // uses in drop are special because `#[may_dangle]`
        // attributes can affect whether lifetimes must be live.

        PlaceContext::MutatingUse(MutatingUseContext::Drop) =>
            Some(DefUse::Drop),

        // Debug info is neither def nor use.
        PlaceContext::NonUse(NonUseContext::VarDebugInfo) => None,

        PlaceContext::MutatingUse(MutatingUseContext::Deinit | MutatingUseContext::SetDiscriminant) => {
            panic!("These statements are not allowed in this MIR phase")
        }
    }
}

struct DefUseVisitor<'tcx, 'a> {
    facts: &'a mut AllFacts,
    maps: &'a mut AtomMaps<'tcx>,
}

impl<'tcx> Visitor<'tcx> for DefUseVisitor<'tcx, '_> {
    fn visit_place(&mut self, place: &Place<'tcx>, context: PlaceContext, location: Location) {
        self.super_place(place, context, location);
        debug!(
            "visit place {:?} with context {:?} = {:?} at {:?}",
            place,
            context,
            categorize(context),
            location
        );

        if place.is_indirect() {
            // TODO
            //return;
        }

        let path = self.maps.path(self.facts, *place);
        let point = self.maps.point_mid_location(location);

        // TODO: figure out when exactly paths should be recorded as assigned/accessed/moved
        if let PlaceContext::NonMutatingUse(NonMutatingUseContext::Move) = context {
            self.facts.path_accessed_at_base.push((path, point));
            self.facts.path_moved_at_base.push((path, point));
            return;
        }

        match categorize(context) {
            Some(DefUse::Def) => {
                self.facts.path_assigned_at_base.push((path, point));
            }
            Some(DefUse::Use) => {
                self.facts.path_accessed_at_base.push((path, point));
            }
            Some(DefUse::Drop) => {}
            None => {}
        }
    }

    fn visit_local(&mut self, local: Local, context: PlaceContext, location: Location) {
        debug!(
            "visit local {:?} with context {:?} = {:?} at {:?}",
            local,
            context,
            categorize(context),
            location
        );
        let var = self.maps.variable(local);
        let point = self.maps.point_mid_location(location);
        match categorize(context) {
            Some(DefUse::Def) => {
                self.facts.var_defined_at.push((var, point));
            }
            Some(DefUse::Use) => {
                self.facts.var_used_at.push((var, point));
            }
            Some(DefUse::Drop) => {
                self.facts.var_dropped_at.push((var, point));
            }
            None => {}
        }
    }

    fn visit_statement(&mut self, stmt: &Statement<'tcx>, location: Location) {
        self.super_statement(stmt, location);
        debug!("visit stmt {:?} at {:?}", stmt, location);

        if let StatementKind::StorageDead(local) = stmt.kind {
            // Observed: `StorageDead` emits `path_moved_at_base` at the `Mid` point.
            let place = Place {
                local,
                projection: List::empty(),
            };
            let path = self.maps.path(self.facts, place);
            let point = self.maps.point_mid_location(location);
            self.facts.path_moved_at_base.push((path, point));
        }
    }
}

pub fn visit<'tcx>(facts: &mut AllFacts, maps: &mut AtomMaps<'tcx>, mir: &Body<'tcx>) {
    let mut v = DefUseVisitor { facts, maps };
    v.visit_body(mir);
}

struct LoanInvalidatedAtVisitor<'tcx, 'a> {
    tcx: TyCtxt<'tcx>,
    facts: &'a mut AllFacts,
    maps: &'a mut AtomMaps<'tcx>,
    loans: &'a HashMap<Local, Vec<(Path, Loan, BorrowKind)>>,
}

impl<'tcx> LoanInvalidatedAtVisitor<'tcx, '_> {
    /// Handle an access of a path overlapping `loan` in `context` at `location`.  `borrow_kind` is
    /// the original kind of the loan.
    fn access_loan_at_location(
        &mut self,
        loan: Loan,
        borrow_kind: BorrowKind,
        context: PlaceContext,
        location: Location,
    ) {
        debug!(
            "access loan {:?} (kind {:?}) at location {:?} (context {:?} = {:?})",
            loan,
            borrow_kind,
            location,
            context,
            categorize(context)
        );
        // `match` is easier to read.
        #[allow(clippy::match_like_matches_macro)]
        let invalidate = match (borrow_kind, categorize(context)) {
            (BorrowKind::Shared, Some(DefUse::Use)) => false,
            (_, None) => false,
            _ => true,
        };
        if !invalidate {
            return;
        }

        let point = self
            .maps
            .point(location.block, location.statement_index, SubPoint::Start);
        self.facts.loan_invalidated_at.push((point, loan));
    }
}

impl<'tcx> Visitor<'tcx> for LoanInvalidatedAtVisitor<'tcx, '_> {
    fn visit_place(&mut self, place: &Place<'tcx>, context: PlaceContext, location: Location) {
        //self.super_place(place, context, location);
        debug!(
            "loan_invalidated_at: visit place {:?} with context {:?} = {:?} at {:?}",
            place,
            context,
            categorize(context),
            location
        );

        if place.is_indirect() {
            // TODO
            //return;
        }

        let local_loans = self.loans.get(&place.local).map_or(&[] as &[_], |x| x);
        for &(path, loan, borrow_kind) in local_loans {
            let proj = self.maps.get_path_projection(self.tcx, path);

            // If `proj` is a prefix of `place.projection` or vice versa, then the paths overlap.
            let common_len = cmp::min(proj.len(), place.projection.len());
            let overlap = proj[..common_len]
                .iter()
                .zip(place.projection[..common_len].iter())
                .all(|(&elem1, &elem2)| match (elem1, elem2) {
                    (ProjectionElem::Field(f1, _), ProjectionElem::Field(f2, _)) => f1 == f2,
                    (ProjectionElem::Index(_), ProjectionElem::Index(_)) => true,
                    // Conservatively assume that any unsupported variants overlap.
                    _ => true,
                });
            if !overlap {
                continue;
            }

            self.access_loan_at_location(loan, borrow_kind, context, location);
        }
    }

    fn visit_local(&mut self, local: Local, context: PlaceContext, location: Location) {
        debug!(
            "loan_invalidated_at: visit local {:?} with context {:?} = {:?} at {:?}",
            local,
            context,
            categorize(context),
            location
        );

        let local_loans = self.loans.get(&local).map_or(&[] as &[_], |x| x);
        for &(_path, loan, borrow_kind) in local_loans {
            // All paths rooted in this local overlap the local.
            self.access_loan_at_location(loan, borrow_kind, context, location);
        }
    }
}

pub fn visit_loan_invalidated_at<'tcx>(
    tcx: TyCtxt<'tcx>,
    facts: &mut AllFacts,
    maps: &mut AtomMaps<'tcx>,
    loans: &HashMap<Local, Vec<(Path, Loan, BorrowKind)>>,
    mir: &Body<'tcx>,
) {
    let mut v = LoanInvalidatedAtVisitor {
        tcx,
        facts,
        maps,
        loans,
    };
    v.visit_body(mir)
}
