use rustc_middle::mir::{Body, Place, Local, Location, Statement, StatementKind};
use rustc_middle::mir::visit::{
    Visitor, PlaceContext, MutatingUseContext, NonMutatingUseContext, NonUseContext,
};
use rustc_middle::ty::List;
use crate::atoms::{AllFacts, AtomMaps};


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
        PlaceContext::MutatingUse(MutatingUseContext::Yield) |

        // Storage live and storage dead aren't proper defines, but we can ignore
        // values that come before them.
        PlaceContext::NonUse(NonUseContext::StorageLive) |
        PlaceContext::NonUse(NonUseContext::StorageDead) => Some(DefUse::Def),

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
    }
}


struct DefUseVisitor<'tcx, 'a> {
    facts: &'a mut AllFacts,
    maps: &'a mut AtomMaps<'tcx>,
}

impl<'tcx> Visitor<'tcx> for DefUseVisitor<'tcx, '_> {
    fn visit_place(&mut self, place: &Place<'tcx>, context: PlaceContext, location: Location) {
        self.super_place(place, context, location);
        eprintln!("visit place {:?} with context {:?} = {:?} at {:?}",
                  place, context, categorize(context), location);

        if place.is_indirect() {
            // TODO
            return;
        }

        let path = self.maps.path(self.facts, *place);
        let point = self.maps.point_mid_location(location);

        // TODO: figure out when exactly paths should be recorded as assigned/accessed/moved
        if let PlaceContext::NonMutatingUse(NonMutatingUseContext::Move) = context {
            self.facts.path_moved_at_base.push((path, point));
            return;
        }

        match categorize(context) {
            Some(DefUse::Def) => {
                self.facts.path_assigned_at_base.push((path, point));
            },
            Some(DefUse::Use) => {
                self.facts.path_accessed_at_base.push((path, point));
            },
            Some(DefUse::Drop) => {},
            None => {},
        }
    }

    fn visit_local(&mut self, local: &Local, context: PlaceContext, location: Location) {
        eprintln!("visit local {:?} with context {:?} = {:?} at {:?}",
                  local, context, categorize(context), location);
        let var = self.maps.variable(*local);
        let point = self.maps.point_mid_location(location);
        match categorize(context) {
            Some(DefUse::Def) => {
                self.facts.var_defined_at.push((var, point));
            },
            Some(DefUse::Use) => {
                self.facts.var_used_at.push((var, point));
            },
            Some(DefUse::Drop) => {
                self.facts.var_dropped_at.push((var, point));
            },
            None => {},
        }
    }

    fn visit_statement(&mut self, stmt: &Statement<'tcx>, location: Location) {
        self.super_statement(stmt, location);
        eprintln!("visit stmt {:?} at {:?}", stmt, location);

        if let StatementKind::StorageDead(local) = stmt.kind {
            // Observed: `StorageDead` emits `path_moved_at_base` at the `Mid` point.
            let place = Place { local, projection: List::empty() };
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
