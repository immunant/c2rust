use rustc_middle::{
    mir::{Place, PlaceRef, ProjectionElem},
    ty::TyCtxt,
};

pub fn has_outer_deref(p: &Place) -> bool {
    matches!(
        p.iter_projections().last(),
        Some((_, ProjectionElem::Deref))
    )
}

/// Strip the initital [`Deref`](ProjectionElem::Deref)
/// from a [`projection`](PlaceRef::projection) sequence.
pub fn remove_outer_deref<'tcx>(p: Place<'tcx>, tcx: TyCtxt<'tcx>) -> Place<'tcx> {
    // Remove outer deref if present
    match p.as_ref() {
        PlaceRef {
            local,
            projection: &[ref base @ .., ProjectionElem::Deref],
        } => Place {
            local,
            projection: tcx.intern_place_elems(base),
        },
        _ => p,
    }
}
