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

/// Get the inner-most dereferenced [`Place`].
pub fn strip_all_deref<'tcx>(p: &Place<'tcx>, tcx: TyCtxt<'tcx>) -> Place<'tcx> {
    let mut base_dest = p.as_ref();
    let mut place_ref = p.clone().as_ref();
    while let Some((cur_ref, proj)) = place_ref.last_projection() {
        if let ProjectionElem::Deref = proj {
            base_dest = cur_ref;
        }
        place_ref = cur_ref;
    }

    Place {
        local: base_dest.local,
        projection: tcx.intern_place_elems(base_dest.projection),
    }
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
