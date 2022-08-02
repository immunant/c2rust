use rustc_middle::{
    mir::{Place, PlaceRef, ProjectionElem},
    ty::TyCtxt,
};

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

fn try_remove_outer_deref_as_ref<'tcx>(p: &Place<'tcx>) -> Option<PlaceRef<'tcx>> {
    // Remove outer deref if present
    match p.as_ref() {
        PlaceRef {
            local,
            projection: &[ref projection @ .., ProjectionElem::Deref],
        } => Some(PlaceRef { local, projection }),
        _ => None,
    }
}

/// Try to strip the initital [`Deref`](ProjectionElem::Deref)
/// from a [`projection`](PlaceRef::projection) sequence.
pub fn try_remove_outer_deref<'tcx>(p: Place<'tcx>, tcx: TyCtxt<'tcx>) -> Option<Place<'tcx>> {
    try_remove_outer_deref_as_ref(&p).map(|PlaceRef { local, projection }| Place {
        local,
        projection: tcx.intern_place_elems(projection),
    })
}

/// Strip the initital [`Deref`](ProjectionElem::Deref)
/// from a [`projection`](PlaceRef::projection) sequence
/// if there is one.
pub fn remove_outer_deref<'tcx>(p: Place<'tcx>, tcx: TyCtxt<'tcx>) -> Place<'tcx> {
    try_remove_outer_deref(p, tcx).unwrap_or(p)
}
