use std::borrow::Borrow;
use std::collections::HashMap;

use rustc_middle::{
    mir::{
        BasicBlock, Body, LocalDecls, Location, Place, Statement, StatementKind, Terminator,
        TerminatorKind,
    },
    ty::{TyCtxt, TyKind},
};

use assert_matches::assert_matches;

use crate::util::{get_assign_sides, get_cast_place, ty_callee, Callee};

/// The direction of a [`*c_void`](core::ffi::c_void) cast.
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum CVoidCastDirection {
    /// From [`*c_void`](core::ffi::c_void) to another pointer type.
    ///
    /// This is used immediately after allocating,
    /// after both [`Callee::Malloc`], [`Callee::Calloc`], and [`Callee::Realloc`].
    From,

    /// To [`*c_void`](core::ffi::c_void) from another pointer type.
    ///
    /// This is used immediately before freeing,
    /// before both [`Callee::Free`] and [`Callee::Realloc`].
    To,
}

impl CVoidCastDirection {
    /// For applicable [`Callee`]s that take or return [`*c_void`](core::ffi::c_void),
    /// return the [`CVoidCastDirection`]s that they use for casts.
    ///
    /// That is, these [`Callee`]s are [`CVoidCastDirection::From`]:
    /// * [`Callee::Malloc`]
    /// * [`Callee::Calloc`]
    /// * [`Callee::Realloc`]
    ///
    /// And these [`Callee`]s are [`CVoidCastDirection::To`]:
    /// * [`Callee::Free`]
    /// * [`Callee::Realloc`]
    pub fn from_callee(callee: Callee) -> &'static [Self] {
        use CVoidCastDirection::*;
        use Callee::*;
        match callee {
            Malloc | Calloc => &[From][..],
            Realloc => &[To, From][..],
            Free => &[To][..],
            _ => &[],
        }
    }
}

/// The [`Place`] of a [`*c_void`].
///
/// It is checked to be a [`*c_void`] upon construction.
///
/// [`*c_void`]: core::ffi::c_void
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct CVoidPtr<'tcx> {
    place: Place<'tcx>,
}

impl<'tcx> CVoidPtr<'tcx> {
    /// Check if a [`Place`] is really a [`*c_void`](core::ffi::c_void)
    /// by checking its [`TyCtxt::def_path`] and [`TyCtxt::item_name`].
    ///
    /// This panics on failure.
    pub fn checked(place: Place<'tcx>, local_decls: &LocalDecls<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        let deref_ty = place
            .ty(local_decls, tcx)
            .ty
            .builtin_deref(true)
            .unwrap()
            .ty
            .kind();
        assert_matches!(deref_ty, TyKind::Adt(adt, _) => {
            assert_eq!(tcx.def_path(adt.did()).data[0].to_string(), "ffi");
            assert_eq!(tcx.item_name(adt.did()).as_str(), "c_void");
        });
        Self { place }
    }

    /// Check another [`Place`] for being a [`*c_void`](core::ffi::c_void)
    /// by comparing it to ourself, which is already checked.
    ///
    /// If the [`Place`] is the same [`CVoidPtr`], return it as [`Ok`];
    /// otherwise, return the same [`Place`] as [`Err`].
    pub fn checked_by_eq(&self, place: Place<'tcx>) -> Result<Self, Place<'tcx>> {
        if self.place == place {
            Ok(Self { place })
        } else {
            Err(place)
        }
    }

    /// Try to get the appropriate [`CVoidCast`]
    /// from this [`Statement`] in the given [`CVoidCastDirection`].
    ///
    /// For the [`From`] direction, the [`*c_void`] is on the rhs.\
    /// For the [`To`] direction, the [`*c_void`] is on the lhs.
    ///
    /// [`*c_void`]: core::ffi::c_void
    /// [`From`]: CVoidCastDirection::From
    /// [`To`]: CVoidCastDirection::To
    pub fn get_cast_from_stmt(
        &self,
        direction: CVoidCastDirection,
        stmt: &Statement<'tcx>,
    ) -> Option<CVoidCast<'tcx>> {
        let (lhs, rv) = get_assign_sides(stmt)?;
        let rhs = get_cast_place(rv)?;

        use CVoidCastDirection::*;
        let cast = match direction {
            From => CVoidCast {
                c_void_ptr: self.checked_by_eq(rhs).ok()?,
                other_ptr: lhs,
            },
            To => CVoidCast {
                c_void_ptr: self.checked_by_eq(lhs).ok()?,
                other_ptr: rhs,
            },
        };
        Some(cast)
    }
}

impl<'tcx> Borrow<Place<'tcx>> for CVoidPtr<'tcx> {
    fn borrow(&self) -> &Place<'tcx> {
        &self.place
    }
}

#[derive(Clone)]
/// A cast to/from a [`*c_void`](core::ffi::c_void) to/from a properly typed pointer.
pub struct CVoidCast<'tcx> {
    /// The [`*c_void`](core::ffi::c_void) side of the cast.
    ///
    /// It can be either the [`From`] or [`To`] side.
    ///
    /// [`From`]: CVoidCastDirection::From
    /// [`To`]: CVoidCastDirection::To
    c_void_ptr: CVoidPtr<'tcx>,

    /// The non-[`*c_void`](core::ffi::c_void), properly typed pointer side of the cast.
    ///
    /// It can be either the [`From`] or [`To`] side.
    ///
    /// [`From`]: CVoidCastDirection::From
    /// [`To`]: CVoidCastDirection::To
    other_ptr: Place<'tcx>,
}

/// A mapping from [`*c_void`](core::ffi::c_void)s ([`CVoidPtr`]s)
/// to their properly typed pointers,
/// like [`CVoidCasts`], but in a single direction, meaning it represents either
/// [`CVoidCastDirection::From`] or [`CVoidCastDirection::To`].
#[derive(Default, Clone, Debug)]
pub struct CVoidCastsUniDirectional<'tcx>(HashMap<Location, (CVoidPtr<'tcx>, Place<'tcx>)>);

impl<'tcx> CVoidCastsUniDirectional<'tcx> {
    pub fn contains(&self, loc: &Location) -> bool {
        self.0.contains_key(loc)
    }

    /// Get the adjusted [`Place`], skipping over [`*c_void`](core::ffi::c_void) intermediaries.
    ///
    /// That is, if `place` is a [`CVoidPtr`] in this map of [`CVoidCast`]s,
    /// then the [`Place`] of its other, property-typed pointer is returned.
    /// Otherwise, the same `place` is returned, as no adjustments are necessary.
    pub fn get_adjusted_place_or_default_to(
        &self,
        loc: &Location,
        place: Place<'tcx>,
    ) -> Place<'tcx> {
        *self.0.get(loc).map(|(_void, subst)| subst).unwrap_or(&place)
    }

    pub fn insert(&mut self, loc: Location, cast: CVoidCast<'tcx>) {
        self.0.insert(loc, (cast.c_void_ptr, cast.other_ptr));
    }
}

/// A mapping from [`*c_void`](core::ffi::c_void)s ([`CVoidPtr`]s)
/// to their properly typed pointers.
///
/// For example, in
///
/// ```mir
/// _1 = malloc(...);
/// _2 = _1 as *mut T;
/// ```
///
/// this would map `_1` => `_2`.
///
/// And in
///
/// ```mir
/// _1 = ...;
/// _2 = _1 as *mut c_void;
/// free(_2);
/// ```
///
/// this would map `_2` to `_1`.
///
/// This is in order to map them back,
/// skipping over the `*c_void`,
/// thus yielding (for the two above examples),
///
/// ```mir
/// _2 = malloc(...);
/// ```
///
/// and
///
/// ```mir
/// _1 = ...;
/// free(_1);
/// ```
#[derive(Default, Clone)]
pub struct CVoidCasts<'tcx> {
    from: CVoidCastsUniDirectional<'tcx>,
    to: CVoidCastsUniDirectional<'tcx>,
}

impl<'tcx> CVoidCasts<'tcx> {
    pub fn direction(&self, direction: CVoidCastDirection) -> &CVoidCastsUniDirectional<'tcx> {
        use CVoidCastDirection::*;
        match direction {
            From => &self.from,
            To => &self.to,
        }
    }

    fn direction_mut(
        &mut self,
        direction: CVoidCastDirection,
    ) -> &mut CVoidCastsUniDirectional<'tcx> {
        use CVoidCastDirection::*;
        match direction {
            From => &mut self.from,
            To => &mut self.to,
        }
    }

    /// See [`CVoidCastsUniDirectional::get_adjusted_place_or_default_to`].
    pub fn get_adjusted_place_or_default_to(
        &self,
        loc: &Location,
        direction: CVoidCastDirection,
        place: Place<'tcx>,
    ) -> Place<'tcx> {
        self.direction(direction)
            .get_adjusted_place_or_default_to(loc, place)
    }

    /// See [`CVoidCastsUniDirectional::insert`].
    fn insert(&mut self, loc: Location, direction: CVoidCastDirection, cast: CVoidCast<'tcx>) {
        self.direction_mut(direction).insert(loc, cast)
    }

    /// Determine if this [`Statement`] should be skipped
    /// because it contains a [`CVoidCast`] that is in this [`CVoidCasts`],
    /// as it will be handled in a way that effectively elides the cast.
    ///
    /// That is, if this [`Statement`] is an [`Assign`],
    /// then it will be skipped if:
    ///
    /// * the lhs [`Place`] of the [`Assign`] is a [`CVoidPtr`] in the [`To`] direction,
    ///     as in the [`To`] direction, the [`CVoidPtr`] is on the lhs.
    ///
    /// * the rhs [`Place`] of the [`Cast`] of the [`Assign`] is a [`CVoidPtr`] in the [`From`] direction,
    ///     as in the [`From`] direction, the [`CVoidPtr`] is on the rhs.
    ///
    /// [`Assign`]: rustc_middle::mir::StatementKind::Assign
    /// [`Cast`]: rustc_middle::mir::Rvalue::Cast
    /// [`From`]: CVoidCastDirection::From
    /// [`To`]: CVoidCastDirection::To
    pub fn should_skip_stmt(&self, loc: &Location) -> bool {
        self.to.contains(loc) || self.from.contains(loc)
    }

    /// Insert all applicable [`*c_void`] casts
    /// from a function [`Body`] into this [`CVoidCasts`].
    ///
    /// That is, find all calls to:
    ///
    /// * `malloc`
    /// * `calloc`
    /// * `realloc`
    /// * `free`
    ///
    /// and insert their casts to and from [`*c_void`].
    ///
    /// Note that since [`Place`]s are local to a function [`Body`],
    /// a [`CVoidCasts`] is only valid when it contains things from one [`Body`].
    ///
    /// [`*c_void`]: core::ffi::c_void
    fn insert_all_from_body(&mut self, body: &Body<'tcx>, tcx: TyCtxt<'tcx>) {
        for (block, bb_data) in body.basic_blocks().iter().enumerate() {
            let term: &Terminator = match &bb_data.terminator {
                Some(term) => term,
                None => continue,
            };
            let (func, args, destination, target) = match term.kind {
                TerminatorKind::Call {
                    ref func,
                    ref args,
                    destination,
                    target,
                    ..
                } => (func, args, destination, target),
                _ => continue,
            };
            let func_ty = func.ty(&body.local_decls, tcx);

            // For [`CVoidCastDirection::From`], we only count
            // a cast from `*c_void` to an arbitrary type in the subsequent block,
            // searching forward.
            let find_first_cast_succ_block = |get_cast| {
                let successor_block_id = target.unwrap();
                body.basic_blocks()[successor_block_id]
                    .statements
                    .iter()
                    .enumerate()
                    .find(|(_, s)| !matches!(s.kind, StatementKind::StorageDead(_)))
                    .map(get_cast)
                    .and_then(|(idx, cast)| Some((idx, cast?)))
            };

            // For [`CVoidCastDirection::From`], we only count
            // a cast to `*c_void` from an arbitrary type in the same block,
            // searching backwards.
            let find_last_cast_curr_block = |get_cast| {
                bb_data
                    .statements
                    .iter()
                    .enumerate()
                    .map(get_cast)
                    .rev()
                    .filter_map(|(idx, cast)| Some((idx, cast?)))
                    .next()
            };

            for direction in CVoidCastDirection::from_callee(ty_callee(tcx, func_ty))
                .iter()
                .copied()
            {
                use CVoidCastDirection::*;
                let c_void_ptr = match direction {
                    From => destination,
                    To => args[0].place().unwrap(),
                };
                let c_void_ptr = CVoidPtr::checked(c_void_ptr, &body.local_decls, tcx);
                let get_cast = move |(idx, stmt): (usize, &Statement<'tcx>)| {
                    (idx, c_void_ptr.get_cast_from_stmt(direction, stmt))
                };
                let cast = match direction {
                    From => find_first_cast_succ_block(get_cast),
                    To => find_last_cast_curr_block(get_cast),
                };
                if let Some((statement_index, cast)) = cast {
                    let loc = Location {
                        statement_index,
                        block: match direction {
                            To => BasicBlock::from_usize(block),
                            From => target.unwrap(),
                        },
                    };
                    self.insert(loc, direction, cast.clone());

                    let loc = Location {
                        statement_index: bb_data.statements.len(),
                        block: BasicBlock::from_usize(block),
                    };
                    self.insert(loc, direction, cast);
                }
            }
        }
    }

    pub fn new(body: &Body<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        let mut this = Self::default();
        this.insert_all_from_body(body, tcx);
        this
    }
}
