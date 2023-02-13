use std::borrow::Borrow;
use std::collections::HashMap;

use rustc_middle::{
    mir::{Body, LocalDecls, Place, Rvalue, Statement, StatementKind, Terminator, TerminatorKind},
    ty::{TyCtxt, TyKind},
};

use assert_matches::assert_matches;

use crate::util::{ty_callee, Callee};

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
    pub fn from_callee(callee: Option<Callee>) -> &'static [Self] {
        let callee = match callee {
            None => return &[],
            Some(it) => it,
        };
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
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
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
}

fn get_cast_place<'tcx>(rv: &Rvalue<'tcx>) -> Option<Place<'tcx>> {
    match rv {
        Rvalue::Cast(_, op, _) => op.place(),
        _ => None,
    }
}

fn get_assign_sides<'tcx, 'a>(
    stmt: &'a Statement<'tcx>,
) -> Option<(Place<'tcx>, &'a Rvalue<'tcx>)> {
    let (pl, ref rv) = match &stmt.kind {
        StatementKind::Assign(it) => Some(&**it),
        _ => None,
    }?;
    Some((*pl, rv))
}

impl<'tcx> CVoidPtr<'tcx> {
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
#[derive(Default)]
pub struct CVoidCastsUniDirectional<'tcx>(HashMap<CVoidPtr<'tcx>, Place<'tcx>>);

impl<'tcx> CVoidCastsUniDirectional<'tcx> {
    pub fn contains(&self, place: Place<'tcx>) -> bool {
        self.0.contains_key(&place)
    }

    /// Get the adjusted [`Place`], skipping over [`*c_void`](core::ffi::c_void) intermediaries.
    ///
    /// That is, if `place` is a [`CVoidPtr`] in this map of [`CVoidCast`]s,
    /// then the [`Place`] of its other, property-typed pointer is returned.
    /// Otherwise, the same `place` is returned, as no adjustments are necessary.
    pub fn get_adjusted_place(&self, place: Place<'tcx>) -> Place<'tcx> {
        *self.0.get(&place).unwrap_or(&place)
    }

    pub fn insert(&mut self, cast: CVoidCast<'tcx>) {
        self.0.insert(cast.c_void_ptr, cast.other_ptr);
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
#[derive(Default)]
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

    pub fn direction_mut(
        &mut self,
        direction: CVoidCastDirection,
    ) -> &mut CVoidCastsUniDirectional<'tcx> {
        use CVoidCastDirection::*;
        match direction {
            From => &mut self.from,
            To => &mut self.to,
        }
    }

    /// See [`CVoidCastsUniDirectional::get_adjusted_place`].
    pub fn get_adjusted_place(
        &self,
        direction: CVoidCastDirection,
        place: Place<'tcx>,
    ) -> Place<'tcx> {
        self.direction(direction).get_adjusted_place(place)
    }

    /// See [`CVoidCastsUniDirectional::insert`].
    pub fn insert(&mut self, direction: CVoidCastDirection, cast: CVoidCast<'tcx>) {
        self.direction_mut(direction).insert(cast)
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
    /// * the rhs [`Place`] of the [`Rvalue::Cast`] of the [`Assign`] is a [`CVoidPtr`] in the [`From`] direction,
    ///     as in the [`From`] direction, the [`CVoidPtr`] is on the rhs.
    ///
    /// [`Assign`]: StatementKind::Assign
    /// [`From`]: CVoidCastDirection::From
    /// [`To`]: CVoidCastDirection::To
    pub fn should_skip_stmt(&self, stmt: &Statement<'tcx>) -> bool {
        || -> Option<bool> {
            let (lhs, rv) = get_assign_sides(stmt)?;
            let skip = self.to.contains(lhs) || self.from.contains(get_cast_place(rv)?);
            Some(skip)
        }()
        .unwrap_or_default()
    }

    /// Insert all applicable [`*c_void`] casts
    /// from the `mir` [`Body`] into this [`CVoidCasts`].
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
    /// [`*c_void`]: core::ffi::c_void
    pub fn insert_all_from_mir(&mut self, mir: &Body<'tcx>, tcx: TyCtxt<'tcx>) {
        for bb_data in mir.basic_blocks().iter() {
            if let Some(term) = &bb_data.terminator {
                let term: &Terminator = term;
                if let TerminatorKind::Call {
                    ref func,
                    ref args,
                    destination,
                    target,
                    ..
                } = term.kind
                {
                    let func_ty = func.ty(&mir.local_decls, tcx);

                    // For [`CVoidCastDirection::From`], we only count
                    // a cast from `*c_void` to an arbitrary type in the subsequent block,
                    // searching forward.
                    let find_first_cast_succ_block = |get_cast| {
                        let successor_block_id = target.unwrap();
                        mir.basic_blocks()[successor_block_id]
                            .statements
                            .iter()
                            .find_map(get_cast)
                    };

                    // For [`CVoidCastDirection::From`], we only count
                    // a cast to `*c_void` from an arbitrary type in the same block,
                    // searching backwards.
                    let find_last_cast_curr_block =
                        |get_cast| bb_data.statements.iter().rev().find_map(get_cast);

                    for direction in CVoidCastDirection::from_callee(ty_callee(tcx, func_ty))
                        .iter()
                        .copied()
                    {
                        use CVoidCastDirection::*;
                        let c_void_ptr = match direction {
                            From => destination,
                            To => args[0].place().unwrap(),
                        };
                        let c_void_ptr = CVoidPtr::checked(c_void_ptr, &mir.local_decls, tcx);
                        let get_cast = move |stmt: &Statement<'tcx>| {
                            c_void_ptr.get_cast_from_stmt(direction, stmt)
                        };
                        let cast = match direction {
                            From => find_first_cast_succ_block(get_cast),
                            To => find_last_cast_curr_block(get_cast),
                        };
                        if let Some(cast) = cast {
                            self.insert(direction, cast);
                        }
                    }
                }
            }
        }
    }

    pub fn new(mir: &Body<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        let mut this = Self::default();
        this.insert_all_from_mir(mir, tcx);
        this
    }
}
