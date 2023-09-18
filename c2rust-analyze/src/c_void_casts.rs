use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};

use rustc_middle::mir::{BasicBlock, LocalKind};
use rustc_middle::ty::Ty;
use rustc_middle::{
    mir::{
        BasicBlockData, Body, LocalDecls, Location, Place, Rvalue, Statement, StatementKind,
        TerminatorKind,
    },
    ty::{TyCtxt, TyKind},
};

use crate::util::{get_assign_sides, get_cast_place, terminator_location, ty_callee, Callee};

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
            Memcpy | Memset => &[To][..],
            _ => &[],
        }
    }
}

pub fn is_c_void_ptr(tcx: TyCtxt, ty: Ty) -> bool {
    if let Some(TyKind::Adt(adt, _)) = ty.builtin_deref(true).map(|ty_mut| ty_mut.ty.kind()) {
        let def_path = tcx.def_path(adt.did()).data[0].to_string();
        let item_name = tcx.item_name(adt.did());
        def_path == "ffi" && item_name.as_str() == "c_void"
    } else {
        false
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
        Self::checked_optional(place, local_decls, tcx).unwrap()
    }

    pub fn checked_optional(
        place: Place<'tcx>,
        local_decls: &LocalDecls<'tcx>,
        tcx: TyCtxt<'tcx>,
    ) -> Option<Self> {
        if is_c_void_ptr(tcx, place.ty(local_decls, tcx).ty) {
            return Some(Self { place });
        }

        None
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
    /// Only returns [Option::Some] if the void* side of the
    /// cast matches `self`.
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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

/// An account of [`CVoidCast`]s.
#[derive(Default, Clone, Debug)]
pub struct CVoidCastsUniDirectional<'tcx> {
    /// Mapping from location of a call that either
    /// produces or consumes a [`CVoidPtr`] to its
    /// succeeding or preceding [`CVoidCast`]s
    calls: HashMap<Location, HashSet<CVoidCast<'tcx>>>,
    /// Set of locations where [`CVoidCast`]s occur.
    casts: HashSet<Location>,
}

impl<'tcx> CVoidCastsUniDirectional<'tcx> {
    /// Get the adjusted [`Place`] from a cast at a particular [`Location`].
    ///
    /// Otherwise, the same `place` is returned, as no adjustments are necessary.
    pub fn get_adjusted_place_or_default_to(
        &self,
        loc: Location,
        place: Place<'tcx>,
    ) -> Place<'tcx> {
        // If there are multiple CVoidCasts for a Location, this logic may need to be adjusted
        // depending on the intended behavior.
        if let Some(casts) = self.calls.get(&loc) {
            for cast in casts {
                if cast.c_void_ptr.place == place {
                    return cast.other_ptr;
                }
            }
        }
        place
    }

    /// Tracks the [Location] of the use of a casted pointer in a [TerminatorKind::Call]
    pub fn insert_call(&mut self, loc: Location, cast: CVoidCast<'tcx>) {
        let entry = self.calls.entry(loc).or_insert_with(HashSet::new);
        entry.insert(cast);
    }

    /// Tracks the [Location] of void pointer [Rvalue::Cast]
    pub fn insert_cast(&mut self, loc: Location) {
        assert!(!self.casts.contains(&loc));
        self.casts.insert(loc);
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

/// Gets the [`Place`] associated with [`Rvalue::Use`] or [`Rvalue::Cast`]
/// and returns `None` otherwise.
pub fn source_place<'tcx>(rv: &Rvalue<'tcx>) -> Option<Place<'tcx>> {
    use Rvalue::*;
    match rv {
        Use(op) => op.place(),
        Cast(_, op, _) => op.place(),
        _ => None,
    }
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
        loc: Location,
        direction: CVoidCastDirection,
        place: Place<'tcx>,
    ) -> Place<'tcx> {
        self.direction(direction)
            .get_adjusted_place_or_default_to(loc, place)
    }

    /// See [`CVoidCastsUniDirectional::insert_cast`].
    fn insert_cast(&mut self, direction: CVoidCastDirection, loc: Location) {
        self.direction_mut(direction).insert_cast(loc)
    }

    /// See [`CVoidCastsUniDirectional::insert_call`].
    fn insert_call(&mut self, direction: CVoidCastDirection, loc: Location, cast: CVoidCast<'tcx>) {
        self.direction_mut(direction).insert_call(loc, cast)
    }

    /// Determine if the [`Statement`] at a [`Location`] should be skipped
    /// because it contains a [`CVoidCast`].
    pub fn should_skip_stmt(&self, loc: Location) -> bool {
        self.to.casts.contains(&loc) || self.from.casts.contains(&loc)
    }

    /// Checking whether a statement could modify a particular Place is
    /// nontrivial (particularly if the Place involves Deref projections),
    /// but currently rustc always uses a temporary for the LHS of the
    /// cast in the desired free(ptr as *mut c_void) pattern, so we
    /// can conservatively treat the Place as unmodified only if (1) it's
    /// just a Local, with no projections, and (2) that local isn't
    /// mentioned in the LHS or isn't mentioned in certain [`Rvalue`]
    /// kind of any statements between the cast and the call.
    fn is_place_modified_by_statement(p: &Place, stmt: &Statement) -> bool {
        if !p.projection.is_empty() {
            return false;
        }

        let local = p.local;

        match &stmt.kind {
            StatementKind::Assign(assign) => {
                let (lhs, rv) = &**assign;
                if lhs.local == local {
                    return true;
                }

                use Rvalue::*;
                let rv_place = match rv {
                    Use(op) => op.place(),
                    Ref(_, _, p) => Some(*p),
                    AddressOf(_, p) => Some(*p),
                    Cast(_, op, _) => op.place(),
                    Discriminant(p) => Some(*p),
                    ShallowInitBox(op, _) => op.place(),
                    Repeat(..) => return false,
                    ThreadLocalRef(..) => return false,
                    Len(..) => return false,
                    BinaryOp(..) => return false,
                    CheckedBinaryOp(..) => return false,
                    UnaryOp(..) => return false,
                    NullaryOp(..) => return false,
                    Aggregate(..) => return false,
                    CopyForDeref(..) => return false,
                };

                if let Some(rv_local) = rv_place.map(|p| p.local) {
                    local == rv_local
                } else {
                    false
                }
            }
            StatementKind::StorageDead(dead_local) => dead_local == &p.local,
            StatementKind::StorageLive(live_local) => live_local == &p.local,
            _ => true,
        }
    }

    /// Search for the first [Rvalue::Cast] from a void pointer
    /// in a sequence of [Statement]s. We expect that the
    /// cast is the first non-[StatementKind::StorageDead]
    /// statement in the block, a special case for
    /// [`Terminator`]s whose destination is casted from a
    /// void pointer to some other pointer type.
    ///
    /// [`Terminator`]: rustc_middle::mir::Terminator
    fn find_first_cast(
        statements: &[Statement<'tcx>],
        c_void_ptr: CVoidPtr<'tcx>,
    ) -> Option<(usize, CVoidCast<'tcx>)> {
        statements
            .iter()
            .enumerate()
            .skip_while(|(_, stmt)| matches!(stmt.kind, StatementKind::StorageDead(_)))
            .find_map(|(i, stmt)| {
                Some((
                    i,
                    c_void_ptr.get_cast_from_stmt(CVoidCastDirection::From, stmt)?,
                ))
            })
    }

    /// Search for the last cast to a void pointer in a sequence of
    /// [Statement]s.
    ///
    /// Gets the last cast in the current block, and ensures that the destination
    /// of that cast remains unmodified between the cast and the call in which
    /// the destination place is used.
    fn find_last_cast(
        statements: &[Statement<'tcx>],
        c_void_ptr: CVoidPtr<'tcx>,
    ) -> Option<(usize, CVoidCast<'tcx>)> {
        for (sidx, stmt) in statements.iter().enumerate().rev() {
            let cast = c_void_ptr.get_cast_from_stmt(CVoidCastDirection::To, stmt);
            if let Some(cast) = cast {
                if cast.c_void_ptr == c_void_ptr {
                    return Some((sidx, cast));
                }
            } else if Self::is_place_modified_by_statement(&c_void_ptr.place, stmt) {
                return None;
            }
        }
        None
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
    /// * `memcpy`
    /// * `memset`
    ///
    /// and insert their casts to and from [`*c_void`].
    ///
    /// Note that since [`Place`]s are local to a function [`Body`],
    /// a [`CVoidCasts`] is only valid when it contains things from one [`Body`].
    ///
    /// [`*c_void`]: core::ffi::c_void
    fn insert_all_from_body(&mut self, body: &Body<'tcx>, tcx: TyCtxt<'tcx>) {
        for (block, bb_data) in body.basic_blocks().iter_enumerated() {
            if let Some(term) = &bb_data.terminator {
                if let TerminatorKind::Call {
                    func,
                    args,
                    destination,
                    target,
                    ..
                } = &term.kind
                {
                    let func_ty = func.ty(&body.local_decls, tcx);
                    let directions = CVoidCastDirection::from_callee(ty_callee(tcx, func_ty));

                    for direction in directions.iter().copied() {
                        use CVoidCastDirection::*;

                        match direction {
                            From => {
                                let c_void_ptr =
                                    CVoidPtr::checked(*destination, &body.local_decls, tcx);
                                if let Some((statement_index, cast)) = Self::find_first_cast(
                                    &body.basic_blocks()[target.unwrap()].statements,
                                    c_void_ptr,
                                ) {
                                    self.insert_cast(
                                        direction,
                                        Location {
                                            statement_index,
                                            block: target.unwrap(),
                                        },
                                    );
                                    self.insert_call(
                                        direction,
                                        terminator_location(block, bb_data),
                                        cast,
                                    );
                                }
                            }
                            To => {
                                for arg in args {
                                    if let Some(place) = arg.place() {
                                        if let Some(c_void_ptr) = CVoidPtr::checked_optional(
                                            place,
                                            &body.local_decls,
                                            tcx,
                                        ) {
                                            self.find_and_insert_pred_cast(
                                                body, c_void_ptr, direction, bb_data,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn find_modifying_assignments(
        current_block: BasicBlock,
        current_block_data: &BasicBlockData<'tcx>,
        c_void_ptr: &CVoidPtr<'tcx>,
    ) -> Vec<(BasicBlock, usize)> {
        let mut modifying_statements = current_block_data
            .statements
            .iter()
            .enumerate()
            .filter(|(_, stmt)| {
                matches!(stmt.kind, StatementKind::Assign(..))
                    && Self::is_place_modified_by_statement(&c_void_ptr.place, stmt)
            })
            .map(|(index, _stmt)| (current_block, index))
            .collect::<Vec<_>>();

        if let Some(terminator) = &current_block_data.terminator {
            if matches!(terminator.kind, TerminatorKind::Call { destination, .. } if destination == c_void_ptr.place)
            {
                modifying_statements.push((current_block, usize::MAX));
            }
        }

        modifying_statements
    }

    fn find_and_insert_pred_cast(
        &mut self,
        body: &Body<'tcx>,
        c_void_ptr: CVoidPtr<'tcx>,
        direction: CVoidCastDirection,
        bb_data: &BasicBlockData<'tcx>,
    ) {
        let mut inserted_places = HashSet::new();
        let mut modifying_statements = Vec::new();

        let mut current_place = c_void_ptr.place;

        for (current_block, current_block_data) in body.basic_blocks().iter_enumerated().rev() {
            for (index, stmt) in current_block_data.statements.iter().enumerate().rev() {
                if let Some((_, rhs)) =
                    get_assign_sides(stmt).filter(|(lhs, _)| *lhs == current_place)
                {
                    // ancestors of a *libc::c_void are given special treatment;
                    // casts between these ancestors are exempt from scrutiny
                    if let Some(place) = source_place(rhs) {
                        if get_cast_place(rhs).is_some() {
                            let location = Location {
                                statement_index: index,
                                block: current_block,
                            };
                            self.insert_cast(direction, location);
                        }

                        current_place = place;
                    }
                }
            }

            modifying_statements.extend(Self::find_modifying_assignments(
                current_block,
                current_block_data,
                &c_void_ptr,
            ));

            if let Some((_, cast)) =
                Self::find_last_cast(&current_block_data.statements, c_void_ptr)
            {
                assert!(
                    inserted_places.insert(c_void_ptr.place),
                    "Duplicate c_void_ptr.place found: {:?}",
                    c_void_ptr.place
                );
                assert!(
                    body.local_kind(c_void_ptr.place.local) == LocalKind::Temp,
                    "Unsupported cast into non-temporary local"
                );
                self.insert_call(direction, terminator_location(current_block, bb_data), cast);
            }
        }

        assert!(
            modifying_statements.len() == 1,
            "c_void_ptr.place modified multiple times: {:?}",
            modifying_statements
        );
    }

    pub fn new(body: &Body<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        let mut this = Self::default();
        this.insert_all_from_body(body, tcx);
        this
    }
}
