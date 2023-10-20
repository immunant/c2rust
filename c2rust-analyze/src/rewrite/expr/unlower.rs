use crate::panic_detail;
use crate::rewrite::expr::mir_op::SubLoc;
use crate::rewrite::span_index::SpanIndex;
use crate::util;
use either::Either;
use log::*;
use rustc_hir as hir;
use rustc_hir::intravisit::{self, Visitor};
use rustc_hir::HirId;
use rustc_middle::hir::nested_filter;
use rustc_middle::mir::{self, Body, Location, Operand};
use rustc_middle::ty::adjustment::{Adjust, AutoBorrow, AutoBorrowMutability, PointerCast};
use rustc_middle::ty::{TyCtxt, TypeckResults};
use rustc_span::Span;
use std::collections::btree_map::{BTreeMap, Entry};
use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct PreciseLoc {
    pub loc: Location,
    pub sub: Vec<SubLoc>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct MirOrigin {
    pub hir_id: HirId,
    pub span: Span,
    pub desc: MirOriginDesc,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum MirOriginDesc {
    /// This MIR represents the whole HIR expression.
    Expr,
    /// This MIR stores the result of the HIR expression into a MIR local of some kind.
    StoreIntoLocal,
    /// This MIR loads the result of the HIR expression from a MIR temporary where it was
    /// previously stored.  Loads from user-visible locals, which originate from HIR local variable
    /// expressions, use the `Expr` variant instead.
    LoadFromTemp,
    /// This MIR applies adjustment `i` from the expression's list of adjustments.
    Adjustment(usize),
}

struct UnlowerVisitor<'a, 'tcx> {
    tcx: TyCtxt<'tcx>,
    mir: &'a Body<'tcx>,
    typeck_results: &'tcx TypeckResults<'tcx>,
    span_index: SpanIndex<Location>,
    /// Maps MIR (sub)locations to the HIR node that produced each one, if known.
    unlower_map: BTreeMap<PreciseLoc, MirOrigin>,

    /// When processing the `hir::Expr` identified by the `HirId`, append some locations to the
    /// list retrieved from the `SpanIndex`.  This is used in cases where some MIR statements have
    /// their spans set to a parent expr but really belong to the child.
    append_extra_locations: HashMap<HirId, Vec<Location>>,
}

impl<'a, 'tcx> UnlowerVisitor<'a, 'tcx> {
    fn location_span(&self, loc: Location) -> Span {
        self.mir
            .stmt_at(loc)
            .either(|stmt| stmt.source_info.span, |term| term.source_info.span)
    }

    /// Record an `unlower_map` entry indicating that MIR location `loc, sub_loc` corresponds to
    /// the HIR expression `ex`.
    fn record(&mut self, loc: Location, sub_loc: &[SubLoc], ex: &hir::Expr) {
        self.record_desc(loc, sub_loc, ex, MirOriginDesc::Expr);
    }

    /// Like [`record`][Self::record], but also takes a [`MirOriginDesc`] to indicate how the MIR
    /// location `loc, sub_loc` relates to the HIR expression `desc`.  For example, this can be
    /// used to record that a particular piece of MIR loads/stores a temporary used in the
    /// evaluation of `ex`.
    fn record_desc(
        &mut self,
        loc: Location,
        sub_loc: &[SubLoc],
        ex: &hir::Expr,
        desc: MirOriginDesc,
    ) {
        let origin = MirOrigin {
            hir_id: ex.hir_id,
            span: ex.span,
            desc,
        };
        let key = PreciseLoc {
            loc,
            sub: sub_loc.to_owned(),
        };
        match self.unlower_map.entry(key) {
            Entry::Vacant(e) => {
                e.insert(origin);
            }
            Entry::Occupied(e) => {
                let old_origin = *e.get();
                if old_origin != origin {
                    error!(
                        "conflicting origins for {:?} {:?} ({:?})\n\
                            origin 1 = {:?}\n\
                            origin 2 = {:?}\n\
                            expr 2 = {:?}",
                        loc,
                        sub_loc,
                        self.location_span(loc),
                        origin,
                        old_origin,
                        ex,
                    );
                }
            }
        }
    }

    fn operand_desc(&self, op: &Operand<'tcx>) -> MirOriginDesc {
        match *op {
            mir::Operand::Copy(pl) | mir::Operand::Move(pl) => {
                if is_temp_var(self.mir, pl.as_ref()) {
                    MirOriginDesc::LoadFromTemp
                } else {
                    MirOriginDesc::Expr
                }
            }
            mir::Operand::Constant(..) => MirOriginDesc::Expr,
        }
    }

    /// Special `record` variant for MIR [`Operand`]s.  This sets the [`MirOriginDesc`] to
    /// `LoadFromTemp` if `op` is a MIR temporary and otherwise sets it to `Expr`.
    ///
    /// [`Operand`]: mir::Operand
    fn record_operand(
        &mut self,
        loc: Location,
        sub_loc: &[SubLoc],
        ex: &hir::Expr,
        op: &mir::Operand<'tcx>,
    ) {
        self.record_desc(loc, sub_loc, ex, self.operand_desc(op));
    }

    fn get_sole_assign(
        &self,
        locs: &[Location],
    ) -> Option<(Location, mir::Place<'tcx>, &'a mir::Rvalue<'tcx>)> {
        if locs.len() != 1 {
            return None;
        }
        self.get_last_assign(locs)
    }

    /// If the last statement in `locs` is `StatementKind::Assign`, return `Some` containing its
    /// parts.  Otherwise, return `None`.
    fn get_last_assign(
        &self,
        locs: &[Location],
    ) -> Option<(Location, mir::Place<'tcx>, &'a mir::Rvalue<'tcx>)> {
        let loc = *locs.last()?;
        let stmt = self.mir.stmt_at(loc).left()?;
        match stmt.kind {
            mir::StatementKind::Assign(ref x) => Some((loc, x.0, &x.1)),
            _ => None,
        }
    }

    fn get_last_call(
        &self,
        locs: &[Location],
    ) -> Option<(
        Location,
        mir::Place<'tcx>,
        &'a mir::Operand<'tcx>,
        &'a [mir::Operand<'tcx>],
    )> {
        for &loc in locs.iter().rev() {
            if let Some(term) = self.mir.stmt_at(loc).right() {
                match term.kind {
                    mir::TerminatorKind::Call {
                        ref func,
                        ref args,
                        destination,
                        ..
                    } => return Some((loc, destination, func, args)),
                    _ => {}
                }
            }
        }
        None
    }

    fn should_ignore_statement(&self, loc: Location) -> bool {
        if let Some(stmt) = self.mir.stmt_at(loc).left() {
            match stmt.kind {
                mir::StatementKind::FakeRead(..)
                | mir::StatementKind::StorageLive(..)
                | mir::StatementKind::StorageDead(..)
                | mir::StatementKind::Nop => return true,
                _ => {}
            }
        }
        false
    }

    fn visit_expr_inner(&mut self, ex: &'tcx hir::Expr<'tcx>) {
        let _g = panic_detail::set_current_span(ex.span);

        let mut locs = self
            .span_index
            .lookup_exact(ex.span)
            .copied()
            .filter(|&loc| !self.should_ignore_statement(loc))
            .collect::<Vec<_>>();
        if let Some(extra) = self.append_extra_locations.remove(&ex.hir_id) {
            locs.extend(
                extra
                    .into_iter()
                    .filter(|&loc| !self.should_ignore_statement(loc)),
            );
        }
        let locs = locs;
        if locs.is_empty() {
            return;
        }

        let warn = |desc| {
            warn!("{}", desc);
            info!("locs:");
            for &loc in &locs {
                self.mir.stmt_at(loc).either(
                    |stmt| info!("  {:?}: {:?}", locs, stmt),
                    |term| info!("  {:?}: {:?}", locs, term),
                );
            }
            info!("span = {:?}", ex.span);
            info!("expr = {:?}", ex);
        };

        // Most exprs end with an assignment, storing the result into a temporary.
        match ex.kind {
            hir::ExprKind::Assign(pl, rv, _span) => {
                // For `Assign`, we expect the assignment to be the whole thing.
                let (loc, _mir_pl, mir_rv) = match self.get_sole_assign(&locs) {
                    Some(x) => x,
                    None => {
                        warn("expected exactly one StatementKind::Assign");
                        return;
                    }
                };
                let desc = match mir_rv {
                    mir::Rvalue::Use(op) => self.operand_desc(op),
                    _ => MirOriginDesc::Expr,
                };
                self.record(loc, &[], ex);
                self.record(loc, &[SubLoc::Dest], pl);
                self.record_desc(loc, &[SubLoc::Rvalue], rv, desc);
            }

            hir::ExprKind::Call(_, args) | hir::ExprKind::MethodCall(_, args, _) => {
                let (loc, _mir_pl, _mir_func, mir_args) = match self.get_last_call(&locs) {
                    Some(x @ (_, pl, _, _)) if is_var(pl) => x,
                    _ => {
                        warn("expected final Call to store into var");
                        return;
                    }
                };
                self.record_desc(loc, &[], ex, MirOriginDesc::StoreIntoLocal);
                self.record(loc, &[SubLoc::Rvalue], ex);
                for (i, (arg, mir_arg)) in args.iter().zip(mir_args).enumerate() {
                    self.record_operand(loc, &[SubLoc::Rvalue, SubLoc::CallArg(i)], arg, mir_arg);
                    self.visit_expr_operand(arg, mir_arg, &[]);
                }

                if locs.len() > 1 {
                    // Special case: if the receiver of a `MethodCall` has adjustments, they are
                    // emitted with the same span as the `MethodCall` itself, and thus show up as
                    // extra `locs` here.  We associate them with the child instead so all of the
                    // child's spans can be processed together.
                    if matches!(ex.kind, hir::ExprKind::MethodCall(..)) {
                        self.append_extra_locations
                            .entry(args[0].hir_id)
                            .or_insert_with(Vec::new)
                            .extend_from_slice(&locs[..locs.len() - 1]);
                    } else {
                        warn!("NYI: extra locations {:?} in Call", &locs[..locs.len() - 1]);
                    }
                }
            }

            _ => {
                // For all other `ExprKind`s, we expect the last `loc` to be an assignment storing
                // the final result into a temporary.
                let (_mir_pl, mut cursor) = match self.make_visit_expr_cursor(ex, &locs) {
                    Some(x @ (pl, _)) if is_var(pl) => x,
                    _ => {
                        warn("expected final Assign to store into var");
                        return;
                    }
                };
                self.record_desc(cursor.loc, &[], ex, MirOriginDesc::StoreIntoLocal);
                self.peel_adjustments(ex, &mut cursor);
                self.record_desc(cursor.loc, &cursor.sub_loc, ex, MirOriginDesc::Expr);
                self.finish_visit_expr_cursor(ex, cursor);
            }
        }
    }

    /// Try to create a `VisitExprCursor` from the RHS of statement `locs.last()`.  Returns the LHS
    /// of the statement and the cursor on success.  Fails (returning `None`) if the statement at
    /// `locs.last()` is not `StatementKind::Assign` or `TerminatorKind::Call`.
    fn make_visit_expr_cursor(
        &self,
        ex: &'tcx hir::Expr<'tcx>,
        locs: &'a [Location],
    ) -> Option<(mir::Place<'tcx>, VisitExprCursor<'a, 'tcx>)> {
        let (&loc, extra_locs) = locs.split_last()?;

        let (pl, expr_mir) = match self.mir.stmt_at(loc) {
            Either::Left(stmt) => match stmt.kind {
                mir::StatementKind::Assign(ref x) => (x.0, ExprMir::Rvalue(&x.1)),
                _ => return None,
            },
            _ => return None,
        };

        let cursor =
            VisitExprCursor::new(self.mir, expr_mir, extra_locs, loc, vec![SubLoc::Rvalue]);

        Some((pl, cursor))
    }

    fn finish_visit_expr_cursor(
        &mut self,
        ex: &'tcx hir::Expr<'tcx>,
        cursor: VisitExprCursor<'_, 'tcx>,
    ) {
        // Record entries for temporaries that are buffered in `temp_info`.
        for (precise_loc, desc) in cursor.temp_info {
            self.record_desc(precise_loc.loc, &precise_loc.sub, ex, desc);
        }
    }

    fn peel_adjustments(
        &mut self,
        ex: &'tcx hir::Expr<'tcx>,
        cursor: &mut VisitExprCursor<'_, 'tcx>,
    ) {
        let _g = panic_detail::set_current_span(ex.span);

        // We proceed backward through `adjusts`, peeling off elements of the current `Rvalue` or
        // `Place` and/or consuming temporary assignments from `extra_locs`.  Once all adjustments
        // have been accounted for, the final remaining `Rvalue` is the `Expr` itself.
        let adjusts = self.typeck_results.expr_adjustments(ex);

        for (i, adjust) in adjusts.iter().enumerate().rev() {
            while cursor.peel_temp().is_some() {
                // No-op.  Just loop until we've peeled all temporaries.
            }
            self.record_desc(
                cursor.loc,
                &cursor.sub_loc,
                ex,
                MirOriginDesc::Adjustment(i),
            );
            match adjust.kind {
                Adjust::Borrow(AutoBorrow::RawPtr(mutbl)) => {
                    if cursor.peel_address_of() != Some(mutbl) {
                        warn!(
                            "expected Rvalue::AddressOf for {adjust:?} on expr {ex:?}, \
                            but got {:?}",
                            cursor.cur
                        );
                        break;
                    }
                }
                Adjust::Borrow(AutoBorrow::Ref(_, AutoBorrowMutability::Not)) => {
                    if cursor.peel_ref() != Some(mir::Mutability::Not) {
                        warn!(
                            "expected Rvalue::Ref(Mutability::Not) for {adjust:?} \
                            on expr {ex:?}, but got {:?}",
                            cursor.cur
                        );
                        break;
                    }
                }
                Adjust::Borrow(AutoBorrow::Ref(_, AutoBorrowMutability::Mut { .. })) => {
                    if cursor.peel_ref() != Some(mir::Mutability::Mut) {
                        warn!(
                            "expected Rvalue::Ref(Mutability::Mut) for {adjust:?} \
                            on expr {ex:?}, but got {:?}",
                            cursor.cur
                        );
                        break;
                    }
                }
                // Non-overloaded deref
                Adjust::Deref(None) => {
                    if cursor.peel_deref().is_none() {
                        warn!(
                            "expected Rvalue::Deref for {adjust:?} on expr {ex:?}, \
                            but got {:?}",
                            cursor.cur
                        );
                        break;
                    }
                }
                Adjust::Pointer(pc) => {
                    if cursor.peel_pointer_cast() != Some(pc) {
                        warn!(
                            "expected Rvalue::Cast(Pointer({pc:?})) for {adjust:?} \
                            on expr {ex:?}, but got {:?}",
                            cursor.cur
                        );
                        break;
                    }
                }
                _ => {
                    warn!("unsupported adjustment {adjust:?} on expr {ex:?}");
                    break;
                }
            }
        }

        while cursor.peel_temp().is_some() {
            // No-op.  Just loop until we've peeled all temporaries.
        }
    }

    fn visit_expr_operand(
        &mut self,
        ex: &'tcx hir::Expr<'tcx>,
        _rv: &'a mir::Operand<'tcx>,
        _extra_locs: &[Location],
    ) {
        let _g = panic_detail::set_current_span(ex.span);
        // TODO: handle adjustments, overloaded operators, etc
    }
}

impl<'a, 'tcx> Visitor<'tcx> for UnlowerVisitor<'a, 'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_expr(&mut self, ex: &'tcx hir::Expr<'tcx>) {
        self.visit_expr_inner(ex);
        intravisit::walk_expr(self, ex);
    }
}

/// The MIR representation of some part of a `hir::Expr` or its adjustments.
#[derive(Clone, Copy, Debug)]
enum ExprMir<'a, 'tcx> {
    Rvalue(&'a mir::Rvalue<'tcx>),
    Operand(&'a mir::Operand<'tcx>),
    Place(mir::PlaceRef<'tcx>),
}

impl<'a, 'tcx> From<&'a mir::Rvalue<'tcx>> for ExprMir<'a, 'tcx> {
    fn from(x: &'a mir::Rvalue<'tcx>) -> ExprMir<'a, 'tcx> {
        ExprMir::Rvalue(x)
    }
}

impl<'a, 'tcx> From<&'a mir::Operand<'tcx>> for ExprMir<'a, 'tcx> {
    fn from(x: &'a mir::Operand<'tcx>) -> ExprMir<'a, 'tcx> {
        ExprMir::Operand(x)
    }
}

impl<'a, 'tcx> From<mir::PlaceRef<'tcx>> for ExprMir<'a, 'tcx> {
    fn from(x: mir::PlaceRef<'tcx>) -> ExprMir<'a, 'tcx> {
        ExprMir::Place(x)
    }
}

/// Helper for `visit_expr_rvalue`.  This type has methods for traversing the MIR produced from a
/// `hir::Expr` and its adjustments.
struct VisitExprCursor<'a, 'tcx> {
    mir: &'a Body<'tcx>,
    cur: ExprMir<'a, 'tcx>,
    locs: &'a [Location],

    loc: Location,
    sub_loc: Vec<SubLoc>,

    /// Buffer for temporaries handled by `peel_temp`.
    ///
    /// When `peel_temp` traverses past a temporary, it also needs to emit some `StoreIntoLocal`
    /// and `LoadFromTemp` entries as a side effect.  But it doesn't have access to the
    /// `UnlowerVisitor` to emit these entries directly, so instead we buffer those entries for the
    /// caller to emit later.
    temp_info: Vec<(PreciseLoc, MirOriginDesc)>,
}

impl<'a, 'tcx> VisitExprCursor<'a, 'tcx> {
    pub fn new(
        mir: &'a Body<'tcx>,
        cur: ExprMir<'a, 'tcx>,
        extra_locs: &'a [Location],
        loc: Location,
        sub_loc: Vec<SubLoc>,
    ) -> VisitExprCursor<'a, 'tcx> {
        VisitExprCursor {
            mir,
            cur,
            locs: extra_locs,
            loc,
            sub_loc,

            temp_info: Vec::new(),
        }
    }

    /// If the current MIR is a temporary, and the previous `Location` is an assignment to
    /// that temporary, peel it off, leaving the temporary's initializer as the current
    /// `Rvalue`.  This also adds `LoadFromTemp` and `StoreIntoLocal` entries in `self.temp_info`
    /// for the temporary's use and definition.
    ///
    /// For example, starting from this position:
    /// ```Rust,ignore
    /// _temp = Add(_1, _2)
    /// _3 = move _temp
    ///           ^^^^^
    /// ```
    /// A successful call to `peel_temp` will advance to this position:
    /// ```Rust,ignore
    /// _temp = Add(_1, _2)
    ///         ^^^^^^^^^^^
    /// _3 = move _temp
    /// ```
    /// That is, it steps from a use of the temporary (an `Rvalue`, `Operand`, or `Place`) to the
    /// `Rvalue` that was used to initialize that temporary.
    pub fn peel_temp(&mut self) -> Option<()> {
        // Run `peel_temp_inner`, and restore `self.cur` and `self.sub_loc` if it fails.
        let old_cur = self.cur;
        let old_sub_loc_len = self.sub_loc.len();
        let old_temp_info_len = self.temp_info.len();
        let r = self.peel_temp_inner();
        if r.is_none() {
            debug_assert!(self.sub_loc.len() >= old_sub_loc_len);
            self.cur = old_cur;
            self.sub_loc.truncate(old_sub_loc_len);
            self.temp_info.truncate(old_temp_info_len);
        }
        r
    }

    fn peel_temp_inner(&mut self) -> Option<()> {
        // We can freely mutate `self.cur` and `self.sub_loc` here, since the outer
        // `peel_temp` will reset them if this function returns `None`.

        // Record the location of the load before `require_place`, so it gets the outermost
        // `sub`-location possible.
        let load_loc = PreciseLoc {
            loc: self.loc,
            sub: self.sub_loc.clone(),
        };

        let pl = self.require_place()?;
        if !is_temp_var(self.mir, pl) {
            return None;
        }

        let (&loc, remaining_locs) = self.locs.split_last()?;
        let stmt = self.mir.stmt_at(loc).left()?;
        let (assign_pl, assign_rv) = match stmt.kind {
            mir::StatementKind::Assign(ref x) => (&x.0, &x.1),
            _ => return None,
        };
        if !is_temp_var(self.mir, assign_pl.as_ref()) {
            return None;
        }

        let store_loc = PreciseLoc { loc, sub: vec![] };
        self.temp_info.push((load_loc, MirOriginDesc::LoadFromTemp));
        self.temp_info
            .push((store_loc, MirOriginDesc::StoreIntoLocal));

        // Success - the next relevant MIR is `assign_rv`.
        self.cur = ExprMir::Rvalue(assign_rv);
        self.locs = remaining_locs;
        self.loc = loc;
        self.sub_loc = vec![SubLoc::Rvalue];

        Some(())
    }

    fn require_place(&mut self) -> Option<mir::PlaceRef<'tcx>> {
        Some(*self.require_place_mut()?)
    }

    fn require_place_mut(&mut self) -> Option<&mut mir::PlaceRef<'tcx>> {
        if let ExprMir::Rvalue(rv) = self.cur {
            match *rv {
                mir::Rvalue::Use(ref op) => {
                    self.sub_loc.push(SubLoc::RvalueOperand(0));
                    self.cur = ExprMir::Operand(op);
                }
                _ => return None,
            }
        }
        if let ExprMir::Operand(op) = self.cur {
            match *op {
                mir::Operand::Copy(pl) | mir::Operand::Move(pl) => {
                    self.sub_loc.push(SubLoc::OperandPlace);
                    self.cur = ExprMir::Place(pl.as_ref());
                }
                _ => return None,
            }
        }
        match self.cur {
            ExprMir::Place(ref mut pl) => Some(pl),
            _ => None,
        }
    }

    /// If the current MIR is `Rvalue::AddressOf`, peel it off and return its `Mutability`.
    pub fn peel_address_of(&mut self) -> Option<mir::Mutability> {
        loop {
            if let ExprMir::Rvalue(&mir::Rvalue::AddressOf(mutbl, pl)) = self.cur {
                self.cur = ExprMir::Place(pl.as_ref());
                self.sub_loc.push(SubLoc::RvaluePlace(0));
                return Some(mutbl);
            }
            self.peel_temp()?;
        }
    }

    /// If the current MIR is `Rvalue::Ref`, peel it off and return its `Mutability`.
    pub fn peel_ref(&mut self) -> Option<mir::Mutability> {
        loop {
            if let ExprMir::Rvalue(&mir::Rvalue::Ref(_, kind, pl)) = self.cur {
                let mutbl = match kind {
                    mir::BorrowKind::Shared => mir::Mutability::Not,
                    mir::BorrowKind::Mut { .. } => mir::Mutability::Mut,
                    _ => return None,
                };
                self.cur = ExprMir::Place(pl.as_ref());
                self.sub_loc.push(SubLoc::RvaluePlace(0));
                return Some(mutbl);
            }
            self.peel_temp()?;
        }
    }

    /// If the current MIR is a `Place` ending with a `Deref` projection, peel off the
    /// `Deref`.
    pub fn peel_deref(&mut self) -> Option<()> {
        loop {
            let pl = self.require_place_mut()?;
            if let Some((&outer_proj, remaining_proj)) = pl.projection.split_last() {
                if matches!(outer_proj, mir::PlaceElem::Deref) {
                    pl.projection = remaining_proj;
                    // Number of derefs, not counting the outermost one we just peeled off.
                    let num_inner_derefs = remaining_proj
                        .iter()
                        .filter(|p| matches!(p, mir::PlaceElem::Deref))
                        .count();
                    self.sub_loc.push(SubLoc::PlacePointer(num_inner_derefs));
                    return Some(());
                }
            }
            self.peel_temp()?;
        }
    }

    /// If the current MIR is `Rvalue::Cast` with `CastKind::Pointer`, peel it off and
    /// return the `PointerCast` kind.
    pub fn peel_pointer_cast(&mut self) -> Option<PointerCast> {
        loop {
            if let ExprMir::Rvalue(&mir::Rvalue::Cast(kind, ref op, _)) = self.cur {
                let pc = match kind {
                    mir::CastKind::Pointer(x) => x,
                    _ => return None,
                };
                self.cur = ExprMir::Operand(op);
                self.sub_loc.push(SubLoc::RvalueOperand(0));
                return Some(pc);
            }
            self.peel_temp()?;
        }
    }
}

fn is_var(pl: mir::Place) -> bool {
    pl.projection.len() == 0
}

fn is_temp_var(mir: &Body, pl: mir::PlaceRef) -> bool {
    pl.projection.len() == 0 && mir.local_kind(pl.local) == mir::LocalKind::Temp
}

fn build_span_index(mir: &Body<'_>) -> SpanIndex<Location> {
    eprintln!("building span index for {:?}:", mir.source);
    let mut span_index_items = Vec::new();
    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        for (i, stmt) in bb_data.statements.iter().enumerate() {
            let loc = Location {
                block: bb,
                statement_index: i,
            };
            eprintln!("  {:?}: {:?}", loc, stmt.source_info.span);
            span_index_items.push((stmt.source_info.span, loc));
        }

        let loc = Location {
            block: bb,
            statement_index: bb_data.statements.len(),
        };
        eprintln!("  {:?}: {:?}", loc, bb_data.terminator().source_info.span);
        span_index_items.push((bb_data.terminator().source_info.span, loc));
    }

    SpanIndex::new(span_index_items)
}

/// Builds the *unlowering map*, which maps each piece of the MIR to the HIR `Expr` that was
/// lowered to produce it.
///
/// For example:
///
/// ```rust
/// fn f(a: i32) -> i32 {
///     a + 1
/// }
///
/// fn g(x: i32, y: i32) -> i32 {
///     x + f(y)
/// }
/// ```
///
/// For `f`, the unlowering map annotates the MIR as follows:
///
/// ```text
/// block bb0:
///   bb0[0]: StorageLive(_2)
///   bb0[1]: _2 = _1
///     []: StoreIntoLocal, `a`
///     [Rvalue]: Expr, `a`
///   bb0[2]: _0 = Add(move _2, const 1_i32)
///     []: StoreIntoLocal, `a + 1`
///     [Rvalue]: Expr, `a + 1`
///   bb0[3]: StorageDead(_2)
///   bb0[4]: Terminator { source_info: ..., kind: return }
/// ```
///
/// Each statement is shown here with its MIR `Location`, such as `bb0[0]`; within each statement,
/// annotations are associated with particular `SubLoc` paths, such as `[]` or `[Rvalue]`.  The
/// statement `_2 = _1` is associated with the expression `a`; the statement as a whole (`bb0[1]`
/// `[]`) is storing the result of evaluating `a` into a MIR local, and the statement's rvalue
/// (`bb0[1]` `[Rvalue]`) `_1` represents the expression `a` itself.  Similarly, `_0 = Add(move _2,
/// const 1)` stores the result of `a + 1` into a local.  If needed, we could extend the `unlower`
/// pass to also record that `move _2` (a.k.a. `bb0[2]` `[Rvalue, RvalueOperand(0)]`) is lowered
/// from the `Expr` `a`.
///
/// On `g`, the unlowering map includes the following (among other entries):
///
/// ```text
/// bb0[5]: Terminator { source_info: ..., kind: _4 = f(move _5) -> [return: bb1, unwind: bb2] }
///   []: StoreIntoLocal, `f(y)`
///   [Rvalue]: Expr, `f(y)`
///   [Rvalue, CallArg(0)]: Expr, `y`
/// bb1[1]: _0 = Add(move _3, move _4)
///   []: StoreIntoLocal, `x + f(y)`
///   [Rvalue]: Expr, `x + f(y)`
/// ```
///
/// The call terminator `_4 = f(move _5)` computes `f(y)` and stores the result
/// into a local; its rvalue is `f(y)` itself, and the first argument of the rvalue
/// is `y`.
///
/// This function returns a `BTreeMap`, which supports iterating in sorted order.  This allows
/// looking up entries by a prefix of their key (for example, finding all entries on a given
/// `Location` regardless of their `SubLoc`s) using the `BTreeMap::range` method.
pub fn unlower<'tcx>(
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    hir_body_id: hir::BodyId,
) -> BTreeMap<PreciseLoc, MirOrigin> {
    // If this MIR body came from a `#[derive]`, ignore it.
    if util::is_automatically_derived(tcx, mir) {
        return BTreeMap::new();
    }

    let typeck_results = tcx.typeck_body(hir_body_id);

    // Build `span_index`, which maps `Span`s to MIR `Locations`.
    let span_index = build_span_index(mir);

    let hir = tcx.hir().body(hir_body_id);

    // Run the visitor.
    let mut visitor = UnlowerVisitor {
        tcx,
        mir,
        typeck_results,
        span_index,
        unlower_map: BTreeMap::new(),
        append_extra_locations: HashMap::new(),
    };
    visitor.visit_body(hir);

    if visitor.append_extra_locations.len() > 0 {
        for (&hir_id, locs) in &visitor.append_extra_locations {
            error!(
                "leftover locations for {hir_id:?} = {:?}: locs = {locs:?}",
                tcx.hir().get(hir_id)
            );
        }
    }

    debug_print_unlower_map(tcx, mir, &visitor.unlower_map);

    visitor.unlower_map
}

fn debug_print_unlower_map<'tcx>(
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    unlower_map: &BTreeMap<PreciseLoc, MirOrigin>,
) {
    eprintln!("unlowering for {:?}:", mir.source);
    for (bb_id, bb) in mir.basic_blocks().iter_enumerated() {
        eprintln!("  block {bb_id:?}:");
        for (i, stmt) in bb.statements.iter().enumerate() {
            let loc = Location {
                block: bb_id,
                statement_index: i,
            };

            eprintln!("    {loc:?}: {stmt:?}");
            for (k, v) in unlower_map.range(&PreciseLoc { loc, sub: vec![] }..) {
                if k.loc != loc {
                    break;
                }
                let sublocs = &k.sub;
                let ex = tcx.hir().expect_expr(v.hir_id);
                eprintln!("      {sublocs:?}: {:?}, {:?}", v.desc, ex.span);
            }
        }

        {
            let term = bb.terminator();
            let loc = Location {
                block: bb_id,
                statement_index: bb.statements.len(),
            };

            eprintln!("    {loc:?}: {term:?}");
            for (k, v) in unlower_map.range(&PreciseLoc { loc, sub: vec![] }..) {
                if k.loc != loc {
                    break;
                }
                let sublocs = &k.sub;
                let ex = tcx.hir().expect_expr(v.hir_id);
                eprintln!("      {sublocs:?}: {:?}, {:?}", v.desc, ex.span);
            }
        }
    }
}
