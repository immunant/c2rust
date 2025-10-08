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
use std::collections::{HashMap, HashSet};

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

/// The `MirOriginDesc`s for a complex case might look like this:
///
/// * `Expr`
/// * `Adjustment(0)`
/// * `StoreIntoLocal`
/// * `LoadFromTempForAdjustment(1)`
/// * `Adjustment(1)`
/// * `Adjustment(2)`
/// * `StoreIntoLocal`
/// * `LoadFromTemp`
///
/// Note there isn't a 1-to-1 relationship between `Adjustment` and `LoadFromTempForAdjustment`;
/// the latter is present only when the expression and/or its adjustments are split across multiple
/// statements.
///
/// Currently, we don't support MIR rewrites on `StoreIntoLocal` nodes, so there's no need to
/// disambiguate between "store into a temporary, to be loaded by a future adjustment" and "store
/// into the final variable/temporary".  In the future, we might need to modify `StoreIntoLocal` or
/// add a new variant to account for this.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum MirOriginDesc {
    /// This MIR represents the main HIR expression.  This is the value before any adjustments are
    /// applied.
    Expr,
    /// This MIR stores the result of the HIR expression into a MIR local of some kind.
    StoreIntoLocal,
    /// This MIR loads the final result of the HIR expression (after all adjustments have been
    /// applied) from a MIR temporary where it was previously stored.  Loads from user-visible
    /// locals, which originate from HIR local variable expressions, use the `Expr` variant
    /// instead.
    LoadFromTemp,
    /// This MIR applies adjustment `i` from the expression's list of adjustments.
    Adjustment(usize),
    /// This MIR loads from a MIR temporary the intermediate value used as input for adjustment `i`
    /// of the HIR expression.
    LoadFromTempForAdjustment(usize),
}

#[derive(Clone, Debug, Default)]
pub struct UnlowerMap {
    /// Maps MIR (sub)locations to the HIR node that produced each one, if known.
    origins: BTreeMap<PreciseLoc, MirOrigin>,
    /// MIR locations for which we discard all rewrites.  This is used for "derived" statements,
    /// such as the `Len` + `Lt` + `Assert` operations that make up an array bounds check, where
    /// rewrites applied to the main statement will automatically affect the derived statements
    /// when the rewritten code is compiled.
    discard: HashSet<Location>,
}

impl UnlowerMap {
    pub fn get(&self, key: &PreciseLoc) -> Option<&MirOrigin> {
        self.origins.get(key)
    }

    pub fn origins_map(&self) -> &BTreeMap<PreciseLoc, MirOrigin> {
        &self.origins
    }

    pub fn discard_rewrites_for(&self, loc: Location) -> bool {
        self.discard.contains(&loc)
    }
}

struct UnlowerVisitor<'a, 'tcx> {
    tcx: TyCtxt<'tcx>,
    mir: &'a Body<'tcx>,
    typeck_results: &'tcx TypeckResults<'tcx>,
    span_index: SpanIndex<Location>,
    unlower_map: UnlowerMap,

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
        match self.unlower_map.origins.entry(key) {
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
        if is_temp_var_operand(self.mir, op) {
            MirOriginDesc::LoadFromTemp
        } else {
            MirOriginDesc::Expr
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
        if locs.is_empty() {
            return;
        }

        let warn = |locs: &[_], desc| {
            warn!("{}", desc);
            info!("locs:");
            for &loc in locs {
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
                let (loc, mir_pl, mir_rv) = match self.get_sole_assign(&locs) {
                    Some(x) => x,
                    None => {
                        warn(&locs, "expected exactly one StatementKind::Assign");
                        return;
                    }
                };
                self.record(loc, &[], ex);
                self.visit_expr_place(pl, loc, vec![SubLoc::Dest], mir_pl, &[]);
                self.visit_expr_rvalue(rv, loc, vec![SubLoc::Rvalue], mir_rv, &[]);
                return;
            }

            hir::ExprKind::Call(_, args) | hir::ExprKind::MethodCall(_, args, _) => {
                // Handle adjustments on the call's output first.
                let (_mir_pl, mut cursor) = match self.make_visit_expr_cursor(&locs) {
                    Some(x @ (pl, _)) if is_var(pl) => x,
                    _ => {
                        warn(&locs, "expected final Assign to store into var");
                        debug!(
                            "visit_expr_inner: bail out: expr at {:?} isn't assigned to a var",
                            ex.span
                        );
                        return;
                    }
                };
                self.record_desc(cursor.loc, &[], ex, MirOriginDesc::StoreIntoLocal);
                self.peel_adjustments(ex, &mut cursor);

                // After all adjustments have been peeled, the next `Location` should be the call
                // itself.
                let mir_args = match cursor.cur {
                    ExprMir::Call(term) => match term.kind {
                        mir::TerminatorKind::Call {
                            ref args,
                            destination,
                            ..
                        } => {
                            if !is_var(destination) {
                                warn(&locs, "expected final Call to store into var");
                                return;
                            }
                            args
                        }
                        _ => unreachable!("ExprMir::Call should always contain Call terminator"),
                    },
                    _ => {
                        warn(&locs, "expected MIR Call for HIR Call/MethodCall");
                        return;
                    }
                };
                let loc = cursor.loc;
                let extra_locs = cursor.locs;
                self.finish_visit_expr_cursor(ex, cursor);

                self.record(loc, &[SubLoc::Rvalue], ex);
                for (i, (arg, mir_arg)) in args.iter().zip(mir_args).enumerate() {
                    let sub_loc = vec![SubLoc::Rvalue, SubLoc::CallArg(i)];
                    self.record_operand(loc, &sub_loc, arg, mir_arg);
                    self.visit_expr_operand(arg, loc, sub_loc, mir_arg, &[]);
                }

                if !extra_locs.is_empty() {
                    // Special case: if the receiver of a `MethodCall` has adjustments, they are
                    // emitted with the same span as the `MethodCall` itself, and thus show up as
                    // leftover `extra_locs` here.  We associate them with the child instead so all
                    // of the child's statements can be processed together.
                    if matches!(ex.kind, hir::ExprKind::MethodCall(..)) {
                        self.append_extra_locations
                            .entry(args[0].hir_id)
                            .or_insert_with(Vec::new)
                            .extend_from_slice(extra_locs);
                    } else {
                        warn!("NYI: extra locations {:?} in Call", &locs[..locs.len() - 1]);
                    }
                }
                return;
            }

            // Remaining cases fall through to the default behavior below.
            hir::ExprKind::Index(_arr_ex, _idx_ex) => {
                // Look for the following pattern:
                //      _3 = Len(((*_1).0: [u32; 4]))
                //      _4 = Lt(_2, _3)
                //      assert(
                //          move _4,
                //          "index out of bounds: the length is {} but the index is {}",
                //          move _3,
                //          _2,
                //      ) -> [success: bb1, unwind: bb2];
                let mut match_pattern = || -> Option<()> {
                    let mut iter = locs.iter().enumerate();
                    // This pattern of `iter.by_ref().filter_map(..).next()` advances `iter` until
                    // it produces an item matching the `filter_map` predicate.  The next call with
                    // this pattern will continue searching from the following item.
                    let (len_idx, len_var) = iter
                        .by_ref()
                        .filter_map(|(i, &loc)| {
                            // Look for `_len = Len(_)`
                            let stmt = self.mir.stmt_at(loc).left()?;
                            if let mir::StatementKind::Assign(ref x) = stmt.kind {
                                let (ref pl, ref rv) = **x;
                                let pl_var = pl.as_local()?;
                                if matches!(rv, mir::Rvalue::Len(_)) {
                                    return Some((i, pl_var));
                                }
                            }
                            None
                        })
                        .next()?;
                    let (lt_idx, lt_var) = iter
                        .by_ref()
                        .filter_map(|(i, &loc)| {
                            // Look for `_ok = Lt(_, _len)`
                            let stmt = self.mir.stmt_at(loc).left()?;
                            if let mir::StatementKind::Assign(ref x) = stmt.kind {
                                let (ref pl, ref rv) = **x;
                                let pl_var = pl.as_local()?;
                                if let mir::Rvalue::BinaryOp(mir::BinOp::Lt, ref ops) = *rv {
                                    let (_, ref op2) = **ops;
                                    let op2_var = op2.place()?.as_local()?;
                                    if op2_var == len_var {
                                        return Some((i, pl_var));
                                    }
                                }
                            }
                            None
                        })
                        .next()?;
                    let assert_idx = iter
                        .by_ref()
                        .filter_map(|(i, &loc)| {
                            // Look for `Assert(_ok, ..)`
                            let term = self.mir.stmt_at(loc).right()?;
                            if let mir::TerminatorKind::Assert { ref cond, .. } = term.kind {
                                let cond_var = cond.place()?.as_local()?;
                                if cond_var == lt_var {
                                    return Some(i);
                                }
                            }
                            None
                        })
                        .next()?;

                    // All three parts were found.  Mark them as `discard`, then remove them from
                    // `locs`.
                    self.unlower_map.discard.insert(locs[len_idx]);
                    self.unlower_map.discard.insert(locs[lt_idx]);
                    self.unlower_map.discard.insert(locs[assert_idx]);

                    if lt_idx == len_idx + 1 && assert_idx == len_idx + 2 {
                        // All three locations are consecutive.  Remove them with `drain`.
                        locs.drain(lt_idx..=assert_idx);
                    } else {
                        // Remove the three locations separately.  Remove in reverse order to avoid
                        // perturbing the other indices.
                        debug_assert!(assert_idx > lt_idx);
                        debug_assert!(lt_idx > len_idx);
                        locs.remove(assert_idx);
                        locs.remove(lt_idx);
                        locs.remove(len_idx);
                    }

                    Some(())
                };
                // `match_pattern` returns `Option` only so we can bail out with `?`.  The result
                // is unused.
                let _ = match_pattern();
            }

            _ => {}
        }

        // `locs` can become empty if some locations were found, but all of them were consumed by
        // earlier processing.
        if locs.is_empty() {
            return;
        }

        // For all other `ExprKind`s, we expect the last `loc` to be an assignment storing the
        // final result into a temporary.
        let (_mir_pl, mut cursor) = match self.make_visit_expr_cursor(&locs) {
            Some(x @ (pl, _)) if is_var(pl) => x,
            _ => {
                warn(&locs, "expected final Assign to store into var");
                return;
            }
        };
        self.record_desc(cursor.loc, &[], ex, MirOriginDesc::StoreIntoLocal);
        self.walk_expr(ex, &mut cursor);
        self.finish_visit_expr_cursor(ex, cursor);
    }

    /// Try to create a `VisitExprCursor` from the RHS of statement `locs.last()`.  Returns the LHS
    /// of the statement and the cursor on success.  Fails (returning `None`) if the statement at
    /// `locs.last()` is not `StatementKind::Assign` or `TerminatorKind::Call`.
    fn make_visit_expr_cursor<'b>(
        &self,
        locs: &'b [Location],
    ) -> Option<(mir::Place<'tcx>, VisitExprCursor<'a, 'b, 'tcx>)> {
        let (&loc, extra_locs) = locs.split_last()?;

        let (pl, expr_mir) = match self.mir.stmt_at(loc) {
            Either::Left(stmt) => match stmt.kind {
                mir::StatementKind::Assign(ref x) => (x.0, ExprMir::Rvalue(&x.1)),
                _ => return None,
            },
            Either::Right(term) => match term.kind {
                mir::TerminatorKind::Call { destination, .. } => (destination, ExprMir::Call(term)),
                _ => return None,
            },
        };

        let cursor =
            VisitExprCursor::new(self.mir, expr_mir, extra_locs, loc, vec![SubLoc::Rvalue]);

        Some((pl, cursor))
    }

    fn finish_visit_expr_cursor(
        &mut self,
        ex: &'tcx hir::Expr<'tcx>,
        cursor: VisitExprCursor<'a, '_, 'tcx>,
    ) {
        // Record entries for temporaries that are buffered in `temp_info`.
        for (precise_loc, desc) in cursor.temp_info {
            self.record_desc(precise_loc.loc, &precise_loc.sub, ex, desc);
        }
    }

    fn peel_adjustments(
        &mut self,
        ex: &'tcx hir::Expr<'tcx>,
        cursor: &mut VisitExprCursor<'a, '_, 'tcx>,
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
            // Remember the current location.  If the following matching operations succeed, we
            // want to record an entry at this location, but the operations in question will also
            // update `cursor.loc`.
            let loc = cursor.loc;
            let sub_loc = cursor.sub_loc.clone();
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
            self.record_desc(loc, &sub_loc, ex, MirOriginDesc::Adjustment(i));
            cursor.set_last_adjustment(i);
        }

        while cursor.peel_temp().is_some() {
            // No-op.  Just loop until we've peeled all temporaries.
        }
    }

    fn visit_expr_rvalue(
        &mut self,
        ex: &'tcx hir::Expr<'tcx>,
        loc: Location,
        sub_loc: Vec<SubLoc>,
        rv: &'a mir::Rvalue<'tcx>,
        extra_locs: &[Location],
    ) {
        let _g = panic_detail::set_current_span(ex.span);

        // TODO: We do this check early to ensure that the `LoadFromTemp` is emitted with subloc
        // `[Rvalue]` rather than `[Rvalue, RvalueOperand(0)]`, since `mir_op` emits rewrites with
        // just `[Rvalue]`.  For `Rvalue::Use`, the rvalue and its operand are basically
        // synonymous, so ideally we would accept both sublocs as well.  We should probably add an
        // aliasing system or some kind of cleverness in `distribute` to make both versions work,
        // or else modify the definition of `SubLoc` so there's only one way to express this
        // location.
        if is_temp_var_rvalue(self.mir, rv) {
            self.record_desc(loc, &sub_loc, ex, MirOriginDesc::LoadFromTemp);
            return;
        }

        let mut cursor = VisitExprCursor::new(
            self.mir,
            ExprMir::Rvalue(rv),
            extra_locs,
            loc,
            sub_loc.to_owned(),
        );
        self.walk_expr(ex, &mut cursor);

        self.finish_visit_expr_cursor(ex, cursor);
    }

    fn visit_expr_operand(
        &mut self,
        ex: &'tcx hir::Expr<'tcx>,
        loc: Location,
        sub_loc: Vec<SubLoc>,
        op: &'a mir::Operand<'tcx>,
        extra_locs: &[Location],
    ) {
        let _g = panic_detail::set_current_span(ex.span);

        if is_temp_var_operand(self.mir, op) {
            self.record_desc(loc, &sub_loc, ex, MirOriginDesc::LoadFromTemp);
            return;
        }

        let mut cursor = VisitExprCursor::new(
            self.mir,
            ExprMir::Operand(op),
            extra_locs,
            loc,
            sub_loc.to_owned(),
        );
        self.walk_expr(ex, &mut cursor);

        self.finish_visit_expr_cursor(ex, cursor);
    }

    fn visit_expr_place(
        &mut self,
        ex: &'tcx hir::Expr<'tcx>,
        loc: Location,
        sub_loc: Vec<SubLoc>,
        pl: mir::Place<'tcx>,
        extra_locs: &[Location],
    ) {
        let _g = panic_detail::set_current_span(ex.span);

        let mut cursor = VisitExprCursor::new(
            self.mir,
            ExprMir::Place(pl.as_ref()),
            extra_locs,
            loc,
            sub_loc,
        );
        self.walk_expr(ex, &mut cursor);

        self.finish_visit_expr_cursor(ex, cursor);
    }

    fn walk_expr(&mut self, ex: &'tcx hir::Expr<'tcx>, cursor: &mut VisitExprCursor<'a, '_, 'tcx>) {
        let mut ex = ex;
        loop {
            self.peel_adjustments(ex, cursor);
            if cursor.is_temp_var() {
                self.record_desc(cursor.loc, &cursor.sub_loc, ex, MirOriginDesc::LoadFromTemp);
                break;
            } else {
                self.record_desc(cursor.loc, &cursor.sub_loc, ex, MirOriginDesc::Expr);
            }

            match ex.kind {
                hir::ExprKind::Unary(hir::UnOp::Deref, ptr_ex) => {
                    if let Some(()) = cursor.peel_deref() {
                        ex = ptr_ex;
                        continue;
                    }
                }
                hir::ExprKind::Field(base_ex, _field_ident) => {
                    if let Some(()) = cursor.peel_field() {
                        ex = base_ex;
                        continue;
                    }
                }
                hir::ExprKind::Index(arr_ex, _idx_ex) => {
                    if let Some(()) = cursor.peel_index() {
                        ex = arr_ex;
                        continue;
                    }
                }
                hir::ExprKind::AddrOf(kind, _mutbl, pl_ex) => match kind {
                    hir::BorrowKind::Ref => {
                        if let Some(_mir_mutbl) = cursor.peel_ref() {
                            ex = pl_ex;
                            continue;
                        }
                    }
                    hir::BorrowKind::Raw => {
                        if let Some(_mir_mutbl) = cursor.peel_address_of() {
                            ex = pl_ex;
                            continue;
                        }
                    }
                },
                _ => {}
            }
            // Keep looping only in cases that we specifically recognize.
            break;
        }
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
    /// The RHS of a `TerminatorKind::Call`.
    Call(&'a mir::Terminator<'tcx>),
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
struct VisitExprCursor<'a, 'b, 'tcx> {
    mir: &'a Body<'tcx>,
    cur: ExprMir<'a, 'tcx>,
    locs: &'b [Location],

    loc: Location,
    sub_loc: Vec<SubLoc>,

    /// Buffer for temporaries handled by `peel_temp`.
    ///
    /// When `peel_temp` traverses past a temporary, it also needs to emit some `StoreIntoLocal`
    /// and `LoadFromTemp` entries as a side effect.  But it doesn't have access to the
    /// `UnlowerVisitor` to emit these entries directly, so instead we buffer those entries for the
    /// caller to emit later.
    temp_info: Vec<(PreciseLoc, MirOriginDesc)>,

    /// Index of the most recently traversed adjustment.  This is updated by the caller through
    /// `set_last_adjustment()`.  If this is `Some`, we generate `LoadFromTempForAdjustment`
    /// instead of `LoadFromTemp`.
    last_adjustment: Option<usize>,
}

impl<'a, 'b, 'tcx> VisitExprCursor<'a, 'b, 'tcx> {
    pub fn new(
        mir: &'a Body<'tcx>,
        cur: ExprMir<'a, 'tcx>,
        extra_locs: &'b [Location],
        loc: Location,
        sub_loc: Vec<SubLoc>,
    ) -> VisitExprCursor<'a, 'b, 'tcx> {
        VisitExprCursor {
            mir,
            cur,
            locs: extra_locs,
            loc,
            sub_loc,

            temp_info: Vec::new(),
            last_adjustment: None,
        }
    }

    /// Check whether the current MIR is a temporary.
    pub fn is_temp_var(&self) -> bool {
        match self.cur {
            ExprMir::Rvalue(rv) => is_temp_var_rvalue(self.mir, rv),
            ExprMir::Operand(op) => is_temp_var_operand(self.mir, op),
            ExprMir::Place(pl) => is_temp_var(self.mir, pl),
            ExprMir::Call(_) => false,
        }
    }

    pub fn set_last_adjustment(&mut self, i: usize) {
        self.last_adjustment = Some(i);
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

        let (assign_pl, rv_expr_mir) = match self.mir.stmt_at(loc) {
            Either::Left(stmt) => match stmt.kind {
                mir::StatementKind::Assign(ref x) => (x.0, ExprMir::Rvalue(&x.1)),
                _ => return None,
            },
            Either::Right(term) => match term.kind {
                mir::TerminatorKind::Call { destination, .. } => (destination, ExprMir::Call(term)),
                _ => return None,
            },
        };

        if !is_temp_var(self.mir, assign_pl.as_ref()) {
            return None;
        }

        let store_loc = PreciseLoc { loc, sub: vec![] };
        let load_desc = match self.last_adjustment {
            Some(i) => MirOriginDesc::LoadFromTempForAdjustment(i),
            None => MirOriginDesc::LoadFromTemp,
        };
        self.temp_info.push((load_loc, load_desc));
        self.temp_info
            .push((store_loc, MirOriginDesc::StoreIntoLocal));

        // Success - the next relevant MIR is `rv_expr_mir`.
        self.cur = rv_expr_mir;
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
                    self.sub_loc.push(SubLoc::PlaceDerefPointer);
                    return Some(());
                }
            }
            self.peel_temp()?;
        }
    }

    /// If the current MIR is a `Place` ending with a `Field` projection, peel off the `Field`.
    pub fn peel_field(&mut self) -> Option<()> {
        loop {
            let pl = self.require_place_mut()?;
            if let Some((&outer_proj, remaining_proj)) = pl.projection.split_last() {
                if matches!(outer_proj, mir::PlaceElem::Field(_idx, _ty)) {
                    pl.projection = remaining_proj;
                    self.sub_loc.push(SubLoc::PlaceFieldBase);
                    return Some(());
                }
            }
            self.peel_temp()?;
        }
    }

    /// If the current MIR is a `Place` ending with an `Index` projection, peel off the `Index`.
    pub fn peel_index(&mut self) -> Option<()> {
        loop {
            let pl = self.require_place_mut()?;
            if let Some((&outer_proj, remaining_proj)) = pl.projection.split_last() {
                if matches!(outer_proj, mir::PlaceElem::Index(_)) {
                    pl.projection = remaining_proj;
                    self.sub_loc.push(SubLoc::PlaceIndexArray);
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
    pl.projection.is_empty() && mir.local_kind(pl.local) == mir::LocalKind::Temp
}

fn is_temp_var_operand(mir: &Body, op: &mir::Operand) -> bool {
    match get_operand_place(op) {
        Some(pl) => is_temp_var(mir, pl.as_ref()),
        None => false,
    }
}

fn is_temp_var_rvalue(mir: &Body, rv: &mir::Rvalue) -> bool {
    match get_rvalue_operand(rv) {
        Some(op) => is_temp_var_operand(mir, op),
        None => false,
    }
}

fn get_rvalue_operand<'a, 'tcx>(rv: &'a mir::Rvalue<'tcx>) -> Option<&'a mir::Operand<'tcx>> {
    match *rv {
        mir::Rvalue::Use(ref op) => Some(op),
        _ => None,
    }
}

fn get_operand_place<'tcx>(op: &mir::Operand<'tcx>) -> Option<mir::Place<'tcx>> {
    match *op {
        mir::Operand::Copy(pl) | mir::Operand::Move(pl) => Some(pl),
        _ => None,
    }
}

/// Indicate whether a given MIR statement should be considered when building the unlowering map.
fn filter_stmt(stmt: &mir::Statement) -> bool {
    // Ignore `AscribeUserType` annotations.  These appear in the middle of some expressions.
    // It's easier to ignore them all at this level rather than try to handle them in all the
    // places they might appear.
    !matches!(stmt.kind, mir::StatementKind::AscribeUserType(..))
}

/// Indicate whether a given MIR terminator should be considered when building the unlowering map.
fn filter_term(_term: &mir::Terminator) -> bool {
    true
}

fn build_span_index(mir: &Body<'_>) -> SpanIndex<Location> {
    debug!("building span index for {:?}:", mir.source);
    let mut span_index_items = Vec::new();
    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        for (i, stmt) in bb_data.statements.iter().enumerate() {
            if !filter_stmt(stmt) {
                continue;
            }
            let loc = Location {
                block: bb,
                statement_index: i,
            };
            debug!("  {:?}: {:?}", loc, stmt.source_info.span);
            span_index_items.push((stmt.source_info.span, loc));
        }

        let term = bb_data.terminator();
        if filter_term(term) {
            let loc = Location {
                block: bb,
                statement_index: bb_data.statements.len(),
            };
            debug!("  {:?}: {:?}", loc, term.source_info.span);
            span_index_items.push((term.source_info.span, loc));
        }
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
pub fn unlower<'tcx>(tcx: TyCtxt<'tcx>, mir: &Body<'tcx>, hir_body_id: hir::BodyId) -> UnlowerMap {
    // If this MIR body came from a `#[derive]`, ignore it.
    if util::is_automatically_derived(tcx, mir) {
        return UnlowerMap::default();
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
        unlower_map: UnlowerMap::default(),
        append_extra_locations: HashMap::new(),
    };
    visitor.visit_body(hir);

    if !visitor.append_extra_locations.is_empty() {
        for (&hir_id, locs) in &visitor.append_extra_locations {
            error!(
                "leftover locations for {hir_id:?} = {:?}: locs = {locs:?}",
                tcx.hir().get(hir_id)
            );
        }
    }

    visitor.unlower_map
}
