//! Rewriting of expressions comes with one extra bit of complexity: sometimes the code we're
//! modifying has had autoderef and/or autoref `Adjustment`s applied to it. To avoid unexpectedly
//! changing which adjustments get applied, we "materialize" the `Adjustment`s, making them
//! explicit in the source code. For example, `vec.len()`, which implicitly applies deref and ref
//! adjustments to `vec`, would be converted to `(&*vec).len()`, where the deref and ref operations
//! are explicit, and might be further rewritten from there. However, we don't want to materialize
//! all adjustments, as this would make even non-rewritten code extremely verbose, so we try to
//! materialize adjustments only on code that's subject to some rewrite.

use crate::context::{AnalysisCtxt, Assignment, DontRewriteFnReason, FlagSet, LTy, PermissionSet};
use crate::last_use::{self, LastUse};
use crate::panic_detail;
use crate::pointee_type::PointeeTypes;
use crate::pointer_id::{GlobalPointerTable, PointerId, PointerTable};
use crate::rewrite;
use crate::type_desc::{self, Ownership, Quantity, TypeDesc};
use crate::util::{self, ty_callee, Callee};
use log::{debug, error, trace};
use rustc_ast::Mutability;
use rustc_middle::mir::{
    BasicBlock, Body, BorrowKind, Location, Operand, Place, PlaceElem, PlaceRef, Rvalue, Statement,
    StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::print::{FmtPrinter, PrettyPrinter, Print};
use rustc_middle::ty::{Ty, TyCtxt, TyKind};
use std::collections::HashMap;
use std::ops::Index;

use rustc_hir::def::Namespace;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum SubLoc {
    /// The LHS of an assignment or call.  `StatementKind::Assign/TerminatorKind::Call -> Place`
    Dest,
    /// The RHS of an assignment or call.  `StatementKind::Assign/TerminatorKind::Call -> Rvalue`
    Rvalue,
    /// The Nth argument of a call.  `TerminatorKind::Call -> Operand`
    CallArg(usize),
    /// The Nth operand of an rvalue.  `Rvalue -> Operand`
    RvalueOperand(usize),
    /// The Nth place of an rvalue.  Used for cases like `Rvalue::Ref` that directly refer to a
    /// `Place`.  `Rvalue -> Place`
    RvaluePlace(usize),
    /// The place referenced by an operand.  `Operand::Move/Operand::Copy -> Place`
    OperandPlace,
    /// The pointer used in a deref projection.  `Place -> Place`
    PlaceDerefPointer,
    /// The base of a field projection.  `Place -> Place`
    PlaceFieldBase,
    /// The array used in an index or slice projection.  `Place -> Place`
    PlaceIndexArray,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum RewriteKind {
    /// Replace `ptr.offset(i)` with something like `&ptr[i..]`.
    OffsetSlice { mutbl: bool },
    /// Replace `ptr.offset(i)` with something like `ptr.as_ref().map(|p| &p[i..])`.
    OptionMapOffsetSlice { mutbl: bool },
    /// Replace `slice` with `&slice[0]`.
    SliceFirst { mutbl: bool },
    /// Replace `ptr` with `&*ptr` or `&mut *ptr`, converting `ptr` to `&T` or `&mut T`.
    Reborrow { mutbl: bool },
    /// Remove a call to `as_ptr` or `as_mut_ptr`.
    RemoveAsPtr,
    /// Remove a cast, changing `x as T` to just `x`.
    RemoveCast,
    /// Replace &raw with & or &raw mut with &mut
    RawToRef { mutbl: bool },

    /// Replace `ptr` with `*ptr`.
    Deref,

    /// Replace `ptr.is_null()` with `ptr.is_none()`.
    IsNullToIsNone,
    /// Replace `ptr.is_null()` with the constant `false`.  We use this in cases where the rewritten
    /// type of `ptr` is non-optional because we inferred `ptr` to be non-nullable.
    IsNullToConstFalse,
    /// Replace `ptr::null()` or `ptr::null_mut()` with `None`.
    PtrNullToNone,
    /// Replace `0 as *const T` or `0 as *mut T` with `None`.
    ZeroAsPtrToNone,

    /// Replace a call to `memcpy(dest, src, n)` with a safe copy operation that works on slices
    /// instead of raw pointers.  `elem_ty` is the pointee type type, whose size is used to convert
    /// the byte length `n` to an element count.  `dest_single` and `src_single` are set when
    /// `dest`/`src` is a pointer to a single item rather than a slice.
    MemcpySafe {
        elem_ty: String,
        dest_single: bool,
        dest_option: bool,
        src_single: bool,
        src_option: bool,
    },
    /// Replace a call to `memset(ptr, 0, n)` with a safe zeroize operation.  `elem_ty` is the type
    /// being zeroized, whose size is used to convert the byte length `n` to an element count.
    /// `dest_single` is set when `dest` is a pointer to a single item rather than a slice.
    MemsetZeroize {
        zero_ty: ZeroizeType,
        elem_ty: String,
        dest_single: bool,
    },

    /// Replace a call to `malloc(n)` with a safe `Box::new` operation.  The new allocation will be
    /// zero-initialized.
    MallocSafe {
        zero_ty: ZeroizeType,
        elem_ty: String,
        single: bool,
    },
    /// Replace a call to `free(p)` with a safe `drop` operation.
    FreeSafe { single: bool },
    ReallocSafe {
        zero_ty: ZeroizeType,
        elem_ty: String,
        src_single: bool,
        dest_single: bool,
        option: bool,
    },
    CallocSafe {
        zero_ty: ZeroizeType,
        elem_ty: String,
        single: bool,
    },

    /// Convert `Option<T>` to `T` by calling `.unwrap()`.
    OptionUnwrap,
    /// Convert `T` to `Option<T>` by wrapping the value in `Some`.
    OptionSome,
    /// Begin an `Option::map` operation, converting `Option<T>` to `T`.
    OptionMapBegin,
    /// End an `Option::map` operation, converting `T` to `Option<T>`.
    ///
    /// `OptionMapBegin` and `OptionMapEnd` could legally be implemented as aliases for
    /// `OptionUnwrap` and `OptionSome` respectively.  However, when `OptionMapBegin` and
    /// `OptionMapEnd` are paired, we instead emit a call to `Option::map` with the intervening
    /// rewrites applied within the closure.  This has the same effect when the input is `Some`,
    /// but passes through `None` unchanged instead of panicking.
    OptionMapEnd,
    /// Downgrade ownership of an `Option` to `Option<&_>` or `Option<&mut _>` by calling
    /// `as_ref()`/`as_mut()` or `as_deref()`/`as_deref_mut()`.
    OptionDowngrade {
        mutbl: bool,
        kind: OptionDowngradeKind,
    },

    /// Extract the `T` from `DynOwned<T>`.
    DynOwnedUnwrap,
    /// Move out of `&mut DynOwned<T>` and set the original location to empty / non-owned.
    DynOwnedTake,
    /// Wrap `T` in `Ok` to produce `DynOwned<T>`.
    DynOwnedWrap,
    /// Downgrade ownership of a `DynOwned<T>` to `&T` or `&mut T` by calling
    /// `as_deref()`/`as_deref_mut()` and `unwrap`.
    DynOwnedDowngrade { mutbl: bool },

    /// Cast `&T` to `*const T` or `&mut T` to `*mut T`.
    CastRefToRaw { mutbl: bool },
    /// Cast `*const T` to `*mut T` or vice versa.  If `to_mutbl` is true, we are casting to
    /// `*mut T`; otherwise, we're casting to `*const T`.
    CastRawToRaw { to_mutbl: bool },
    /// Cast `*const T` to `& T` or `*mut T` to `&mut T`.
    UnsafeCastRawToRef { mutbl: bool },
    /// Cast *mut T to *const Cell<T>
    CastRawMutToCellPtr { ty: String },

    /// Replace `y` in `let x = y` with `Cell::new(y)`, i.e. `let x = Cell::new(y)`
    /// TODO: ensure `y` implements `Copy`
    CellNew,
    /// Replace `*y` with `Cell::get(y)` where `y` is a pointer
    CellGet,
    /// Replace `*y = x` with `Cell::set(x)` where `y` is a pointer
    CellSet,
    /// Wrap `&mut T` in `Cell::from_mut` to get `&Cell<T>`.
    CellFromMut,
    /// `x` to `x.as_ptr()`
    AsPtr,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum OptionDowngradeKind {
    /// E.g. `Option<T>` to `Option<&mut T>` via `p.as_mut()`
    Borrow,
    /// E.g. `Option<Box<T>>` to `Option<&mut T>` via `p.as_deref_mut()`
    Deref,
    /// E.g. `Option<&mut T>` to `Option<&T>` via `p.map(|ptr| &*ptr)`
    MoveAndDeref,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ZeroizeType {
    /// Zeroize by storing the literal `0`.
    Int,
    /// Zeroize by storing the literal `false`.
    Bool,
    /// Zeroize by storing `None`.
    Option,
    /// Iterate over `x.iter_mut()` and zeroize each element.
    Array(Box<ZeroizeType>),
    /// Zeroize each named field.
    Struct(String, Vec<(String, ZeroizeType)>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MirRewrite {
    pub kind: RewriteKind,
    pub sub_loc: Vec<SubLoc>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum PlaceAccess {
    /// Enclosing context intends to read from the place.
    Imm,
    /// Enclosing context intends to write to the place.
    Mut,
    /// Enclosing context intends to move out of the place.
    Move,
}

impl PlaceAccess {
    pub fn from_bool(mutbl: bool) -> PlaceAccess {
        if mutbl {
            PlaceAccess::Mut
        } else {
            PlaceAccess::Imm
        }
    }

    pub fn from_mutbl(mutbl: Mutability) -> PlaceAccess {
        match mutbl {
            Mutability::Not => PlaceAccess::Imm,
            Mutability::Mut => PlaceAccess::Mut,
        }
    }
}

/// Named boolean for use with `visit_place`.  `RequireSinglePointer::Yes` means that if the
/// `Place` ends with a `Deref` projection, the pointer being dereferenced must have
/// `Quantity::Single`.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum RequireSinglePointer {
    No,
    Yes,
}

impl RequireSinglePointer {
    pub fn as_bool(self) -> bool {
        self == RequireSinglePointer::Yes
    }
}

struct ExprRewriteVisitor<'a, 'tcx> {
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    perms: &'a GlobalPointerTable<PermissionSet>,
    flags: &'a GlobalPointerTable<FlagSet>,
    pointee_types: PointerTable<'a, PointeeTypes<'tcx>>,
    last_use: &'a LastUse,
    rewrites: &'a mut HashMap<Location, Vec<MirRewrite>>,
    mir: &'a Body<'tcx>,
    loc: Location,
    sub_loc: Vec<SubLoc>,
    errors: DontRewriteFnReason,
}

impl<'a, 'tcx> ExprRewriteVisitor<'a, 'tcx> {
    pub fn new(
        acx: &'a AnalysisCtxt<'a, 'tcx>,
        asn: &'a Assignment,
        pointee_types: PointerTable<'a, PointeeTypes<'tcx>>,
        last_use: &'a LastUse,
        rewrites: &'a mut HashMap<Location, Vec<MirRewrite>>,
        mir: &'a Body<'tcx>,
    ) -> ExprRewriteVisitor<'a, 'tcx> {
        let perms = asn.perms();
        let flags = asn.flags();
        ExprRewriteVisitor {
            acx,
            perms,
            flags,
            pointee_types,
            last_use,
            rewrites,
            mir,
            loc: Location {
                block: BasicBlock::from_usize(0),
                statement_index: 0,
            },
            sub_loc: Vec::new(),
            errors: DontRewriteFnReason::empty(),
        }
    }

    fn err(&mut self, reason: DontRewriteFnReason) {
        self.errors.insert(reason);
    }

    fn enter<F: FnOnce(&mut Self) -> R, R>(&mut self, sub: SubLoc, f: F) -> R {
        self.sub_loc.push(sub);
        let r = f(self);
        self.sub_loc.pop();
        r
    }

    fn enter_dest<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::Dest, f)
    }

    fn enter_rvalue<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::Rvalue, f)
    }

    fn enter_call_arg<F: FnOnce(&mut Self) -> R, R>(&mut self, i: usize, f: F) -> R {
        self.enter(SubLoc::CallArg(i), f)
    }

    fn enter_rvalue_operand<F: FnOnce(&mut Self) -> R, R>(&mut self, i: usize, f: F) -> R {
        self.enter(SubLoc::RvalueOperand(i), f)
    }

    fn enter_rvalue_place<F: FnOnce(&mut Self) -> R, R>(&mut self, i: usize, f: F) -> R {
        self.enter(SubLoc::RvaluePlace(i), f)
    }

    fn enter_operand_place<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::OperandPlace, f)
    }

    fn enter_place_deref_pointer<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::PlaceDerefPointer, f)
    }

    fn enter_place_field_base<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::PlaceFieldBase, f)
    }

    fn enter_place_index_array<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.enter(SubLoc::PlaceIndexArray, f)
    }

    /// Get the pointee type of `lty`.  Returns the inferred pointee type from `self.pointee_types`
    /// if one is available, or the pointee type as represented in `lty` itself otherwise.  Returns
    /// `None` if `lty` is not a `RawPtr` or `Ref` type.
    ///
    /// TODO: This does not yet have any pointer-to-pointer support.  For example, if `lty` is
    /// `*mut *mut c_void` where the inner pointer is known to point to `u8`, this method will
    /// still return `*mut c_void` instead of `*mut u8`.
    fn pointee_lty(&self, lty: LTy<'tcx>) -> Option<LTy<'tcx>> {
        if !matches!(lty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)) {
            return None;
        }
        debug_assert_eq!(lty.args.len(), 1);
        let ptr = lty.label;
        if !ptr.is_none() {
            if let Some(pointee_lty) = self.pointee_types[ptr].get_sole_lty() {
                return Some(pointee_lty);
            }
        }
        Some(lty.args[0])
    }

    fn is_nullable(&self, ptr: PointerId) -> bool {
        !ptr.is_none()
            && !self.perms[ptr].contains(PermissionSet::NON_NULL)
            && !self.flags[ptr].contains(FlagSet::FIXED)
    }

    fn is_dyn_owned(&self, lty: LTy) -> bool {
        if !matches!(lty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)) {
            return false;
        }
        if lty.label.is_none() {
            return false;
        }
        let perms = self.perms[lty.label];
        let flags = self.flags[lty.label];
        if flags.contains(FlagSet::FIXED) {
            return false;
        }
        let desc = type_desc::perms_to_desc(lty.ty, perms, flags);
        desc.dyn_owned
    }

    /// Check whether the `Place` identified by `which` within the current statement or terminator
    /// is the last use of a local.
    fn is_last_use(&self, which: last_use::WhichPlace) -> bool {
        self.last_use.is_last_use(self.loc, which)
    }

    fn rewrite_lty(&self, lty: LTy<'tcx>) -> Ty<'tcx> {
        let tcx = self.acx.tcx();
        rewrite::ty::rewrite_lty(
            tcx,
            lty,
            &self.perms,
            &self.flags,
            &self.pointee_types,
            &self.acx.gacx.adt_metadata,
        )
    }

    fn lty_to_rewritten_str(&self, lty: LTy<'tcx>) -> (Ty<'tcx>, String) {
        let rewritten_ty = self.rewrite_lty(lty);
        let tcx = self.acx.tcx();
        let printer = FmtPrinter::new(tcx, Namespace::TypeNS);
        let s = rewritten_ty.print(printer).unwrap().into_buffer();
        (rewritten_ty, s)
    }

    fn current_sub_loc_as_which_place(&self) -> Option<last_use::WhichPlace> {
        // This logic is a bit imprecise, but should be correct in the cases where we actually call
        // it (namely, when the `Place`/`Rvalue` is simply a `Local`).
        let mut which = None;
        for sl in &self.sub_loc {
            match *sl {
                SubLoc::Dest => {
                    which = Some(last_use::WhichPlace::Lvalue);
                }
                SubLoc::Rvalue => {
                    which = Some(last_use::WhichPlace::Operand(0));
                    // Note there may be more entries in `self.sub_loc` giving a more precise
                    // position, as in `[SubLoc::Rvalue, SubLoc::CallArg(0)]`.  We keep looping
                    // over `self.sub_loc` and take the last (most precise) entry.
                }
                SubLoc::CallArg(i) => {
                    // In a call, `WhichPlace::Operand(0)` refers to the callee function.
                    which = Some(last_use::WhichPlace::Operand(i + 1));
                }
                SubLoc::RvalueOperand(i) => {
                    which = Some(last_use::WhichPlace::Operand(i));
                }
                SubLoc::RvaluePlace(_) => {}
                SubLoc::OperandPlace => {}
                SubLoc::PlaceDerefPointer => {}
                SubLoc::PlaceFieldBase => {}
                SubLoc::PlaceIndexArray => {}
            }
        }
        which
    }

    /// Like `is_last_use`, but infers `WhichPlace` based on `self.sub_loc`.
    fn current_sub_loc_is_last_use(&self) -> bool {
        if let Some(which) = self.current_sub_loc_as_which_place() {
            self.is_last_use(which)
        } else {
            false
        }
    }

    fn visit_statement(&mut self, stmt: &Statement<'tcx>, loc: Location) {
        let _g = panic_detail::set_current_span(stmt.source_info.span);
        debug!(
            "mir_op::visit_statement: {:?} @ {:?}: {:?}",
            loc, stmt.source_info.span, stmt
        );
        self.loc = loc;
        debug_assert!(self.sub_loc.is_empty());

        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;

                let pl_lty = self.acx.type_of(pl);

                // FIXME: Needs changes to handle CELL pointers in struct fields.  Suppose `pl` is
                // something like `*(_1.0)`, where the `.0` field is CELL.  This should be
                // converted to a `Cell::get` call, but we would fail to enter this case because
                // `_1` fails the `is_any_ptr()` check.
                if pl.is_indirect() && self.acx.local_tys[pl.local].ty.is_any_ptr() {
                    let local_lty = self.acx.local_tys[pl.local];
                    let local_ptr = local_lty.label;
                    let perms = self.perms[local_ptr];
                    let flags = self.flags[local_ptr];
                    if !flags.contains(FlagSet::FIXED) {
                        let desc = type_desc::perms_to_desc(local_lty.ty, perms, flags);
                        if desc.own == Ownership::Cell {
                            if pl.projection.len() > 1 || desc.qty != Quantity::Single {
                                // NYI: `Cell` inside structs, arrays, or ptr-to-ptr
                                self.err(DontRewriteFnReason::COMPLEX_CELL);
                            }
                            // this is an assignment like `*x = 2` but `x` has CELL permissions
                            self.emit(RewriteKind::CellSet);
                        }
                    }
                }

                #[allow(clippy::single_match)]
                match rv {
                    Rvalue::Use(rv_op) => {
                        let local_ty = self.acx.local_tys[pl.local].ty;
                        let local_addr = self.acx.addr_of_local[pl.local];
                        let perms = self.perms[local_addr];
                        let flags = self.flags[local_addr];
                        let desc = type_desc::local_perms_to_desc(local_ty, perms, flags);
                        if desc.own == Ownership::Cell {
                            // this is an assignment like `let x = 2` but `x` has CELL permissions
                            if !pl.projection.is_empty() || desc.qty != Quantity::Single {
                                // NYI: `Cell` inside structs, arrays, or ptr-to-ptr
                                self.err(DontRewriteFnReason::COMPLEX_CELL);
                            }
                            self.enter_rvalue(|v| v.emit(RewriteKind::CellNew))
                        }

                        if let Some(rv_place) = rv_op.place() {
                            if rv_place.is_indirect()
                                && self.acx.local_tys[rv_place.local].ty.is_any_ptr()
                            {
                                let local_lty = self.acx.local_tys[rv_place.local];
                                let local_ptr = local_lty.label;
                                let flags = self.flags[local_ptr];
                                if !flags.contains(FlagSet::FIXED) && flags.contains(FlagSet::CELL)
                                {
                                    // this is an assignment like `let x = *y` but `y` has CELL permissions
                                    if pl.projection.len() > 1 || desc.qty != Quantity::Single {
                                        // NYI: `Cell` inside structs, arrays, or ptr-to-ptr
                                        self.err(DontRewriteFnReason::COMPLEX_CELL);
                                    }
                                    self.enter_rvalue(|v| v.emit(RewriteKind::CellGet))
                                }
                            }
                        }
                    }
                    _ => {}
                };

                let rv_lty = self.acx.type_of_rvalue(rv, loc);
                // The cast from `rv_lty` to `pl_lty` should be applied to the RHS.
                self.enter_rvalue(|v| {
                    // Special case: when reading from a `DynOwned` place to another `DynOwned`
                    // place, visit the RHS place mutably and `mem::take` out of it to avoid a
                    // static ownership transfer.

                    // Determine whether it's okay for the cast emitted below to move the
                    // expression that it's casting.  This is the case if the rvalue is not a
                    // place, which means it produces a temporary value that can be moved as
                    // needed, or if the rvalue is a place but is the last use of a local.
                    let cast_can_move =
                        !rv.is_place() || (rv.is_local() && v.current_sub_loc_is_last_use());

                    // Check whether this assignment transfers ownership of its RHS.  If so, return
                    // the RHS `Place`.
                    let assignment_transfers_ownership = || {
                        if !v.is_dyn_owned(v.acx.type_of(pl)) {
                            return None;
                        }
                        let op = match rv {
                            Rvalue::Use(ref x) => x,
                            _ => return None,
                        };
                        let rv_pl = op.place()?;
                        if !v.is_dyn_owned(v.acx.type_of(rv_pl)) {
                            return None;
                        }
                        Some(rv_pl)
                    };
                    if let Some(rv_pl) = assignment_transfers_ownership() {
                        // Obtain mutable access to `pl`, so we can `mem::take(&mut pl)`.
                        // Normally, `Operand::Move` would ask for `PlaceAccess::Move`; we
                        // instead bypass `visit_rvalue` and `visit_operand` so we can call
                        // `visit_place` directly with the desired access.
                        v.enter_rvalue_operand(0, |v| {
                            v.enter_operand_place(|v| {
                                eprintln!("BEGIN visit_place for ownership transfer");
                                v.visit_place(rv_pl, PlaceAccess::Mut, RequireSinglePointer::No);
                                eprintln!("END visit_place for ownership transfer");
                            });
                        });
                        // Obtain a reference to the place containing the `DynOwned` pointer.
                        if v.is_nullable(rv_lty.label) {
                            v.emit(RewriteKind::OptionDowngrade {
                                mutbl: true,
                                kind: OptionDowngradeKind::Borrow,
                            });
                            v.emit(RewriteKind::OptionMapBegin);
                            v.emit(RewriteKind::Deref);
                        }
                        // Take the pointer out of that place.
                        v.emit(RewriteKind::DynOwnedTake);
                        if v.is_nullable(rv_lty.label) {
                            v.emit(RewriteKind::OptionMapEnd);
                        }
                        v.emit_cast_lty_lty(rv_lty, pl_lty, cast_can_move);
                        return;
                    }

                    // Normal case: just `visit_rvalue` and emit a cast if needed.
                    v.visit_rvalue(rv, Some(rv_lty));
                    v.emit_cast_lty_lty_or_borrow(rv_lty, pl_lty, cast_can_move)
                });
                self.enter_dest(|v| v.visit_place(pl, PlaceAccess::Mut, RequireSinglePointer::Yes));
            }
            StatementKind::FakeRead(..) => {}
            StatementKind::SetDiscriminant { .. } => todo!("statement {:?}", stmt),
            StatementKind::Deinit(..) => {}
            StatementKind::StorageLive(..) => {}
            StatementKind::StorageDead(..) => {}
            StatementKind::Retag(..) => {}
            StatementKind::AscribeUserType(..) => {}
            StatementKind::Coverage(..) => {}
            StatementKind::CopyNonOverlapping(..) => todo!("statement {:?}", stmt),
            StatementKind::Nop => {}
        }
    }

    fn visit_terminator(&mut self, term: &Terminator<'tcx>, loc: Location) {
        let tcx = self.acx.tcx();
        let _g = panic_detail::set_current_span(term.source_info.span);
        self.loc = loc;
        debug_assert!(self.sub_loc.is_empty());

        match term.kind {
            TerminatorKind::Goto { .. } => {}
            TerminatorKind::SwitchInt { .. } => {}
            TerminatorKind::Resume => {}
            TerminatorKind::Abort => {}
            TerminatorKind::Return => {}
            TerminatorKind::Unreachable => {}
            TerminatorKind::Drop { .. } => {}
            TerminatorKind::DropAndReplace { .. } => {}
            TerminatorKind::Call {
                ref func,
                ref args,
                destination,
                target: _,
                ..
            } => {
                let func_ty = func.ty(self.mir, tcx);
                let pl_ty = self.acx.type_of(destination);

                // Special cases for particular functions.
                match ty_callee(tcx, func_ty) {
                    Callee::PtrOffset { .. } => {
                        self.visit_ptr_offset(&args[0], pl_ty);
                    }
                    Callee::SliceAsPtr { elem_ty, .. } => {
                        self.visit_slice_as_ptr(elem_ty, &args[0], pl_ty);
                    }

                    Callee::LocalDef { def_id, substs: _ } => {
                        // TODO: handle substs (if nonempty)
                        if let Some(lsig) = self.acx.gacx.fn_sigs.get(&def_id) {
                            self.enter_rvalue(|v| {
                                for (i, op) in args.iter().enumerate() {
                                    if let Some(&lty) = lsig.inputs.get(i) {
                                        v.enter_call_arg(i, |v| v.visit_operand(op, Some(lty)));
                                    } else {
                                        // This is a call to a variadic function, and we've gone
                                        // past the end of the declared arguments.
                                        // TODO: insert a cast to turn `op` back into its original
                                        // declared type (i.e. upcast the chosen reference type
                                        // back to a raw pointer)
                                        continue;
                                    }
                                }

                                if !pl_ty.label.is_none() {
                                    // Emit a cast.  The call returns a temporary, which the cast
                                    // is always allowed to move.
                                    v.emit_cast_lty_lty(lsig.output, pl_ty, true);
                                }
                            });
                        }
                    }

                    Callee::Memcpy => {
                        self.enter_rvalue(|v| {
                            // TODO: Only emit `MemcpySafe` if the rewritten argument types and
                            // pointees are suitable.  Specifically, the `src` and `dest` arguments
                            // must both be rewritten to safe references, their pointee types must
                            // be the same, and the pointee type must implement `Copy`.  If these
                            // conditions don't hold, leave the `memcpy` call intact and emit casts
                            // back to `void*` on the `dest` and `src` arguments.
                            let dest_lty = v.acx.type_of(&args[0]);
                            let dest_pointee = v.pointee_lty(dest_lty);
                            let src_lty = v.acx.type_of(&args[1]);
                            let src_pointee = v.pointee_lty(src_lty);
                            let common_pointee = dest_pointee.filter(|&x| Some(x) == src_pointee);
                            let pointee_lty = match common_pointee {
                                Some(x) => x,
                                // TODO: emit void* casts before bailing out, as described above
                                None => return,
                            };

                            let (_, elem_ty) = v.lty_to_rewritten_str(pointee_lty);
                            let dest_single = !v.perms[dest_lty.label]
                                .intersects(PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB);
                            let src_single = !v.perms[src_lty.label]
                                .intersects(PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB);
                            let dest_option =
                                !v.perms[dest_lty.label].contains(PermissionSet::NON_NULL);
                            let src_option =
                                !v.perms[src_lty.label].contains(PermissionSet::NON_NULL);
                            v.emit(RewriteKind::MemcpySafe {
                                elem_ty,
                                src_single,
                                src_option,
                                dest_single,
                                dest_option,
                            });

                            if !pl_ty.label.is_none()
                                && v.perms[pl_ty.label].intersects(PermissionSet::USED)
                            {
                                let dest_lty = v.acx.type_of(&args[0]);
                                // TODO: The result of `MemcpySafe` is always a slice, so this cast
                                // may be using an incorrect input type.  See the comment on the
                                // `MemcpySafe` case of `rewrite::expr::convert` for details.
                                v.emit_cast_lty_lty(dest_lty, pl_ty, true);
                            }
                        });
                    }

                    Callee::Memset => {
                        self.enter_rvalue(|v| {
                            // TODO: Only emit `MemsetSafe` if the rewritten argument type and
                            // pointee are suitable.  Specifically, the `dest` arguments must be
                            // rewritten to a safe reference type.  If these conditions don't hold,
                            // leave the `memset` call intact and emit casts back to `void*` on the
                            // `dest` argument.
                            let dest_lty = v.acx.type_of(&args[0]);
                            let dest_pointee = v.pointee_lty(dest_lty);
                            let pointee_lty = match dest_pointee {
                                Some(x) => x,
                                // TODO: emit void* cast before bailing out, as described above
                                None => return,
                            };

                            let (elem_ty, elem_ty_str) = v.lty_to_rewritten_str(pointee_lty);
                            let dest_single = !v.perms[dest_lty.label]
                                .intersects(PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB);

                            let zero_ty = match ZeroizeType::from_ty(tcx, elem_ty) {
                                Some(x) => x,
                                // TODO: emit void* cast before bailing out, as described above
                                None => return,
                            };

                            v.emit(RewriteKind::MemsetZeroize {
                                zero_ty,
                                elem_ty: elem_ty_str,
                                dest_single,
                            });

                            if !pl_ty.label.is_none()
                                && v.perms[pl_ty.label].intersects(PermissionSet::USED)
                            {
                                let dest_lty = v.acx.type_of(&args[0]);
                                v.emit_cast_lty_lty(dest_lty, pl_ty, true);
                            }
                        });
                    }

                    Callee::IsNull => {
                        self.enter_rvalue(|v| {
                            let arg_lty = v.acx.type_of(&args[0]);
                            if !v.flags[arg_lty.label].contains(FlagSet::FIXED) {
                                let arg_non_null =
                                    v.perms[arg_lty.label].contains(PermissionSet::NON_NULL);
                                if arg_non_null {
                                    v.emit(RewriteKind::IsNullToConstFalse);
                                } else {
                                    v.emit(RewriteKind::IsNullToIsNone);
                                }
                            }
                        });
                    }

                    Callee::Null { .. } => {
                        self.enter_rvalue(|v| {
                            if !v.flags[pl_ty.label].contains(FlagSet::FIXED) {
                                assert!(
                                    !v.perms[pl_ty.label].contains(PermissionSet::NON_NULL),
                                    "impossible: result of null() is a NON_NULL pointer?"
                                );
                                v.emit(RewriteKind::PtrNullToNone);
                            }
                        });
                    }

                    ref callee @ (Callee::Malloc | Callee::Calloc) => {
                        self.enter_rvalue(|v| {
                            let dest_lty = v.acx.type_of(destination);
                            let dest_pointee = v.pointee_lty(dest_lty);
                            let pointee_lty = match dest_pointee {
                                Some(x) => x,
                                // TODO: emit void* cast before bailing out
                                None => {
                                    trace!("{callee:?}: no pointee type for dest");
                                    return;
                                }
                            };

                            let (_, elem_ty) = v.lty_to_rewritten_str(pointee_lty);
                            let single = !v.perms[dest_lty.label]
                                .intersects(PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB);

                            let opt_zero_ty =
                                ZeroizeType::from_lty(&v.acx, v.perms, v.flags, pointee_lty);
                            let zero_ty = match opt_zero_ty {
                                Some(x) => x,
                                // TODO: emit void* cast before bailing out
                                None => {
                                    trace!(
                                        "{callee:?}: failed to compute ZeroizeType \
                                        for {pointee_lty:?}"
                                    );
                                    return;
                                }
                            };

                            let rw = match *callee {
                                Callee::Malloc => RewriteKind::MallocSafe {
                                    zero_ty,
                                    elem_ty,
                                    single,
                                },
                                Callee::Calloc => RewriteKind::CallocSafe {
                                    zero_ty,
                                    elem_ty,
                                    single,
                                },
                                _ => unreachable!(),
                            };
                            v.emit(rw);

                            // `MallocSafe` produces either `Box<T>` or `Box<[T]>`.  Emit a cast
                            // from that type to the required output type.
                            v.emit_cast_adjust_lty(
                                |desc| TypeDesc {
                                    own: Ownership::Box,
                                    qty: if single {
                                        Quantity::Single
                                    } else {
                                        Quantity::Slice
                                    },
                                    dyn_owned: false,
                                    option: false,
                                    pointee_ty: desc.pointee_ty,
                                },
                                dest_lty,
                            );
                        });
                    }

                    Callee::Free => {
                        self.enter_rvalue(|v| {
                            let src_lty = v.acx.type_of(&args[0]);
                            let src_pointee = v.pointee_lty(src_lty);
                            if src_pointee.is_none() {
                                // TODO: emit void* cast before bailing out
                                return;
                            }

                            let single = !v.perms[src_lty.label]
                                .intersects(PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB);

                            // Cast to either `Box<T>` or `Box<[T]>` (depending on `single`).  This
                            // ensures a panic occurs when `free`ing a pointer that no longer has
                            // ownership.
                            v.enter_call_arg(0, |v| {
                                v.emit_cast_lty_adjust(src_lty, |desc| TypeDesc {
                                    own: Ownership::Box,
                                    qty: if single {
                                        Quantity::Single
                                    } else {
                                        Quantity::Slice
                                    },
                                    dyn_owned: false,
                                    option: desc.option,
                                    pointee_ty: desc.pointee_ty,
                                });
                            });

                            v.emit(RewriteKind::FreeSafe { single });
                        });
                    }

                    ref callee @ Callee::Realloc => {
                        self.enter_rvalue(|v| {
                            let src_lty = v.acx.type_of(&args[0]);
                            let src_pointee = v.pointee_lty(src_lty);
                            let dest_lty = v.acx.type_of(destination);
                            let dest_pointee = v.pointee_lty(dest_lty);
                            let common_pointee = dest_pointee.filter(|&x| Some(x) == src_pointee);
                            let pointee_lty = match common_pointee {
                                Some(x) => x,
                                // TODO: emit void* cast before bailing out
                                None => {
                                    trace!(
                                        "{callee:?}: no common pointee type \
                                        between {src_pointee:?} and {dest_pointee:?}"
                                    );
                                    return;
                                }
                            };

                            let (elem_ty, elem_ty_str) = v.lty_to_rewritten_str(pointee_lty);
                            let dest_single = !v.perms[dest_lty.label]
                                .intersects(PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB);
                            let src_single = !v.perms[src_lty.label]
                                .intersects(PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB);

                            let zero_ty = match ZeroizeType::from_ty(tcx, elem_ty) {
                                Some(x) => x,
                                // TODO: emit void* cast before bailing out
                                None => {
                                    trace!(
                                        "{callee:?}: failed to compute ZeroizeType \
                                        for {elem_ty:?}"
                                    );
                                    return;
                                }
                            };

                            // Cast input to either `Box<T>` or `Box<[T]>`, as in `free`.
                            let mut option = false;
                            v.enter_call_arg(0, |v| {
                                v.emit_cast_lty_adjust(src_lty, |desc| {
                                    // `realloc(NULL, ...)` is explicitly allowed by the spec, so
                                    // we can't force an unwrap here by returning `option: false`.
                                    // Instead, we record the `option` flag as part of the rewrite
                                    // so the nullable case can be handled appropriately.
                                    option = desc.option;
                                    TypeDesc {
                                        own: Ownership::Box,
                                        qty: if src_single {
                                            Quantity::Single
                                        } else {
                                            Quantity::Slice
                                        },
                                        dyn_owned: false,
                                        option: desc.option,
                                        pointee_ty: desc.pointee_ty,
                                    }
                                });
                            });

                            v.emit(RewriteKind::ReallocSafe {
                                zero_ty,
                                elem_ty: elem_ty_str,
                                src_single,
                                dest_single,
                                option,
                            });

                            // Cast output from `Box<T>`/`Box<[T]>` to the target type, as in
                            // `malloc`.
                            v.emit_cast_adjust_lty(
                                |desc| {
                                    TypeDesc {
                                        own: Ownership::Box,
                                        qty: if dest_single {
                                            Quantity::Single
                                        } else {
                                            Quantity::Slice
                                        },
                                        dyn_owned: false,
                                        // We always return non-null from `realloc`.
                                        option: false,
                                        pointee_ty: desc.pointee_ty,
                                    }
                                },
                                dest_lty,
                            );
                        });
                    }

                    _ => {}
                }
            }
            TerminatorKind::Assert { .. } => {}
            TerminatorKind::Yield { .. } => {}
            TerminatorKind::GeneratorDrop => {}
            TerminatorKind::FalseEdge { .. } => {}
            TerminatorKind::FalseUnwind { .. } => {}
            TerminatorKind::InlineAsm { .. } => todo!("terminator {:?}", term),
        }
    }

    /// Visit an `Rvalue`.  If `expect_ty` is `Some`, also emit whatever casts are necessary to
    /// make the `Rvalue` produce a value of type `expect_ty`.
    fn visit_rvalue(&mut self, rv: &Rvalue<'tcx>, expect_ty: Option<LTy<'tcx>>) {
        debug!("mir_op::visit_rvalue: {:?}, expect {:?}", rv, expect_ty);
        match *rv {
            Rvalue::Use(ref op) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(op, expect_ty));
            }
            Rvalue::Repeat(ref op, _) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(op, None));
            }
            Rvalue::Ref(_rg, kind, pl) => {
                let mutbl = match kind {
                    BorrowKind::Mut { .. } => true,
                    BorrowKind::Shared | BorrowKind::Shallow | BorrowKind::Unique => false,
                };
                self.enter_rvalue_place(0, |v| {
                    v.visit_place(pl, PlaceAccess::from_bool(mutbl), RequireSinglePointer::No)
                });

                if let Some(expect_ty) = expect_ty {
                    if self.is_nullable(expect_ty.label) {
                        // Nullable (`Option`) output is expected, but `Ref` always produces a
                        // `NON_NULL` pointer.  Cast rvalue from `&T` to `Option<&T>` or similar.
                        self.emit(RewriteKind::OptionSome);
                    }
                }
            }
            Rvalue::ThreadLocalRef(_def_id) => {
                // TODO
            }
            Rvalue::AddressOf(mutbl, pl) => {
                self.enter_rvalue_place(0, |v| {
                    v.visit_place(pl, PlaceAccess::from_mutbl(mutbl), RequireSinglePointer::No)
                });
                if let Some(expect_ty) = expect_ty {
                    let desc = type_desc::perms_to_desc_with_pointee(
                        self.acx.tcx(),
                        self.acx.type_of(pl).ty,
                        expect_ty.ty,
                        self.perms[expect_ty.label],
                        self.flags[expect_ty.label],
                    );
                    match desc.own {
                        Ownership::Cell => self.emit(RewriteKind::RawToRef { mutbl: false }),
                        Ownership::Imm | Ownership::Mut => self.emit(RewriteKind::RawToRef {
                            mutbl: mutbl == Mutability::Mut,
                        }),
                        _ => (),
                    }
                    if desc.option {
                        self.emit(RewriteKind::OptionSome);
                    }
                }
            }
            Rvalue::Len(pl) => {
                self.enter_rvalue_place(0, |v| {
                    v.visit_place(pl, PlaceAccess::Imm, RequireSinglePointer::No)
                });
            }
            Rvalue::Cast(_kind, ref op, ty) => {
                if util::is_null_const_operand(op) && ty.is_unsafe_ptr() {
                    // Special case: convert `0 as *const T` to `None`.
                    if let Some(rv_lty) = expect_ty {
                        if self.is_nullable(rv_lty.label) {
                            self.emit(RewriteKind::ZeroAsPtrToNone);
                        }
                    }
                }

                self.enter_rvalue_operand(0, |v| v.visit_operand(op, None));
                if let Some(rv_lty) = expect_ty {
                    let op_lty = self.acx.type_of(op);
                    let op_pointee = self.pointee_lty(op_lty);
                    let rv_pointee = self.pointee_lty(rv_lty);
                    // TODO: Check `pointee_types` recursively to handle pointer-to-pointer cases.
                    // For example `op_pointee = *mut /*p1*/ c_void` and `rv_pointee = *mut /*p2*/
                    // c_void`, where `p1` and `p2` both have `pointee_types` entries of `u8`.
                    let common_pointee = op_pointee.filter(|&x| Some(x) == rv_pointee);
                    if let Some(pointee_lty) = common_pointee {
                        let op_desc = type_desc::perms_to_desc_with_pointee(
                            self.acx.tcx(),
                            pointee_lty.ty,
                            op_lty.ty,
                            self.perms[op_lty.label],
                            self.flags[op_lty.label],
                        );
                        let rv_desc = type_desc::perms_to_desc_with_pointee(
                            self.acx.tcx(),
                            pointee_lty.ty,
                            rv_lty.ty,
                            self.perms[rv_lty.label],
                            self.flags[rv_lty.label],
                        );
                        debug!("Cast with common pointee {:?}:\n  op_desc = {:?}\n  rv_desc = {:?}\n  matches? {}",
                            pointee_lty, op_desc, rv_desc, op_desc == rv_desc);
                        if op_desc == rv_desc {
                            // After rewriting, the input and output types of the cast will be
                            // identical.  This means we can delete the cast.
                            self.emit(RewriteKind::RemoveCast);
                        }
                    }
                }
            }
            Rvalue::BinaryOp(_bop, ref ops) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(&ops.0, None));
                self.enter_rvalue_operand(1, |v| v.visit_operand(&ops.1, None));
            }
            Rvalue::CheckedBinaryOp(_bop, ref ops) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(&ops.0, None));
                self.enter_rvalue_operand(1, |v| v.visit_operand(&ops.1, None));
            }
            Rvalue::NullaryOp(..) => {}
            Rvalue::UnaryOp(_uop, ref op) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(op, None));
            }
            Rvalue::Discriminant(pl) => {
                self.enter_rvalue_place(0, |v| {
                    v.visit_place(pl, PlaceAccess::Imm, RequireSinglePointer::No)
                });
            }
            Rvalue::Aggregate(ref _kind, ref ops) => {
                for (i, op) in ops.iter().enumerate() {
                    self.enter_rvalue_operand(i, |v| v.visit_operand(op, None));
                }
            }
            Rvalue::ShallowInitBox(ref op, _ty) => {
                self.enter_rvalue_operand(0, |v| v.visit_operand(op, None));
            }
            Rvalue::CopyForDeref(pl) => {
                self.enter_rvalue_place(0, |v| {
                    v.visit_place(pl, PlaceAccess::Imm, RequireSinglePointer::No)
                });
            }
        }
    }

    /// Visit an `Operand`.  If `expect_ty` is `Some`, also emit whatever casts are necessary to
    /// make the `Operand` produce a value of type `expect_ty`.
    fn visit_operand(&mut self, op: &Operand<'tcx>, expect_ty: Option<LTy<'tcx>>) {
        match *op {
            Operand::Copy(pl) | Operand::Move(pl) => {
                let pl_lty = self.acx.type_of(pl);
                // Moving out of a `DynOwned` pointer requires `Mut` access rather than `Move`.
                // TODO: this should probably check `desc.dyn_owned` rather than perms directly
                let access = if !pl_lty.label.is_none()
                    && self.perms[pl_lty.label].contains(PermissionSet::FREE)
                {
                    PlaceAccess::Mut
                } else {
                    PlaceAccess::Move
                };
                self.enter_operand_place(|v| v.visit_place(pl, access, RequireSinglePointer::Yes));

                if let Some(expect_ty) = expect_ty {
                    if !pl_lty.label.is_none() {
                        let cast_can_move = pl.is_local() && self.current_sub_loc_is_last_use();
                        self.emit_cast_lty_lty(pl_lty, expect_ty, cast_can_move);
                    }
                }
            }
            Operand::Constant(..) => {}
        }
    }

    /// Like [`Self::visit_operand`], but takes an expected `TypeDesc` instead of an expected `LTy`.
    fn visit_operand_desc(&mut self, op: &Operand<'tcx>, expect_desc: TypeDesc<'tcx>) {
        match *op {
            Operand::Copy(pl) | Operand::Move(pl) => {
                let pl_lty = self.acx.type_of(pl);
                // Moving out of a `DynOwned` pointer requires `Mut` access rather than `Move`.
                // TODO: this should probably check `desc.dyn_owned` rather than perms directly
                let access = if !pl_lty.label.is_none()
                    && self.perms[pl_lty.label].contains(PermissionSet::FREE)
                {
                    PlaceAccess::Mut
                } else {
                    PlaceAccess::Move
                };
                self.enter_operand_place(|v| v.visit_place(pl, access, RequireSinglePointer::Yes));

                if !pl_lty.label.is_none() {
                    self.emit_cast_lty_desc(pl_lty, expect_desc);
                }
            }
            Operand::Constant(..) => {}
        }
    }

    fn visit_place(
        &mut self,
        pl: Place<'tcx>,
        access: PlaceAccess,
        require_single_ptr: RequireSinglePointer,
    ) {
        let mut ltys = Vec::with_capacity(1 + pl.projection.len());
        ltys.push(self.acx.type_of(pl.local));
        for proj in pl.projection {
            let prev_lty = ltys.last().copied().unwrap();
            ltys.push(self.acx.projection_lty(prev_lty, &proj));
        }
        self.visit_place_ref(pl.as_ref(), &ltys, access, require_single_ptr);
    }

    /// Generate rewrites for a `Place` represented as a `PlaceRef`.  `proj_ltys` gives the `LTy`
    /// for the `Local` and after each projection.  `access` describes how the place is being used:
    /// immutably, mutably, or being moved out of.
    fn visit_place_ref(
        &mut self,
        pl: PlaceRef<'tcx>,
        proj_ltys: &[LTy<'tcx>],
        access: PlaceAccess,
        require_single_ptr: RequireSinglePointer,
    ) {
        let (&last_proj, rest) = match pl.projection.split_last() {
            Some(x) => x,
            None => return,
        };

        // TODO: downgrade Move to Imm if the new type is Copy

        debug_assert!(pl.projection.len() >= 1);
        // `LTy` of the base place, before the last projection.
        let base_lty = proj_ltys[pl.projection.len() - 1];
        // `LTy` resulting from applying `last_proj` to `base_lty`.
        let _proj_lty = proj_ltys[pl.projection.len()];

        let base_pl = PlaceRef {
            local: pl.local,
            projection: rest,
        };
        match last_proj {
            PlaceElem::Deref => {
                let cast_can_move = base_pl.is_local() && self.current_sub_loc_is_last_use();
                self.enter_place_deref_pointer(|v| {
                    v.visit_place_ref(base_pl, proj_ltys, access, RequireSinglePointer::Yes);
                    if !v.flags[base_lty.label].contains(FlagSet::FIXED) {
                        let desc = type_desc::perms_to_desc(
                            base_lty.ty,
                            v.perms[base_lty.label],
                            v.flags[base_lty.label],
                        );
                        // TODO: This logic is quite similar to the cast builder code and could
                        // probably be replaced with a call to `emit_cast_lty_adjust`.  But
                        // currently this tries to introduce casts on the borrow projection in
                        // `array.as_mut_ptr()`, which causes rewriting to fail.
                        if v.is_nullable(base_lty.label) {
                            // If the pointer type is non-copy, downgrade (borrow) before calling
                            // `unwrap()`.
                            if !desc.own.is_copy() {
                                v.emit(RewriteKind::OptionDowngrade {
                                    mutbl: access == PlaceAccess::Mut,
                                    kind: if desc.dyn_owned {
                                        OptionDowngradeKind::Borrow
                                    } else if cast_can_move {
                                        OptionDowngradeKind::MoveAndDeref
                                    } else {
                                        OptionDowngradeKind::Deref
                                    },
                                });
                            }
                            v.emit(RewriteKind::OptionUnwrap);
                            if desc.dyn_owned {
                                v.emit(RewriteKind::Deref);
                            }
                        }
                        if desc.dyn_owned {
                            // TODO: do we need a `MoveAndDeref` equivalent for `DynOwned`?
                            v.emit(RewriteKind::DynOwnedDowngrade {
                                mutbl: access == PlaceAccess::Mut,
                            });
                        }
                        if require_single_ptr.as_bool() && desc.qty != Quantity::Single {
                            v.emit(RewriteKind::SliceFirst {
                                mutbl: access == PlaceAccess::Mut,
                            });
                        }
                    }
                });
            }
            PlaceElem::Field(_idx, _ty) => {
                self.enter_place_field_base(|v| {
                    v.visit_place_ref(base_pl, proj_ltys, access, RequireSinglePointer::Yes)
                });
            }
            PlaceElem::Index(_) | PlaceElem::ConstantIndex { .. } | PlaceElem::Subslice { .. } => {
                self.enter_place_index_array(|v| {
                    v.visit_place_ref(base_pl, proj_ltys, access, RequireSinglePointer::Yes)
                });
            }
            PlaceElem::Downcast(_, _) => {}
        }
    }

    fn visit_ptr_offset(&mut self, op: &Operand<'tcx>, result_ty: LTy<'tcx>) {
        // Compute the expected type for the argument, and emit a cast if needed.
        let result_ptr = result_ty.label;
        let result_desc =
            type_desc::perms_to_desc(result_ty.ty, self.perms[result_ptr], self.flags[result_ptr]);

        let arg_expect_desc = TypeDesc {
            own: result_desc.own,
            qty: match result_desc.qty {
                Quantity::Single => Quantity::Slice,
                Quantity::Slice => Quantity::Slice,
                Quantity::OffsetPtr => Quantity::OffsetPtr,
                Quantity::Array => unreachable!("perms_to_desc should not return Quantity::Array"),
            },
            dyn_owned: result_desc.dyn_owned,
            option: result_desc.option,
            pointee_ty: result_desc.pointee_ty,
        };

        self.enter_rvalue(|v| {
            v.enter_call_arg(0, |v| v.visit_operand_desc(op, arg_expect_desc));

            // Emit `OffsetSlice` for the offset itself.
            let mutbl = matches!(result_desc.own, Ownership::Mut);
            if !result_desc.option {
                v.emit(RewriteKind::OffsetSlice { mutbl });
            } else {
                v.emit(RewriteKind::OptionMapOffsetSlice { mutbl });
            }

            // The `OffsetSlice` operation returns something of the same type as its input.
            // Afterward, we must cast the result to the `result_ty`/`result_desc`.
            v.emit_cast_desc_desc(arg_expect_desc, result_desc);
        });
    }

    fn visit_slice_as_ptr(&mut self, elem_ty: Ty<'tcx>, op: &Operand<'tcx>, result_lty: LTy<'tcx>) {
        let op_lty = self.acx.type_of(op);
        let op_ptr = op_lty.label;
        let result_ptr = result_lty.label;

        let op_desc = type_desc::perms_to_desc_with_pointee(
            self.acx.tcx(),
            elem_ty,
            op_lty.ty,
            self.perms[op_ptr],
            self.flags[op_ptr],
        );

        let result_desc = type_desc::perms_to_desc_with_pointee(
            self.acx.tcx(),
            elem_ty,
            result_lty.ty,
            self.perms[result_ptr],
            self.flags[result_ptr],
        );

        self.enter_rvalue(|v| {
            // Generate a cast of our own, replacing the `as_ptr` call.
            // TODO: leave the `as_ptr` in place if we can't produce a working cast
            v.emit(RewriteKind::RemoveAsPtr);
            v.emit_cast_desc_desc(op_desc, result_desc);
        });
    }

    fn emit(&mut self, rw: RewriteKind) {
        self.rewrites
            .entry(self.loc)
            .or_insert_with(Vec::new)
            .push(MirRewrite {
                kind: rw,
                sub_loc: self.sub_loc.clone(),
            });
    }

    fn emit_cast_desc_desc(&mut self, from: TypeDesc<'tcx>, to: TypeDesc<'tcx>) {
        let perms = self.perms;
        let flags = self.flags;
        let mut builder = CastBuilder::new(self.acx.tcx(), perms, flags, |rk| self.emit(rk));
        builder.build_cast_desc_desc(from, to);
    }

    fn emit_cast_lty_desc(&mut self, from_lty: LTy<'tcx>, to: TypeDesc<'tcx>) {
        let perms = self.perms;
        let flags = self.flags;
        let mut builder = CastBuilder::new(self.acx.tcx(), perms, flags, |rk| self.emit(rk));
        builder.build_cast_lty_desc(from_lty, to);
    }

    #[allow(dead_code)]
    fn emit_cast_desc_lty(&mut self, from: TypeDesc<'tcx>, to_lty: LTy<'tcx>) {
        let perms = self.perms;
        let flags = self.flags;
        let mut builder = CastBuilder::new(self.acx.tcx(), perms, flags, |rk| self.emit(rk));
        builder.build_cast_desc_lty(from, to_lty);
    }

    /// Emit a cast from `from_lty` to `to_lty` at the current `(loc, sub_loc)`.  `is_local` should
    /// be set when the thing being cast is a plain local with no projections; in that case, we may
    /// apply special handling if this is the last use of the local.
    fn emit_cast_lty_lty(&mut self, from_lty: LTy<'tcx>, to_lty: LTy<'tcx>, cast_can_move: bool) {
        let perms = self.perms;
        let flags = self.flags;
        let mut builder = CastBuilder::new(self.acx.tcx(), perms, flags, |rk| self.emit(rk))
            .can_move(cast_can_move);
        builder.build_cast_lty_lty(from_lty, to_lty);
    }

    fn emit_cast_lty_lty_or_borrow(
        &mut self,
        from_lty: LTy<'tcx>,
        to_lty: LTy<'tcx>,
        cast_can_move: bool,
    ) {
        let perms = self.perms;
        let flags = self.flags;
        let mut builder = CastBuilder::new(self.acx.tcx(), perms, flags, |rk| self.emit(rk))
            .can_move(cast_can_move)
            .borrow(true);
        builder.build_cast_lty_lty(from_lty, to_lty);
    }

    /// Cast `from_lty` to an adjusted version of itself.  If `from_desc` is the `TypeDesc`
    /// corresponding to `from_lty`, this emits a cast from `from_desc` to `to_adjust(from_desc)`.
    fn emit_cast_lty_adjust(
        &mut self,
        from_lty: LTy<'tcx>,
        to_adjust: impl FnOnce(TypeDesc<'tcx>) -> TypeDesc<'tcx>,
    ) {
        let perms = self.perms;
        let flags = self.flags;
        let mut builder = CastBuilder::new(self.acx.tcx(), perms, flags, |rk| self.emit(rk));
        builder.build_cast_lty_adjust(from_lty, to_adjust);
    }

    /// Cast an adjusted version of `to_lty` to `to_lty` itself.  If `to_desc` is the `TypeDesc`
    /// corresponding to `to_lty`, this emits a cast from `from_adjust(to_desc)` to `to_desc`.
    fn emit_cast_adjust_lty(
        &mut self,
        from_adjust: impl FnOnce(TypeDesc<'tcx>) -> TypeDesc<'tcx>,
        to_lty: LTy<'tcx>,
    ) {
        let perms = self.perms;
        let flags = self.flags;
        let mut builder = CastBuilder::new(self.acx.tcx(), perms, flags, |rk| self.emit(rk));
        builder.build_cast_adjust_lty(from_adjust, to_lty);
    }
}

impl ZeroizeType {
    fn from_ty<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Option<ZeroizeType> {
        Some(match *ty.kind() {
            TyKind::Int(_) | TyKind::Uint(_) => ZeroizeType::Int,
            TyKind::Bool => ZeroizeType::Bool,
            TyKind::Adt(adt_def, substs) => {
                if !adt_def.is_struct() {
                    return None;
                }
                let variant = adt_def.non_enum_variant();
                let mut fields = Vec::with_capacity(variant.fields.len());
                for field in &variant.fields {
                    let name = field.name.to_string();
                    let ty = field.ty(tcx, substs);
                    let zero = ZeroizeType::from_ty(tcx, ty)?;
                    fields.push((name, zero));
                }

                let name_printer = FmtPrinter::new(tcx, Namespace::ValueNS);
                let name = name_printer
                    .print_value_path(adt_def.did(), &[])
                    .unwrap()
                    .into_buffer();

                ZeroizeType::Struct(name, fields)
            }
            TyKind::Array(elem_ty, _) => {
                let elem_zero = ZeroizeType::from_ty(tcx, elem_ty)?;
                ZeroizeType::Array(Box::new(elem_zero))
            }
            _ => return None,
        })
    }

    fn from_lty<'tcx>(
        acx: &AnalysisCtxt<'_, 'tcx>,
        perms: &GlobalPointerTable<PermissionSet>,
        flags: &GlobalPointerTable<FlagSet>,
        lty: LTy<'tcx>,
    ) -> Option<ZeroizeType> {
        Some(match *lty.kind() {
            TyKind::Int(_) | TyKind::Uint(_) => ZeroizeType::Int,
            TyKind::Bool => ZeroizeType::Bool,
            TyKind::Adt(adt_def, substs) => {
                if !adt_def.is_struct() {
                    eprintln!("ZeroizeType::from_lty({lty:?}): not a struct");
                    return None;
                }
                let variant = adt_def.non_enum_variant();
                let mut fields = Vec::with_capacity(variant.fields.len());
                for (field_idx, field) in variant.fields.iter().enumerate() {
                    let name = field.name.to_string();
                    let ty = field.ty(acx.tcx(), substs);
                    let lty = acx.projection_lty(lty, &PlaceElem::Field(field_idx.into(), ty));
                    let zero = ZeroizeType::from_lty(acx, perms, flags, lty)?;
                    fields.push((name, zero));
                }

                let name_printer = FmtPrinter::new(acx.tcx(), Namespace::ValueNS);
                let name = name_printer
                    .print_value_path(adt_def.did(), &[])
                    .unwrap()
                    .into_buffer();

                ZeroizeType::Struct(name, fields)
            }
            TyKind::Array(_, _) => {
                let elem_zero = ZeroizeType::from_lty(acx, perms, flags, lty.args[0])?;
                ZeroizeType::Array(Box::new(elem_zero))
            }
            TyKind::Ref(..) | TyKind::RawPtr(..) => {
                if lty.label.is_none() {
                    eprintln!("ZeroizeType::from_lty({lty:?}): ptr has no label");
                    return None;
                }
                let ptr = lty.label;
                if flags[ptr].contains(FlagSet::FIXED) {
                    eprintln!("ZeroizeType::from_lty({lty:?}): ptr is FIXED");
                    return None;
                }
                let nullable = !perms[ptr].contains(PermissionSet::NON_NULL);
                if nullable {
                    ZeroizeType::Option
                } else {
                    eprintln!("ZeroizeType::from_lty({lty:?}): ptr is NON_NULL");
                    return None;
                }
            }
            _ => return None,
        })
    }
}

pub struct CastBuilder<'a, 'tcx, PT1, PT2, F> {
    tcx: TyCtxt<'tcx>,
    perms: &'a PT1,
    flags: &'a PT2,
    emit: F,
    /// If set, the cast builder is allowed to move the expression being cast.  For example, we
    /// might emit `ptr.map(|p| &mut *p).unwrap()` instead of `ptr.as_deref_mut().unwrap()` when
    /// `can_move` is set; the former moves out of `ptr` but avoids an additional borrow, making it
    /// suitable to be used as the result expression of a function.
    can_move: bool,
    /// If set, the cast builder will emit a downgrade/borrow operation even for no-op casts, if
    /// the thing being cast can't be moved (`!can_move`) and also can't be copied.
    borrow: bool,
}

impl<'a, 'tcx, PT1, PT2, F> CastBuilder<'a, 'tcx, PT1, PT2, F>
where
    PT1: Index<PointerId, Output = PermissionSet>,
    PT2: Index<PointerId, Output = FlagSet>,
    F: FnMut(RewriteKind),
{
    pub fn new(
        tcx: TyCtxt<'tcx>,
        perms: &'a PT1,
        flags: &'a PT2,
        emit: F,
    ) -> CastBuilder<'a, 'tcx, PT1, PT2, F> {
        CastBuilder {
            tcx,
            perms,
            flags,
            emit,
            can_move: false,
            borrow: false,
        }
    }

    pub fn can_move(mut self, can_move: bool) -> Self {
        self.can_move = can_move;
        self
    }

    pub fn borrow(mut self, borrow: bool) -> Self {
        self.borrow = borrow;
        self
    }

    pub fn build_cast_desc_desc(&mut self, from: TypeDesc<'tcx>, to: TypeDesc<'tcx>) {
        self.try_build_cast_desc_desc(from, to).unwrap()
    }

    /// Try to build a cast between `from` and `to`, emitting any intermediate rewrites that are
    /// necessary through the `self.emit` callback.
    ///
    /// Note that when cast building fails, this method may still call `self.emit` one or more
    /// times before returning `Err`.  The caller should be prepared to roll back the effects of
    /// any `self.emit` calls if the overall operation fails.
    pub fn try_build_cast_desc_desc(
        &mut self,
        from: TypeDesc<'tcx>,
        to: TypeDesc<'tcx>,
    ) -> Result<(), String> {
        let orig_from = from;
        let mut from = orig_from;

        // The `from` and `to` pointee types should only differ in their lifetimes.
        let from_pointee_erased = self.tcx.erase_regions(from.pointee_ty);
        let to_pointee_erased = self.tcx.erase_regions(to.pointee_ty);
        if from_pointee_erased != to_pointee_erased {
            return Err(format!(
                "pointee type mismatch: {from_pointee_erased:?} != {to_pointee_erased:?}"
            ));
        }
        // There might still be differences in lifetimes, which we don't care about here.
        // Overwriting `from.pointee_ty` allows the final `from == to` check to succeed below.
        from.pointee_ty = to.pointee_ty;

        if from == to {
            // We normally do nothing if the `from` and `to` types are the same.  However, in some
            // cases we need to introduce a downgrade to avoid moving the operand inappropriately.
            let can_reborrow = !from.option && !from.dyn_owned;
            let need_downgrade = !self.can_move && !from.own.is_copy() && !can_reborrow;
            if self.borrow && need_downgrade {
                let mutbl = match from.own {
                    Ownership::Raw | Ownership::Imm | Ownership::Cell => false,
                    Ownership::RawMut | Ownership::Mut => true,
                    // Can't downgrade in these cases.
                    Ownership::Rc | Ownership::Box => return Ok(()),
                };
                if from.option {
                    (self.emit)(RewriteKind::OptionDowngrade {
                        mutbl,
                        kind: if from.dyn_owned {
                            OptionDowngradeKind::Borrow
                        } else {
                            OptionDowngradeKind::Deref
                        },
                    });
                }
                if from.dyn_owned {
                    if from.option {
                        (self.emit)(RewriteKind::OptionMapBegin);
                        (self.emit)(RewriteKind::Deref);
                    }
                    (self.emit)(RewriteKind::DynOwnedDowngrade { mutbl });
                    if from.option {
                        (self.emit)(RewriteKind::OptionMapEnd);
                    }
                }
            }

            return Ok(());
        }

        // To downgrade and unwrap a non-`dyn_owned` pointer, we call `p.as_deref().unwrap()`,
        // which goes from `Option<Box<T>>` to `&T`.  To downgrade a `dyn_owned` pointer, we
        // instead do `*p.as_ref().unwrap()`, which goes from `Option<DynOwned<T>>` to
        // `DynOwned<T>`, with the latter being a read-only `Place` that can be further downgraded
        // to eventually reach `&T`.
        let mut deref_after_unwrap = false;
        if from.option && from.own != to.own {
            // Downgrade ownership before unwrapping the `Option` when possible.  This can avoid
            // moving/consuming the input.  For example, if the `from` type is `Option<Box<T>>` and
            // `to` is `&mut T`, we start by calling `p.as_deref_mut()`, which produces
            // `Option<&mut T>` without consuming `p`.
            if !from.own.is_copy() {
                // Note that all non-`Copy` ownership types are also safe.  We don't reach this
                // code when `from.own` is `Raw` or `RawMut`.
                match to.own {
                    Ownership::Raw | Ownership::Imm => {
                        (self.emit)(RewriteKind::OptionDowngrade {
                            mutbl: false,
                            kind: if from.dyn_owned {
                                OptionDowngradeKind::Borrow
                            } else if self.can_move {
                                OptionDowngradeKind::MoveAndDeref
                            } else {
                                OptionDowngradeKind::Deref
                            },
                        });
                        deref_after_unwrap = from.dyn_owned;
                        from.own = Ownership::Imm;
                    }
                    Ownership::RawMut | Ownership::Cell | Ownership::Mut => {
                        (self.emit)(RewriteKind::OptionDowngrade {
                            mutbl: true,
                            kind: if from.dyn_owned {
                                OptionDowngradeKind::Borrow
                            } else if self.can_move {
                                OptionDowngradeKind::MoveAndDeref
                            } else {
                                OptionDowngradeKind::Deref
                            },
                        });
                        deref_after_unwrap = from.dyn_owned;
                        from.own = Ownership::Mut;
                    }
                    Ownership::Rc if from.own == Ownership::Rc => {
                        // `p.clone()` allows using an `Option<Rc<T>>` without consuming the
                        // original.  However, `RewriteKind::Clone` is not yet implemented.
                        error!("Option<Rc> -> Option<Rc> clone rewrite NYI");
                    }
                    _ => {
                        // Remaining cases don't have a valid downgrade operation.  We leave them
                        // as is, and the `unwrap`/`map` operations below will consume the original
                        // value.  Some cases are also impossible to implement, like casting from
                        // `Rc` to `Box`, which will be caught when attempting the `qty`/`own`
                        // casts below.
                    }
                }
            }
        }

        let mut in_option_map = false;
        if from.option && !to.option {
            // Unwrap first, then perform remaining casts.
            (self.emit)(RewriteKind::OptionUnwrap);
            if deref_after_unwrap {
                (self.emit)(RewriteKind::Deref);
            }
            from.option = false;
        } else if from.option && to.option {
            trace!("try_build_cast_desc_desc: emit OptionMapBegin");
            if from.own != to.own {
                trace!("  own differs: {:?} != {:?}", from.own, to.own);
            }
            if from.qty != to.qty {
                trace!("  qty differs: {:?} != {:?}", from.qty, to.qty);
            }
            if from.pointee_ty != to.pointee_ty {
                trace!(
                    "  pointee_ty differs: {:?} != {:?}",
                    from.pointee_ty,
                    to.pointee_ty
                );
            }
            (self.emit)(RewriteKind::OptionMapBegin);
            if deref_after_unwrap {
                (self.emit)(RewriteKind::Deref);
            }
            from.option = false;
            in_option_map = true;
        }

        if from.dyn_owned {
            match to.own {
                Ownership::Raw | Ownership::Imm => {
                    (self.emit)(RewriteKind::DynOwnedDowngrade { mutbl: false });
                }
                Ownership::RawMut | Ownership::Cell | Ownership::Mut => {
                    (self.emit)(RewriteKind::DynOwnedDowngrade { mutbl: true });
                }
                Ownership::Rc | Ownership::Box => {
                    (self.emit)(RewriteKind::DynOwnedUnwrap);
                }
            }
            from.dyn_owned = false;
        }

        // Early `Ownership` casts.  We do certain casts here in hopes of reaching an `Ownership`
        // on which we can safely adjust `Quantity`.
        from.own = self.cast_ownership(from, to, true)?;

        // Safe casts that change `Quantity`.
        while from.qty != to.qty {
            // Mutability of `from`.  `None` here means that safe `Quantity` conversions aren't
            // possible given `from`'s `Ownership`.  For example, we can't convert `Box<[T]>` to
            // `Box<T>`.
            let opt_mutbl = match from.own {
                // Note that `Cell` + `Slice` is `&[Cell<T>]`, not `&Cell<[T]>`, so it can be
                // handled like any other `&[_]`.
                Ownership::Imm | Ownership::Cell => Some(false),
                Ownership::Mut => Some(true),
                _ => None,
            };
            match (from.qty, to.qty) {
                (Quantity::Array, _) => {
                    // `Array` goes only to `Slice` directly.  All other `Array` conversions go
                    // through `Slice` first.
                    return Err(format!("TODO: cast Array to {:?}", to.qty));
                    //from.qty = Quantity::Slice;
                }
                // Bidirectional conversions between `Slice` and `OffsetPtr`.
                (Quantity::Slice, Quantity::OffsetPtr) | (Quantity::OffsetPtr, Quantity::Slice) => {
                    // Currently a no-op, since `Slice` and `OffsetPtr` are identical.
                    from.qty = to.qty;
                }
                // `Slice` and `OffsetPtr` convert to `Single` the same way.
                // TODO: when converting to `Ownership::Raw`/`RawMut`, use `slice.as_ptr()` to
                // avoid panic on 0-length inputs
                (_, Quantity::Single) => {
                    let rw = match opt_mutbl {
                        Some(mutbl) => RewriteKind::SliceFirst { mutbl },
                        None => break,
                    };
                    (self.emit)(rw);
                    from.qty = Quantity::Single;
                }

                // Unsupported cases
                (Quantity::Single, _) => break,
                (_, Quantity::Array) => break,

                // Remaining cases are impossible, since `from.qty != to.qty`.
                (Quantity::Slice, Quantity::Slice) | (Quantity::OffsetPtr, Quantity::OffsetPtr) => {
                    unreachable!()
                }
            }
        }

        // Late `Ownership` casts.
        from.own = self.cast_ownership(from, to, false)?;

        if to.dyn_owned {
            (self.emit)(RewriteKind::DynOwnedWrap);
            from.dyn_owned = true;
        }

        if in_option_map {
            assert!(!from.option);
            assert!(to.option);
            (self.emit)(RewriteKind::OptionMapEnd);
            from.option = true;
        } else if !from.option && to.option {
            // Wrap at the end, after performing all other steps of the cast.
            (self.emit)(RewriteKind::OptionSome);
            from.option = true;
        }

        if from != to {
            return Err(format!(
                "unsupported cast kind: {:?} -> {:?} (original input: {:?})",
                from, to, orig_from
            ));
        }

        Ok(())
    }

    fn cast_ownership(
        &mut self,
        from: TypeDesc<'tcx>,
        to: TypeDesc<'tcx>,
        early: bool,
    ) -> Result<Ownership, String> {
        let mut from = from;
        while from.own != to.own {
            match self.cast_ownership_one_step(from, to, early)? {
                Some(new_own) => {
                    from.own = new_own;
                }
                None => break,
            }
        }
        Ok(from.own)
    }

    fn cast_ownership_one_step(
        &mut self,
        from: TypeDesc<'tcx>,
        to: TypeDesc<'tcx>,
        early: bool,
    ) -> Result<Option<Ownership>, String> {
        Ok(match from.own {
            Ownership::Box => match to.own {
                Ownership::Raw | Ownership::Imm => {
                    (self.emit)(RewriteKind::Reborrow { mutbl: false });
                    Some(Ownership::Imm)
                }
                Ownership::RawMut | Ownership::Mut | Ownership::Cell => {
                    (self.emit)(RewriteKind::Reborrow { mutbl: true });
                    Some(Ownership::Mut)
                }
                _ => None,
            },
            Ownership::Rc => match to.own {
                Ownership::Imm | Ownership::Raw | Ownership::RawMut => {
                    return Err("TODO: cast Rc to Imm".to_string());
                    //Some(Ownership::Imm)
                }
                _ => None,
            },
            Ownership::Mut => match to.own {
                Ownership::Imm | Ownership::Raw => {
                    (self.emit)(RewriteKind::Reborrow { mutbl: false });
                    Some(Ownership::Imm)
                }
                Ownership::Cell => {
                    (self.emit)(RewriteKind::CellFromMut);
                    Some(Ownership::Cell)
                }
                Ownership::RawMut if !early => {
                    (self.emit)(RewriteKind::CastRefToRaw { mutbl: true });
                    Some(Ownership::RawMut)
                }
                _ => None,
            },
            Ownership::Cell => match to.own {
                Ownership::RawMut | Ownership::Raw if !early => {
                    (self.emit)(RewriteKind::AsPtr);
                    Some(Ownership::RawMut)
                }
                _ => None,
            },
            Ownership::Imm => match to.own {
                Ownership::Raw | Ownership::RawMut if !early => {
                    (self.emit)(RewriteKind::CastRefToRaw { mutbl: false });
                    Some(Ownership::Raw)
                }
                _ => None,
            },
            Ownership::RawMut => match to.own {
                // For `RawMut` to `Imm`, we go through `Raw` instead of through `Mut` because
                // `&mut` adds more implicit constraints under the Rust memory model.
                Ownership::Raw | Ownership::Imm if !early => {
                    (self.emit)(RewriteKind::CastRawToRaw { to_mutbl: false });
                    Some(Ownership::Raw)
                }
                Ownership::Mut if !early => {
                    (self.emit)(RewriteKind::UnsafeCastRawToRef { mutbl: true });
                    Some(Ownership::Mut)
                }
                Ownership::Cell if !early => {
                    let printer = FmtPrinter::new(self.tcx, Namespace::TypeNS);
                    let ty = to.pointee_ty.print(printer).unwrap().into_buffer();
                    (self.emit)(RewriteKind::CastRawMutToCellPtr { ty });
                    (self.emit)(RewriteKind::UnsafeCastRawToRef { mutbl: false });
                    Some(Ownership::Cell)
                }
                _ => None,
            },
            Ownership::Raw => match to.own {
                Ownership::RawMut | Ownership::Mut if !early => {
                    (self.emit)(RewriteKind::CastRawToRaw { to_mutbl: true });
                    Some(Ownership::RawMut)
                }
                Ownership::Imm if !early => {
                    (self.emit)(RewriteKind::UnsafeCastRawToRef { mutbl: false });
                    Some(Ownership::Imm)
                }
                _ => None,
            },
        })
    }

    pub fn build_cast_lty_desc(&mut self, from_lty: LTy<'tcx>, to: TypeDesc<'tcx>) {
        let from = type_desc::perms_to_desc_with_pointee(
            self.tcx,
            to.pointee_ty,
            from_lty.ty,
            self.perms[from_lty.label],
            self.flags[from_lty.label],
        );
        self.build_cast_desc_desc(from, to);
    }

    pub fn build_cast_desc_lty(&mut self, from: TypeDesc<'tcx>, to_lty: LTy<'tcx>) {
        let to = type_desc::perms_to_desc_with_pointee(
            self.tcx,
            from.pointee_ty,
            to_lty.ty,
            self.perms[to_lty.label],
            self.flags[to_lty.label],
        );
        self.build_cast_desc_desc(from, to);
    }

    fn lty_to_desc(&self, lty: LTy<'tcx>) -> TypeDesc<'tcx> {
        type_desc::perms_to_desc(lty.ty, self.perms[lty.label], self.flags[lty.label])
    }

    pub fn build_cast_lty_lty(&mut self, from_lty: LTy<'tcx>, to_lty: LTy<'tcx>) {
        if from_lty.label.is_none() && to_lty.label.is_none() {
            // Input and output are both non-pointers.
            return;
        }

        let from_raw = matches!(from_lty.ty.kind(), TyKind::RawPtr(..));
        let to_raw = matches!(to_lty.ty.kind(), TyKind::RawPtr(..));
        if !from_raw && !to_raw {
            // TODO: hack to work around issues with already-safe code
            return;
        }

        let from_fixed = self.flags[from_lty.label].contains(FlagSet::FIXED);
        let to_fixed = self.flags[to_lty.label].contains(FlagSet::FIXED);

        match (from_fixed, to_fixed) {
            (false, false) => {
                let from = self.lty_to_desc(from_lty);
                let to = self.lty_to_desc(to_lty);
                self.build_cast_desc_desc(from, to);
            }

            (false, true) => {
                let from = self.lty_to_desc(from_lty);
                self.build_cast_desc_lty(from, to_lty);
            }

            (true, false) => {
                let to = self.lty_to_desc(to_lty);
                self.build_cast_lty_desc(from_lty, to);
            }

            (true, true) => {
                // No-op.  Both sides are `FIXED`, so we assume the existing code is already valid.
            }
        }
    }

    pub fn build_cast_lty_adjust(
        &mut self,
        from_lty: LTy<'tcx>,
        to_adjust: impl FnOnce(TypeDesc<'tcx>) -> TypeDesc<'tcx>,
    ) {
        if from_lty.label.is_none() {
            // Input and output are both non-pointers.
            return;
        }
        if !matches!(from_lty.ty.kind(), TyKind::RawPtr(..)) {
            // TODO: hack to work around issues with already-safe code
            return;
        }
        if self.flags[from_lty.label].contains(FlagSet::FIXED) {
            return;
        }

        let from = self.lty_to_desc(from_lty);
        let to = to_adjust(from);
        self.build_cast_desc_desc(from, to);
    }

    pub fn build_cast_adjust_lty(
        &mut self,
        from_adjust: impl FnOnce(TypeDesc<'tcx>) -> TypeDesc<'tcx>,
        to_lty: LTy<'tcx>,
    ) {
        if to_lty.label.is_none() {
            // Input and output are both non-pointers.
            return;
        }
        if !matches!(to_lty.ty.kind(), TyKind::RawPtr(..)) {
            // TODO: hack to work around issues with already-safe code
            return;
        }
        if self.flags[to_lty.label].contains(FlagSet::FIXED) {
            return;
        }

        let to = self.lty_to_desc(to_lty);
        let from = from_adjust(to);
        self.build_cast_desc_desc(from, to);
    }
}

trait IsLocal {
    fn is_local(&self) -> bool;
}

impl IsLocal for Place<'_> {
    fn is_local(&self) -> bool {
        self.projection.is_empty()
    }
}

impl IsLocal for PlaceRef<'_> {
    fn is_local(&self) -> bool {
        self.projection.is_empty()
    }
}

impl IsLocal for Operand<'_> {
    fn is_local(&self) -> bool {
        self.place().map_or(false, |pl| pl.is_local())
    }
}

impl IsLocal for Rvalue<'_> {
    fn is_local(&self) -> bool {
        match *self {
            Rvalue::Use(ref op) => op.is_local(),
            _ => false,
        }
    }
}

trait IsPlace {
    fn is_place(&self) -> bool;
}

impl IsPlace for Operand<'_> {
    fn is_place(&self) -> bool {
        self.place().is_some()
    }
}

impl IsPlace for Rvalue<'_> {
    fn is_place(&self) -> bool {
        match *self {
            Rvalue::Use(ref op) => op.is_place(),
            _ => false,
        }
    }
}

pub fn gen_mir_rewrites<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    pointee_types: PointerTable<PointeeTypes<'tcx>>,
    last_use: &LastUse,
    mir: &Body<'tcx>,
) -> (HashMap<Location, Vec<MirRewrite>>, DontRewriteFnReason) {
    let mut out = HashMap::new();

    let mut v = ExprRewriteVisitor::new(acx, asn, pointee_types, last_use, &mut out, mir);

    for (bb_id, bb) in mir.basic_blocks().iter_enumerated() {
        for (i, stmt) in bb.statements.iter().enumerate() {
            let loc = Location {
                block: bb_id,
                statement_index: i,
            };
            v.visit_statement(stmt, loc);
        }

        if let Some(ref term) = bb.terminator {
            let loc = Location {
                block: bb_id,
                statement_index: bb.statements.len(),
            };
            v.visit_terminator(term, loc);
        }
    }

    let errors = v.errors;
    (out, errors)
}
