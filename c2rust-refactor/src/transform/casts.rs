use rustc::ty::{self, TyKind, ParamEnv};
use syntax::ast::*;
use syntax::ptr::P;

use c2rust_ast_builder::mk;
use crate::command::{CommandState, Registry};
use crate::driver::Phase;
use crate::matcher::{MatchCtxt, mut_visit_match_with, replace_expr};
use crate::transform::Transform;
use crate::RefactorCtxt;

#[cfg(test)]
mod tests;

/// # `remove_redundant_casts` Command
///
/// Usage: `remove_redundant_casts`
///
/// Removes all casts of the form `$e as $t` where the expression already has the `$t` type,
/// and double casts like `$e as $t1 as $t2` where the inner cast is redundant.
pub struct RemoveRedundantCasts;

impl Transform for RemoveRedundantCasts {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let tcx = cx.ty_ctxt();
        let mut mcx = MatchCtxt::new(st, cx);
        let outer_pat = mcx.parse_expr("$oe:Expr as $ot:Ty");
        let inner_pat = mcx.parse_expr("$ie:Expr as $it:Ty");
        mut_visit_match_with(mcx, outer_pat, krate, |ast, mut mcx| {
            let oe = mcx.bindings.get::<_, P<Expr>>("$oe").unwrap();
            let oe_ty = cx.adjusted_node_type(oe.id);
            let oe_ty = tcx.normalize_erasing_regions(ParamEnv::empty(), oe_ty);

            let ot = mcx.bindings.get::<_, P<Ty>>("$ot").unwrap();
            let ot_ty = cx.adjusted_node_type(ot.id);
            let ot_ty = tcx.normalize_erasing_regions(ParamEnv::empty(), ot_ty);
            if oe_ty == ot_ty {
                *ast = oe.clone();
                return;
            }

            let oe = oe.clone(); // Un-borrow mcx
            let ot = ot.clone();
            if mcx.try_match(&*inner_pat, &oe).is_ok() {
                // Found a double cast
                let ie = mcx.bindings.get::<_, P<Expr>>("$ie").unwrap();
                let ie_ty = cx.adjusted_node_type(ie.id);
                let ie_ty = tcx.normalize_erasing_regions(ParamEnv::empty(), ie_ty);

                let it = mcx.bindings.get::<_, P<Ty>>("$it").unwrap();
                let it_ty = cx.adjusted_node_type(it.id);
                let it_ty = tcx.normalize_erasing_regions(ParamEnv::empty(), it_ty);
                assert!(it_ty != ot_ty);

                match check_double_cast(ie_ty.into(), it_ty.into(), ot_ty.into()) {
                    DoubleCastAction::RemoveBoth => {
                        *ast = ie.clone();
                    }
                    DoubleCastAction::RemoveInner => {
                        // Rewrite to `$ie as $ot`, removing the inner cast
                        *ast = mk().cast_expr(ie, ot);
                    }
                    DoubleCastAction::KeepBoth => { }
                }
                return;
            }
            // TODO: constant + cast, e.g., `0i32 as f32`
            // TODO: unary/binaryop op + cast, e.g., `(x as i32 + y as i32) as i8`
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

enum DoubleCastAction {
    RemoveBoth,
    RemoveInner,
    KeepBoth,
}

// Check and decide what to do for a double-cast, e.g., `$e as $ty1 as $ty2`
fn check_double_cast<'tcx>(
    e_ty: SimpleTy,
    t1_ty: SimpleTy,
    t2_ty: SimpleTy,
) -> DoubleCastAction {
    // WARNING!!! This set of operations is verified for soundness
    // using Z3. If you make any changes, please re-run the verifier using
    // `cargo test --package c2rust-refactor`
    use CastKind::*;
    let inner_cast = cast_kind(e_ty, t1_ty);
    let outer_cast = cast_kind(t1_ty, t2_ty);
    match (inner_cast, outer_cast) {
        // 2 consecutive sign flips or extend-truncate
        // back to the same original type
        (SameWidth, SameWidth) |
        (Extend(_), Truncate) if e_ty == t2_ty => DoubleCastAction::RemoveBoth,

        (Extend(_), Extend(s)) |
        (SameWidth, Extend(s)) |
        (SameWidth, FromPointer(s)) |
        (SameWidth, ToPointer(s)) if s == e_ty.is_signed() => DoubleCastAction::RemoveInner,

        (_, SameWidth) | (_, Truncate) => DoubleCastAction::RemoveInner,

        _ => DoubleCastAction::KeepBoth
    }
}

enum CastKind {
    Extend(bool),
    Truncate,
    SameWidth,
    FromPointer(bool),
    ToPointer(bool),
    Unknown,
}

fn cast_kind(from_ty: SimpleTy, to_ty: SimpleTy) -> CastKind {
    use SimpleTy::*;
    match (from_ty, to_ty) {
        (Int(fw, fs), Int(tw, _)) if fw < tw => CastKind::Extend(fs),
        (Int(fw, _), Int(tw, _)) if fw > tw => CastKind::Truncate,
        (Int(..), Int(..)) => CastKind::SameWidth,

        // Into size/pointer
        (Int(fw, fs), Size(_)) |
        (Int(fw, fs), Pointer) if fw <= 16 => CastKind::Extend(fs),
        (Int(fw, _), Size(_)) |
        (Int(fw, _), Pointer) if fw >= 64 => CastKind::Truncate,
        (Int(..), Size(ts)) => CastKind::ToPointer(ts),
        (Int(..), Pointer) => CastKind::ToPointer(false),

        // From size/pointer
        (Size(fs), Int(tw, _)) if tw >= 64 => CastKind::Extend(fs),
        (Pointer, Int(tw, _)) if tw >= 64 => CastKind::Extend(false),
        (Size(_), Int(tw, _)) |
        (Pointer, Int(tw, _)) if tw <= 16 => CastKind::Truncate,
        (Size(fs), Int(..)) => CastKind::FromPointer(fs),
        (Pointer, Int(..)) => CastKind::FromPointer(false),

        // Pointer-to-size and vice versa
        (Pointer, Pointer) |
        (Pointer, Size(_)) |
        (Size(_), Pointer) |
        (Size(_), Size(_)) => CastKind::SameWidth,

        (Float32, Float32) => CastKind::SameWidth,
        (Float32, Float64) => CastKind::Extend(true),
        (Float64, Float32) => CastKind::Truncate,
        (Float64, Float64) => CastKind::SameWidth,

        //// Any integer that fits into sign+mantissa is getting extended
        //// TODO: these require a Z3 bitwise simulation for the conversions
        //(Int(fw, fs), Float32) if fw <= 23 => CastKind::Extend(fs),
        //(Int(fw, fs), Float64) if fw <= 52 => CastKind::Extend(fs),
        //(Int(..), Float32) => CastKind::Truncate,
        //(Int(..), Float64) => CastKind::Truncate,

        (_, _) => CastKind::Unknown,
    }
}

// We need to lower `ty::Ty` into our own `SimpleTy`
// because the unit tests have no way of creating new `TyS` values
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum SimpleTy {
    Int(usize, bool),
    Size(bool),
    Float32,
    Float64,
    Pointer,
    Other,
}

impl SimpleTy {
    fn is_signed(&self) -> bool {
        match self {
            SimpleTy::Int(_, s) => *s,
            SimpleTy::Size(s) => *s,
            SimpleTy::Float32 => true,
            SimpleTy::Float64 => true,
            _ => false,
        }
    }
}

impl<'tcx> From<ty::Ty<'tcx>> for SimpleTy {
    fn from(ty: ty::Ty<'tcx>) -> Self {
        use SimpleTy::*;
        match ty.sty {
            TyKind::Int(IntTy::Isize) => Size(true),
            TyKind::Uint(UintTy::Usize) => Size(false),

            TyKind::Int(int_ty) => Int(int_ty.bit_width().unwrap(), true),
            TyKind::Uint(uint_ty) => Int(uint_ty.bit_width().unwrap(), false),

            TyKind::Float(FloatTy::F32) => Float32,
            TyKind::Float(FloatTy::F64) => Float64,

            TyKind::RawPtr(_) |
            TyKind::Ref(..) |
            TyKind::FnPtr(_) => Pointer,

            _ => Other,
        }
    }
}

/// # `convert_cast_as_ptr` Command
///
/// Usage: `convert_cast_as_ptr`
///
/// Converts all expressions like `$e as *const $t` (with mutable or const pointers)
/// where `$e` is a slice or array into `$e.as_ptr()` calls.
pub struct ConvertCastAsPtr;

impl Transform for ConvertCastAsPtr {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        replace_expr(st, cx, krate,
            "typed!($expr:Expr, &[$ty:Ty]) as *const $ty",
            "$expr.as_ptr()");
        replace_expr(st, cx, krate,
            "typed!($expr:Expr, &[$ty:Ty]) as *mut $ty",
            "$expr.as_mut_ptr()");
        replace_expr(st, cx, krate,
            "typed!($expr:Expr, &[$ty:Ty; $len]) as *const $ty",
            "$expr.as_ptr()");
        replace_expr(st, cx, krate,
            "typed!($expr:Expr, &[$ty:Ty; $len]) as *mut $ty",
            "$expr.as_mut_ptr()");
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("remove_redundant_casts", |_| mk(RemoveRedundantCasts));
    reg.register("convert_cast_as_ptr", |_| mk(ConvertCastAsPtr));
}
