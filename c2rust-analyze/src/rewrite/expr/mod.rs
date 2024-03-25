use crate::context::{AnalysisCtxt, Assignment};
use crate::pointee_type::PointeeTypes;
use crate::pointer_id::PointerTable;
use crate::rewrite::Rewrite;
use rustc_hir::def_id::DefId;
use rustc_hir::BodyId;
use rustc_middle::mir::Body;
use rustc_span::Span;

mod convert;
mod distribute;
mod hir_only_casts;
mod mir_op;
mod unlower;

// Helpers used by the shim builder.
pub use self::convert::convert_cast_rewrite;
pub use self::mir_op::CastBuilder;

pub fn gen_expr_rewrites<'tcx>(
    acx: &mut AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    pointee_types: PointerTable<PointeeTypes<'tcx>>,
    def_id: DefId,
    mir: &Body<'tcx>,
    hir_body_id: BodyId,
) -> Vec<(Span, Rewrite)> {
    let (mir_rewrites, errors) = mir_op::gen_mir_rewrites(acx, asn, pointee_types, mir);
    if !errors.is_empty() {
        acx.gacx.dont_rewrite_fns.add(def_id, errors);
    }
    let unlower_map = unlower::unlower(acx.tcx(), mir, hir_body_id);
    let rewrites_by_expr = distribute::distribute(acx.tcx(), unlower_map, mir_rewrites);
    let address_of_rewrites = hir_only_casts::remove_hir_only_casts(acx.tcx(), hir_body_id, |ex| {
        rewrites_by_expr.contains_key(&ex.hir_id)
    });
    let mut hir_rewrites = convert::convert_rewrites(acx.tcx(), hir_body_id, rewrites_by_expr);
    hir_rewrites.extend(address_of_rewrites);
    hir_rewrites
}
