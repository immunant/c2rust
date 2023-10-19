use crate::context::{AnalysisCtxt, Assignment};
use crate::rewrite::Rewrite;
use rustc_hir::BodyId;
use rustc_middle::mir::Body;
use rustc_span::Span;

mod convert;
mod distribute;
mod mir_op;
mod two_part_address_of;
mod unlower;

// Helpers used by the shim builder.
pub use self::convert::convert_cast_rewrite;
pub use self::mir_op::CastBuilder;

pub fn gen_expr_rewrites<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    mir: &Body<'tcx>,
    hir_body_id: BodyId,
) -> Vec<(Span, Rewrite)> {
    let mir_rewrites = mir_op::gen_mir_rewrites(acx, asn, mir);
    let unlower_map = unlower::unlower(acx.tcx(), mir, hir_body_id);
    let rewrites_by_expr = distribute::distribute(acx.tcx(), unlower_map, mir_rewrites);
    let address_of_rewrites =
        two_part_address_of::remove_two_part_address_of_casts(acx.tcx(), hir_body_id, |ex| {
            rewrites_by_expr.contains_key(&ex.hir_id)
        });
    let mut hir_rewrites = convert::convert_rewrites(acx.tcx(), hir_body_id, rewrites_by_expr);
    hir_rewrites.extend(address_of_rewrites);
    hir_rewrites
}
