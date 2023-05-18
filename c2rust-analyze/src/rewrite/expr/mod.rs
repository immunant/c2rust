use crate::context::{AnalysisCtxt, Assignment};
use crate::rewrite::Rewrite;
use rustc_hir::BodyId;
use rustc_middle::mir::Body;
use rustc_span::Span;

mod hir_op;
mod mir_op;

mod convert;
mod distribute;
mod unlower;

pub fn gen_expr_rewrites<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    mir: &Body<'tcx>,
    hir_body_id: BodyId,
) -> Vec<(Span, Rewrite)> {
    let mir_rewrites = mir_op::gen_mir_rewrites(acx, asn, mir);
    let hir_rewrites = hir_op::gen_hir_rewrites(acx.tcx(), mir, hir_body_id, &mir_rewrites);

    let unlower_map = unlower::unlower(acx.tcx(), mir, hir_body_id);
    let mir_rewrites2 = distribute::distribute(acx.tcx(), unlower_map, mir_rewrites);
    let hir_rewrites2 = convert::convert_rewrites(acx.tcx(), hir_body_id, mir_rewrites2);

    hir_rewrites
}
