use crate::context::{AnalysisCtxt, Assignment};
use crate::rewrite::Rewrite;
use log::*;
use rustc_hir::BodyId;
use rustc_middle::mir::Body;
use rustc_span::Span;

mod convert;
mod distribute;
mod mir_op;
mod unlower;

pub fn gen_expr_rewrites<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    mir: &Body<'tcx>,
    hir_body_id: BodyId,
) -> Vec<(Span, Rewrite)> {
    let mir_rewrites = mir_op::gen_mir_rewrites(acx, asn, mir);
    let unlower_map = unlower::unlower(acx.tcx(), mir, hir_body_id);
    let rewrites_by_expr = distribute::distribute(acx.tcx(), unlower_map, mir_rewrites);
    let hir_rewrites = convert::convert_rewrites(acx.tcx(), hir_body_id, rewrites_by_expr);
    hir_rewrites
}
