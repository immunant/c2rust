use crate::context::{AnalysisCtxt, Assignment};
use rustc_hir::BodyId;
use rustc_middle::mir::Body;
use rustc_span::Span;

mod apply;
mod hir_op;
mod mir_op;
mod span_index;

pub use self::hir_op::Rewrite;

pub fn gen_expr_rewrites<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    mir: &Body<'tcx>,
    hir_body_id: BodyId,
) -> Vec<(Span, Rewrite)> {
    let mir_rewrites = mir_op::gen_mir_rewrites(acx, asn, mir);
    let hir_rewrites = hir_op::gen_hir_rewrites(acx.tcx(), mir, hir_body_id, &mir_rewrites);
    hir_rewrites
}
