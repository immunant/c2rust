use crate::context::{AnalysisCtxt, Assignment};
use crate::rewrite::Rewrite;
use log::*;
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

    if log_enabled!(log::Level::Debug) {
        use std::collections::{BTreeMap, BTreeSet};
        let mut rw_map1 = hir_rewrites
            .iter()
            .map(|&(sp, ref rw)| (sp, rw))
            .collect::<BTreeMap<_, _>>();
        let mut rw_map2 = hir_rewrites2
            .iter()
            .map(|&(sp, ref rw)| (sp, rw))
            .collect::<BTreeMap<_, _>>();
        let all_keys = rw_map1
            .keys()
            .cloned()
            .chain(rw_map2.keys().cloned())
            .collect::<BTreeSet<_>>();

        debug!("rewrite matching:");
        let mut mismatch_count = 0;
        for &sp in &all_keys {
            let rw1 = rw_map1.get(&sp);
            let rw2 = rw_map2.get(&sp);
            if rw1 == rw2 {
                debug!("  {:?}: {}", sp, rw1.unwrap());
            } else {
                match (rw1, rw2) {
                    (Some(rw1), Some(rw2)) => {
                        debug!("  OLD: {:?}: {}", sp, rw1);
                        debug!("  NEW: {:?}: {}", sp, rw2);
                    }
                    (Some(rw1), None) => {
                        debug!("  OLD ONLY: {:?}: {}", sp, rw1);
                    }
                    (None, Some(rw2)) => {
                        debug!("  NEW ONLY: {:?}: {}", sp, rw2);
                    }
                    (None, None) => unreachable!(),
                }
                mismatch_count += 1;
            }
        }
        debug!(
            "found {} mismatched / {} total rewrites",
            mismatch_count,
            all_keys.len()
        );
    }

    hir_rewrites
}
