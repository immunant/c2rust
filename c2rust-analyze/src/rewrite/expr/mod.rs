use self::mir_op::MirRewrite;
use self::unlower::{PreciseLoc, UnlowerMap};
use crate::context::{AnalysisCtxt, Assignment};
use crate::last_use::LastUse;
use crate::pointee_type::PointeeTypes;
use crate::pointer_id::PointerTable;
use crate::rewrite::Rewrite;
use rustc_hir::def_id::DefId;
use rustc_hir::BodyId;
use rustc_middle::mir::{Body, Location};
use rustc_middle::ty::TyCtxt;
use rustc_span::Span;
use std::collections::HashMap;

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
    last_use: &LastUse,
    def_id: DefId,
    mir: &Body<'tcx>,
    hir_body_id: BodyId,
) -> Vec<(Span, Rewrite)> {
    let (mir_rewrites, errors) = mir_op::gen_mir_rewrites(acx, asn, pointee_types, last_use, mir);
    if !errors.is_empty() {
        acx.gacx.dont_rewrite_fns.add(def_id, errors);
    }
    let unlower_map = unlower::unlower(acx.tcx(), mir, hir_body_id);
    debug_print_unlower_map(acx.tcx(), mir, &unlower_map, &mir_rewrites);
    let rewrites_by_expr = distribute::distribute(acx.tcx(), unlower_map, mir_rewrites);

    eprintln!("distributed rewrites:");
    for (&hir_id, dist_rws) in &rewrites_by_expr {
        let ex = acx.tcx().hir().expect_expr(hir_id);
        eprintln!("  {:?}:", ex.span);
        for rw in dist_rws {
            eprintln!("    {rw:?}");
        }
    }

    let address_of_rewrites = hir_only_casts::remove_hir_only_casts(acx.tcx(), hir_body_id, |ex| {
        rewrites_by_expr.contains_key(&ex.hir_id)
    });
    let mut hir_rewrites = convert::convert_rewrites(acx.tcx(), hir_body_id, rewrites_by_expr);
    hir_rewrites.extend(address_of_rewrites);
    hir_rewrites
}

fn debug_print_unlower_map<'tcx>(
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    unlower_map: &UnlowerMap,
    mir_rewrites: &HashMap<Location, Vec<MirRewrite>>,
) {
    let print_for_loc = |loc| {
        let mut rewrites_by_subloc = HashMap::new();
        for rw in mir_rewrites.get(&loc).map_or(&[] as &[_], |x| x) {
            rewrites_by_subloc
                .entry(&rw.sub_loc)
                .or_insert(Vec::new())
                .push(&rw.kind);
        }

        if unlower_map.discard_rewrites_for(loc) {
            eprintln!("      DISCARD all rewrites for this location");
        }

        let mut found_at_least_one_origin = false;
        for (k, v) in unlower_map
            .origins_map()
            .range(&PreciseLoc { loc, sub: vec![] }..)
        {
            if k.loc != loc {
                break;
            }
            let sublocs = &k.sub;
            let ex = tcx.hir().expect_expr(v.hir_id);
            eprintln!("      {sublocs:?}: {:?}, {:?}", v.desc, ex.span);
            for rw_kind in rewrites_by_subloc.remove(&sublocs).unwrap_or_default() {
                eprintln!("        {rw_kind:?}");
            }
            found_at_least_one_origin = true;
        }

        if !found_at_least_one_origin {
            let span = mir
                .stmt_at(loc)
                .either(|s| s.source_info.span, |t| t.source_info.span);
            eprintln!("      {span:?} (no unlowering entries found)");
        }

        for (sublocs, rw_kinds) in rewrites_by_subloc {
            eprintln!("      {sublocs:?} (missing unlowering)");
            for rw_kind in rw_kinds {
                eprintln!("        {rw_kind:?}");
            }
        }
    };

    eprintln!("unlowering for {:?}:", mir.source);
    for (bb_id, bb) in mir.basic_blocks().iter_enumerated() {
        eprintln!("  block {bb_id:?}:");
        for (i, stmt) in bb.statements.iter().enumerate() {
            let loc = Location {
                block: bb_id,
                statement_index: i,
            };

            eprintln!("    {loc:?}: {stmt:?}");
            print_for_loc(loc);
        }

        {
            let term = bb.terminator();
            let loc = Location {
                block: bb_id,
                statement_index: bb.statements.len(),
            };

            eprintln!("    {loc:?}: {term:?}");
            print_for_loc(loc);
        }
    }
}
