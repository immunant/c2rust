use crate::rewrite::expr::mir_op::{self, MirRewrite};
use crate::rewrite::expr::unlower::{MirOrigin, MirOriginDesc, PreciseLoc};
use itertools::Itertools;
use log::*;
use rustc_hir::HirId;
use rustc_middle::mir::Location;
use rustc_middle::ty::TyCtxt;
use std::collections::{BTreeMap, HashMap};

struct RewriteInfo {
    rw: mir_op::RewriteKind,
    loc: PreciseLoc,
    desc: MirOriginDesc,
    priority: Priority,
}

/// This enum defines a sort order for [`RewriteInfo`], from innermost (applied earlier) to
/// outermost (applied later).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Priority {
    Eval,
    LoadResult,
}

/// Distributes MIR rewrites to HIR nodes.  This takes a list of MIR rewrites (from `mir_op`) and a
/// map from MIR location to `HirId` (from `unlower`) and produces a map from `HirId` to a list of
/// MIR rewrites.
///
/// Using the `x + f(y)` example from `unlower`:
///
/// ```text
/// bb0[5]: Terminator { source_info: ..., kind: _4 = f(move _5) -> [return: bb1, unwind: bb2] }
///   []: StoreIntoLocal, `f(y)`
///   [Rvalue]: Expr, `f(y)`
///   [Rvalue, CallArg(0)]: Expr, `y`
/// ```
///
/// A MIR rewrite on `bb0[5]` `[Rvalue, CallArg(0)]` would be attached to the HIR
/// `Expr` `y`, and a rewrite on `bb0[5]` `[Rvalue]` would be attached to `f(y)`.
/// A MIR rewrite on `bb0[5]` `[]` (i.e. on the call terminator itself) would
/// result in an error: this MIR assignment is a store to a temporary that was introduced during
/// HIR-to-MIR lowering, so there is no corresponding HIR assignment where such a rewrite could be
/// attached.
pub fn distribute(
    tcx: TyCtxt,
    unlower_map: BTreeMap<PreciseLoc, MirOrigin>,
    mir_rewrites: HashMap<Location, Vec<MirRewrite>>,
) -> HashMap<HirId, Vec<mir_op::RewriteKind>> {
    let mut info_map = HashMap::<HirId, Vec<RewriteInfo>>::new();

    for (loc, mir_rws) in mir_rewrites {
        for mir_rw in mir_rws {
            let key = PreciseLoc {
                loc,
                sub: mir_rw.sub_loc,
            };

            let origin = match unlower_map.get(&key) {
                Some(x) => x,
                None => {
                    error!("unlower_map has no origin for {:?}", key);
                    continue;
                }
            };

            let priority = match origin.desc {
                MirOriginDesc::Expr => Priority::Eval,
                MirOriginDesc::LoadFromTemp => Priority::LoadResult,
                _ => {
                    panic!(
                        "can't distribute rewrites onto {:?} origin {:?}\n\
                            key = {:?}\n\
                            mir_rw = {:?}",
                        origin.desc, origin, key, mir_rw.kind
                    );
                }
            };

            info_map
                .entry(origin.hir_id)
                .or_default()
                .push(RewriteInfo {
                    rw: mir_rw.kind,
                    loc: key,
                    desc: origin.desc,
                    priority,
                });
        }
    }

    // If a single `HirId` has rewrites from multiple different pieces of MIR at the same
    // `Priority`, it's ambiguous how to order those rewrites.  (`mir_rewrites` only establishes an
    // ordering between rewrites on the same `Location`.)  For now, we complain if we see this
    // ambiguity; in the future, we may need to add rules to resolve it in a particular way, such
    // as prioritizing one `SubLoc` over another.
    for (&hir_id, infos) in &mut info_map {
        infos.sort_by_key(|i| i.priority);
        let all_same_loc = infos
            .iter()
            .group_by(|i| i.priority)
            .into_iter()
            .all(|(_, group)| group.map(|i| (&i.loc, i.desc)).all_equal());
        if !all_same_loc {
            info!("rewrite info:");
            for i in infos {
                info!(
                    "  {:?}, {:?}, {:?}: {:?}",
                    i.loc.loc, i.loc.sub, i.desc, i.rw
                );
            }
            let ex = tcx.hir().expect_expr(hir_id);
            error!(
                "multiple distinct locations produced rewrites for {:?} {:?}",
                ex.span, ex,
            );
        }
    }

    // Discard parts of `RewriteInfo` that are only used for the ambiguity check, and return only
    // the `RewriteKind`s.
    info_map
        .into_iter()
        .map(|(k, vs)| (k, vs.into_iter().map(|v| v.rw).collect()))
        .collect()
}
