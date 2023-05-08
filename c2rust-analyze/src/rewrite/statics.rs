use crate::context::PermissionSet;
use crate::rewrite::Rewrite;
use crate::{GlobalAnalysisCtxt, GlobalAssignment};
use rustc_hir::{ItemKind, Mutability, Node};
use rustc_span::Span;

/// For every static, if its write permission does not match its declared mutability, emit a rewrite
/// changing the declaration to match observed/analyzed usage.
pub fn gen_static_rewrites<'tcx>(
    gacx: &GlobalAnalysisCtxt<'tcx>,
    gasn: &GlobalAssignment,
) -> Vec<(Span, Rewrite)> {
    let mut hir_rewrites = Vec::new();
    for (did, &ptr) in gacx.addr_of_static.iter() {
        // The map of statics and their ty + permissions tracks statics by did; map this to an Item
        // node to look at the static's spans and declared mutability.
        let item = if let Some(Node::Item(item)) = gacx.tcx.hir().get_if_local(*did) {
            item
        } else {
            panic!("def id {:?} not found", did);
        };
        if let ItemKind::Static(_ty, mutbl, _body_id) = item.kind {
            let perms = gasn.perms[ptr];
            let written_to = perms.contains(PermissionSet::WRITE);
            let is_mutable = mutbl == Mutability::Mut;
            if written_to != is_mutable {
                let ident = gacx
                    .tcx
                    .opt_item_ident(*did)
                    .expect("did not find ident when trying to generate rewrite for static item");
                // Generate a span from beginning of ident to end of body.
                let span = ident.span.with_hi(item.span.hi());
                hir_rewrites.push((
                    item.span,
                    Rewrite::StaticMut(
                        if written_to {
                            Mutability::Mut
                        } else {
                            Mutability::Not
                        },
                        span,
                    ),
                ))
            }
        } else {
            panic!("expected item {:?} to be a `static`", item);
        }
    }

    hir_rewrites
}
