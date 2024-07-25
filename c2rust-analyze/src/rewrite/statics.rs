use crate::context::Assignment;
use crate::context::{FlagSet, PermissionSet};
use crate::pointer_id::PointerId;
use crate::rewrite::Rewrite;
use rustc_hir::def_id::DefId;
use rustc_hir::{ItemKind, Mutability, Node};
use rustc_middle::ty::TyCtxt;
use rustc_span::Span;

/// For every static, if its write permission does not match its declared mutability, emit a rewrite
/// changing the declaration to match observed/analyzed usage.
pub fn gen_static_rewrites<'tcx>(
    tcx: TyCtxt<'tcx>,
    asn: &Assignment,
    def_id: DefId,
    ptr: PointerId,
) -> Option<(Span, Rewrite)> {
    // If the `addr_of_static` `PointerId` is `FIXED`, then we're forbidden from emitting this
    // rewrite.
    let flags = asn.flags[ptr];
    if flags.contains(FlagSet::FIXED) {
        return None;
    }

    // The map of statics and their ty + permissions tracks statics by DefId; map this to an Item
    // node to look at the static's spans and declared mutability.
    let item = if let Some(Node::Item(item)) = tcx.hir().get_if_local(def_id) {
        item
    } else {
        panic!("def id {:?} not found", def_id);
    };
    let is_mutable = match item.kind {
        ItemKind::Static(_ty, mutbl, _body_id) => mutbl == Mutability::Mut,
        _ => panic!("expected item {:?} to be a `static`", item),
    };
    let perms = asn.perms[ptr];
    let written_to = perms.contains(PermissionSet::WRITE);
    if written_to != is_mutable {
        let ident = tcx
            .opt_item_ident(def_id)
            .expect("def_id has no ident when trying to generate rewrite for static item");
        // Generate a span from beginning of ident to end of body.
        let span = ident.span.with_hi(item.span.hi());
        Some((
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
    } else {
        None
    }
}
