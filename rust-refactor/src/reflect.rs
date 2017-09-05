use rustc::hir;
use rustc::hir::def::Def;
use rustc::hir::def_id::DefId;
use rustc::hir::map::Node::*;
use rustc::ty::{self, TyCtxt};
use rustc::ty::item_path::{ItemPathBuffer, RootMode};
use syntax::ast::*;
use syntax::codemap::{CodeMap, Span, DUMMY_SP};
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::symbol::keywords;
use syntax::tokenstream::{TokenStream, ThinTokenStream};
use syntax::util::small_vector::SmallVector;

use make_ast::mk;
use util::IntoSymbol;


pub fn reflect_tcx_ty(tcx: TyCtxt, ty: ty::Ty) -> P<Ty> {
    use rustc::ty::TypeVariants::*;
    match ty.sty {
        TyBool => mk().ident_ty("bool"),
        TyChar => mk().ident_ty("char"),
        TyInt(ity) => mk().ident_ty(ity.ty_to_string()),
        TyUint(uty) => mk().ident_ty(uty.ty_to_string()),
        TyFloat(fty) => mk().ident_ty(fty.ty_to_string()),
        // TODO: handle substs
        TyAdt(def, substs) => mk().path_ty(reflect_path(tcx, def.did)),
        TyStr => mk().ident_ty("str"),
        TyArray(ty, len) => mk().array_ty(
            reflect_tcx_ty(tcx, ty),
            mk().lit_expr(mk().int_lit(len as u128, "usize"))),
        TySlice(ty) => mk().slice_ty(reflect_tcx_ty(tcx, ty)),
        TyRawPtr(mty) => mk().set_mutbl(mty.mutbl).ptr_ty(reflect_tcx_ty(tcx, mty.ty)),
        TyRef(_, mty) => mk().set_mutbl(mty.mutbl).ref_ty(reflect_tcx_ty(tcx, mty.ty)),
        TyFnDef(_, _) => mk().ident_ty("_"), // TODO
        TyFnPtr(_) => mk().ident_ty("_"), // TODO
        TyDynamic(_, _) => mk().ident_ty("_"), // TODO
        TyClosure(_, _) => mk().ident_ty("_"), // unsupported
        TyNever => mk().never_ty(),
        TyTuple(tys, _) => mk().tuple_ty(tys.iter().map(|&ty| reflect_tcx_ty(tcx, ty)).collect()),
        TyProjection(_) => mk().ident_ty("_"), // TODO
        TyAnon(_, _) => mk().ident_ty("_"), // TODO
        TyParam(param) => mk().ident_ty(param.name),
        TyInfer(_) => mk().infer_ty(),
        TyError => mk().ident_ty("_"), // unsupported
    }
}

pub fn reflect_path(tcx: TyCtxt, id: DefId) -> Path {
    let root = PathSegment {
        identifier: keywords::CrateRoot.ident(),
        span: DUMMY_SP,
        parameters: None,
    };
    let mut buf = ItemPathVec(RootMode::Local, vec![root]);
    tcx.push_item_path(&mut buf, id);

    let mut segs = buf.1;
    // If `id` refers to an `extern` item, there will be an entry in the path for the `extern`
    // block (`ItemKind::ForeignMod`), with an empty ident.  Filter it out.
    segs.retain(|seg| seg.identifier.name.as_str() != "");

    Path {
        span: DUMMY_SP,
        segments: segs,
    }
}

/// Wrapper around `reflect_path` that checks first to ensure its argument is the sort of def that
/// has a path.  `reflect_path` will panic if called on a def with no path.
pub fn can_reflect_path(hir_map: &hir::map::Map, id: NodeId) -> bool {
    let node = match hir_map.find(id) {
        Some(x) => x,
        None => return false,
    };
    match node {
        NodeItem(_) |
        NodeForeignItem(_) |
        NodeTraitItem(_) |
        NodeImplItem(_) |
        NodeVariant(_) |
        NodeField(_) |
        NodeStructCtor(_) => true,

        NodeExpr(_) |
        NodeStmt(_) |
        NodeTy(_) |
        NodeTraitRef(_) |
        NodeBinding(_) |
        NodePat(_) |
        NodeBlock(_) |
        NodeLocal(_) |
        NodeLifetime(_) |
        NodeTyParam(_) |
        NodeVisibility(_) => false,
    }
}


struct ItemPathVec(RootMode, Vec<PathSegment>);

impl ItemPathBuffer for ItemPathVec {
    fn root_mode(&self) -> &RootMode {
        &self.0
    }

    fn push(&mut self, text: &str) {
        self.1.push(PathSegment {
            identifier: Ident::with_empty_ctxt(text.into_symbol()),
            span: DUMMY_SP,
            parameters: None,
        });
    }
}
