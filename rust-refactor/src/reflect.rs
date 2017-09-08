//! Functions for building AST representations of higher-level values.
use rustc::hir;
use rustc::hir::def_id::{DefId, LOCAL_CRATE};
use rustc::hir::map::Node::*;
use rustc::hir::map::definitions::DefPathData;
use rustc::ty::{self, TyCtxt};
use rustc::ty::item_path::{ItemPathBuffer, RootMode};
use rustc::ty::subst::Subst;
use syntax::ast::*;
use syntax::codemap::DUMMY_SP;
use syntax::ptr::P;
use syntax::symbol::keywords;

use ast_manip::make_ast::mk;
use util::IntoSymbol;


/// Build an AST representing a `ty::Ty`.
pub fn reflect_tcx_ty<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                                      ty: ty::Ty<'tcx>) -> P<Ty> {
    reflect_tcx_ty_inner(tcx, ty, false)
}

fn reflect_tcx_ty_inner<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                                        ty: ty::Ty<'tcx>,
                                        infer_args: bool) -> P<Ty> {
    use rustc::ty::TypeVariants::*;
    match ty.sty {
        TyBool => mk().ident_ty("bool"),
        TyChar => mk().ident_ty("char"),
        TyInt(ity) => mk().ident_ty(ity.ty_to_string()),
        TyUint(uty) => mk().ident_ty(uty.ty_to_string()),
        TyFloat(fty) => mk().ident_ty(fty.ty_to_string()),
        TyAdt(def, substs) => {
            if infer_args {
                let (qself, path) = reflect_path(tcx, def.did);
                mk().qpath_ty(qself, path)
            } else {
                let substs = substs.types().collect::<Vec<_>>();
                let (qself, path) = reflect_path_inner(tcx, def.did, &substs);
                mk().qpath_ty(qself, path)
            }
        },
        TyStr => mk().ident_ty("str"),
        TyArray(ty, len) => mk().array_ty(
            reflect_tcx_ty(tcx, ty),
            mk().lit_expr(mk().int_lit(len as u128, "usize"))),
        TySlice(ty) => mk().slice_ty(reflect_tcx_ty(tcx, ty)),
        TyRawPtr(mty) => mk().set_mutbl(mty.mutbl).ptr_ty(reflect_tcx_ty(tcx, mty.ty)),
        TyRef(_, mty) => mk().set_mutbl(mty.mutbl).ref_ty(reflect_tcx_ty(tcx, mty.ty)),
        TyFnDef(_, _) => mk().ident_ty("_"), // unsupported (type cannot be named)
        TyFnPtr(_) => mk().ident_ty("_"), // TODO
        TyDynamic(_, _) => mk().ident_ty("_"), // TODO
        TyClosure(_, _) => mk().ident_ty("_"), // unsupported (type cannot be named)
        TyNever => mk().never_ty(),
        TyTuple(tys, _) => mk().tuple_ty(tys.iter().map(|&ty| reflect_tcx_ty(tcx, ty)).collect()),
        TyProjection(_) => mk().ident_ty("_"), // TODO
        TyAnon(_, _) => mk().ident_ty("_"), // TODO
        // (Note that, despite the name, `TyAnon` *can* be named - it's `impl SomeTrait`.)
        TyParam(param) => {
            if infer_args {
                mk().infer_ty()
            } else {
                mk().ident_ty(param.name)
            }
        },
        TyInfer(_) => mk().infer_ty(),
        TyError => mk().ident_ty("_"), // unsupported
    }
}

/// Build a path referring to a specific def.
pub fn reflect_path(tcx: TyCtxt, id: DefId) -> (Option<QSelf>, Path) {
    reflect_path_inner(tcx, id, &[])
}

/// Build a path referring to a specific def.
fn reflect_path_inner<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                                      id: DefId,
                                      substs: &[ty::Ty<'tcx>]) -> (Option<QSelf>, Path) {
    // Access to `sess` relies on the `TyCtxt -> GlobalCtxt` deref.  `GlobalCtxt` is private (and
    // undocumented), so this may break at some point.
    let sess = tcx.sess;

    let mut segments = Vec::new();
    let mut qself = None;

    // Build the path in reverse order.  Push the name of the current def first, then the name of
    // its parent, and so on.  We flip the path around at the end.
    let mut id = id;
    let mut substs = substs;
    loop {
        let dk = tcx.def_key(id);
        match dk.disambiguated_data.data {
            DefPathData::CrateRoot => {
                if id.krate == LOCAL_CRATE {
                    segments.push(mk().path_segment(keywords::CrateRoot.ident()));
                    break;
                } else {
                    if let Some(ref ec) = *tcx.extern_crate(id) {
                        // The name of the crate is the path to its `extern crate` item.
                        id = ec.def_id;
                        continue;
                    } else {
                        // Write `::crate_name` as the name of the crate.  This is incorrect, since
                        // there's no actual `extern crate crate_name` at top level (else we'd be
                        // in the previous case), but the resulting error should be obvious to the
                        // user.
                        segments.push(mk().path_segment(tcx.crate_name(id.krate)));
                        segments.push(mk().path_segment(keywords::CrateRoot.ident()));
                        break;
                    }
                }
            },

            // No idea what this is, but it doesn't have a name, so let's ignore it.
            DefPathData::Misc => {},

            DefPathData::Impl => {
                let ty = tcx.type_of(id);
                let gen = tcx.generics_of(id);
                let num_params = gen.types.len();

                // Reflect the type.  If we have substs available, we apply them to the type.
                let ast_ty = if substs.len() > 0 {
                    let start = substs.len() - num_params;
                    let tcx_substs = substs[start..].iter().map(|&t| t.into())
                        .collect::<Vec<_>>();
                    let ty = ty.subst(tcx, &tcx_substs);
                    reflect_tcx_ty(tcx, ty)
                } else {
                    reflect_tcx_ty_inner(tcx, ty, true)
                };

                // 
                match ast_ty.node {
                    TyKind::Path(ref ty_qself, ref ty_path) => {
                        qself = ty_qself.clone();
                        segments.extend(ty_path.segments.iter().rev().cloned());
                    },
                    _ => {
                        qself = Some(QSelf {
                            ty: ast_ty.clone(),
                            position: 0,
                        });
                    },
                }

                break;
            },

            DefPathData::ValueNs(name) => {
                if segments.len() == 0 {
                    if name.as_str().len() > 0 {
                        segments.push(mk().path_segment(name));
                    }
                } else {
                    // This is a function, which the original DefId was inside of.  `::f::g` is not
                    // a valid path if `f` is a function.  Instead, we stop now, leaving `g` as the
                    // path.  This is not an absolute path, but it should be valid inside of `f`,
                    // which is the only place `g` is visible.
                    break;
                }
            },

            DefPathData::TypeNs(name) |
            DefPathData::MacroDef(name) |
            DefPathData::TypeParam(name) |
            DefPathData::LifetimeDef(name) |
            DefPathData::EnumVariant(name) |
            DefPathData::Module(name) |
            DefPathData::Field(name) |
            DefPathData::Binding(name) |
            DefPathData::GlobalMetaData(name) => {
                if name.as_str().len() > 0 {
                    segments.push(mk().path_segment(name));
                }
            },

            DefPathData::ClosureExpr |
            DefPathData::StructCtor |
            DefPathData::Initializer |
            DefPathData::ImplTrait |
            DefPathData::Typeof => {},
        }

        match dk.disambiguated_data.data {
            DefPathData::ValueNs(_) |
            DefPathData::TypeNs(_) => {
                let gen = tcx.generics_of(id);
                let num_params = gen.types.len();
                if substs.len() > 0 {
                    assert!(substs.len() >= num_params);
                    let start = substs.len() - num_params;
                    let mut abpd = AngleBracketedParameterData {
                        //span: DUMMY_SP,
                        lifetimes: Vec::new(),
                        types: Vec::new(),
                        bindings: Vec::new(),
                    };
                    for &ty in &substs[start..] {
                        abpd.types.push(reflect_tcx_ty(tcx, ty));
                    }
                    segments.last_mut().unwrap().parameters = abpd.into();
                    substs = &substs[..start];
                }
            },
            _ => {},
        }


        let visible_parent_map = sess.cstore.visible_parent_map(sess);
        if let Some(&parent_id) = visible_parent_map.get(&id) {
            id = parent_id;
        } else if let Some(parent_id) = tcx.parent_def_id(id) {
            id = parent_id;
        } else {
            break;
        }
    }

    segments.reverse();
    (qself, mk().path(segments))
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
        NodeLocal(_) |
        NodePat(_) |
        NodeBlock(_) |
        NodeLifetime(_) |
        NodeTyParam(_) |
        NodeVisibility(_) => false,
    }
}


/// Helper struct for `reflect_path`.
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
