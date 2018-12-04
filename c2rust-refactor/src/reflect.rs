//! Functions for building AST representations of higher-level values.
use rustc::hir;
use rustc::hir::map::Map as HirMap;
use rustc::hir::Node;
use rustc::hir::def_id::{DefId, LOCAL_CRATE};
use rustc::hir::map::definitions::DefPathData;
use rustc::mir::interpret::ConstValue;
use rustc::ty::{self, TyCtxt, GenericParamDefKind};
use rustc::ty::subst::Subst;
use syntax::ast::*;
use syntax::source_map::DUMMY_SP;
use syntax::ptr::P;
use syntax::symbol::keywords;
use rustc::middle::cstore::{ExternCrate, ExternCrateSource};
use c2rust_ast_builder::mk;
use command::{Registry, DriverCommand};
use driver::Phase;


/// Build an AST representing a `ty::Ty`.
pub fn reflect_tcx_ty<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                                      ty: ty::Ty<'tcx>) -> P<Ty> {
    reflect_tcx_ty_inner(tcx, ty, false)
}

fn reflect_tcx_ty_inner<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                                        ty: ty::Ty<'tcx>,
                                        infer_args: bool) -> P<Ty> {
    use rustc::ty::TyKind::*;
    match ty.sty {
        Bool => mk().ident_ty("bool"),
        Char => mk().ident_ty("char"),
        Int(ity) => mk().ident_ty(ity.ty_to_string()),
        Uint(uty) => mk().ident_ty(uty.ty_to_string()),
        Float(fty) => mk().ident_ty(fty.ty_to_string()),
        Adt(def, substs) => {
            if infer_args {
                let (qself, path) = reflect_def_path(tcx, def.did);
                mk().qpath_ty(qself, path)
            } else {
                let substs = substs.types().collect::<Vec<_>>();
                let (qself, path) = reflect_def_path_inner(tcx, def.did, Some(&substs));
                mk().qpath_ty(qself, path)
            }
        },
        Foreign(did) => {
            let (qself, path) = reflect_def_path_inner(tcx, did, None);
            mk().qpath_ty(qself, path)
        },
        Str => mk().ident_ty("str"),
        Array(ty, len) => mk().array_ty(
            reflect_tcx_ty(tcx, ty),
            match len.val {
                ConstValue::Unevaluated(did, _substs) => anon_const_to_expr(&tcx.hir, did),
                _ => mk().lit_expr(mk().int_lit(len.unwrap_usize(tcx) as u128, "usize")),
            },
        ),
        Slice(ty) => mk().slice_ty(reflect_tcx_ty(tcx, ty)),
        RawPtr(mty) => mk().set_mutbl(mty.mutbl).ptr_ty(reflect_tcx_ty(tcx, mty.ty)),
        Ref(_, ty, m) => mk().set_mutbl(m).ref_ty(reflect_tcx_ty(tcx, ty)),
        FnDef(_, _) => mk().infer_ty(), // unsupported (type cannot be named)
        FnPtr(_) => mk().infer_ty(), // TODO (fn(...) -> ...)
        Dynamic(_, _) => mk().infer_ty(), // TODO (dyn Trait)
        Closure(_, _) => mk().infer_ty(), // unsupported (type cannot be named)
        Generator(_, _, _) => mk().infer_ty(), // unsupported (type cannot be named)
        GeneratorWitness(_) => mk().infer_ty(), // unsupported (type cannot be named)
        Never => mk().never_ty(),
        Tuple(tys) => mk().tuple_ty(tys.iter().map(|&ty| reflect_tcx_ty(tcx, ty)).collect()),
        Projection(..) => mk().infer_ty(), // TODO
        UnnormalizedProjection(..) => mk().infer_ty(), // TODO
        Opaque(..) => mk().infer_ty(), // TODO (impl Trait)
        Param(param) => {
            if infer_args {
                mk().infer_ty()
            } else {
                mk().ident_ty(param.name)
            }
        },
        // `Bound` is "used only when preparing a trait query", so hopefully we never actually
        // encouter one.
        Bound(..) => mk().infer_ty(),   
        // No idea what `Placeholder` is, but it sounds like something rustc-internal.
        Placeholder(..) => mk().infer_ty(),
        Infer(_) => mk().infer_ty(),
        Error => mk().infer_ty(), // unsupported
    }
}

fn anon_const_to_expr(hir_map: &HirMap, def_id: DefId) -> P<Expr> {
    let node = hir_map.get_if_local(def_id).unwrap();
    let ac = expect!([node] Node::AnonConst(ac) => ac);
    let body_id = ac.body;
    let body = hir_map.krate().body(body_id);
    hir_expr_to_expr(&body.value)
}

fn hir_expr_to_expr(e: &hir::Expr) -> P<Expr> {
    use rustc::hir::ExprKind::*;
    match e.node {
        Binary(op, ref a, ref b) => {
            let op: BinOpKind = op.node.into();
            mk().binary_expr(op, hir_expr_to_expr(a), hir_expr_to_expr(b))
        },
        Unary(op, ref a) => {
            mk().unary_expr(op.as_str(), hir_expr_to_expr(a))
        },
        Lit(ref l) => mk().lit_expr((**l).clone()),
        ref k => panic!("unsupported variant in hir_expr_to_expr: {:?}", k),
    }
}

/// Build a path referring to a specific def.
pub fn reflect_def_path(tcx: TyCtxt, id: DefId) -> (Option<QSelf>, Path) {
    reflect_def_path_inner(tcx, id, None)
}

/// Build a path referring to a specific def.
fn reflect_def_path_inner<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                                          id: DefId,
                                          opt_substs: Option<&[ty::Ty<'tcx>]>)
                                          -> (Option<QSelf>, Path) {
    let mut segments = Vec::new();
    let mut qself = None;

    // Build the path in reverse order.  Push the name of the current def first, then the name of
    // its parent, and so on.  We flip the path around at the end.
    let mut id = id;
    let mut opt_substs = opt_substs;
    loop {
        let dk = tcx.def_key(id);
        match dk.disambiguated_data.data {
            DefPathData::CrateRoot => {
                if id.krate == LOCAL_CRATE {
                    segments.push(mk().path_segment(keywords::CrateRoot.ident()));
                    break;
                } else {
                    if let Some(ExternCrate { src: ExternCrateSource::Extern(def_id), .. }) = *tcx.extern_crate(id) {
                        // The name of the crate is the path to its `extern crate` item.
                        id = def_id;
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
                let num_params = gen.params.len();

                // Reflect the type.  If we have substs available, apply them to the type first.
                let ast_ty = if let Some(substs) = opt_substs {
                    let start = substs.len() - num_params;
                    let tcx_substs = substs[start..].iter().map(|&t| t.into())
                        .collect::<Vec<_>>();
                    let ty = ty.subst(tcx, &tcx_substs);
                    reflect_tcx_ty(tcx, ty)
                } else {
                    reflect_tcx_ty_inner(tcx, ty, true)
                };

                match ast_ty.node {
                    TyKind::Path(ref ty_qself, ref ty_path) => {
                        qself = ty_qself.clone();
                        segments.extend(ty_path.segments.iter().rev().cloned());
                    },
                    _ => {
                        qself = Some(QSelf {
                            ty: ast_ty.clone(),
                            path_span: DUMMY_SP,
                            position: 0,
                        });
                    },
                }

                break;
            },

            DefPathData::ValueNs(name) => {
                if segments.len() == 0 {
                    if name != "" {
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
            DefPathData::Module(name) |
            DefPathData::MacroDef(name) |
            DefPathData::EnumVariant(name) |
            DefPathData::Field(name) |
            DefPathData::GlobalMetaData(name) => {
                if name != "" {
                    segments.push(mk().path_segment(name));
                }
            },

            DefPathData::TypeParam(name) => {
                if name != "" {
                    segments.push(mk().path_segment(name));
                    break;
                }
            },

            DefPathData::Trait(_) |
            DefPathData::AssocTypeInTrait(_) |
            DefPathData::AssocTypeInImpl(_) |
            DefPathData::AssocExistentialInImpl(_) |
            DefPathData::ClosureExpr |
            DefPathData::LifetimeParam(_) |
            DefPathData::StructCtor |
            DefPathData::AnonConst |
            DefPathData::ImplTrait => {},
        }

        // Special logic for certain node kinds
        match dk.disambiguated_data.data {
            DefPathData::ValueNs(_) |
            DefPathData::TypeNs(_) => {
                let gen = tcx.generics_of(id);
                let num_params = gen.params.iter().filter(|x| match x.kind {
                    GenericParamDefKind::Lifetime{..} => false,
                    GenericParamDefKind::Type{..} => true,
                }).count();
                if let Some(substs) = opt_substs {
                    if substs.len() > 0 {
                        assert!(substs.len() >= num_params);
                        let start = substs.len() - num_params;
                        let tys = substs[start..].iter()
                            .map(|ty| reflect_tcx_ty(tcx, ty))
                            .collect::<Vec<_>>();
                        let abpd = mk().angle_bracketed_args(tys);
                        segments.last_mut().unwrap().args = abpd.into();
                        opt_substs = Some(&substs[..start]);
                    }
                }
            },

            DefPathData::StructCtor => {
                // The parent of the struct ctor in `visible_parent_map` is the parent of the
                // struct.  But we want to visit the struct first, so we can add its name.
                if let Some(parent_id) = tcx.parent_def_id(id) {
                    id = parent_id;
                    continue;
                } else {
                    break;
                }
            },

            _ => {},
        }

        let visible_parent_map = tcx.visible_parent_map(LOCAL_CRATE);
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
        Node::Item(_) |
        Node::ForeignItem(_) |
        Node::TraitItem(_) |
        Node::ImplItem(_) |
        Node::Variant(_) |
        Node::Field(_) |
        Node::Binding(_) |
        Node::Local(_) |
        Node::MacroDef(_) |
        Node::StructCtor(_) |
        Node::GenericParam(_) => true,

        Node::AnonConst(_) |
        Node::Expr(_) |
        Node::Stmt(_) |
        Node::PathSegment(_) |
        Node::Ty(_) |
        Node::TraitRef(_) |
        Node::Pat(_) |
        Node::Block(_) |
        Node::Lifetime(_) |
        Node::Visibility(_) |
        Node::Crate => false,
    }
}


/// # `test_reflect` Command
/// 
/// Test command - not intended for general use.
/// 
/// Usage: `test_reflect`
/// 
/// Applies path and ty reflection on every expr in the program.
fn register_test_reflect(reg: &mut Registry) {
    reg.register("test_reflect", |_args| {
        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            st.map_krate(|krate| {
                use api::*;
                use rustc::ty::TyKind;

                let krate = fold_nodes(krate, |e: P<Expr>| {
                    let ty = cx.node_type(e.id);

                    let e = if let TyKind::FnDef(def_id, ref substs) = ty.sty {
                        let substs = substs.types().collect::<Vec<_>>();
                        let (qself, path) = reflect_def_path_inner(
                            cx.ty_ctxt(), def_id, Some(&substs));
                        mk().qpath_expr(qself, path)
                    } else if let Some(def_id) = cx.try_resolve_expr(&e) {
                        let parent = cx.hir_map().get_parent(e.id);
                        let parent_body = cx.hir_map().body_owned_by(parent);
                        let tables = cx.ty_ctxt().body_tables(parent_body);
                        let hir_id = cx.hir_map().node_to_hir_id(e.id);
                        let substs = tables.node_substs(hir_id);
                        let substs = substs.types().collect::<Vec<_>>();
                        let (qself, path) = reflect_def_path_inner(
                            cx.ty_ctxt(), def_id, Some(&substs));
                        mk().qpath_expr(qself, path)
                    } else {
                        e
                    };

                    mk().type_expr(e, reflect_tcx_ty(cx.ty_ctxt(), ty))
                });

                krate
            });
        }))
    });
}

pub fn register_commands(reg: &mut Registry) {
    register_test_reflect(reg);
}
