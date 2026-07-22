//! Functions for building AST representations of higher-level values.
use crate::ast_builder::mk;
use rustc_ast::ptr::P;
use rustc_ast::*;
use rustc_hir as hir;
use rustc_hir::def::DefKind;
use rustc_hir::def_id::{DefId, LOCAL_CRATE};
use rustc_hir::definitions::DefPathData;
use rustc_hir::Node;
use rustc_middle::hir::map::Map as HirMap;
use rustc_middle::ty::{self, GenericParamDefKind, TyCtxt};
use rustc_span::source_map::DUMMY_SP;
use rustc_span::symbol::kw;
use rustc_type_ir::sty::TyKind as IrTyKind;

use std::collections::HashMap;

use crate::ast_manip::MutVisitNodes;
use crate::command::{DriverCommand, Registry};
use crate::context::RefactorCtxt;
use crate::driver::Phase;
use crate::expect;

pub struct Reflector<'a, 'tcx> {
    tcx: TyCtxt<'tcx>,

    /// Mapping from old DefId to new DefId for defs that have been replaced
    /// after types were resolved.
    def_mapping: Option<&'a HashMap<DefId, (Option<P<QSelf>>, Path)>>,
}

impl<'a, 'tcx> Reflector<'a, 'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            def_mapping: None,
        }
    }

    /// Create a Reflector that uses `def_mapping` to look up new paths for
    /// replaced defs.
    pub fn new_with_mapping(
        tcx: TyCtxt<'tcx>,
        def_mapping: &'a HashMap<DefId, (Option<P<QSelf>>, Path)>,
    ) -> Self {
        Self {
            tcx,
            def_mapping: Some(def_mapping),
        }
    }

    /// Build an AST representing a `ty::Ty`.
    pub fn reflect_ty(&self, ty: ty::Ty<'tcx>) -> P<Ty> {
        self.reflect_ty_inner(ty, false)
    }

    fn reflect_ty_inner(&self, ty: ty::Ty<'tcx>, infer_args: bool) -> P<Ty> {
        match ty.kind() {
            IrTyKind::Bool => mk().ident_ty("bool"),
            IrTyKind::Char => mk().ident_ty("char"),
            IrTyKind::Int(ity) => mk().ident_ty(ity.name_str()),
            IrTyKind::Uint(uty) => mk().ident_ty(uty.name_str()),
            IrTyKind::Float(fty) => mk().ident_ty(fty.name_str()),
            IrTyKind::Adt(def, substs) => {
                if infer_args {
                    let (qself, path) = self.reflect_def_path_inner(def.did(), None);
                    mk().qpath_ty(qself, path)
                } else {
                    let explicit_type_args = self
                        .tcx
                        .generics_of(def.did())
                        .own_substs_no_defaults(self.tcx, substs)
                        .iter()
                        .filter(|arg| matches!(arg.unpack(), ty::GenericArgKind::Type(_)))
                        .count();
                    let substs = substs.types().collect::<Vec<_>>();
                    let (qself, mut path) = self.reflect_def_path_inner(def.did(), Some(&substs));
                    if let Some(segment) = path.segments.last_mut() {
                        if explicit_type_args == 0 {
                            segment.args = None;
                        } else if let Some(args) = &mut segment.args {
                            let GenericArgs::AngleBracketed(args) = &mut **args else {
                                unreachable!("reflected type arguments must be angle bracketed")
                            };
                            args.args.truncate(explicit_type_args);
                        }
                    }
                    mk().qpath_ty(qself, path)
                }
            }
            IrTyKind::Foreign(did) => {
                let (qself, path) = self.reflect_def_path_inner(*did, None);
                mk().qpath_ty(qself, path)
            }
            IrTyKind::Str => mk().ident_ty("str"),
            IrTyKind::Array(ty, len) => mk().array_ty(
                self.reflect_ty(*ty),
                mk().lit_expr(mk().int_lit(
                    len.eval_target_usize(self.tcx, ty::ParamEnv::empty()) as u128,
                    "usize",
                )),
            ),
            IrTyKind::Slice(ty) => mk().slice_ty(self.reflect_ty(*ty)),
            IrTyKind::RawPtr(mty) => mk().set_mutbl(mty.mutbl).ptr_ty(self.reflect_ty(mty.ty)),
            IrTyKind::Ref(_, ty, m) => mk().set_mutbl(m).ref_ty(self.reflect_ty(*ty)),
            IrTyKind::FnDef(_, _) => mk().infer_ty(), // unsupported (type cannot be named)
            IrTyKind::FnPtr(poly_fn_sig) => {
                if let Some(fn_sig) = poly_fn_sig.no_bound_vars() {
                    let inputs = fn_sig
                        .inputs()
                        .iter()
                        .map(|input| mk().arg(self.reflect_ty(*input), mk().wild_pat()))
                        .collect();
                    let output = FnRetTy::Ty(self.reflect_ty(fn_sig.output()));
                    mk().unsafety(fn_sig.unsafety.to_string().as_str())
                        .extern_(fn_sig.abi)
                        .barefn_ty(mk().fn_decl(inputs, output))
                } else {
                    mk().infer_ty() // TODO higher-rank lifetimes (for<'a> fn(...) -> ...)
                }
            }
            IrTyKind::Dynamic(..) => mk().infer_ty(), // TODO (dyn Trait)
            IrTyKind::Closure(_, _) => mk().infer_ty(), // unsupported (type cannot be named)
            IrTyKind::Generator(_, _, _) => mk().infer_ty(), // unsupported (type cannot be named)
            IrTyKind::GeneratorWitness(_) => mk().infer_ty(), // unsupported (type cannot be named)
            IrTyKind::GeneratorWitnessMIR(..) => mk().infer_ty(), // unsupported (type cannot be named)
            IrTyKind::Never => mk().never_ty(),
            IrTyKind::Tuple(tys) => {
                mk().tuple_ty(tys.iter().map(|ty| self.reflect_ty(ty)).collect())
            }
            IrTyKind::Alias(..) => mk().infer_ty(), // TODO (projection and impl Trait)
            IrTyKind::Param(param) => {
                if infer_args {
                    mk().infer_ty()
                } else {
                    mk().ident_ty(param.name)
                }
            }
            // `Bound` is "used only when preparing a trait query", so hopefully we never actually
            // encounter one.
            IrTyKind::Bound(..) => mk().infer_ty(),
            // No idea what `Placeholder` is, but it sounds like something rustc-internal.
            IrTyKind::Placeholder(..) => mk().infer_ty(),
            IrTyKind::Infer(_) => mk().infer_ty(),
            IrTyKind::Error(_) => mk().infer_ty(), // unsupported
        }
    }

    /// Build a path referring to a specific def.
    fn reflect_def_path_inner(
        &self,
        id: DefId,
        opt_substs: Option<&[ty::Ty<'tcx>]>,
    ) -> (Option<P<QSelf>>, Path) {
        if let Some(mapping) = self.def_mapping {
            if let Some(new_path) = mapping.get(&id) {
                return new_path.clone();
            }
        }

        let mut segments = Vec::new();
        let mut qself = None;

        // Build the path in reverse order.  Push the name of the current def first, then the name of
        // its parent, and so on.  We flip the path around at the end.
        let mut id = id;
        let mut opt_substs = opt_substs;
        loop {
            let dk = self.tcx.def_key(id);
            match dk.disambiguated_data.data {
                DefPathData::CrateRoot => {
                    if id.krate == LOCAL_CRATE {
                        segments.push(mk().path_segment(kw::Crate));
                        break;
                    } else {
                        // Write `::crate_name` as the name of the crate. This is
                        // now correct in Rust 2018, regardless of whether we have
                        // an `extern crate`.
                        segments.push(mk().path_segment(self.tcx.crate_name(id.krate)));
                        segments.push(mk().path_segment(kw::PathRoot));
                        break;
                    }
                }

                DefPathData::Impl => {
                    let ty = self.tcx.type_of(id);
                    let gen = self.tcx.generics_of(id);
                    let num_params = gen.params.len();

                    // Reflect the type.  If we have substs available, apply them to the type first.
                    let ast_ty = if let Some(substs) = opt_substs {
                        let start = substs.len() - num_params;
                        let tcx_substs = substs[start..]
                            .iter()
                            .map(|&t| t.into())
                            .collect::<Vec<_>>();
                        let ty = ty.subst(self.tcx, &tcx_substs);
                        reflect_tcx_ty(self.tcx, ty)
                    } else {
                        self.reflect_ty_inner(ty.subst_identity(), true)
                    };

                    match ast_ty.kind {
                        TyKind::Path(ref ty_qself, ref ty_path) => {
                            qself = ty_qself.clone();
                            segments.extend(ty_path.segments.iter().rev().cloned());
                        }
                        _ => {
                            qself = Some(P(QSelf {
                                ty: ast_ty.clone(),
                                path_span: DUMMY_SP,
                                position: 0,
                            }));
                        }
                    }

                    break;
                }

                DefPathData::ValueNs(name) => {
                    if segments.is_empty() {
                        if name.as_str() != "" {
                            segments.push(mk().path_segment(name));
                        }
                    } else {
                        // This is a function, which the original DefId was inside of.  `::f::g` is not
                        // a valid path if `f` is a function.  Instead, we stop now, leaving `g` as the
                        // path.  This is not an absolute path, but it should be valid inside of `f`,
                        // which is the only place `g` is visible.
                        break;
                    }
                }

                DefPathData::TypeNs(name) => {
                    if name.as_str() != "" {
                        segments.push(mk().path_segment(name));
                    }
                }

                DefPathData::LifetimeNs(_)
                    | DefPathData::MacroNs(_)
                    // TODO: do we need to handle ForeignMod and Use?
                    | DefPathData::ForeignMod
                    | DefPathData::Use
                    | DefPathData::GlobalAsm
                    | DefPathData::ClosureExpr
                    | DefPathData::Ctor
                    | DefPathData::AnonConst
                    | DefPathData::ImplTrait
                    | DefPathData::ImplTraitAssocTy => {}
            }

            // Special logic for certain node kinds
            if let DefPathData::Ctor = dk.disambiguated_data.data {
                // The parent of the struct ctor in `visible_parent_map` is the parent of the
                // struct.  But we want to visit the struct first, so we can add its name.
                if let Some(parent_id) = self.tcx.opt_parent(id) {
                    id = parent_id;
                    continue;
                } else {
                    break;
                }
            }
            match self.tcx.def_kind(id) {
                // If we query for generics_of non-local defs, we may get a
                // panic if the def cannot be generic. This is a list of
                // DefKinds that can have generic type params.
                DefKind::Struct
                | DefKind::Union
                | DefKind::Enum
                | DefKind::Variant
                | DefKind::Trait
                | DefKind::OpaqueTy
                | DefKind::TyAlias
                | DefKind::ForeignTy
                | DefKind::TraitAlias
                | DefKind::AssocTy
                | DefKind::TyParam
                | DefKind::Fn
                | DefKind::AssocFn
                | DefKind::Ctor(..) => {
                    let gen = self.tcx.generics_of(id);
                    let num_params = gen
                        .params
                        .iter()
                        .filter(|x| match x.kind {
                            GenericParamDefKind::Lifetime { .. } => false,
                            GenericParamDefKind::Type { .. } => true,
                            GenericParamDefKind::Const { .. } => false,
                        })
                        .count();
                    if num_params != 0 {
                        if let Some(substs) = opt_substs {
                            assert!(substs.len() >= num_params);
                            let start = substs.len() - num_params;
                            let tys = substs[start..]
                                .iter()
                                .map(|ty| self.reflect_ty(*ty))
                                .collect::<Vec<_>>();
                            let abpd = mk().angle_bracketed_args(tys);
                            segments.last_mut().unwrap().args =
                                Some(P(GenericArgs::AngleBracketed(abpd)));
                            opt_substs = Some(&substs[..start]);
                        }
                    }
                }
                _ => {}
            }

            let visible_parent_map = self.tcx.visible_parent_map(());
            if let Some(&parent_id) = visible_parent_map.get(&id) {
                id = parent_id;
            } else if let Some(parent_id) = self.tcx.opt_parent(id) {
                id = parent_id;
            } else {
                break;
            }
        }

        segments.reverse();
        (qself, mk().path(segments))
    }
}

/// Build an AST representing a `ty::Ty`.
pub fn reflect_tcx_ty<'a, 'gcx, 'tcx>(tcx: TyCtxt<'tcx>, ty: ty::Ty<'tcx>) -> P<Ty> {
    Reflector::new(tcx).reflect_ty(ty)
}

pub fn anon_const_to_expr(hir_map: &HirMap, def_id: DefId) -> P<Expr> {
    let node = hir_map.get_if_local(def_id).unwrap();
    let ac = expect!([node] Node::AnonConst(ac) => ac);
    let body_id = ac.body;
    // TODO: This used to be hir_map.krate().body(body_id).
    // Is the replacement correct?
    let body = hir_map.body(body_id);
    hir_expr_to_expr(&body.value)
}

fn hir_expr_to_expr(e: &hir::Expr) -> P<Expr> {
    use rustc_hir::ExprKind::*;
    // The token::Lit inside an AST literal expression cannot carry a span on
    // this compiler, so the HIR literal's span is applied to the enclosing
    // expression instead. Binary and unary expressions are deliberately left
    // with the builder's dummy span, matching the previous behavior where
    // only the literal itself was spanned; propagating parent spans would
    // change the rewriter's text-recovery decisions and is a separate,
    // deliberate improvement.
    match e.kind {
        Binary(op, ref a, ref b) => {
            let op: BinOpKind = op.node.into();
            mk().binary_expr(op, hir_expr_to_expr(a), hir_expr_to_expr(b))
        }
        Unary(op, ref a) => mk().unary_expr(op.as_str(), hir_expr_to_expr(a)),
        Lit(ref l) => mk().span(l.span).lit_expr(l.clone()),
        ref k => panic!("unsupported variant in hir_expr_to_expr: {:?}", k),
    }
}

/// Build a path referring to a specific def.
pub fn reflect_def_path(tcx: TyCtxt, id: DefId) -> (Option<P<QSelf>>, Path) {
    Reflector::new(tcx).reflect_def_path_inner(id, None)
}

/// Wrapper around `reflect_path` that checks first to ensure its argument is the sort of def that
/// has a path.  `reflect_path` will panic if called on a def with no path.
pub fn can_reflect_path(cx: &RefactorCtxt, id: NodeId) -> bool {
    let node = match cx.hir_map().find(id) {
        Some(x) => x,
        None => return false,
    };
    match node {
        Node::Item(_)
        | Node::ForeignItem(_)
        | Node::TraitItem(_)
        | Node::ImplItem(_)
        | Node::Variant(_)
        | Node::Field(_)
        | Node::Local(_)
        | Node::Ctor(_)
        | Node::GenericParam(_) => true,

        Node::AnonConst(_)
        | Node::Expr(_)
        | Node::Stmt(_)
        | Node::PathSegment(_)
        | Node::Ty(_)
        // TODO: return true here?
        | Node::TypeBinding(_)
        | Node::TraitRef(_)
        | Node::Pat(_)
        | Node::Arm(_)
        | Node::Param(_)
        | Node::Block(_)
        | Node::Lifetime(_)
        // TODO: return true here?
        | Node::Infer(_)
        | Node::ExprField(_)
        | Node::PatField(_)
        | Node::Crate(_) => false,
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
            let reflector = Reflector::new(cx.ty_ctxt());
            st.map_krate(|krate| {
                MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
                    let ty = cx.node_type(e.id);

                    let new_expr = if let IrTyKind::FnDef(def_id, ref substs) = ty.kind() {
                        let substs = substs.types().collect::<Vec<_>>();
                        let (qself, path) =
                            reflector.reflect_def_path_inner(*def_id, Some(&substs));
                        mk().qpath_expr(qself, path)
                    } else if let Some(def_id) = cx.try_resolve_expr(&e) {
                        let parent = cx
                            .hir_map()
                            .get_parent_item(cx.hir_map().node_to_hir_id(e.id));
                        let parent_body = cx.hir_map().body_owned_by(parent);
                        let tables = cx.ty_ctxt().typeck_body(parent_body);
                        let hir_id = cx.hir_map().node_to_hir_id(e.id);
                        let substs = tables.node_substs(hir_id);
                        let substs = substs.types().collect::<Vec<_>>();
                        let (qself, path) = reflector.reflect_def_path_inner(def_id, Some(&substs));
                        mk().qpath_expr(qself, path)
                    } else {
                        e.clone()
                    };

                    *e = mk().type_expr(new_expr, reflect_tcx_ty(cx.ty_ctxt(), ty));
                });
            });
        }))
    });
}

pub fn register_commands(reg: &mut Registry) {
    register_test_reflect(reg);
}

#[cfg(test)]
mod tests {
    use super::*;
    use rustc_hir::def_id::CRATE_DEF_ID;
    use rustc_span::source_map::Spanned;
    use rustc_span::{BytePos, Span, SyntaxContext};

    /// The HIR-to-AST conversion must retain a literal's source location on
    /// the enclosing AST expression, because the literal token itself cannot
    /// carry a span on this compiler. No current refactor command reflects
    /// spanned anonymous constants end-to-end, so this is covered at the
    /// unit level.
    #[test]
    fn hir_literal_span_is_kept_on_ast_expr() {
        rustc_span::create_default_session_globals_then(|| {
            let lit_span = Span::new(BytePos(10), BytePos(12), SyntaxContext::root(), None);
            let hir_id = hir::HirId::make_owner(CRATE_DEF_ID);
            let hir_lit_expr = hir::Expr {
                hir_id,
                kind: hir::ExprKind::Lit(Spanned {
                    node: LitKind::Int(42, LitIntType::Unsuffixed),
                    span: lit_span,
                }),
                span: lit_span,
            };

            let ast_expr = hir_expr_to_expr(&hir_lit_expr);
            assert_eq!(ast_expr.span, lit_span);
            assert!(matches!(ast_expr.kind, ExprKind::Lit(..)));

            // A nested constant keeps the span on the literal leaf; the
            // parent expression deliberately stays unspanned (see
            // `hir_expr_to_expr`).
            let outer_span = Span::new(BytePos(9), BytePos(12), SyntaxContext::root(), None);
            let hir_neg_expr = hir::Expr {
                hir_id,
                kind: hir::ExprKind::Unary(hir::UnOp::Neg, &hir_lit_expr),
                span: outer_span,
            };

            let ast_expr = hir_expr_to_expr(&hir_neg_expr);
            assert_eq!(ast_expr.span, DUMMY_SP);
            let inner = expect!([&ast_expr.kind] ExprKind::Unary(_, e) => e);
            assert_eq!(inner.span, lit_span);
        });
    }
}
