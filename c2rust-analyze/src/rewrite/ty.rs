//! Rewriting of types comes with one extra bit of complexity: sometimes we need to rewrite a
//! pointer type that's behind a type alias, such as `type MyPtr = *mut u8;`. To do this, we unfold
//! the alias, replacing `x: MyPtr` with `x: *mut u8`, and applying further rewrites from there. As
//! with the materialization of adjustments in expr rewriting, we try to apply this transformation
//! selectively, since we don't want to unfold all type aliases in the program.

use std::collections::HashMap;
use std::ops::Index;

use crate::borrowck::{OriginArg, OriginParam};
use crate::context::{
    AnalysisCtxt, Assignment, FlagSet, GlobalAnalysisCtxt, GlobalAssignment, LTy, PermissionSet,
};
use crate::labeled_ty::{LabeledTy, LabeledTyCtxt};
use crate::pointer_id::PointerId;
use crate::rewrite::Rewrite;
use crate::type_desc::{self, Ownership, Quantity};
use crate::AdtMetadataTable;
use hir::{GenericParamKind, ItemKind, LifetimeParamKind, Path, PathSegment, VariantData};
use log::warn;
use rustc_ast::ast;
use rustc_hir as hir;
use rustc_hir::def::{DefKind, Namespace, Res};
use rustc_hir::def_id::{DefId, LocalDefId};
use rustc_hir::intravisit;
use rustc_hir::{Mutability, Node};
use rustc_middle::hir::nested_filter;
use rustc_middle::mir::{self, Body, LocalDecl};
use rustc_middle::ty::print::{FmtPrinter, Print};
use rustc_middle::ty::{self, AdtDef, GenericArg, GenericArgKind, List, ReErased, TyCtxt};
use rustc_middle::ty::{PredicateKind, TyKind};
use rustc_span::Span;

use super::LifetimeName;

/// A label for use with `LabeledTy` to indicate what rewrites to apply at each position in a type.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
struct RewriteLabel<'a> {
    /// Rewrite a raw pointer, whose ownership and quantity have been inferred as indicated.
    ty_desc: Option<(Ownership, Quantity)>,
    /// If set, a child or other descendant of this type requires rewriting.
    descendant_has_rewrite: bool,
    /// A lifetime rewrite for a pointer or reference.
    lifetime: &'a [OriginArg<'a>],
}

type RwLTy<'tcx> = LabeledTy<'tcx, RewriteLabel<'tcx>>;

fn has_lifetime_rws(rw_lty: &RwLTy, adt_metadata: &AdtMetadataTable) -> bool {
    let has_pointer_lifetime = rw_lty
        .label
        .lifetime
        .iter()
        .any(|lt| matches!(lt, OriginArg::Hypothetical(..)));
    let has_adt_lifetime = match rw_lty.ty.kind() {
        TyKind::Adt(adt_def, _) => adt_metadata
            .table
            .get(&adt_def.did())
            .map(|adt| {
                adt.lifetime_params
                    .iter()
                    .any(|lt| matches!(lt, OriginParam::Hypothetical(..)))
            })
            .unwrap_or(false),
        _ => false,
    };
    has_adt_lifetime || has_pointer_lifetime
}

fn descendant_has_rewrite(args: &[RwLTy], adt_metadata: &AdtMetadataTable) -> bool {
    args.iter().any(|child| {
        child.label.ty_desc.is_some()
            || child.label.descendant_has_rewrite
            || has_lifetime_rws(child, adt_metadata)
    })
}

fn create_rewrite_label<'tcx>(
    pointer_lty: LTy<'tcx>,
    args: &[RwLTy<'tcx>],
    perms: &impl Index<PointerId, Output = PermissionSet>,
    flags: &impl Index<PointerId, Output = FlagSet>,
    lifetime: &'tcx [OriginArg<'tcx>],
    adt_metadata: &AdtMetadataTable,
) -> RewriteLabel<'tcx> {
    let ty_desc = if pointer_lty.label.is_none() {
        None
    } else {
        let perms = perms[pointer_lty.label];
        let flags = flags[pointer_lty.label];
        if flags.contains(FlagSet::FIXED) {
            None
        } else {
            // TODO: if the `Ownership` and `Quantity` exactly match `lty.ty`, then `ty_desc`
            // can be `None` (no rewriting required).  This might let us avoid inlining a type
            // alias for some pointers where no actual improvement was possible.
            let desc = type_desc::perms_to_desc(pointer_lty.ty, perms, flags);
            Some((desc.own, desc.qty))
        }
    };

    RewriteLabel {
        ty_desc,
        descendant_has_rewrite: descendant_has_rewrite(args, adt_metadata),
        lifetime,
    }
}

fn relabel_rewrites<'tcx, P, F>(
    perms: &P,
    flags: &F,
    lcx: LabeledTyCtxt<'tcx, RewriteLabel<'tcx>>,
    lty: LTy<'tcx>,
    gacx: &GlobalAnalysisCtxt<'tcx>,
) -> RwLTy<'tcx>
where
    P: Index<PointerId, Output = PermissionSet>,
    F: Index<PointerId, Output = FlagSet>,
{
    lcx.relabel_with_args(lty, &mut |pointer_lty, args| {
        // FIXME: get function lifetime parameters and pass them to this
        create_rewrite_label(pointer_lty, args, perms, flags, &[], &gacx.adt_metadata)
    })
}

// Gets the generic type arguments of an HIR type.
fn hir_generic_ty_args<'tcx>(ty: &hir::Ty<'tcx>) -> Option<Vec<&'tcx hir::Ty<'tcx>>> {
    let args = match ty.kind {
        hir::TyKind::Path(hir::QPath::Resolved(
            _,
            Path {
                segments: [.., segment @ PathSegment { .. }],
                ..
            },
        )) => Some(segment.args()),
        _ => None,
    };

    args.map(|args| {
        args.args
            .iter()
            .filter_map(|arg| match arg {
                hir::GenericArg::Type(ty) => Some(ty),
                _ => None,
            })
            .collect()
    })
}

/// Extract arguments from `hir_ty` if it corresponds to the tcx type `ty`.  If the two types
/// differ in shape (for example, if `hir_ty` is `_` or a type alias), then the result is `None`.
/// If they have the same shape, the result is a list of `hir::Ty`s of arguments, where "argument"
/// is defined the same way as for the `args` field of `LabeledTy`.
fn deconstruct_hir_ty<'a, 'tcx>(
    ty: ty::Ty<'tcx>,
    hir_ty: &'a hir::Ty<'tcx>,
) -> Option<Vec<&'a hir::Ty<'tcx>>> {
    /// Match a specific primitive type against a `hir::Ty`.  Returns `Some([])` on success (since
    /// primitive types have no arguments) or `None` on failure.
    fn do_prim<'a, 'tcx>(
        expect_prim: hir::PrimTy,
        hir_ty: &hir::Ty<'tcx>,
    ) -> Option<Vec<&'a hir::Ty<'tcx>>> {
        let path = match hir_ty.kind {
            hir::TyKind::Path(hir::QPath::Resolved(_, path)) => path,
            _ => return None,
        };
        let prim = match path.res {
            Res::PrimTy(prim) => prim,
            _ => return None,
        };
        if prim != expect_prim {
            return None;
        }
        Some(Vec::new())
    }

    match (ty.kind(), &hir_ty.kind) {
        // Types with no arguments
        (&ty::TyKind::Bool, _) => do_prim(hir::PrimTy::Bool, hir_ty),
        (&ty::TyKind::Char, _) => do_prim(hir::PrimTy::Char, hir_ty),
        (&ty::TyKind::Int(ity), _) => do_prim(hir::PrimTy::Int(ity.convert()), hir_ty),
        (&ty::TyKind::Uint(uty), _) => do_prim(hir::PrimTy::Uint(uty.convert()), hir_ty),
        (&ty::TyKind::Float(fty), _) => do_prim(hir::PrimTy::Float(fty.convert()), hir_ty),
        (&ty::TyKind::Str, _) => do_prim(hir::PrimTy::Str, hir_ty),

        // Types with arguments
        (&ty::TyKind::Array(_, _), &hir::TyKind::Array(arg_ty, _)) => Some(vec![arg_ty]),
        (&ty::TyKind::Slice(_), &hir::TyKind::Slice(arg_ty)) => Some(vec![arg_ty]),
        (&ty::TyKind::RawPtr(tm), &hir::TyKind::Ptr(ref hir_mt)) => {
            if hir_mt.mutbl == tm.mutbl.convert() {
                Some(vec![hir_mt.ty])
            } else {
                None
            }
        }
        (&ty::TyKind::Ref(_, _, mutbl), &hir::TyKind::Rptr(_, ref hir_mt)) => {
            if hir_mt.mutbl == mutbl.convert() {
                Some(vec![hir_mt.ty])
            } else {
                None
            }
        }
        (&ty::TyKind::Tuple(tys), &hir::TyKind::Tup(hir_tys)) => {
            if tys.len() == hir_tys.len() {
                Some(hir_tys.iter().collect())
            } else {
                None
            }
        }

        (&ty::TyKind::Adt(adt_def, substs), &hir::TyKind::Path(..)) => {
            hir_generic_ty_args(hir_ty).map(|type_args| {
                if type_args.len() < substs.types().count() {
                    // this situation occurs when there are hidden type arguments
                    // such as the allocator `std::alloc::Global` type argument in `Vec`
                    eprintln!("warning: extra MIR type argument for {adt_def:?}:");
                    for mir_arg in substs.types().into_iter().skip(type_args.len()) {
                        eprintln!("\t{:?}", mir_arg)
                    }
                } else if type_args.len() != substs.types().count() {
                    panic!("mismatched number of type arguments for {adt_def:?} and {hir_ty:?}")
                }
                type_args
            })
        }
        (tk, hir_tk) => {
            eprintln!("deconstruct_hir_ty: {tk:?} -- {hir_tk:?} not supported");
            None
        }
    }
}

/// Orphan-rule-compatible replacement for `From`/`Into`.
trait Convert<T> {
    fn convert(self) -> T;
}

impl Convert<ast::IntTy> for ty::IntTy {
    fn convert(self) -> ast::IntTy {
        match self {
            ty::IntTy::Isize => ast::IntTy::Isize,
            ty::IntTy::I8 => ast::IntTy::I8,
            ty::IntTy::I16 => ast::IntTy::I16,
            ty::IntTy::I32 => ast::IntTy::I32,
            ty::IntTy::I64 => ast::IntTy::I64,
            ty::IntTy::I128 => ast::IntTy::I128,
        }
    }
}

impl Convert<ast::UintTy> for ty::UintTy {
    fn convert(self) -> ast::UintTy {
        match self {
            ty::UintTy::Usize => ast::UintTy::Usize,
            ty::UintTy::U8 => ast::UintTy::U8,
            ty::UintTy::U16 => ast::UintTy::U16,
            ty::UintTy::U32 => ast::UintTy::U32,
            ty::UintTy::U64 => ast::UintTy::U64,
            ty::UintTy::U128 => ast::UintTy::U128,
        }
    }
}

impl Convert<ast::FloatTy> for ty::FloatTy {
    fn convert(self) -> ast::FloatTy {
        match self {
            ty::FloatTy::F32 => ast::FloatTy::F32,
            ty::FloatTy::F64 => ast::FloatTy::F64,
        }
    }
}

impl Convert<hir::Mutability> for mir::Mutability {
    fn convert(self) -> hir::Mutability {
        match self {
            mir::Mutability::Mut => hir::Mutability::Mut,
            mir::Mutability::Not => hir::Mutability::Not,
        }
    }
}

fn mk_cell<'tcx>(tcx: TyCtxt<'tcx>, ty: ty::Ty<'tcx>) -> ty::Ty<'tcx> {
    let core_crate = tcx
        .crates(())
        .iter()
        .cloned()
        .find(|&krate| tcx.crate_name(krate).as_str() == "core")
        .expect("failed to find crate `core`");

    let cell_mod_child = tcx
        .module_children(core_crate.as_def_id())
        .iter()
        .find(|child| child.ident.as_str() == "cell")
        .expect("failed to find module `core::cell`");
    let cell_mod_did = match cell_mod_child.res {
        Res::Def(DefKind::Mod, did) => did,
        ref r => panic!("unexpected resolution {:?} for `core::cell`", r),
    };

    let cell_struct_child = tcx
        .module_children(cell_mod_did)
        .iter()
        .find(|child| child.ident.as_str() == "Cell")
        .expect("failed to find struct `core::cell::Cell`");
    let cell_struct_did = match cell_struct_child.res {
        Res::Def(DefKind::Struct, did) => did,
        ref r => panic!("unexpected resolution {:?} for `core::cell::Cell`", r),
    };

    let cell_adt = tcx.adt_def(cell_struct_did);
    let substs = tcx.mk_substs([GenericArg::from(ty)].into_iter());
    tcx.mk_adt(cell_adt, substs)
}

/// Produce a `Ty` reflecting the rewrites indicated by the labels in `rw_lty`.
fn mk_rewritten_ty<'tcx>(
    lcx: LabeledTyCtxt<'tcx, RewriteLabel<'tcx>>,
    rw_lty: RwLTy<'tcx>,
) -> ty::Ty<'tcx> {
    let tcx = *lcx;
    lcx.rewrite_unlabeled(rw_lty, &mut |ty, args, label| {
        let (own, qty) = match label.ty_desc {
            Some(x) => x,
            None => return ty,
        };

        // The `ty` should be a pointer; the sole argument is the pointee type.
        assert_eq!(args.len(), 1);
        let mut ty = args[0];

        if own == Ownership::Cell {
            ty = mk_cell(tcx, ty);
        }

        ty = match qty {
            Quantity::Single => ty,
            Quantity::Slice => tcx.mk_slice(ty),
            // TODO: This should generate `OffsetPtr<T>` rather than `&[T]`, but `OffsetPtr` is NYI
            Quantity::OffsetPtr => tcx.mk_slice(ty),
            Quantity::Array => panic!("can't mk_rewritten_ty with Quantity::Array"),
        };

        ty = match own {
            Ownership::Raw => tcx.mk_imm_ptr(ty),
            Ownership::RawMut => tcx.mk_mut_ptr(ty),
            Ownership::Imm => tcx.mk_imm_ref(tcx.mk_region(ReErased), ty),
            Ownership::Cell => tcx.mk_imm_ref(tcx.mk_region(ReErased), ty),
            Ownership::Mut => tcx.mk_mut_ref(tcx.mk_region(ReErased), ty),
            Ownership::Rc => todo!(),
            Ownership::Box => tcx.mk_box(ty),
        };

        ty
    })
}

struct HirTyVisitor<'a, 'tcx> {
    asn: &'a Assignment<'a>,
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    rw_lcx: LabeledTyCtxt<'tcx, RewriteLabel<'tcx>>,
    mir: &'a Body<'tcx>,
    hir_rewrites: Vec<(Span, Rewrite)>,
    hir_span_to_mir_local: HashMap<Span, rustc_middle::mir::Local>,
}

fn adt_ty_rw<S>(
    adt_def: &AdtDef,
    lifetime_params: &[OriginArg],
    substs: &List<GenericArg>,
) -> Rewrite<S> {
    let lifetime_names = lifetime_params
        .iter()
        .map(|p| Rewrite::PrintTy(format!("{p:?}")));
    let other_param_names = substs.iter().filter_map(|p| match p.unpack() {
        GenericArgKind::Lifetime(..) => None,
        _ => Some(Rewrite::PrintTy(format!("{p:?}"))),
    });

    Rewrite::TyCtor(
        format!("{adt_def:?}"),
        lifetime_names.chain(other_param_names).collect(),
    )
}

/// Generate rewrites on `hir_ty` according to its labeled representation `rw_lty`.
fn rewrite_ty<'tcx>(
    rw_lcx: LabeledTyCtxt<'tcx, RewriteLabel<'tcx>>,
    hir_rewrites: &mut Vec<(Span, Rewrite)>,
    rw_lty: RwLTy<'tcx>,
    hir_ty: &hir::Ty<'tcx>,
) {
    if !rw_lty.ty.is_adt() && rw_lty.label.ty_desc.is_none() && !rw_lty.label.descendant_has_rewrite
    {
        // No rewrites here or in any descendant of this HIR node.
        return;
    }

    let hir_args = match deconstruct_hir_ty(rw_lty.ty, hir_ty) {
        Some(x) => x,
        None => {
            // `hir_ty` doesn't have the expected structure (for example, we expected a type
            // like `*mut T`, but it's actually an alias `MyPtr`), so we can't rewrite inside
            // it.  Instead, we discard it completely and pretty-print `rw_lty` (with rewrites
            // applied).
            let ty = mk_rewritten_ty(rw_lcx, rw_lty);
            let printer = FmtPrinter::new(*rw_lcx, Namespace::TypeNS);
            let s = ty.print(printer).unwrap().into_buffer();
            hir_rewrites.push((hir_ty.span, Rewrite::PrintTy(s)));
            return;
        }
    };

    if let Some((own, qty)) = rw_lty.label.ty_desc {
        assert_eq!(hir_args.len(), 1);

        let mut rw = Rewrite::Sub(0, hir_args[0].span);

        if own == Ownership::Cell {
            rw = Rewrite::TyCtor("core::cell::Cell".into(), vec![rw]);
        }

        rw = match qty {
            Quantity::Single => rw,
            Quantity::Slice => Rewrite::TySlice(Box::new(rw)),
            // TODO: This should generate `OffsetPtr<T>` rather than `&[T]`, but `OffsetPtr` is
            // NYI
            Quantity::OffsetPtr => Rewrite::TySlice(Box::new(rw)),
            Quantity::Array => panic!("can't rewrite to Quantity::Array"),
        };

        let lifetime_type = match rw_lty.label.lifetime {
            [lifetime] => LifetimeName::Explicit(format!("{lifetime:?}")),
            [] => LifetimeName::Elided,
            _ => panic!("Pointer or reference type cannot have multiple lifetime parameters"),
        };
        rw = match own {
            Ownership::Raw => Rewrite::TyPtr(Box::new(rw), Mutability::Not),
            Ownership::RawMut => Rewrite::TyPtr(Box::new(rw), Mutability::Mut),
            Ownership::Imm => Rewrite::TyRef(lifetime_type, Box::new(rw), Mutability::Not),
            Ownership::Cell => Rewrite::TyRef(lifetime_type, Box::new(rw), Mutability::Not),
            Ownership::Mut => Rewrite::TyRef(lifetime_type, Box::new(rw), Mutability::Mut),
            Ownership::Rc => todo!(),
            Ownership::Box => todo!(),
        };

        hir_rewrites.push((hir_ty.span, rw));
    }

    if let TyKind::Adt(adt_def, substs) = rw_lty.ty.kind() {
        if !rw_lty.label.lifetime.is_empty() {
            hir_rewrites.push((
                hir_ty.span,
                adt_ty_rw(adt_def, rw_lty.label.lifetime, substs),
            ))
        };
    }

    if rw_lty.label.descendant_has_rewrite {
        for (&arg_rw_lty, arg_hir_ty) in rw_lty.args.iter().zip(hir_args.into_iter()) {
            // FIXME: get the actual lifetime from ADT/Field Metadata
            rewrite_ty(rw_lcx, hir_rewrites, arg_rw_lty, arg_hir_ty);
        }
    }
}

impl<'a, 'tcx> HirTyVisitor<'a, 'tcx> {
    fn handle_ty(&mut self, rw_lty: RwLTy<'tcx>, hir_ty: &hir::Ty<'tcx>) {
        rewrite_ty(self.rw_lcx, &mut self.hir_rewrites, rw_lty, hir_ty);
    }
}

impl<'a, 'tcx> intravisit::Visitor<'tcx> for HirTyVisitor<'a, 'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.acx.tcx().hir()
    }

    fn visit_stmt(&mut self, s: &'tcx hir::Stmt<'tcx>) {
        match s.kind {
            // A local with a user type annotation
            hir::StmtKind::Local(hir_local) if hir_local.ty.is_some() => {
                if let Some(mir_local) = self.hir_span_to_mir_local.get(&hir_local.pat.span) {
                    let mir_local_decl = &self.mir.local_decls[*mir_local];
                    assert_eq!(mir_local_decl.source_info.span, hir_local.pat.span);
                    let lty = self.acx.local_tys[*mir_local];
                    let rw_lty = relabel_rewrites(
                        &self.asn.perms(),
                        &self.asn.flags(),
                        self.rw_lcx,
                        lty,
                        &self.acx.gacx,
                    );
                    let hir_ty = hir_local.ty.unwrap();
                    self.handle_ty(rw_lty, hir_ty);
                }
            }
            _ => (),
        }
    }
}

pub fn gen_ty_rewrites<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    mir: &Body<'tcx>,
    ldid: LocalDefId,
) -> Vec<(Span, Rewrite)> {
    let mut span_to_mir_local = HashMap::new();
    for (local, local_decl) in mir.local_decls.iter_enumerated() {
        span_to_mir_local.insert(local_decl.source_info.span, local);
    }

    let rw_lcx = LabeledTyCtxt::new(acx.tcx());
    let mut v = HirTyVisitor {
        asn,
        acx,
        rw_lcx,
        mir,
        hir_rewrites: Vec::new(),
        hir_span_to_mir_local: span_to_mir_local,
    };

    // Update function signature
    let hir_id = acx.tcx().hir().local_def_id_to_hir_id(ldid);
    let hir_sig = acx
        .tcx()
        .hir()
        .fn_sig_by_hir_id(hir_id)
        .unwrap_or_else(|| panic!("expected def {:?} to be a function", ldid));

    let (origin_params, input_origin_args, output_origin_args) =
        &acx.gacx.fn_origins.fn_info[&ldid.to_def_id()];
    let hir_generics = acx.tcx().hir().get_generics(ldid);

    let predicates = acx.tcx().predicates_of(ldid);

    if let Some(generics) = hir_generics {
        let mut generics_rws: Vec<Rewrite> = vec![];

        for param in generics.params {
            if let GenericParamKind::Lifetime {
                kind: LifetimeParamKind::Explicit,
            } = param.kind
            {
                let param_def_id = acx.tcx().hir().local_def_id(param.hir_id).to_def_id();
                let mut lifetime_bounds: Vec<String> = Vec::new();
                for (predicate, _) in predicates.predicates {
                    match predicate.kind().skip_binder() {
                        PredicateKind::RegionOutlives(outlives_predicate) => {
                            match (outlives_predicate.0.kind(), outlives_predicate.1.kind()) {
                                (
                                    ty::RegionKind::ReEarlyBound(x),
                                    ty::RegionKind::ReEarlyBound(ebr),
                                ) if x.def_id == param_def_id => {
                                    let outlived_region_path = acx.tcx().def_path_str(ebr.def_id);
                                    let outlived_region_path = outlived_region_path
                                        .split("::")
                                        .last()
                                        .unwrap_or(&outlived_region_path)
                                        .to_string();
                                    lifetime_bounds.push(outlived_region_path);
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
                if lifetime_bounds.is_empty() {
                    generics_rws.push(Rewrite::PrintTy(format!("{}", param.name.ident().as_str())));
                } else {
                    let s = format!(
                        "{}: {}",
                        param.name.ident().as_str(),
                        lifetime_bounds.join(" + ")
                    );
                    generics_rws.push(Rewrite::PrintTy(s));
                }
            }
        }

        // Handle hypothetical rewrites
        for p in origin_params {
            if let OriginParam::Hypothetical(_) = p {
                generics_rws.push(Rewrite::PrintTy(format!("{p:?}")))
            }
        }

        // Handle type rewrites
        for (param_index, param) in generics.params.iter().enumerate() {
            if let GenericParamKind::Type { .. } = param.kind {
                let mut trait_bounds: Vec<String> = Vec::new();
                for (predicate, _) in predicates.predicates {
                    match predicate.kind().skip_binder() {
                        PredicateKind::Trait(trait_predicate) => {
                            if trait_predicate.self_ty().is_param(param_index as u32) {
                                let trait_path =
                                    acx.tcx().def_path_str(trait_predicate.trait_ref.def_id);
                                trait_bounds.push(trait_path);
                            }
                        }
                        _ => {}
                    }
                }
                if trait_bounds.is_empty() {
                    generics_rws.push(Rewrite::PrintTy(format!("{}", param.name.ident().as_str())));
                } else {
                    generics_rws.push(Rewrite::PrintTy(format!(
                        "{}: {}",
                        param.name.ident().as_str(),
                        trait_bounds.join(" + ")
                    )));
                }
            }
        }

        if !generics_rws.is_empty() {
            v.hir_rewrites
                .push((generics.span, Rewrite::TyGenericParams(generics_rws)))
        }
    }

    let lty_sig = acx.gacx.fn_sigs.get(&ldid.to_def_id()).unwrap();
    assert_eq!(lty_sig.inputs.len(), hir_sig.decl.inputs.len());
    for ((&lty, hir_ty), origin_args) in lty_sig
        .inputs
        .iter()
        .zip(hir_sig.decl.inputs.iter())
        .zip(input_origin_args.iter())
    {
        let rw_lty =
            rw_lcx.zip_labels_with(lty, &origin_args, &mut |pointer_lty, lifetime_lty, args| {
                create_rewrite_label(
                    pointer_lty,
                    args,
                    &asn.perms(),
                    &asn.flags(),
                    lifetime_lty.label,
                    &acx.gacx.adt_metadata,
                )
            });

        v.handle_ty(rw_lty, hir_ty);
    }

    if let hir::FnRetTy::Return(hir_ty) = hir_sig.decl.output {
        let output_rw_lty = rw_lcx.zip_labels_with(
            lty_sig.output,
            &output_origin_args,
            &mut |pointer_lty, lifetime_lty, args| {
                create_rewrite_label(
                    pointer_lty,
                    args,
                    &asn.perms(),
                    &asn.flags(),
                    lifetime_lty.label,
                    &acx.gacx.adt_metadata,
                )
            },
        );

        v.handle_ty(output_rw_lty, hir_ty);
    }

    let hir_body_id = acx.tcx().hir().body_owned_by(ldid);
    let body = acx.tcx().hir().body(hir_body_id);
    intravisit::Visitor::visit_body(&mut v, body);

    // TODO: update cast RHS types

    v.hir_rewrites
}

pub fn gen_adt_ty_rewrites(
    gacx: &GlobalAnalysisCtxt,
    gasn: &GlobalAssignment,
    did: DefId,
) -> Vec<(Span, Rewrite)> {
    let tcx = gacx.tcx;
    let mut hir_rewrites = Vec::new();
    let item = if let Some(Node::Item(item)) = tcx.hir().get_if_local(did) {
        item
    } else {
        panic!("def id {:?} not found", did);
    };

    let field_ltys = &gacx.field_ltys;

    let (field_defs, generics) = match item.kind {
        ItemKind::Struct(VariantData::Struct(ref fd, _), ref g) => (fd, g),
        ItemKind::Union(VariantData::Struct(ref fd, _), ref g) => (fd, g),
        ItemKind::Struct(..) | ItemKind::Enum(..) | ItemKind::Union(..) => {
            warn!("unsupported item kind {:?}", item.kind);
            return Vec::new();
        }
        _ => panic!("expected struct, enum, or union, but got {:?}", item.kind),
    };

    let adt_metadata = &gacx.adt_metadata.table[&did];
    let updated_lifetime_params = &adt_metadata.lifetime_params;

    let original_lifetime_param_count = generics
        .params
        .iter()
        .filter(|p| matches!(p.kind, GenericParamKind::Lifetime { .. }))
        .count();

    if updated_lifetime_params.len() != original_lifetime_param_count {
        let new_substs: Vec<_> = {
            let mut new_lifetime_params_iter = updated_lifetime_params.iter();

            let mut updated_lifetimes = vec![];
            let mut new_lifetimes = vec![];
            let mut other_params = vec![];

            for gp in generics.params {
                match gp.kind {
                    GenericParamKind::Lifetime { .. } => {
                        let updated_lifetime_param = new_lifetime_params_iter
                            .next()
                            .expect("Not enough updated_lifetime_params");
                        updated_lifetimes
                            .push(Rewrite::PrintTy(format!("{:?}", updated_lifetime_param)))
                    }
                    _ => other_params.push(Rewrite::PrintTy(gp.name.ident().to_string())),
                }
            }

            for ul in new_lifetime_params_iter {
                new_lifetimes.push(Rewrite::PrintTy(format!("{:?}", ul)))
            }

            [updated_lifetimes, new_lifetimes, other_params]
                .into_iter()
                .flatten()
                .collect()
        };

        // only the generic parameters need to be rewritten, not the
        // struct name itself
        hir_rewrites.push((generics.span, Rewrite::TyGenericParams(new_substs)));
    }

    for field_def in field_defs.iter() {
        let fdid = tcx.hir().local_def_id(field_def.hir_id).to_def_id();
        let field_metadata = &adt_metadata.field_info[&fdid];
        let f_lty = field_ltys[&fdid];
        let lcx = LabeledTyCtxt::<RewriteLabel>::new(tcx);
        let rw_lty = lcx.zip_labels_with(
            f_lty,
            field_metadata.origin_args,
            &mut |pointer_lty, lifetime_lty, args| {
                create_rewrite_label(
                    pointer_lty,
                    args,
                    &gasn.perms,
                    &gasn.flags,
                    lifetime_lty.label,
                    &gacx.adt_metadata,
                )
            },
        );

        rewrite_ty(lcx, &mut hir_rewrites, rw_lty, field_def.ty);
    }

    hir_rewrites
}

/// Print the rewritten types for all locals in `mir`.  This is used for tests and debugging, as it
/// reveals the inference results even for temporaries and other locals with no type annotation in
/// the HIR.
pub fn dump_rewritten_local_tys<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    mir: &Body<'tcx>,
    mut describe_local: impl FnMut(TyCtxt<'tcx>, &LocalDecl) -> String,
) {
    let rw_lcx = LabeledTyCtxt::new(acx.tcx());
    for (local, decl) in mir.local_decls.iter_enumerated() {
        // TODO: apply `Cell` if `addr_of_local` indicates it's needed
        let rw_lty = relabel_rewrites(
            &asn.perms(),
            &asn.flags(),
            rw_lcx,
            acx.local_tys[local],
            &acx.gacx,
        );
        let ty = mk_rewritten_ty(rw_lcx, rw_lty);
        eprintln!(
            "{:?} ({}): {:?}",
            local,
            describe_local(acx.tcx(), decl),
            ty
        );
    }
}
