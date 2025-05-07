//! Rewriting of types comes with one extra bit of complexity: sometimes we need to rewrite a
//! pointer type that's behind a type alias, such as `type MyPtr = *mut u8;`. To do this, we unfold
//! the alias, replacing `x: MyPtr` with `x: *mut u8`, and applying further rewrites from there. As
//! with the materialization of adjustments in expr rewriting, we try to apply this transformation
//! selectively, since we don't want to unfold all type aliases in the program.

use std::collections::HashMap;
use std::ops::Index;

use crate::borrowck::{OriginArg, OriginParam};
use crate::context::AdtMetadataTable;
use crate::context::{
    AnalysisCtxt, Assignment, FlagSet, FnSigOrigins, GlobalAnalysisCtxt, LTy, PermissionSet,
};
use crate::labeled_ty::{LabeledTy, LabeledTyCtxt};
use crate::pointee_type::PointeeTypes;
use crate::pointer_id::{GlobalPointerTable, PointerId, PointerTable};
use crate::rewrite::Rewrite;
use crate::type_desc::{self, Ownership, PtrDesc, Quantity, TypeDesc};
use hir::{
    FnRetTy, GenericParamKind, Generics, ItemKind, Path, PathSegment, VariantData, WherePredicate,
};
use log::{debug, warn};
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
use rustc_middle::ty::{Ty, TyKind, TypeAndMut};
use rustc_span::symbol::Symbol;
use rustc_span::Span;

use super::LifetimeName;

/// A label for use with `LabeledTy` to indicate what rewrites to apply at each position in a type.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
struct RewriteLabel<'tcx> {
    /// Rewrite a raw pointer, whose ownership and quantity have been inferred as indicated.
    ty_desc: Option<PtrDesc>,
    /// Rewrite the pointee type of this pointer.
    pointee_ty: Option<Ty<'tcx>>,
    /// If set, a child or other descendant of this type requires rewriting.
    descendant_has_rewrite: bool,
    /// A lifetime rewrite for a pointer or reference.
    lifetime: &'tcx [OriginArg<'tcx>],
}

type RwLTy<'tcx> = LabeledTy<'tcx, RewriteLabel<'tcx>>;

fn ty_has_adt_lifetime(ty: Ty, adt_metadata: &AdtMetadataTable) -> bool {
    match ty.kind() {
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
    }
}

fn has_lifetime_rws(rw_lty: &RwLTy, adt_metadata: &AdtMetadataTable) -> bool {
    let has_pointer_lifetime = rw_lty
        .label
        .lifetime
        .iter()
        .any(|lt| matches!(lt, OriginArg::Hypothetical(..)));
    let has_adt_lifetime = ty_has_adt_lifetime(rw_lty.ty, adt_metadata);
    has_adt_lifetime || has_pointer_lifetime
}

fn descendant_has_rewrite(args: &[RwLTy], adt_metadata: &AdtMetadataTable) -> bool {
    args.iter().any(|child| {
        child.label.ty_desc.is_some()
            || child.label.pointee_ty.is_some()
            || child.label.descendant_has_rewrite
            || has_lifetime_rws(child, adt_metadata)
    })
}

fn create_rewrite_label<'tcx>(
    pointer_lty: LTy<'tcx>,
    args: &[RwLTy<'tcx>],
    perms: &impl Index<PointerId, Output = PermissionSet>,
    flags: &impl Index<PointerId, Output = FlagSet>,
    pointee_types: &impl Index<PointerId, Output = PointeeTypes<'tcx>>,
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
            Some(desc.into())
        }
    };

    let mut pointee_ty = None;
    // For now, we only rewrite in cases where the inferred pointee has no arguments.
    // TODO: expand this to handle pointer-to-pointer cases and other complex inferred pointees
    if !pointer_lty.label.is_none() && !flags[pointer_lty.label].contains(FlagSet::FIXED) {
        if let Some(lty) = pointee_types[pointer_lty.label].get_sole_lty() {
            let ty = lty.ty;
            if lty.args.is_empty() && !ty_has_adt_lifetime(ty, adt_metadata) {
                // Don't rewrite if the old and new types are the same.
                if ty != args[0].ty {
                    pointee_ty = Some(ty);
                }
            }
        }
    }

    RewriteLabel {
        ty_desc,
        pointee_ty,
        descendant_has_rewrite: descendant_has_rewrite(args, adt_metadata),
        lifetime,
    }
}

fn relabel_rewrites<'tcx, P, F, PT>(
    perms: &P,
    flags: &F,
    pointee_types: &PT,
    lcx: LabeledTyCtxt<'tcx, RewriteLabel<'tcx>>,
    lty: LTy<'tcx>,
    adt_metadata: &AdtMetadataTable,
) -> RwLTy<'tcx>
where
    P: Index<PointerId, Output = PermissionSet>,
    F: Index<PointerId, Output = FlagSet>,
    PT: Index<PointerId, Output = PointeeTypes<'tcx>>,
{
    lcx.relabel_with_args(lty, &mut |pointer_lty, args| {
        // FIXME: get function lifetime parameters and pass them to this
        create_rewrite_label(
            pointer_lty,
            args,
            perms,
            flags,
            pointee_types,
            &[],
            adt_metadata,
        )
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

        (&ty::TyKind::Adt(adt_def, substs), &hir::TyKind::Path(hir::QPath::Resolved(_, path)))
            if path.res.def_id() == adt_def.did() =>
        {
            hir_generic_ty_args(hir_ty).map(|type_args| {
                if type_args.len() < substs.types().count() {
                    // this situation occurs when there are hidden type arguments
                    // such as the allocator `std::alloc::Global` type argument in `Vec`
                    debug!("warning: extra MIR type argument for {adt_def:?}:");
                    for mir_arg in substs.types().into_iter().skip(type_args.len()) {
                        debug!("\t{:?}", mir_arg)
                    }
                } else if type_args.len() != substs.types().count() {
                    panic!("mismatched number of type arguments for {adt_def:?} and {hir_ty:?}")
                }
                type_args
            })
        }
        (&ty::TyKind::FnPtr(sig), &hir::TyKind::BareFn(bfnty)) => {
            let args = sig.skip_binder().inputs_and_output;
            if args.len() != bfnty.decl.inputs.len() + 1 {
                panic!("mismatched number of function inputs for {sig:?} and {bfnty:?}");
            }

            let mut v: Vec<_> = bfnty.decl.inputs.iter().collect();
            if let FnRetTy::Return(return_ty) = bfnty.decl.output {
                v.push(return_ty)
            }
            Some(v)
        }
        (tk, hir_tk) => {
            debug!("deconstruct_hir_ty: {tk:?} -- {hir_tk:?} not supported");
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

fn mk_adt_with_generic_args<'tcx>(
    tcx: TyCtxt<'tcx>,
    path: &str,
    args: impl IntoIterator<Item = GenericArg<'tcx>>,
) -> ty::Ty<'tcx> {
    let mut path_parts_iter = path.split("::");
    let crate_name = path_parts_iter
        .next()
        .unwrap_or_else(|| panic!("couldn't find crate name in {path:?}"));
    let crate_name = Symbol::intern(crate_name);

    let krate = tcx
        .crates(())
        .iter()
        .cloned()
        .find(|&krate| tcx.crate_name(krate) == crate_name)
        .unwrap_or_else(|| panic!("couldn't find crate {crate_name:?} for {path:?}"));

    let mut cur_did = krate.as_def_id();
    for part in path_parts_iter {
        let mod_child = tcx
            .module_children(cur_did)
            .iter()
            .find(|child| child.ident.as_str() == part)
            .unwrap_or_else(|| panic!("failed to find {part:?} for {path:?}"));
        cur_did = match mod_child.res {
            Res::Def(DefKind::Mod, did) => did,
            Res::Def(DefKind::Struct, did) => did,
            Res::Def(DefKind::Enum, did) => did,
            Res::Def(DefKind::Union, did) => did,
            ref r => panic!("unexpected resolution {r:?} for {part:?} in {path:?}"),
        };
    }

    let adt = tcx.adt_def(cur_did);
    let substs = tcx.mk_substs(args.into_iter());
    tcx.mk_adt(adt, substs)
}

fn mk_adt_with_arg<'tcx>(tcx: TyCtxt<'tcx>, path: &str, arg_ty: ty::Ty<'tcx>) -> ty::Ty<'tcx> {
    mk_adt_with_generic_args(tcx, path, [GenericArg::from(arg_ty)])
}

fn mk_cell<'tcx>(tcx: TyCtxt<'tcx>, ty: ty::Ty<'tcx>) -> ty::Ty<'tcx> {
    mk_adt_with_arg(tcx, "core::cell::Cell", ty)
}

fn mk_option<'tcx>(tcx: TyCtxt<'tcx>, ty: ty::Ty<'tcx>) -> ty::Ty<'tcx> {
    mk_adt_with_arg(tcx, "core::option::Option", ty)
}

fn mk_dyn_owned<'tcx>(tcx: TyCtxt<'tcx>, ty: ty::Ty<'tcx>) -> ty::Ty<'tcx> {
    let args = [GenericArg::from(ty), GenericArg::from(tcx.mk_unit())];
    mk_adt_with_generic_args(tcx, "core::result::Result", args)
}

/// Produce a `Ty` reflecting the rewrites indicated by the labels in `rw_lty`.
fn mk_rewritten_ty<'tcx>(
    lcx: LabeledTyCtxt<'tcx, RewriteLabel<'tcx>>,
    rw_lty: RwLTy<'tcx>,
) -> ty::Ty<'tcx> {
    let tcx = *lcx;
    lcx.rewrite_unlabeled(rw_lty, &mut |ptr_ty, args, label| {
        let (ty, ptr_desc) = match (label.pointee_ty, label.ty_desc) {
            (Some(pointee_ty), Some(ptr_desc)) => {
                // The `ty` should be a pointer.
                assert_eq!(args.len(), 1);
                (pointee_ty, ptr_desc)
            }
            (Some(pointee_ty), None) => {
                // Just change the pointee type and nothing else.
                let new_ty = match *ptr_ty.kind() {
                    TyKind::Ref(rg, _ty, mutbl) => tcx.mk_ty(TyKind::Ref(rg, pointee_ty, mutbl)),
                    TyKind::RawPtr(tm) => tcx.mk_ty(TyKind::RawPtr(TypeAndMut {
                        ty: pointee_ty,
                        mutbl: tm.mutbl,
                    })),
                    _ => panic!("expected Ref or RawPtr, but got {:?}", ptr_ty),
                };
                return new_ty;
            }
            (None, Some(ptr_desc)) => {
                // The `ty` should be a pointer; the sole argument is the pointee type.
                assert_eq!(args.len(), 1);
                (args[0], ptr_desc)
            }
            (None, None) => {
                // No rewrite to apply.
                return ptr_ty;
            }
        };

        desc_parts_to_ty(tcx, ptr_desc, ty)
    })
}

pub fn desc_parts_to_ty<'tcx>(
    tcx: TyCtxt<'tcx>,
    ptr_desc: PtrDesc,
    pointee_ty: Ty<'tcx>,
) -> Ty<'tcx> {
    let mut ty = pointee_ty;
    let PtrDesc {
        own,
        qty,
        dyn_owned,
        option,
    } = ptr_desc;

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

    if dyn_owned {
        ty = mk_dyn_owned(tcx, ty);
    }

    if option {
        ty = mk_option(tcx, ty);
    }

    ty
}

pub fn desc_to_ty<'tcx>(tcx: TyCtxt<'tcx>, desc: TypeDesc<'tcx>) -> Ty<'tcx> {
    desc_parts_to_ty(tcx, PtrDesc::from(desc), desc.pointee_ty)
}

struct HirTyVisitor<'a, 'tcx> {
    asn: &'a Assignment,
    pointee_types: PointerTable<'a, PointeeTypes<'tcx>>,
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
        .map(|p| Rewrite::Print(format!("{p:?}")));
    let other_param_names = substs.iter().filter_map(|p| match p.unpack() {
        GenericArgKind::Lifetime(..) => None,
        _ => Some(Rewrite::Print(format!("{p:?}"))),
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
    adt_metadata: &AdtMetadataTable,
) {
    if !rw_lty.ty.is_adt()
        && rw_lty.label.ty_desc.is_none()
        && rw_lty.label.pointee_ty.is_none()
        && !rw_lty.label.descendant_has_rewrite
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
            hir_rewrites.push((hir_ty.span, Rewrite::Print(s)));
            return;
        }
    };

    if rw_lty.label.ty_desc.is_some() || rw_lty.label.pointee_ty.is_some() {
        assert!(matches!(
            rw_lty.ty.kind(),
            TyKind::Ref(..) | TyKind::RawPtr(..)
        ));
        let lifetime_type = match rw_lty.label.lifetime {
            [lifetime] => LifetimeName::Explicit(format!("{lifetime:?}")),
            [] => LifetimeName::Elided,
            _ => panic!("Pointer or reference type cannot have multiple lifetime parameters"),
        };

        let mut rw = if let Some(pointee_ty) = rw_lty.label.pointee_ty {
            let printer = FmtPrinter::new(*rw_lcx, Namespace::TypeNS);
            let s = pointee_ty.print(printer).unwrap().into_buffer();
            Rewrite::Print(s)
        } else {
            Rewrite::Sub(0, hir_args[0].span)
        };

        if let Some(ptr_desc) = rw_lty.label.ty_desc {
            assert_eq!(hir_args.len(), 1);
            let PtrDesc {
                own,
                qty,
                dyn_owned,
                option,
            } = ptr_desc;

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

            rw = match own {
                Ownership::Raw => Rewrite::TyPtr(Box::new(rw), Mutability::Not),
                Ownership::RawMut => Rewrite::TyPtr(Box::new(rw), Mutability::Mut),
                Ownership::Imm => Rewrite::TyRef(lifetime_type, Box::new(rw), Mutability::Not),
                Ownership::Cell => Rewrite::TyRef(lifetime_type, Box::new(rw), Mutability::Not),
                Ownership::Mut => Rewrite::TyRef(lifetime_type, Box::new(rw), Mutability::Mut),
                Ownership::Rc => todo!(),
                Ownership::Box => Rewrite::TyCtor("std::boxed::Box".into(), vec![rw]),
            };

            if dyn_owned {
                // Ideally, we would use a custom `DynOwned<T>` type here to make the meaning
                // clear.  However, we don't currently have a run-time support library for
                // c2rust-analyze where we could define such a type.  As an alternative, for now we
                // use `Result<T, ()>`, which has roughly the same semantics (equivalent to
                // `Option<T>`).  We don't use `Option<T>` because it would result in confusing
                // `Option<Option<T>>` types for pointers that are both owned and nullable.
                rw = Rewrite::TyCtor(
                    "core::result::Result".into(),
                    vec![rw, Rewrite::Print("()".into())],
                );
            }

            if option {
                rw = Rewrite::TyCtor("core::option::Option".into(), vec![rw]);
            }
        } else {
            rw = match *rw_lty.ty.kind() {
                TyKind::Ref(_rg, _ty, mutbl) => Rewrite::TyRef(lifetime_type, Box::new(rw), mutbl),
                TyKind::RawPtr(tm) => Rewrite::TyPtr(Box::new(rw), tm.mutbl),
                _ => unreachable!(),
            };
        }

        if rw != Rewrite::Identity {
            hir_rewrites.push((hir_ty.span, rw));
        }
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
        let (rw_lty_args, rw_lty_remainder) = rw_lty.args.split_at(hir_args.len());
        assert!(
            !descendant_has_rewrite(rw_lty_remainder, adt_metadata),
            "descendant_has_rewrite is true for the remainder of rw_lty.args"
        );
        for (&arg_rw_lty, arg_hir_ty) in rw_lty_args.iter().zip(hir_args.into_iter()) {
            rewrite_ty(rw_lcx, hir_rewrites, arg_rw_lty, arg_hir_ty, adt_metadata);
        }
    }
}

impl<'a, 'tcx> HirTyVisitor<'a, 'tcx> {
    fn handle_ty(&mut self, rw_lty: RwLTy<'tcx>, hir_ty: &hir::Ty<'tcx>) {
        rewrite_ty(
            self.rw_lcx,
            &mut self.hir_rewrites,
            rw_lty,
            hir_ty,
            &self.acx.gacx.adt_metadata,
        );
    }
}

impl<'tcx, 'a> intravisit::Visitor<'tcx> for HirTyVisitor<'a, 'tcx> {
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
                        self.asn.perms(),
                        self.asn.flags(),
                        &self.pointee_types,
                        self.rw_lcx,
                        lty,
                        &self.acx.gacx.adt_metadata,
                    );
                    let hir_ty = hir_local.ty.unwrap();
                    self.handle_ty(rw_lty, hir_ty);
                }
            }
            _ => (),
        }
        intravisit::walk_stmt(self, s);
    }
}

pub fn gen_ty_rewrites<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    pointee_types: PointerTable<PointeeTypes<'tcx>>,
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
        pointee_types,
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

    let FnSigOrigins {
        origin_params,
        inputs: input_origin_args,
        output: output_origin_args,
    } = &acx.gacx.fn_origins.fn_info[&ldid.to_def_id()];
    let hir_generics = acx.tcx().hir().get_generics(ldid);

    let generics = hir_generics.unwrap_or(Generics::empty());
    gen_generics_rws(&mut v.hir_rewrites, generics, origin_params.iter());

    let lty_sig = acx.gacx.fn_sigs.get(&ldid.to_def_id()).unwrap();
    assert_eq!(lty_sig.inputs.len(), hir_sig.decl.inputs.len());
    for ((&lty, hir_ty), origin_args) in lty_sig
        .inputs
        .iter()
        .zip(hir_sig.decl.inputs.iter())
        .zip(input_origin_args.iter())
    {
        let rw_lty =
            rw_lcx.zip_labels_with(lty, origin_args, &mut |pointer_lty, lifetime_lty, args| {
                create_rewrite_label(
                    pointer_lty,
                    args,
                    asn.perms(),
                    asn.flags(),
                    &pointee_types,
                    lifetime_lty.label,
                    &acx.gacx.adt_metadata,
                )
            });

        v.handle_ty(rw_lty, hir_ty);
    }

    if let hir::FnRetTy::Return(hir_ty) = hir_sig.decl.output {
        let output_rw_lty = rw_lcx.zip_labels_with(
            lty_sig.output,
            output_origin_args,
            &mut |pointer_lty, lifetime_lty, args| {
                create_rewrite_label(
                    pointer_lty,
                    args,
                    asn.perms(),
                    asn.flags(),
                    &pointee_types,
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

pub fn gen_generics_rws<'p, 'tcx>(
    hir_rewrites: &mut Vec<(Span, Rewrite)>,
    generics: &Generics<'tcx>,
    origin_params: impl Iterator<Item = &'p OriginParam>,
) {
    let mut last_lifetime_span: Option<Span> = None;
    let mut first_generic_type_span: Option<Span> = None;
    let mut first_generic_const_span: Option<Span> = None;

    for predicate in generics.predicates {
        let predicate_span = predicate.span();
        if matches!(predicate, WherePredicate::RegionPredicate(_))
            && !predicate.in_where_clause()
            && (last_lifetime_span.is_none()
                || last_lifetime_span.unwrap().hi() < predicate_span.hi())
        {
            last_lifetime_span = Some(predicate_span)
        }
    }
    for param in generics.params {
        match param.kind {
            GenericParamKind::Lifetime { .. } => {
                if last_lifetime_span.is_none()
                    || last_lifetime_span.unwrap().hi() < param.span.hi()
                {
                    last_lifetime_span = Some(param.span)
                }
            }
            GenericParamKind::Type { .. } => {
                if first_generic_type_span.is_none()
                    || first_generic_type_span.unwrap().lo() < param.span.lo()
                {
                    first_generic_type_span = Some(param.span)
                }
            }
            GenericParamKind::Const { .. } => {
                if first_generic_const_span.is_none()
                    || first_generic_const_span.unwrap().lo() < param.span.lo()
                {
                    first_generic_const_span = Some(param.span)
                }
            }
        }
    }

    let generics_spans = (
        last_lifetime_span,
        first_generic_type_span,
        first_generic_const_span,
    );

    let hypothetical_origin_params: Vec<_> = origin_params
        .filter_map(|p| {
            if let OriginParam::Hypothetical(_) = p {
                Some(format!("{:?}", p))
            } else {
                None
            }
        })
        .collect();
    let hypothetical_origin_string = hypothetical_origin_params.join(",");

    if !hypothetical_origin_string.is_empty() {
        let (hypothetical_origin_span, format_string) = match generics_spans {
            (Some(last_lifetime_span), ..) => (
                last_lifetime_span.shrink_to_hi(),
                format!(",{}", hypothetical_origin_string),
            ),
            (_, Some(first_generics_span), _) | (_, _, Some(first_generics_span)) => (
                first_generics_span.shrink_to_lo(),
                format!("{},", hypothetical_origin_string),
            ),
            _ => (generics.span, format!("<{}>", hypothetical_origin_string)),
        };
        hir_rewrites.push((hypothetical_origin_span, Rewrite::Print(format_string)));
    }
}

pub fn gen_adt_ty_rewrites<'tcx>(
    gacx: &GlobalAnalysisCtxt<'tcx>,
    asn: &Assignment,
    pointee_types: &GlobalPointerTable<PointeeTypes<'tcx>>,
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

    gen_generics_rws(
        &mut hir_rewrites,
        generics,
        gacx.adt_metadata.table[&did].lifetime_params.iter(),
    );

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
                    &asn.perms,
                    &asn.flags,
                    pointee_types,
                    lifetime_lty.label,
                    &gacx.adt_metadata,
                )
            },
        );

        rewrite_ty(
            lcx,
            &mut hir_rewrites,
            rw_lty,
            field_def.ty,
            &gacx.adt_metadata,
        );
    }

    hir_rewrites
}

/// Compute the new, rewritten version of `lty`.
pub fn rewrite_lty<'tcx>(
    tcx: TyCtxt<'tcx>,
    lty: LTy<'tcx>,
    perms: &GlobalPointerTable<PermissionSet>,
    flags: &GlobalPointerTable<FlagSet>,
    pointee_types: &PointerTable<PointeeTypes<'tcx>>,
    adt_metadata: &AdtMetadataTable,
) -> Ty<'tcx> {
    let rw_lcx = LabeledTyCtxt::<RewriteLabel>::new(tcx);
    let rw_lty = relabel_rewrites(perms, flags, pointee_types, rw_lcx, lty, adt_metadata);
    mk_rewritten_ty(rw_lcx, rw_lty)
}

/// Print the rewritten types for all locals in `mir`.  This is used for tests and debugging, as it
/// reveals the inference results even for temporaries and other locals with no type annotation in
/// the HIR.
pub fn dump_rewritten_local_tys<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    pointee_types: PointerTable<PointeeTypes<'tcx>>,
    mir: &Body<'tcx>,
    mut describe_local: impl FnMut(TyCtxt<'tcx>, &LocalDecl) -> String,
) {
    let rw_lcx = LabeledTyCtxt::new(acx.tcx());
    for (local, decl) in mir.local_decls.iter_enumerated() {
        // TODO: apply `Cell` if `addr_of_local` indicates it's needed
        let rw_lty = relabel_rewrites(
            asn.perms(),
            asn.flags(),
            &pointee_types,
            rw_lcx,
            acx.local_tys[local],
            &acx.gacx.adt_metadata,
        );
        let ty = mk_rewritten_ty(rw_lcx, rw_lty);
        debug!(
            "{:?} ({}): {:?}",
            local,
            describe_local(acx.tcx(), decl),
            ty
        );
    }
}
