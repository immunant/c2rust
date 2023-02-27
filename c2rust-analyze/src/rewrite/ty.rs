use crate::context::{AnalysisCtxt, Assignment, LTy};
use crate::labeled_ty::{LabeledTy, LabeledTyCtxt};
use crate::rewrite::Rewrite;
use crate::type_desc::{self, Ownership, Quantity};
use rustc_ast::ast;
use rustc_hir as hir;
use rustc_hir::def::{DefKind, Namespace, Res};
use rustc_hir::def_id::LocalDefId;
use rustc_hir::intravisit;
use rustc_hir::Mutability;
use rustc_middle::hir::nested_filter;
use rustc_middle::mir::{self, Body, LocalDecl};
use rustc_middle::ty::print::{FmtPrinter, Print};
use rustc_middle::ty::subst::GenericArg;
use rustc_middle::ty::{self, ReErased, TyCtxt};
use rustc_span::Span;

/// A label for use with `LabeledTy` to indicate what rewrites to apply at each position in a type.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
struct RewriteLabel {
    /// Rewrite a raw pointer, whose ownership and quantity have been inferred as indicated.
    ty_desc: Option<(Ownership, Quantity)>,
    /// If set, a child or other descendant of this type requires rewriting.
    descendant_has_rewrite: bool,
}

type RwLTy<'tcx> = LabeledTy<'tcx, RewriteLabel>;

/// Given an `LTy`, which is labeled with `PointerId`s, determine which rewrites to apply based on
/// the permissions and flags inferred for each `PointerId`.
fn relabel_rewrites<'tcx>(
    asn: &Assignment,
    lcx: LabeledTyCtxt<'tcx, RewriteLabel>,
    lty: LTy<'tcx>,
) -> RwLTy<'tcx> {
    lcx.relabel_with_args(lty, &mut |lty, args| {
        let ty_desc = if lty.label.is_none() {
            None
        } else {
            let perms = asn.perms()[lty.label];
            let flags = asn.flags()[lty.label];
            // TODO: if the `Ownership` and `Quantity` exactly match `lty.ty`, then `ty_desc` can
            // be `None` (no rewriting required).  This might let us avoid inlining a type alias
            // for some pointers where no actual improvement was possible.
            Some(type_desc::perms_to_desc(perms, flags))
        };
        let descendant_has_rewrite = args.iter().any(|child| {
            let has_rewrite = child.label.ty_desc.is_some();
            has_rewrite || child.label.descendant_has_rewrite
        });
        RewriteLabel {
            ty_desc,
            descendant_has_rewrite,
        }
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

        _ => None,
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
    lcx: LabeledTyCtxt<'tcx, RewriteLabel>,
    rw_lty: RwLTy<'tcx>,
) -> ty::Ty<'tcx> {
    let tcx = *lcx;
    lcx.rewrite_unlabeled(rw_lty, &mut |ty, args, label| {
        let (own, qty) = match label.ty_desc {
            Some(x) => x,
            None => return ty,
        };

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
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    rw_lcx: LabeledTyCtxt<'tcx, RewriteLabel>,
    //mir: &'a Body<'tcx>,
    //span_index: SpanIndex<Location>,
    hir_rewrites: Vec<(Span, Rewrite)>,
}

impl<'a, 'tcx> HirTyVisitor<'a, 'tcx> {
    fn handle_ty(&mut self, rw_lty: RwLTy<'tcx>, hir_ty: &hir::Ty<'tcx>) {
        if rw_lty.label.ty_desc.is_none() && !rw_lty.label.descendant_has_rewrite {
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
                let ty = mk_rewritten_ty(self.rw_lcx, rw_lty);
                let printer = FmtPrinter::new(*self.rw_lcx, Namespace::TypeNS);
                let s = ty.print(printer).unwrap().into_buffer();
                self.hir_rewrites.push((hir_ty.span, Rewrite::PrintTy(s)));
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
            };

            rw = match own {
                Ownership::Raw => Rewrite::TyPtr(Box::new(rw), Mutability::Not),
                Ownership::RawMut => Rewrite::TyPtr(Box::new(rw), Mutability::Mut),
                Ownership::Imm => Rewrite::TyRef(Box::new(rw), Mutability::Not),
                Ownership::Cell => Rewrite::TyRef(Box::new(rw), Mutability::Not),
                Ownership::Mut => Rewrite::TyRef(Box::new(rw), Mutability::Mut),
                Ownership::Rc => todo!(),
                Ownership::Box => todo!(),
            };

            self.hir_rewrites.push((hir_ty.span, rw));
        }

        if rw_lty.label.descendant_has_rewrite {
            for (&arg_rw_lty, arg_hir_ty) in rw_lty.args.iter().zip(hir_args.into_iter()) {
                self.handle_ty(arg_rw_lty, arg_hir_ty);
            }
        }
    }
}

impl<'a, 'tcx> intravisit::Visitor<'tcx> for HirTyVisitor<'a, 'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.acx.tcx().hir()
    }
}

pub fn gen_ty_rewrites<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    asn: &Assignment,
    _mir: &Body<'tcx>,
    ldid: LocalDefId,
) -> Vec<(Span, Rewrite)> {
    let rw_lcx = LabeledTyCtxt::new(acx.tcx());

    let mut v = HirTyVisitor {
        acx,
        rw_lcx,
        hir_rewrites: Vec::new(),
    };

    // Update function signature

    let hir_id = acx.tcx().hir().local_def_id_to_hir_id(ldid);
    let hir_sig = acx
        .tcx()
        .hir()
        .fn_sig_by_hir_id(hir_id)
        .unwrap_or_else(|| panic!("expected def {:?} to be a function", ldid));

    let lty_sig = acx.gacx.fn_sigs.get(&ldid.to_def_id()).unwrap();

    assert_eq!(lty_sig.inputs.len(), hir_sig.decl.inputs.len());
    for (&lty, hir_ty) in lty_sig.inputs.iter().zip(hir_sig.decl.inputs.iter()) {
        let rw_lty = relabel_rewrites(asn, rw_lcx, lty);
        v.handle_ty(rw_lty, hir_ty);
    }

    if let hir::FnRetTy::Return(hir_ty) = hir_sig.decl.output {
        let rw_lty = relabel_rewrites(asn, rw_lcx, lty_sig.output);
        v.handle_ty(rw_lty, hir_ty);
    }

    // TODO: update let bindings
    // TODO: wrap locals in `Cell` if `addr_of_local` indicates that it's needed

    // TODO: update cast RHS types

    // TODO: update struct field types

    v.hir_rewrites
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
        let rw_lty = relabel_rewrites(asn, rw_lcx, acx.local_tys[local]);
        let ty = mk_rewritten_ty(rw_lcx, rw_lty);
        eprintln!(
            "{:?} ({}): {:?}",
            local,
            describe_local(acx.tcx(), decl),
            ty
        );
    }
}
