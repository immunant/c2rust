use rustc_hir::def::{DefKind, Res};

use crate::context::{AnalysisCtxt, FlagSet, LTy, PermissionSet, PointerId};
use rustc_middle::ty::subst::GenericArg;
use rustc_middle::ty::{ReErased, Ty, TyCtxt};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Ownership {
    /// E.g. `*const T`
    Raw,
    /// E.g. `*mut T`
    RawMut,
    /// E.g. `&T`
    Imm,
    /// E.g. `&Cell<T>`
    Cell,
    /// E.g. `&mut T`
    Mut,
    /// E.g. `Rc<T>`
    Rc,
    /// E.g. `Box<T>`
    Box,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Quantity {
    /// E.g. `&T`
    Single,
    /// E.g. `&[T]`
    Slice,
    /// E.g. `OffsetPtr<T>`
    OffsetPtr,
}

pub fn perms_to_desc(perms: PermissionSet, flags: FlagSet) -> (Ownership, Quantity) {
    let own = if perms.contains(PermissionSet::UNIQUE | PermissionSet::WRITE) {
        Ownership::Mut
    } else if flags.contains(FlagSet::CELL) {
        Ownership::Cell
    } else {
        // Anything with WRITE and not UNIQUE should have CELL set, and use the previous case.
        assert!(!perms.contains(PermissionSet::WRITE));
        Ownership::Imm
    };

    let qty = if perms.contains(PermissionSet::OFFSET_SUB) {
        // TODO: should be Quantity::OffsetPtr, but that's not implemented yet
        Quantity::Slice
    } else if perms.contains(PermissionSet::OFFSET_ADD) {
        Quantity::Slice
    } else {
        Quantity::Single
    };

    (own, qty)
}

fn mk_cell<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Ty<'tcx> {
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
    let cell_mod = match cell_mod_child.res {
        Res::Def(DefKind::Mod, did) => did,
        ref r => panic!("unexpected resolution {:?} for `core::cell`", r),
    };

    let cell_struct_child = tcx
        .module_children(cell_mod)
        .iter()
        .find(|child| child.ident.as_str() == "Cell")
        .expect("failed to find struct `core::cell::Cell`");
    let cell_struct = match cell_struct_child.res {
        Res::Def(DefKind::Struct, did) => did,
        ref r => panic!("unexpected resolution {:?} for `core::cell::Cell`", r),
    };

    let cell_adt = tcx.adt_def(cell_struct);
    let substs = tcx.mk_substs([GenericArg::from(ty)].into_iter());
    tcx.mk_adt(cell_adt, substs)
}

pub fn convert_type<'tcx>(
    acx: &AnalysisCtxt<'tcx>,
    lty: LTy<'tcx>,
    perms: &[PermissionSet],
    flags: &[FlagSet],
) -> Ty<'tcx> {
    let tcx = acx.tcx;
    acx.lcx.rewrite_unlabeled(lty, &mut |ty, args, label| {
        if label == PointerId::NONE {
            return ty;
        }
        let ptr = label;

        let (own, qty) = perms_to_desc(perms[ptr.index()], flags[ptr.index()]);

        assert_eq!(args.len(), 1);
        let mut ty = args[0];

        if own == Ownership::Cell {
            ty = mk_cell(tcx, ty);
        }

        ty = match qty {
            Quantity::Single => ty,
            Quantity::Slice => tcx.mk_slice(ty),
            Quantity::OffsetPtr => todo!(),
        };

        ty = match own {
            Ownership::Raw => tcx.mk_imm_ptr(ty),
            Ownership::RawMut => tcx.mk_mut_ptr(ty),
            Ownership::Imm => tcx.mk_imm_ref(tcx.mk_region(ReErased), ty),
            Ownership::Cell => tcx.mk_imm_ref(tcx.mk_region(ReErased), ty),
            Ownership::Mut => tcx.mk_mut_ref(tcx.mk_region(ReErased), ty),
            Ownership::Rc => todo!(),
            Ownership::Box => todo!(),
        };

        ty
    })
}
