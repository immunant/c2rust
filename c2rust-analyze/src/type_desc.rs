use crate::context::{FlagSet, PermissionSet};
use rustc_middle::ty::{Ty, TyKind};

#[allow(dead_code)]
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

    /// E.g. `&[T; 10]`.  This is used only for existing `FIXED` pointers; `perms_to_desc` on a raw
    /// pointer never produces `Array`.
    Array,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct TypeDesc<'tcx> {
    pub own: Ownership,
    pub qty: Quantity,
    pub pointee_ty: Ty<'tcx>,
}

fn perms_to_own_and_qty(perms: PermissionSet, flags: FlagSet) -> (Ownership, Quantity) {
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
        Quantity::OffsetPtr
    } else if perms.contains(PermissionSet::OFFSET_ADD) {
        Quantity::Slice
    } else {
        Quantity::Single
    };

    (own, qty)
}

pub fn perms_to_desc<'tcx>(
    ptr_ty: Ty<'tcx>,
    perms: PermissionSet,
    flags: FlagSet,
) -> TypeDesc<'tcx> {
    let (own, qty) = perms_to_own_and_qty(perms, flags);

    let pointee_ty = match *ptr_ty.kind() {
        TyKind::Ref(_, ty, _) => ty,
        TyKind::RawPtr(mt) => mt.ty,
        TyKind::Adt(adt_def, substs) if adt_def.is_box() => substs.type_at(0),
        // TODO: other ADTs, e.g. `Rc`
        _ => panic!("expected a pointer type, but got {:?}", ptr_ty),
    };

    TypeDesc {
        own,
        qty,
        pointee_ty,
    }
}

pub fn local_perms_to_desc<'tcx>(
    local_ty: Ty<'tcx>,
    perms: PermissionSet,
    flags: FlagSet,
) -> TypeDesc<'tcx> {
    let (own, qty) = perms_to_own_and_qty(perms, flags);
    let pointee_ty = local_ty;
    TypeDesc {
        own,
        qty,
        pointee_ty,
    }
}
