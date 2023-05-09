use crate::context::{FlagSet, PermissionSet};
use rustc_middle::mir::Mutability;
use rustc_middle::ty::{AdtDef, Ty, TyCtxt, TyKind, TypeAndMut};

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

/// Obtain the `TypeDesc` for a pointer.  `ptr_ty` should be the `Ty` of the pointer, and `perms`
/// and `flags` should be taken from its outermost `PointerId`.
pub fn perms_to_desc<'tcx>(
    ptr_ty: Ty<'tcx>,
    perms: PermissionSet,
    flags: FlagSet,
) -> TypeDesc<'tcx> {
    // The FIXED case should be handled by calling `perm_to_desc_with_pointee` instead.
    assert!(
        !flags.contains(FlagSet::FIXED),
        "building TypeDesc for FIXED pointer requires a related pointee type"
    );

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

/// Obtain the `TypeDesc` for a pointer to a local.  `local_ty` should be the `Ty` of the local
/// itself, and `perms` and `flags` should be taken from its `addr_of_local` `PointerId`.
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

pub fn perms_to_desc_with_pointee<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointee_ty: Ty<'tcx>,
    ptr_ty: Ty<'tcx>,
    perms: PermissionSet,
    flags: FlagSet,
) -> TypeDesc<'tcx> {
    let (own, qty) = if flags.contains(FlagSet::FIXED) {
        unpack_pointer_type(tcx, ptr_ty, pointee_ty)
    } else {
        perms_to_own_and_qty(perms, flags)
    };
    TypeDesc {
        own,
        qty,
        pointee_ty,
    }
}

/// Unpack an existing `Ty` into its ownership, quantity, and pointee type.
pub fn unpack_pointer_type<'tcx>(
    tcx: TyCtxt<'tcx>,
    ty: Ty<'tcx>,
    pointee_ty: Ty<'tcx>,
) -> (Ownership, Quantity) {
    #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
    enum Step {
        Ref(Mutability),
        RawPtr(Mutability),
        Cell,
        Box,
        Rc,
        Slice,
        OffsetPtr,
        Array,
    }

    let mut steps = Vec::new();
    let mut cur_ty = ty;
    while cur_ty != pointee_ty {
        match *cur_ty.kind() {
            TyKind::Ref(_, inner_ty, mutbl) => {
                steps.push(Step::Ref(mutbl));
                cur_ty = inner_ty;
            }
            TyKind::RawPtr(tm) => {
                steps.push(Step::RawPtr(tm.mutbl));
                cur_ty = tm.ty;
            }
            TyKind::Adt(adt_def, substs) if adt_def.is_box() => {
                steps.push(Step::Box);
                cur_ty = substs.type_at(0);
            }
            TyKind::Adt(adt_def, substs) if is_rc(tcx, adt_def) => {
                steps.push(Step::Rc);
                cur_ty = substs.type_at(0);
            }
            TyKind::Adt(adt_def, substs) if is_cell(tcx, adt_def) => {
                steps.push(Step::Cell);
                cur_ty = substs.type_at(0);
            }
            TyKind::Adt(adt_def, substs) if is_offset_ptr(tcx, adt_def) => {
                steps.push(Step::OffsetPtr);
                cur_ty = substs.type_at(0);
            }

            TyKind::Slice(inner_ty) => {
                steps.push(Step::Slice);
                cur_ty = inner_ty;
            }
            TyKind::Array(inner_ty, _) => {
                steps.push(Step::Array);
                cur_ty = inner_ty;
            }

            _ => panic!(
                "failed to deconstruct {:?} as a pointer to {:?}: unexpected {:?}",
                ty,
                pointee_ty,
                cur_ty.kind()
            ),
        }
    }

    let mut i = 0;
    let next = |i: &mut usize| -> Option<&Step> {
        let s = steps.get(*i);
        *i += 1;
        s
    };

    let own = match next(&mut i) {
        Some(&Step::Ref(Mutability::Not)) => {
            if let Some(&Step::Cell) = steps.get(i) {
                i += 1;
                Ownership::Cell
            } else {
                Ownership::Imm
            }
        }
        Some(&Step::Ref(Mutability::Mut)) => Ownership::Mut,
        Some(&Step::RawPtr(Mutability::Not)) => Ownership::Raw,
        Some(&Step::RawPtr(Mutability::Mut)) => Ownership::RawMut,
        Some(&Step::Box) => Ownership::Box,
        Some(&Step::Rc) => Ownership::Rc,
        _ => {
            // Un-consume this step.
            i -= 1;
            panic!(
                "failed to deconstruct {:?} as a pointer to {:?}: \
                    steps {:?} don't start with a pointer",
                ty,
                pointee_ty,
                &steps[i - 1..]
            );
        }
    };

    let qty = match next(&mut i) {
        Some(&Step::Slice) => Quantity::Slice,
        Some(&Step::OffsetPtr) => Quantity::OffsetPtr,
        Some(&Step::Array) => Quantity::Array,
        _ => {
            // Un-consume this step.
            i -= 1;
            Quantity::Single
        }
    };

    assert!(
        i == steps.len(),
        "failed to deconstruct {:?} as a pointer to {:?}: got extra steps {:?}",
        ty,
        pointee_ty,
        &steps[i..],
    );

    (own, qty)
}

/// If `ty` is `Cell<T>`, returns `Some(T)`.  Otherwise, returns `None`.
fn unpack_cell<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Option<Ty<'tcx>> {
    match *ty.kind() {
        TyKind::Adt(adt_def, substs) if is_cell(tcx, adt_def) => Some(substs.type_at(0)),
        _ => None,
    }
}

/// Returns `true` if `adt_def` is the type `std::cell::Cell`.
fn is_cell<'tcx>(tcx: TyCtxt<'tcx>, adt_def: AdtDef<'tcx>) -> bool {
    // TODO
    false
}

/// Returns `true` if `adt_def` is the type `std::rc::Rc`.
fn is_rc<'tcx>(tcx: TyCtxt<'tcx>, adt_def: AdtDef<'tcx>) -> bool {
    // TODO
    false
}

/// Returns `true` if `adt_def` is the type `OffsetPtr` from the C2Rust support library.
fn is_offset_ptr<'tcx>(tcx: TyCtxt<'tcx>, adt_def: AdtDef<'tcx>) -> bool {
    // TODO
    false
}
