use crate::context::{FlagSet, PermissionSet};
use rustc_middle::mir::Mutability;
use rustc_middle::ty::{AdtDef, Ty, TyCtxt, TyKind};

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
    pub option: bool,
    pub pointee_ty: Ty<'tcx>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct PtrDesc {
    pub own: Ownership,
    pub qty: Quantity,
    pub option: bool,
}

impl<'tcx> From<TypeDesc<'tcx>> for PtrDesc {
    fn from(x: TypeDesc<'tcx>) -> PtrDesc {
        let TypeDesc {
            own,
            qty,
            option,
            pointee_ty: _,
        } = x;
        PtrDesc { own, qty, option }
    }
}

impl PtrDesc {
    pub fn to_type_desc<'tcx>(self, pointee_ty: Ty<'tcx>) -> TypeDesc<'tcx> {
        let PtrDesc { own, qty, option } = self;
        TypeDesc {
            own,
            qty,
            option,
            pointee_ty,
        }
    }
}

impl Ownership {
    pub fn is_copy(&self) -> bool {
        match *self {
            Ownership::Raw | Ownership::RawMut | Ownership::Imm | Ownership::Cell => true,
            Ownership::Mut | Ownership::Rc | Ownership::Box => false,
        }
    }
}

fn perms_to_ptr_desc(perms: PermissionSet, flags: FlagSet) -> PtrDesc {
    let own = if perms.contains(PermissionSet::FREE) {
        Ownership::Box
    } else if perms.contains(PermissionSet::UNIQUE | PermissionSet::WRITE) {
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

    let option = !perms.contains(PermissionSet::NON_NULL);

    PtrDesc { own, qty, option }
}

/// Obtain the `TypeDesc` for a pointer.  `ptr_ty` should be the `Ty` of the pointer, and `perms`
/// and `flags` should be taken from its outermost `PointerId`.
pub fn perms_to_desc(ptr_ty: Ty, perms: PermissionSet, flags: FlagSet) -> TypeDesc {
    // The FIXED case should be handled by calling `perm_to_desc_with_pointee` instead.
    assert!(
        !flags.contains(FlagSet::FIXED),
        "building TypeDesc for FIXED pointer requires a related pointee type"
    );

    let ptr_desc = perms_to_ptr_desc(perms, flags);

    let pointee_ty = match *ptr_ty.kind() {
        TyKind::Ref(_, ty, _) => ty,
        TyKind::RawPtr(mt) => mt.ty,
        TyKind::Adt(adt_def, substs) if adt_def.is_box() => substs.type_at(0),
        // TODO: other ADTs, e.g. `Rc`
        _ => panic!("expected a pointer type, but got {:?}", ptr_ty),
    };

    ptr_desc.to_type_desc(pointee_ty)
}

/// Obtain the `TypeDesc` for a pointer to a local.  `local_ty` should be the `Ty` of the local
/// itself, and `perms` and `flags` should be taken from its `addr_of_local` `PointerId`.
pub fn local_perms_to_desc(local_ty: Ty, perms: PermissionSet, flags: FlagSet) -> TypeDesc {
    let ptr_desc = perms_to_ptr_desc(perms, flags);
    let pointee_ty = local_ty;
    ptr_desc.to_type_desc(pointee_ty)
}

pub fn perms_to_desc_with_pointee<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointee_ty: Ty<'tcx>,
    ptr_ty: Ty<'tcx>,
    perms: PermissionSet,
    flags: FlagSet,
) -> TypeDesc<'tcx> {
    let ptr_desc = if flags.contains(FlagSet::FIXED) {
        unpack_pointer_type(tcx, ptr_ty, pointee_ty)
    } else {
        perms_to_ptr_desc(perms, flags)
    };
    ptr_desc.to_type_desc(pointee_ty)
}

/// Unpack an existing `Ty` into its ownership and quantity.  The pointee type must already be
/// known.  Panics if there are no `Ownership` and `Quantity` that combine with `pointee_ty` to
/// produce `ty`.
pub fn unpack_pointer_type<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>, pointee_ty: Ty<'tcx>) -> PtrDesc {
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
        Option,
    }

    let mut steps = Vec::new();
    let mut cur_ty = ty;
    while cur_ty != pointee_ty {
        let (step, new_ty) = match *cur_ty.kind() {
            TyKind::Ref(_, inner_ty, mutbl) => (Step::Ref(mutbl), inner_ty),
            TyKind::RawPtr(tm) => (Step::RawPtr(tm.mutbl), tm.ty),
            TyKind::Adt(adt_def, substs) if adt_def.is_box() => (Step::Box, substs.type_at(0)),
            TyKind::Adt(adt_def, substs) if is_rc(tcx, adt_def) => (Step::Rc, substs.type_at(0)),
            TyKind::Adt(adt_def, substs) if is_cell(tcx, adt_def) => {
                (Step::Cell, substs.type_at(0))
            }
            TyKind::Adt(adt_def, substs) if is_option(tcx, adt_def) => {
                (Step::Option, substs.type_at(0))
            }
            TyKind::Adt(adt_def, substs) if is_offset_ptr(tcx, adt_def) => {
                (Step::OffsetPtr, substs.type_at(0))
            }
            TyKind::Slice(inner_ty) => (Step::Slice, inner_ty),
            TyKind::Array(inner_ty, _) => (Step::Array, inner_ty),

            _ => panic!(
                "failed to deconstruct {:?} as a pointer to {:?}: unexpected {:?}",
                ty,
                pointee_ty,
                cur_ty.kind()
            ),
        };
        steps.push(step);
        cur_ty = new_ty;
    }

    // Parse the sequence `steps` to extract the ownership and quantity.
    let mut i = 0;
    let mut eat = |s: Step| -> bool {
        if steps.get(i) == Some(&s) {
            i += 1;
            true
        } else {
            false
        }
    };

    // This logic is roughly the inverse of that in `rewrite::ty::mk_rewritten_ty`.
    let option = eat(Step::Option);

    let mut own = if eat(Step::Ref(Mutability::Not)) {
        Ownership::Imm
    } else if eat(Step::Ref(Mutability::Mut)) {
        Ownership::Mut
    } else if eat(Step::RawPtr(Mutability::Not)) {
        Ownership::Raw
    } else if eat(Step::RawPtr(Mutability::Mut)) {
        Ownership::RawMut
    } else if eat(Step::Box) {
        Ownership::Box
    } else if eat(Step::Rc) {
        Ownership::Rc
    } else {
        panic!(
            "failed to deconstruct {:?} as a pointer to {:?}: \
                steps {:?} don't start with a pointer",
            ty,
            pointee_ty,
            &steps[i..]
        );
    };

    let qty = if eat(Step::Slice) {
        Quantity::Slice
    } else if eat(Step::OffsetPtr) {
        Quantity::OffsetPtr
    } else if eat(Step::Array) {
        Quantity::Array
    } else {
        Quantity::Single
    };

    // Note that e.g. Slice + Cell means `&[Cell<T>]`, not `&Cell<[T]>`.
    if own == Ownership::Imm && eat(Step::Cell) {
        own = Ownership::Cell;
    }

    assert!(
        i == steps.len(),
        "failed to deconstruct {:?} as a pointer to {:?}: got extra steps {:?}",
        ty,
        pointee_ty,
        &steps[i..],
    );

    PtrDesc { own, qty, option }
}

/// Returns `true` if `adt_def` is the type `std::cell::Cell`.
fn is_cell<'tcx>(_tcx: TyCtxt<'tcx>, _adt_def: AdtDef<'tcx>) -> bool {
    // TODO
    false
}

/// Returns `true` if `adt_def` is the type `std::option::Option`.
fn is_option<'tcx>(_tcx: TyCtxt<'tcx>, _adt_def: AdtDef<'tcx>) -> bool {
    // TODO
    false
}

/// Returns `true` if `adt_def` is the type `std::rc::Rc`.
fn is_rc<'tcx>(_tcx: TyCtxt<'tcx>, _adt_def: AdtDef<'tcx>) -> bool {
    // TODO
    false
}

/// Returns `true` if `adt_def` is the type `OffsetPtr` from the C2Rust support library.
fn is_offset_ptr<'tcx>(_tcx: TyCtxt<'tcx>, _adt_def: AdtDef<'tcx>) -> bool {
    // TODO
    false
}
