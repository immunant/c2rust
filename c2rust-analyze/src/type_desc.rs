use crate::context::{FlagSet, PermissionSet};

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

#[allow(dead_code)]
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

    #[allow(clippy::if_same_then_else)]
    let qty = if perms.contains(PermissionSet::OFFSET_SUB) {
        Quantity::OffsetPtr
    } else if perms.contains(PermissionSet::OFFSET_ADD) {
        Quantity::Slice
    } else {
        Quantity::Single
    };

    (own, qty)
}
