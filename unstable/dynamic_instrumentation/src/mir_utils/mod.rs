use rustc_middle::mir::{Place, Rvalue};

mod deref;

pub use deref::*;

/// Get the one and only input [`Place`], if applicable.
pub fn rv_place<'tcx>(rv: &Rvalue<'tcx>) -> Option<Place<'tcx>> {
    use Rvalue::*;
    match rv {
        Use(op) => op.place(),
        Repeat(op, _) => op.place(),
        Ref(_, _, p) => Some(*p),
        // ThreadLocalRef
        AddressOf(_, p) => Some(*p),
        Len(p) => Some(*p),
        Cast(_, op, _) => op.place(),
        // BinaryOp
        // CheckedBinaryOp
        // NullaryOp
        UnaryOp(_, op) => op.place(),
        Discriminant(p) => Some(*p),
        // Aggregate
        ShallowInitBox(op, _) => op.place(),
        _ => None,
    }
}
