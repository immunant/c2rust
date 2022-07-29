use c2rust_analysis_rt::mir_loc::MirPlace;
use rustc_middle::mir::{Local, Operand, Place, Rvalue};

use crate::util::Convert;

pub trait Source {
    fn source(&self) -> Option<MirPlace>;
}

impl Source for Place<'_> {
    fn source(&self) -> Option<MirPlace> {
        Some(self.convert())
    }
}

impl Source for Operand<'_> {
    fn source(&self) -> Option<MirPlace> {
        self.place().as_ref().and_then(Place::source)
    }
}

impl Source for Vec<Operand<'_>> {
    fn source(&self) -> Option<MirPlace> {
        // TODO: have hook-specific sources
        self.first().and_then(Operand::source)
    }
}

impl Source for Rvalue<'_> {
    fn source(&self) -> Option<MirPlace> {
        rv_place(self).as_ref().and_then(Place::source)
    }
}

impl Source for Local {
    fn source(&self) -> Option<MirPlace> {
        Place::from(*self).source()
    }
}

impl Source for u32 {
    fn source(&self) -> Option<MirPlace> {
        Local::from_u32(*self).source()
    }
}

/// Get the one and only input [`Place`], if applicable.
fn rv_place<'tcx>(rv: &'tcx Rvalue) -> Option<Place<'tcx>> {
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
