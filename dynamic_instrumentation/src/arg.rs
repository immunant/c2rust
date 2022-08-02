use rustc_middle::{mir::Operand, ty};

#[derive(Clone, Debug)]
pub enum ArgKind<'tcx> {
    AddressUsize(Operand<'tcx>),
    Reference(Operand<'tcx>),
    RawPtr(Operand<'tcx>),
}

impl<'tcx> ArgKind<'tcx> {
    pub fn inner(&self) -> &Operand<'tcx> {
        use ArgKind::*;
        match self {
            AddressUsize(x) => x,
            Reference(x) => x,
            RawPtr(x) => x,
        }
    }

    pub fn from_type(op: Operand<'tcx>, ty: &ty::Ty<'tcx>) -> Self {
        use ArgKind::*;
        (if ty.is_unsafe_ptr() {
            RawPtr
        } else if ty.is_region_ptr() {
            Reference
        } else if ty.is_integral() {
            AddressUsize
        } else {
            let kind = ty.kind();
            panic!("operand is not of integer-castable type: op = {op:?}, ty = {ty:?}, ty.kind(): {kind:?}")
        })(op)
    }
}

#[derive(Clone, Debug)]
pub enum InstrumentationArg<'tcx> {
    AddrOf(Operand<'tcx>),
    Op(ArgKind<'tcx>),
}

impl<'tcx> InstrumentationArg<'tcx> {
    pub fn inner(&self) -> &Operand<'tcx> {
        use InstrumentationArg::*;
        match self {
            AddrOf(x) => x,
            Op(x) => x.inner(),
        }
    }
}
