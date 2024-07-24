use rustc_middle::{
    mir::{Constant, ConstantKind, Local, Operand, Place, PlaceRef},
    ty::{self, ParamEnv, TyCtxt},
};
use rustc_span::DUMMY_SP;

/// Types that can be passed as an argument to instrumentation hook functions.
pub trait IntoOperand<'tcx> {
    fn op(self, tcx: TyCtxt<'tcx>) -> Operand<'tcx>;
}

impl<'tcx> IntoOperand<'tcx> for Place<'tcx> {
    fn op(self, _tcx: TyCtxt<'tcx>) -> Operand<'tcx> {
        Operand::Copy(self)
    }
}

impl<'tcx> IntoOperand<'tcx> for PlaceRef<'tcx> {
    fn op(self, tcx: TyCtxt<'tcx>) -> Operand<'tcx> {
        let place = Place {
            local: self.local,
            projection: tcx.mk_place_elems(self.projection.iter()),
        };
        place.op(tcx)
    }
}

impl<'tcx> IntoOperand<'tcx> for u32 {
    fn op(self, tcx: TyCtxt<'tcx>) -> Operand<'tcx> {
        make_const(tcx, self)
    }
}

impl<'tcx> IntoOperand<'tcx> for usize {
    fn op(self, tcx: TyCtxt<'tcx>) -> Operand<'tcx> {
        Operand::Constant(Box::new(Constant {
            span: DUMMY_SP,
            user_ty: None,
            literal: ConstantKind::Ty(ty::Const::from_bits(
                tcx,
                self.try_into().unwrap(),
                ParamEnv::empty().and(tcx.types.usize),
            )),
        }))
    }
}

impl<'tcx> IntoOperand<'tcx> for Local {
    fn op(self, tcx: TyCtxt<'tcx>) -> Operand<'tcx> {
        Place::from(self).op(tcx)
    }
}

impl<'tcx> IntoOperand<'tcx> for Operand<'tcx> {
    fn op(self, _tcx: TyCtxt<'tcx>) -> Self {
        self
    }
}

fn make_const(tcx: TyCtxt, idx: u32) -> Operand {
    Operand::Constant(Box::new(Constant {
        span: DUMMY_SP,
        user_ty: None,
        literal: ConstantKind::Ty(ty::Const::from_bits(
            tcx,
            idx.into(),
            ParamEnv::empty().and(tcx.types.u32),
        )),
    }))
}
