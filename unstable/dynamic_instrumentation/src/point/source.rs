use c2rust_analysis_rt::mir_loc::MirPlace;
use rustc_middle::mir::{Local, Operand, Place, Rvalue};

use crate::{mir_utils::rv_place, util::Convert};

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
