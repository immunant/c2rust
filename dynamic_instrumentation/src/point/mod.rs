pub mod apply;
pub mod build;
mod cast;
pub mod source;

use c2rust_analysis_rt::mir_loc::{EventMetadata, FuncId};
use rustc_middle::{
    mir::{Body, HasLocalDecls, LocalDecls, Location, Place, Rvalue},
    ty::TyCtxt,
};
use rustc_span::def_id::DefId;

use crate::{arg::InstrumentationArg, hooks::Hooks, util::Convert};

pub use apply::InstrumentationApplier;
pub use cast::cast_ptr_to_usize;

/// Sets the priority of an instrumentation point.
#[derive(PartialEq, Eq, Ord, Default, Copy, Clone)]
pub enum InstrumentationPriority {
    /// Signifies higher priority and implies that an instrumentation
    /// with early priority, will be placed before one with unspecified
    /// priority.
    Early,
    #[default]
    Unspecified,
}

impl PartialOrd for InstrumentationPriority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use InstrumentationPriority::*;
        // We want instrumentations to be ordered early -> unspecified,
        // which shall correspond to ordered ascending.
        match (self, other) {
            (Early, Unspecified) => Some(std::cmp::Ordering::Less),
            (Unspecified, Early) => Some(std::cmp::Ordering::Greater),
            _ => Some(std::cmp::Ordering::Equal),
        }
    }
}

pub struct InstrumentationPoint<'tcx> {
    id: usize,
    pub original_location: Location,
    pub instrumentation_location: Location,
    pub func: DefId,
    pub args: Vec<InstrumentationArg<'tcx>>,
    pub is_cleanup: bool,
    pub after_call: bool,
    pub instrumentation_priority: InstrumentationPriority,
    pub metadata: EventMetadata,
}

pub struct CollectInstrumentationPoints<'a, 'tcx: 'a> {
    tcx: TyCtxt<'tcx>,
    hooks: Hooks<'tcx>,
    body: &'a Body<'tcx>,
    instrumentation_points: Vec<InstrumentationPoint<'tcx>>,
    assignment: Option<(Place<'tcx>, Rvalue<'tcx>)>,
}

impl<'a, 'tcx: 'a> CollectInstrumentationPoints<'a, 'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, hooks: Hooks<'tcx>, body: &'a Body<'tcx>) -> Self {
        Self {
            tcx,
            hooks,
            body,
            instrumentation_points: Default::default(),
            assignment: Default::default(),
        }
    }

    pub fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }
}

impl<'a, 'tcx: 'a> HasLocalDecls<'tcx> for CollectInstrumentationPoints<'a, 'tcx> {
    fn local_decls(&self) -> &'a LocalDecls<'tcx> {
        self.body.local_decls()
    }
}

impl<'a, 'tcx: 'a> CollectInstrumentationPoints<'a, 'tcx> {
    pub fn hooks(&self) -> &Hooks<'tcx> {
        &self.hooks
    }

    pub fn assignment(&self) -> Option<&(Place<'tcx>, Rvalue<'tcx>)> {
        self.assignment.as_ref()
    }

    pub fn with_assignment(
        &mut self,
        assignment: (Place<'tcx>, Rvalue<'tcx>),
        f: impl Fn(&mut Self),
    ) {
        let old_assignment = self.assignment.replace(assignment);
        f(self);
        self.assignment = old_assignment;
    }

    pub fn func_id(&self) -> FuncId {
        FuncId(
            self.tcx()
                .def_path_hash(self.body.source.def_id())
                .convert(),
        )
    }
}
