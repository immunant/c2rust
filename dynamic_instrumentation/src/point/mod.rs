pub mod apply;
pub mod build;
pub mod cast;
pub mod source;

use c2rust_analysis_rt::mir_loc::{self, EventMetadata};
use rustc_middle::{
    mir::{Body, HasLocalDecls, LocalDecls, Location, Place, Rvalue},
    ty::TyCtxt,
};
use rustc_span::def_id::DefId;

use crate::{arg::InstrumentationArg, hooks::Hooks, util::Convert};

pub use apply::InstrumentationApplier;

pub struct InstrumentationPoint<'tcx> {
    id: usize,
    pub loc: Location,
    pub func: DefId,
    pub args: Vec<InstrumentationArg<'tcx>>,
    pub is_cleanup: bool,
    pub after_call: bool,
    pub metadata: EventMetadata,
}

pub struct InstrumentationAdder<'a, 'tcx: 'a> {
    tcx: TyCtxt<'tcx>,
    hooks: Hooks<'tcx>,
    body: &'a Body<'tcx>,
    instrumentation_points: Vec<InstrumentationPoint<'tcx>>,
    assignment: Option<(Place<'tcx>, Rvalue<'tcx>)>,
}

impl<'a, 'tcx: 'a> InstrumentationAdder<'a, 'tcx> {
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

impl<'a, 'tcx: 'a> HasLocalDecls<'tcx> for InstrumentationAdder<'a, 'tcx> {
    fn local_decls(&self) -> &'a LocalDecls<'tcx> {
        self.body.local_decls()
    }
}

impl<'a, 'tcx: 'a> InstrumentationAdder<'a, 'tcx> {
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

    pub fn func_hash(&self) -> mir_loc::DefPathHash {
        self.tcx()
            .def_path_hash(self.body.source.def_id())
            .convert()
    }
}
