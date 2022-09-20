pub mod apply;
pub mod build;
mod cast;
pub mod source;

use c2rust_analysis_rt::mir_loc::{self, EventMetadata};
use indexmap::{IndexMap, IndexSet};
use rustc_middle::{
    mir::{Body, HasLocalDecls, Local, LocalDecls, Location, Place, Rvalue},
    ty::TyCtxt,
};
use rustc_span::def_id::DefId;

use crate::{arg::InstrumentationArg, hooks::Hooks, util::Convert};

pub use apply::InstrumentationApplier;
pub use cast::cast_ptr_to_usize;

pub struct InstrumentationPoint<'tcx> {
    id: usize,
    pub original_location: Location,
    pub instrumentation_location: Location,
    pub func: DefId,
    pub args: Vec<InstrumentationArg<'tcx>>,
    pub is_cleanup: bool,
    pub after_call: bool,
    pub metadata: EventMetadata,
}

/// Gathers a set of all address-taken locals in a function
/// body.
///
/// The set may include address-taken arguments.
pub struct CheckAddressTakenLocals<'a, 'tcx: 'a> {
    /// The set of address-taken locals.
    pub address_taken: IndexSet<Local>,
    tcx: TyCtxt<'tcx>,
    body: &'a Body<'tcx>,
}

impl<'a, 'tcx: 'a> CheckAddressTakenLocals<'a, 'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, body: &'a Body<'tcx>) -> Self {
        Self {
            address_taken: IndexSet::new(),
            tcx,
            body,
        }
    }

    pub fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }
}

/// Rewrites all address-taken locals in terms of their underlying
/// address.
///
/// For example, if `_1` is an address-taken local and `_3` is the local
/// storing the address of `_1`, the statement `_2 = _1` will be
/// rewritten as `_2 = (*_3)`.
pub struct RewriteAddressTakenLocals<'tcx> {
    /// The set of address-taken locals.
    pub address_taken: IndexSet<Local>,
    /// A mapping from the address-taken local to the local
    /// storing the former's address.
    pub local_substitute: IndexMap<Local, Local>,
    tcx: TyCtxt<'tcx>,
}

impl<'tcx> RewriteAddressTakenLocals<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, address_taken: IndexSet<Local>) -> Self {
        Self {
            address_taken,
            local_substitute: IndexMap::new(),
            tcx,
        }
    }

    pub fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }
}

pub struct CollectInstrumentationPoints<'a, 'tcx: 'a> {
    tcx: TyCtxt<'tcx>,
    hooks: Hooks<'tcx>,
    pub body: &'a Body<'tcx>,
    instrumentation_points: Vec<InstrumentationPoint<'tcx>>,
    assignment: Option<(Place<'tcx>, Rvalue<'tcx>)>,
    pub addr_taken_local_substitutions: IndexMap<Local, Local>,
}

impl<'a, 'tcx: 'a> CollectInstrumentationPoints<'a, 'tcx> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        hooks: Hooks<'tcx>,
        body: &'a Body<'tcx>,
        addr_taken_local_substitutions: IndexMap<Local, Local>,
    ) -> Self {
        Self {
            tcx,
            hooks,
            body,
            instrumentation_points: Default::default(),
            assignment: Default::default(),
            addr_taken_local_substitutions,
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

impl<'a, 'tcx: 'a> HasLocalDecls<'tcx> for CheckAddressTakenLocals<'a, 'tcx> {
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

    pub fn func_hash(&self) -> mir_loc::DefPathHash {
        self.tcx()
            .def_path_hash(self.body.source.def_id())
            .convert()
    }
}
