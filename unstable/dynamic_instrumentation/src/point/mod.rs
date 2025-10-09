pub mod apply;
pub mod build;
mod cast;
pub mod source;

use std::cmp::Ordering;

use c2rust_analysis_rt::mir_loc::{EventMetadata, FuncId};
use indexmap::{IndexMap, IndexSet};
use rustc_middle::{
    mir::{Body, HasLocalDecls, Local, LocalDecls, Location, Place, Rvalue},
    ty::TyCtxt,
};
use rustc_span::def_id::DefId;

use crate::{arg::InstrumentationArg, hooks::Hooks, util::Convert};

pub use apply::InstrumentationApplier;
pub use cast::cast_ptr_to_usize;

/// Sets the priority of an instrumentation point.
#[derive(PartialEq, Eq, Default, Copy, Clone)]
pub enum InstrumentationPriority {
    /// Signifies higher priority and implies that an instrumentation
    /// with early priority, will be placed before one with unspecified
    /// priority.
    Early,
    #[default]
    Unspecified,
}

impl Ord for InstrumentationPriority {
    fn cmp(&self, other: &Self) -> Ordering {
        use InstrumentationPriority::*;
        // We want instrumentations to be ordered early -> unspecified,
        // which shall correspond to ordered ascending.
        match (self, other) {
            (Early, Unspecified) => Ordering::Less,
            (Unspecified, Early) => Ordering::Greater,
            _ => Ordering::Equal,
        }
    }
}

impl PartialOrd for InstrumentationPriority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
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

/// Collects a set of all address-taken locals in a function body.
///
/// The set may include address-taken arguments.
pub struct CollectAddressTakenLocals<'a, 'tcx: 'a> {
    /// The set of address-taken locals.
    pub address_taken: IndexSet<Local>,
    tcx: TyCtxt<'tcx>,
    body: &'a Body<'tcx>,
}

impl<'a, 'tcx: 'a> CollectAddressTakenLocals<'a, 'tcx> {
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

/// Rewrites all address-taken locals in terms of their underlying address.
///
/// For example, if `_x` is an address-taken local and `_y` is the local
/// storing the address of `_x`, the statement `_z = _x` will be
/// rewritten as `_z = (*_y)`.
pub struct RewriteAddressTakenLocals<'tcx> {
    /// The set of address-taken locals.
    pub address_taken: IndexSet<Local>,
    /// A mapping from the address-taken local to the local
    /// storing the former's address.
    pub local_to_address: IndexMap<Local, Local>,
    tcx: TyCtxt<'tcx>,
}

impl<'tcx> RewriteAddressTakenLocals<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, address_taken: IndexSet<Local>) -> Self {
        Self {
            address_taken,
            local_to_address: IndexMap::new(),
            tcx,
        }
    }

    pub fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }
}

pub trait ProjectionSet {
    fn add_proj(&self, proj: Vec<usize>) -> u64;
}

pub struct CollectInstrumentationPoints<'a, 'tcx: 'a> {
    tcx: TyCtxt<'tcx>,
    hooks: Hooks<'tcx>,
    pub body: &'a Body<'tcx>,
    pub instrumentation_points: Vec<InstrumentationPoint<'tcx>>,
    assignment: Option<(Place<'tcx>, Rvalue<'tcx>)>,
    pub addr_taken_local_addresses: IndexMap<Local, Local>,
    pub projections: &'a dyn ProjectionSet,
}

impl<'a, 'tcx: 'a> CollectInstrumentationPoints<'a, 'tcx> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        hooks: Hooks<'tcx>,
        body: &'a Body<'tcx>,
        addr_taken_local_addresses: IndexMap<Local, Local>,
        projections: &'a dyn ProjectionSet,
    ) -> Self {
        Self {
            tcx,
            hooks,
            body,
            instrumentation_points: Default::default(),
            assignment: Default::default(),
            addr_taken_local_addresses,
            projections,
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

impl<'a, 'tcx: 'a> HasLocalDecls<'tcx> for CollectAddressTakenLocals<'a, 'tcx> {
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
