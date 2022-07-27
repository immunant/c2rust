use c2rust_analysis_rt::mir_loc::{self, EventMetadata};
use rustc_middle::{
    mir::{Body, HasLocalDecls, LocalDecls, Location, Place, Rvalue},
    ty::TyCtxt,
};
use rustc_span::def_id::DefId;

use crate::{
    arg::InstrumentationArg, hooks::Hooks, util::Convert,
};

#[derive(Clone)]
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
    hooks: Hooks<'tcx>,
    body: &'a Body<'tcx>,
    instrumentation_points: Vec<InstrumentationPoint<'tcx>>,
    assignment: Option<(Place<'tcx>, Rvalue<'tcx>)>,
}

impl<'a, 'tcx: 'a> InstrumentationAdder<'a, 'tcx> {
    pub fn new(hooks: Hooks<'tcx>, body: &'a Body<'tcx>) -> Self {
        Self {
            hooks,
            body,
            instrumentation_points: Default::default(),
            assignment: Default::default(),
        }
    }

    pub fn tcx(&self) -> TyCtxt<'tcx> {
        self.hooks.tcx()
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
        self.tcx().def_path_hash(self.body.source.def_id()).convert()
    }
}

pub mod build {
    use c2rust_analysis_rt::mir_loc::{EventMetadata, TransferKind};
    use rustc_index::vec::Idx;
    use rustc_middle::{mir::{Location, Body, Place}, ty::TyCtxt};
    use rustc_span::def_id::DefId;

    use crate::{arg::{InstrumentationArg, ArgKind}, into_operand::IntoOperand, source::Source, util::Convert};

    use super::{InstrumentationAdder, InstrumentationPoint};

    #[derive(Default)]
    struct InstrumentationPointBuilder<'tcx> {
        pub args: Vec<InstrumentationArg<'tcx>>,
        pub is_cleanup: bool,
        pub after_call: bool,
        pub metadata: EventMetadata,
    }
    
    impl<'tcx> InstrumentationAdder<'_, 'tcx> {
        fn add(&mut self, point: InstrumentationPointBuilder<'tcx>, loc: Location, func: DefId) {
            let id = self.instrumentation_points.len();
            let InstrumentationPointBuilder {
                args,
                is_cleanup,
                after_call,
                metadata,
            } = point;
            self.instrumentation_points.push(InstrumentationPoint {
                id,
                loc,
                func,
                args,
                is_cleanup,
                after_call,
                metadata,
            });
        }
    }
    
    pub struct InstrumentationBuilder<'a, 'tcx: 'a> {
        tcx: TyCtxt<'tcx>,
        body: &'a Body<'tcx>,
        loc: Location,
        func: DefId,
        point: InstrumentationPointBuilder<'tcx>,
    }
    
    impl<'a, 'tcx: 'a> InstrumentationAdder<'a, 'tcx> {
        pub fn loc(&self, loc: Location, func: DefId) -> InstrumentationBuilder<'a, 'tcx> {
            InstrumentationBuilder {
                tcx: self.tcx(),
                body: self.body,
                loc,
                func,
                point: Default::default(),
            }
        }
    
        pub fn into_instrumentation_points(mut self) -> Vec<InstrumentationPoint<'tcx>> {
            // Sort by reverse location so that we can split blocks without
            // perturbing future statement indices
            let key = |p: &InstrumentationPoint| (p.loc, p.after_call, p.id);
            self.instrumentation_points
                .sort_unstable_by(|a, b| key(a).cmp(&key(b)).reverse());
            self.instrumentation_points
        }
    }
    
    impl<'tcx> InstrumentationBuilder<'_, 'tcx> {
        /// Add an argument to this [`InstrumentationPoint`].
        pub fn arg_var(mut self, arg: impl IntoOperand<'tcx>) -> Self {
            let op = arg.op(self.tcx);
            let op_ty = op.ty(self.body, self.tcx);
            self.point
                .args
                .push(InstrumentationArg::Op(ArgKind::from_type(op, &op_ty)));
            self
        }
    
        /// Add multiple arguments to this [`InstrumentationPoint`], using `Self::arg_var`.
        pub fn arg_vars(mut self, args: impl IntoIterator<Item = impl IntoOperand<'tcx>>) -> Self {
            for arg in args {
                self = self.arg_var(arg);
            }
            self
        }
    
        /// Add an argument to this [`InstrumentationPoint`] that is the index of the argument.
        ///
        /// TODO(kkysen, aneksteind) Currently `Idx`/`u32` types are the only types we support passing as arguments as is,
        /// but we eventually want to be able to pass other serializable types as well.
        pub fn arg_index_of(self, arg: impl Idx) -> Self {
            let index: u32 = arg.index().try_into()
                .expect("`rustc_index::vec::newtype_index!` should use `u32` as the underlying index type, so this shouldn't fail unless that changes");
            self.arg_var(index)
        }
    
        /// Add an argument to this [`InstrumentationPoint`] that is the address of the argument.
        pub fn arg_addr_of(mut self, arg: impl IntoOperand<'tcx>) -> Self {
            let op = arg.op(self.tcx);
            self.point.args.push(InstrumentationArg::AddrOf(op));
            self
        }
    
        pub fn after_call(mut self) -> Self {
            self.point.after_call = true;
            self
        }
    
        pub fn source<S: Source>(mut self, source: &S) -> Self {
            self.point.metadata.source = source.source();
            self
        }
    
        pub fn dest(mut self, p: &Place) -> Self {
            self.point.metadata.destination = Some(p.convert());
            self
        }
    
        pub fn dest_from<F>(mut self, f: F) -> Self
        where
            F: Fn() -> Option<Place<'tcx>>,
        {
            if let Some(p) = f() {
                self.point.metadata.destination = Some(p.convert());
            }
            self
        }
    
        pub fn transfer(mut self, transfer_kind: TransferKind) -> Self {
            self.point.metadata.transfer_kind = transfer_kind;
            self
        }
    
        /// Queue insertion of a call to [`func`].
        ///
        /// The call will be inserted before the statement
        /// at index [`statement_idx`] in `block`.
        /// If [`statement_idx`] is the number of statements in the block,
        /// the call will be inserted at the end of the block.
        ///
        /// [`func`] must not unwind, as it will have no cleanup destination.
        ///
        /// [`func`]: InstrumentationPoint::func
        /// [`statement_idx`]: Location::statement_index
        pub fn add_to(self, adder: &mut InstrumentationAdder<'_, 'tcx>) {
            adder.add(self.point, self.loc, self.func);
        }
    }    
}
