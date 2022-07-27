use c2rust_analysis_rt::mir_loc::{self, EventMetadata};
use rustc_middle::{
    mir::{Body, HasLocalDecls, LocalDecls, Location, Place, Rvalue},
    ty::TyCtxt,
};
use rustc_span::def_id::DefId;

use crate::{arg::InstrumentationArg, hooks::Hooks, util::Convert};

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
        self.tcx()
            .def_path_hash(self.body.source.def_id())
            .convert()
    }
}

pub mod build {
    use c2rust_analysis_rt::mir_loc::{EventMetadata, TransferKind};
    use rustc_index::vec::Idx;
    use rustc_middle::{
        mir::{Body, Location, Place},
        ty::TyCtxt,
    };
    use rustc_span::def_id::DefId;

    use crate::{
        arg::{ArgKind, InstrumentationArg},
        into_operand::IntoOperand,
        source::Source,
        util::Convert,
    };

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

pub mod apply {
    use std::mem;

    use c2rust_analysis_rt::mir_loc::TransferKind;
    use rustc_middle::{
        mir::{Body, Operand, TerminatorKind},
        ty::TyCtxt,
    };
    use rustc_span::def_id::{DefId, DefPathHash};

    use crate::{
        arg::{ArgKind, InstrumentationArg},
        cast::cast_ptr_to_usize,
        instrument::{Instrumenter, insert_call},
        into_operand::IntoOperand,
        util::Convert,
    };

    use super::InstrumentationPoint;

    pub struct InstrumentationApplier<'tcx, 'a> {
        state: &'a Instrumenter,
        tcx: TyCtxt<'tcx>,
        body: &'a mut Body<'tcx>,
        body_def: DefPathHash,
    }

    impl<'tcx, 'a> InstrumentationApplier<'tcx, 'a> {
        pub fn new(
            state: &'a Instrumenter,
            tcx: TyCtxt<'tcx>,
            body: &'a mut Body<'tcx>,
            body_did: DefId,
        ) -> Self {
            let body_def = tcx.def_path_hash(body_did);
            Self {
                state,
                tcx,
                body,
                body_def,
            }
        }
    }

    impl<'tcx, 'a> InstrumentationApplier<'tcx, 'a> {
        /// Rewrite the [`body`](Self::body) to apply the specified [`InstrumentationPoint`].
        fn apply_point(&mut self, point: &InstrumentationPoint<'tcx>) {
            let &mut InstrumentationApplier {
                state,
                tcx,
                ref mut body,
                body_def,
            } = self;
            let &InstrumentationPoint {
                loc,
                func,
                ref args,
                is_cleanup,
                after_call,
                ref metadata,
                ..
            } = point;
            let mut args = args.clone();

            if let TransferKind::Arg(def_path_hash) = metadata.transfer_kind {
                let callee_id = tcx.def_path_hash_to_def_id(def_path_hash.convert(), &mut || {
                    panic!("cannot find DefId of callee func hash")
                });
                state.add_fn(callee_id, tcx);
            }

            // Add the MIR location as the first argument to the instrumentation function
            let loc_idx = state.get_mir_loc_idx(body_def, loc, metadata.clone());
            args.insert(
                0,
                InstrumentationArg::Op(ArgKind::AddressUsize(loc_idx.op(tcx))),
            );

            let (blocks, locals) = body.basic_blocks_and_local_decls_mut();
            let mut extra_statements = None;
            if after_call {
                let call = blocks[loc.block].terminator_mut();
                let ret_value = if let TerminatorKind::Call {
                    destination: Some((place, _next_block)),
                    args,
                    ..
                } = &mut call.kind
                {
                    // Make the call operands copies so we don't reuse a moved value
                    args.iter_mut().for_each(|arg| *arg = arg.to_copy());

                    let place_ty = &place.ty(locals, tcx).ty;
                    // The return type of a hooked fn is always a raw ptr, reference, or unit
                    if place_ty.is_unit() {
                        // It's somewhat wrong to call unit an AddressUsize, but it has the pass-through
                        // semantics we want
                        InstrumentationArg::Op(ArgKind::AddressUsize(Operand::Copy(*place)))
                    } else {
                        assert!(place_ty.is_unsafe_ptr());
                        InstrumentationArg::Op(ArgKind::RawPtr(Operand::Copy(*place)))
                    }
                } else {
                    panic!(
                        "Expected a call terminator in block to instrument, found: {:?}",
                        call
                    );
                };

                // push return value to argument list
                if let Some((casts, cast_local)) = cast_ptr_to_usize(tcx, locals, &ret_value) {
                    extra_statements = Some(casts);
                    args.push(InstrumentationArg::Op(ArgKind::AddressUsize(cast_local)));
                } else {
                    args.push(ret_value);
                }
            }

            let (successor_block, _) =
                insert_call(tcx, *body, loc.block, loc.statement_index, func, args);

            let blocks = body.basic_blocks_mut();
            if after_call {
                // Swap the newly inserted instrumentation call to the following
                // block and move the original call back to the current block
                let mut instrument_call = blocks[loc.block].terminator.take().unwrap();
                let orig_call = blocks[successor_block].terminator_mut();
                if let (
                    TerminatorKind::Call {
                        destination: Some((_, instrument_dest)),
                        ..
                    },
                    TerminatorKind::Call {
                        destination: Some((_, orig_dest)),
                        ..
                    },
                ) = (&mut instrument_call.kind, &mut orig_call.kind)
                {
                    mem::swap(instrument_dest, orig_dest);
                }
                let orig_call = mem::replace(orig_call, instrument_call);
                blocks[loc.block].terminator = Some(orig_call);

                if let Some(stmts) = extra_statements {
                    blocks[successor_block].statements.extend(stmts);
                }
            }

            if is_cleanup {
                blocks[successor_block].is_cleanup = true;
            }
        }

        /// Rewrite the [`body`](Self::body) to apply the specified [`InstrumentationPoint`]s.
        pub fn apply_points(&mut self, points: impl IntoIterator<Item = &'a InstrumentationPoint<'tcx>>) {
            for point in points {
                self.apply_point(point);
            }
        }
    }
}
