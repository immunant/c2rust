use std::mem;

use c2rust_analysis_rt::mir_loc::TransferKind;
use rustc_middle::{
    mir::{Body, Operand, TerminatorKind},
    ty::TyCtxt,
};
use rustc_span::def_id::{DefId, DefPathHash};

use crate::{
    arg::{ArgKind, InstrumentationArg},
    instrument::{insert_call, Instrumenter},
    into_operand::IntoOperand,
    point::cast_ptr_to_usize,
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
            original_location,
            instrumentation_location,
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

        let (blocks, locals) = body.basic_blocks_and_local_decls_mut();

        // Add the MIR location as the first argument to the instrumentation function
        let loc_idx = state.get_mir_loc_idx(body_def, original_location, metadata.clone());
        args.insert(
            0,
            InstrumentationArg::Op(ArgKind::AddressUsize(loc_idx.op(tcx))),
        );

        let mut extra_statements = None;
        if after_call {
            let call = blocks[instrumentation_location.block].terminator_mut();
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

        let (successor_block, _) = insert_call(
            tcx,
            *body,
            instrumentation_location.block,
            instrumentation_location.statement_index,
            func,
            args,
        );

        let blocks = body.basic_blocks_mut();
        if after_call {
            // Swap the newly inserted instrumentation call to the following
            // block and move the original call back to the current block
            let mut instrument_call = blocks[instrumentation_location.block]
                .terminator
                .take()
                .unwrap();
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
            blocks[instrumentation_location.block].terminator = Some(orig_call);

            if let Some(stmts) = extra_statements {
                blocks[successor_block].statements.extend(stmts);
            }
        }

        if is_cleanup {
            blocks[successor_block].is_cleanup = true;
        }
    }

    /// Rewrite the [`body`](Self::body) to apply the specified [`InstrumentationPoint`]s.
    pub fn apply_points(
        &mut self,
        points: impl IntoIterator<Item = &'a InstrumentationPoint<'tcx>>,
    ) {
        for point in points {
            self.apply_point(point);
        }
    }
}
