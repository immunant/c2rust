use anyhow::Context;
use c2rust_analysis_rt::HOOK_FUNCTIONS;
use c2rust_analysis_rt::{SourcePos, SourceSpan, SpanId};
use indexmap::IndexSet;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, Body, CastKind, Constant, Local, LocalDecl, Operand, Rvalue,
    SourceInfo, Statement, StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{self, ParamEnv, TyCtxt};
use rustc_span::def_id::{DefId, LocalDefId, CRATE_DEF_INDEX};
use rustc_span::{FileName, Span, Symbol, DUMMY_SP};
use std::fs::File;
use std::mem;
use std::path::Path;
use std::sync::Mutex;

pub struct InstrumentMemoryOps {
    spans: Mutex<IndexSet<SourceSpan>>,
}

/// Cast an argument from pointer to usize, if needed.
///
/// Casts `arg` to `usize` if needed, returning the cast statement and new,
/// `usize` typed operand if the cast was needed. This cast statement must be
/// inserted into the function's body before the new operand is used. `arg` will
/// be used as a copy in the new statement, so this statement must be inserted
/// in a position where `arg` is alive.
fn cast_ptr_to_usize<'tcx>(
    tcx: TyCtxt<'tcx>,
    locals: &mut IndexVec<Local, LocalDecl<'tcx>>,
    arg: &Operand<'tcx>,
) -> Option<(Statement<'tcx>, Operand<'tcx>)> {
    let arg_ty = match arg {
        Operand::Copy(place) | Operand::Move(place) => place.ty(locals, tcx).ty,
        Operand::Constant(c) => c.ty(),
    };
    if !arg_ty.is_any_ptr() {
        return None;
    }

    // Cast this argument to a usize before passing to the
    // instrumentation function
    let usize_ty = tcx.mk_mach_uint(ty::UintTy::Usize);
    let casted_local = locals.push(LocalDecl::new(usize_ty, DUMMY_SP));
    let casted_arg = Operand::Move(casted_local.into());
    let cast_stmt = Statement {
        source_info: SourceInfo::outermost(DUMMY_SP),
        kind: StatementKind::Assign(Box::new((
            casted_local.into(),
            Rvalue::Cast(CastKind::Misc, arg.to_copy(), usize_ty),
        ))),
    };
    Some((cast_stmt, casted_arg))
}

impl InstrumentMemoryOps {
    /// Create a new instrumentation object.
    ///
    /// A single `InstrumentMemoryOps` instance should be shared across the
    /// entire crate being instrumented, as the indexed source locations are
    /// shared and should be global.
    pub fn new() -> Self {
        Self {
            spans: Mutex::new(IndexSet::new()),
        }
    }

    /// Instrument memory operations in-place in the function `body`.
    pub fn instrument_fn<'tcx>(
        &self,
        tcx: TyCtxt<'tcx>,
        body: &mut Body<'tcx>,
        body_did: LocalDefId,
    ) {
        FunctionInstrumenter::instrument(self, tcx, body, body_did);
    }

    /// Finish instrumentation and write out metadata to `metadata_file_path`.
    pub fn finalize(&self, metadata_file_path: &Path) -> anyhow::Result<()> {
        let mut spans = self.spans.lock().unwrap();
        dbg!("Writing metadata to {:?}", metadata_file_path);
        let span_file = File::create(metadata_file_path).context("Could not open metadata file")?;
        let spans: Vec<SourceSpan> = spans.drain(..).collect();
        bincode::serialize_into(span_file, &spans).context("Span serialization failed")?;
        Ok(())
    }

    /// Get the unique index corresponding to a particular `Span`.
    ///
    /// Returned indices will not be sorted in any particular order, but are
    /// unique and constant across the entire lifetime of this instrumentation
    /// instance.
    fn get_source_location_idx<'tcx>(&self, tcx: TyCtxt<'tcx>, span: Span) -> SpanId {
        let lo = tcx.sess.source_map().lookup_byte_offset(span.lo());
        let hi = tcx.sess.source_map().lookup_byte_offset(span.hi());

        if lo.sf.start_pos != hi.sf.start_pos {
            tcx.sess.span_err(span, "Location crosses source files");
        }
        let file_path = match &lo.sf.name {
            FileName::Real(path) => path.to_owned(),
            _ => {
                tcx.sess
                    .span_err(span, "Location does not refer to a source file");
                unreachable!()
            }
        };

        let source_span = SourceSpan::new(
            file_path.remapped_path_if_available().to_owned(),
            SourcePos(lo.pos.0),
            SourcePos(hi.pos.0),
        );

        let (idx, _) = self.spans.lock().unwrap().insert_full(source_span);
        u32::try_from(idx).unwrap()
    }
}

struct FunctionInstrumenter<'a, 'tcx: 'a> {
    state: &'a InstrumentMemoryOps,
    tcx: TyCtxt<'tcx>,
    body: &'a mut Body<'tcx>,
    runtime_crate_did: DefId,
}

impl<'a, 'tcx: 'a> FunctionInstrumenter<'a, 'tcx> {
    fn instrument(
        state: &'a InstrumentMemoryOps,
        tcx: TyCtxt<'tcx>,
        body: &'a mut Body<'tcx>,
        _body_did: LocalDefId,
    ) {
        let runtime_crate = tcx
            .crates(())
            .iter()
            .cloned()
            .find(|&krate| tcx.crate_name(krate).as_str() == "c2rust_analysis_rt")
            .unwrap();

        let runtime_crate_did = DefId {
            krate: runtime_crate,
            index: CRATE_DEF_INDEX,
        };

        let mut instrumenter = Self {
            state,
            tcx,
            body,
            runtime_crate_did,
        };
        instrumenter.instrument_calls();
    }

    fn instrument_calls(&mut self) {
        let calls_to_instrument = self.find_calls_to_instrument();
        dbg!(&calls_to_instrument);
        if calls_to_instrument.is_empty() {
            return;
        }

        for (block, callee_name) in calls_to_instrument {
            let (blocks, locals) = self.body.basic_blocks_and_local_decls_mut();
            let new_block = blocks.push(BasicBlockData::new(None));
            let call = blocks[block].terminator_mut();
            let mut casts = vec![];
            let (ret_value, mut args, next_block, fn_span) = if let TerminatorKind::Call {
                destination: Some((place, next_block)),
                args,
                fn_span,
                ..
            } = &mut call.kind
            {
                let next_block = mem::replace(next_block, new_block);
                let mut instrument_args = args.clone();
                for (orig_arg, instr_arg) in args.iter_mut().zip(instrument_args.iter_mut()) {
                    // Make the call operands copies so we don't reuse a moved value
                    if orig_arg.place().is_some() {
                        *orig_arg = orig_arg.to_copy();
                    }

                    if let Some((cast_stmt, cast_local)) =
                        cast_ptr_to_usize(self.tcx, locals, &orig_arg)
                    {
                        *instr_arg = cast_local;
                        casts.push(cast_stmt);
                    }
                }
                (Operand::Copy(*place), instrument_args, next_block, *fn_span)
            } else {
                panic!(
                    "Expected a call terminator in block to instrument, found: {:?}",
                    call
                );
            };
            blocks[block].statements.append(&mut casts);
            if let Some((cast_stmt, cast_local)) = cast_ptr_to_usize(self.tcx, locals, &ret_value) {
                blocks[new_block].statements.push(cast_stmt);
                args.push(cast_local);
            } else {
                args.push(ret_value);
            }

            let span_idx = self.state.get_source_location_idx(self.tcx, fn_span);
            args.insert(
                0,
                Operand::Constant(Box::new(Constant {
                    span: DUMMY_SP,
                    user_ty: None,
                    literal: ty::Const::from_bits(
                        self.tcx,
                        span_idx.into(),
                        ParamEnv::empty().and(self.tcx.types.u32),
                    )
                    .into(),
                })),
            );

            // Assumes that the instrumentation function returns nil
            let ret_local = locals.push(LocalDecl::new(self.tcx.mk_unit(), DUMMY_SP));

            let func_def_id = self
                .tcx
                .module_children(self.runtime_crate_did)
                .iter()
                .find(|child| child.ident.name == callee_name)
                .unwrap()
                .res
                .def_id();
            let func = Operand::function_handle(self.tcx, func_def_id, ty::List::empty(), DUMMY_SP);
            let instr_call = Terminator {
                kind: TerminatorKind::Call {
                    func,
                    args,
                    destination: Some((ret_local.into(), next_block)),
                    cleanup: None,
                    from_hir_call: true,
                    fn_span: DUMMY_SP,
                },
                source_info: SourceInfo::outermost(DUMMY_SP),
            };

            blocks[new_block].terminator.replace(instr_call);
        }
    }

    /// Find and return all calls to functions in [`HOOK_FUNCTIONS`].
    fn find_calls_to_instrument(&self) -> Vec<(BasicBlock, Symbol)> {
        let mut calls = vec![];
        for (id, block) in self.body.basic_blocks().iter_enumerated() {
            match &block.terminator {
                Some(Terminator {
                    kind: TerminatorKind::Call { func, .. },
                    ..
                }) => {
                    if let ty::FnDef(def_id, _) = func.ty(self.body, self.tcx).kind() {
                        let fn_name = self.tcx.item_name(*def_id);
                        if HOOK_FUNCTIONS.contains(&fn_name.as_str()) {
                            calls.push((id, fn_name));
                        }
                    }
                }
                _ => {}
            }
        }
        calls
    }
}
