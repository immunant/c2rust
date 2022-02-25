use anyhow::Context;
use c2rust_analysis_rt::HOOK_FUNCTIONS;
use c2rust_analysis_rt::{SourcePos, SourceSpan, SpanId};
use indexmap::IndexSet;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::visit::{PlaceContext, Visitor};
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, Body, CastKind, Constant, Local, LocalDecl, Location, Operand,
    Place, Rvalue, SourceInfo, Statement, StatementKind, Terminator, TerminatorKind, START_BLOCK,
};
use rustc_middle::ty::{self, List, ParamEnv, TyCtxt};
use rustc_span::def_id::{DefId, CRATE_DEF_INDEX};
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
        Operand::Copy(place) | Operand::Move(place) => locals[place.local].ty,
        Operand::Constant(c) => c.ty(),
    };
    if !arg_ty.is_any_ptr() {
        return None;
    }

    let mut ptr_arg = arg.to_copy();
    // Remove any deref from this pointer arg
    if let Operand::Copy(place) = &mut ptr_arg {
        place.projection = List::empty();
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
            Rvalue::Cast(CastKind::Misc, ptr_arg, usize_ty),
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
    pub fn instrument_fn<'tcx>(&self, tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>, body_did: DefId) {
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
        body_did: DefId,
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
        let main_did = tcx.entry_fn(()).map(|(def_id, _)| def_id);
        if Some(body_did) == main_did {
            instrumenter.instrument_entry_fn();
        }
        instrumenter.instrument_calls();
        instrumenter.instrument_ptr_derefs();
        instrumenter.instrument_ptr_assignments();
    }

    fn find_instrumentation_def(&self, name: Symbol) -> Option<DefId> {
        Some(
            self.tcx
                .module_children(self.runtime_crate_did)
                .iter()
                .find(|child| child.ident.name == name)?
                .res
                .def_id(),
        )
    }

    /// Inserts a call to `func`.
    ///
    /// The call will be inserted before the statement at index `statement_idx`
    /// in `block`. If `statement_idx` is the number of statements in the block,
    /// the call will be inserted at the end of the block.
    ///
    /// `func` must not unwind, as it will have no cleanup destination. Returns
    /// the local slot for the inserted call's return value.
    fn insert_call(
        &mut self,
        block: BasicBlock,
        statement_idx: usize,
        func: DefId,
        args: Vec<Operand<'tcx>>,
        is_cleanup: bool,
    ) -> Local {
        let (blocks, locals) = self.body.basic_blocks_and_local_decls_mut();

        let successor_stmts = blocks[block].statements.split_off(statement_idx);
        let succesor_terminator = blocks[block].terminator.take();
        let successor_block = blocks.push(BasicBlockData {
            statements: successor_stmts,
            terminator: succesor_terminator,
            is_cleanup: blocks[block].is_cleanup,
        });

        let fn_sig = self.tcx.fn_sig(func);
        let fn_sig = self.tcx.liberate_late_bound_regions(func, fn_sig);

        let ret_local = locals.push(LocalDecl::new(fn_sig.output(), DUMMY_SP));
        let func = Operand::function_handle(self.tcx, func, ty::List::empty(), DUMMY_SP);

        let call = Terminator {
            kind: TerminatorKind::Call {
                func,
                args,
                destination: Some((ret_local.into(), successor_block)),
                cleanup: None,
                from_hir_call: true,
                fn_span: DUMMY_SP,
            },
            source_info: SourceInfo::outermost(DUMMY_SP),
        };
        blocks[block].terminator.replace(call);
        if is_cleanup {
            blocks[block].is_cleanup = true;
        }

        ret_local
    }

    fn instrument_entry_fn(&mut self) {
        let init_fn_did = self
            .find_instrumentation_def(Symbol::intern("initialize"))
            .expect("Could not find instrumentation context constructor definition");

        let fini_fn_did = self
            .find_instrumentation_def(Symbol::intern("finalize"))
            .expect("Could not find instrumentation context constructor definition");

        let _ = self.insert_call(START_BLOCK, 0, init_fn_did, vec![], false);

        let mut return_blocks = vec![];
        let mut resume_blocks = vec![];
        for (block, block_data) in self.body.basic_blocks().iter_enumerated() {
            match &block_data.terminator().kind {
                TerminatorKind::Return => {
                    return_blocks.push(block);
                }
                TerminatorKind::Resume => {
                    resume_blocks.push(block);
                }
                _ => {}
            }
        }

        for block in return_blocks {
            let _ = self.insert_call(block, 0, fini_fn_did, vec![], false);
        }
        for block in resume_blocks {
            let _ = self.insert_call(block, 0, fini_fn_did, vec![], true);
        }
    }

    /// Find and instrument all calls to functions in [`HOOK_FUNCTIONS`].
    fn instrument_calls(&mut self) {
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
        dbg!(&calls);

        for (block, callee_name) in calls {
            self.instrument_call(block, callee_name);
        }
    }

    fn instrument_call(&mut self, block: BasicBlock, callee_name: Symbol) {
        let func_def_id = self
            .find_instrumentation_def(callee_name)
            .expect("Could not find instrumentation hook function");

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

        // Assumes that the instrumentation function returns nil
        let ret_local = locals.push(LocalDecl::new(self.tcx.mk_unit(), DUMMY_SP));

        let span_idx = self.state.get_source_location_idx(self.tcx, fn_span);
        args.insert(0, self.make_span_const(span_idx));

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

        self.body.basic_blocks_mut()[new_block]
            .terminator
            .replace(instr_call);
    }

    fn instrument_ptr_derefs(&mut self) {
        let mut ptr_derefs = PtrUseVisitor {
            body: &self.body,
            uses: vec![],
        };
        ptr_derefs.visit_body(self.body);
        let mut ptr_uses = ptr_derefs.uses;
        if ptr_uses.is_empty() {
            return;
        }

        // Sort by reverse location so that we can split blocks without
        // perturbing future statement indices
        ptr_uses.sort_unstable_by(|a, b| b.1.cmp(&a.1));

        let ptr_deref_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_deref"))
            .expect("Could not find pointer deref hook");

        for (place, location) in ptr_uses {
            let (cast_stmt, ptr_arg) =
                cast_ptr_to_usize(self.tcx, &mut self.body.local_decls, &Operand::Copy(place))
                    .unwrap_or_else(|| panic!("Expected a pointer operand, got {:?}", place));

            let span_idx = self
                .state
                .get_source_location_idx(self.tcx, self.body.source_info(location).span);
            let span = self.make_span_const(span_idx);

            let _ = self.insert_call(
                location.block,
                location.statement_index,
                ptr_deref_fn,
                vec![span, ptr_arg],
                false,
            );
            self.body.basic_blocks_mut()[location.block]
                .statements
                .push(cast_stmt);
        }
    }

    fn instrument_ptr_assignments(&mut self) {
        let mut assignement_stmts = vec![];
        for (block, block_data) in self.body.basic_blocks().iter_enumerated() {
            for (statement_index, statement) in block_data.statements.iter().enumerate() {
                if let StatementKind::Assign(assign) = &statement.kind {
                    let place = assign.0;
                    let local = &self.body.local_decls[place.local];
                    if local.is_user_variable() && local.ty.is_unsafe_ptr() {
                        assignement_stmts.push((
                            place,
                            Location {
                                block,
                                statement_index,
                            },
                        ))
                    }
                }
            }
        }

        // Sort by reverse location so that we can split blocks without
        // perturbing future statement indices
        assignement_stmts.sort_unstable_by(|a, b| b.1.cmp(&a.1));

        let ptr_move_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_assign"))
            .expect("Could not find pointer assign hook");

        for (place, location) in assignement_stmts {
            let (cast_stmt, ptr_arg) =
                cast_ptr_to_usize(self.tcx, &mut self.body.local_decls, &Operand::Copy(place))
                    .unwrap_or_else(|| panic!("Expected a pointer operand, got {:?}", place));

            let span_idx = self
                .state
                .get_source_location_idx(self.tcx, self.body.source_info(location).span);
            let span = self.make_span_const(span_idx);

            // Note that we insert at statement_index + 1 to insert _after_ the
            // assignment statement, making it easy to reuse the local.
            let _ = self.insert_call(
                location.block,
                location.statement_index + 1,
                ptr_move_fn,
                vec![span, ptr_arg],
                false,
            );
            self.body.basic_blocks_mut()[location.block]
                .statements
                .push(cast_stmt);
        }
    }

    fn make_span_const(&self, idx: u32) -> Operand<'tcx> {
        Operand::Constant(Box::new(Constant {
            span: DUMMY_SP,
            user_ty: None,
            literal: ty::Const::from_bits(
                self.tcx,
                idx.into(),
                ParamEnv::empty().and(self.tcx.types.u32),
            )
            .into(),
        }))
    }
}

struct PtrUseVisitor<'a, 'tcx: 'a> {
    body: &'a Body<'tcx>,
    uses: Vec<(Place<'tcx>, Location)>,
}

impl<'a, 'tcx: 'a> Visitor<'tcx> for PtrUseVisitor<'a, 'tcx> {
    fn visit_place(&mut self, place: &Place<'tcx>, context: PlaceContext, location: Location) {
        let is_unsafe_ptr = self.body.local_decls[place.local].ty.is_unsafe_ptr();
        if is_unsafe_ptr && place.is_indirect() && context.is_use() {
            self.uses.push((*place, location));
        }
    }
}
