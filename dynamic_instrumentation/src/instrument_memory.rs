use anyhow::Context;
use c2rust_analysis_rt::HOOK_FUNCTIONS;
use c2rust_analysis_rt::{Metadata, MirLoc, MirLocId};
use indexmap::IndexSet;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::visit::{PlaceContext, Visitor};
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, Body, CastKind, Constant, Local, LocalDecl, Location, Operand,
    Place, Rvalue, SourceInfo, Statement, StatementKind, Terminator, TerminatorKind, START_BLOCK,
};
use rustc_middle::ty::{self, List, ParamEnv, TyCtxt};
use rustc_span::def_id::{DefId, DefPathHash, CRATE_DEF_INDEX};
use rustc_span::{Symbol, DUMMY_SP};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::iter::successors;
use std::mem;
use std::path::Path;
use std::sync::Mutex;

pub struct InstrumentMemoryOps {
    mir_locs: Mutex<IndexSet<MirLoc>>,
    functions: Mutex<HashMap<c2rust_analysis_rt::DefPathHash, String>>,
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
            mir_locs: Mutex::new(IndexSet::new()),
            functions: Mutex::new(HashMap::new()),
        }
    }

    /// Instrument memory operations in-place in the function `body`.
    pub fn instrument_fn<'tcx>(&self, tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>, body_did: DefId) {
        self.functions.lock().unwrap().insert(
            tcx.def_path_hash(body_did).0.as_value().into(),
            tcx.item_name(body_did).to_string(),
        );
        FunctionInstrumenter::instrument(self, tcx, body, body_did);
    }

    /// Finish instrumentation and write out metadata to `metadata_file_path`.
    pub fn finalize(&self, metadata_file_path: &Path) -> anyhow::Result<()> {
        let mut locs = self.mir_locs.lock().unwrap();
        let mut functions = self.functions.lock().unwrap();
        let locs: Vec<MirLoc> = locs.drain(..).collect();
        let functions: HashMap<c2rust_analysis_rt::DefPathHash, String> =
            functions.drain().collect();
        dbg!("Writing metadata to {:?}", metadata_file_path);
        let metadata_file =
            File::create(metadata_file_path).context("Could not open metadata file")?;
        let metadata = Metadata {
            locs,
            functions,
        };
        bincode::serialize_into(metadata_file, &metadata)
            .context("Location serialization failed")?;
        Ok(())
    }

    /// Get the unique index corresponding to a particular MIR location.
    ///
    /// Returned indices will not be sorted in any particular order, but are
    /// unique and constant across the entire lifetime of this instrumentation
    /// instance.
    fn get_mir_loc_idx<'tcx>(&self, body_def: DefPathHash, location: Location) -> MirLocId {
        let mir_loc = MirLoc {
            body_def: body_def.0.as_value().into(),
            basic_block_idx: u32::from(location.block),
            statement_idx: u32::try_from(location.statement_index).unwrap(),
        };
        let (idx, _) = self.mir_locs.lock().unwrap().insert_full(mir_loc);
        u32::try_from(idx).unwrap()
    }
}

struct InstrumentationPoint<'tcx> {
    loc: Location,
    func: DefId,
    args: Vec<Operand<'tcx>>,
    is_cleanup: bool,
    after_call: bool,
}

struct FunctionInstrumenter<'a, 'tcx: 'a> {
    state: &'a InstrumentMemoryOps,
    tcx: TyCtxt<'tcx>,
    body: &'a mut Body<'tcx>,
    body_def_hash: DefPathHash,
    runtime_crate_did: DefId,

    instrumentation_points: RefCell<Vec<InstrumentationPoint<'tcx>>>,
}

impl<'a, 'tcx: 'a> FunctionInstrumenter<'a, 'tcx> {
    fn instrument(
        state: &'a InstrumentMemoryOps,
        tcx: TyCtxt<'tcx>,
        body: &'a mut Body<'tcx>,
        body_did: DefId,
    ) {
        let body_def_hash = tcx.def_path_hash(body_did);

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
            body_def_hash,
            runtime_crate_did,

            instrumentation_points: RefCell::new(vec![]),
        };
        let main_did = tcx.entry_fn(()).map(|(def_id, _)| def_id);
        instrumenter.instrument_ptr_args();
        instrumenter.instrument_ptr_derefs();
        instrumenter.instrument_ptr_assignments();
        instrumenter.instrument_ptr_returns();
        instrumenter.instrument_calls();

        instrumenter.do_instrumentation();

        if Some(body_did) == main_did {
            instrumenter.instrument_entry_fn();
        }
    }

    fn get_mir_loc_idx(&self, location: Location) -> MirLocId {
        self.state.get_mir_loc_idx(self.body_def_hash, location)
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

    /// Queues insertion of a call to `func`
    ///
    /// The call will be inserted before the statement at index `statement_idx`
    /// in `block`. If `statement_idx` is the number of statements in the block,
    /// the call will be inserted at the end of the block.
    ///
    /// `func` must not unwind, as it will have no cleanup destination.
    fn add_instrumentation(
        &self,
        block: BasicBlock,
        statement_index: usize,
        func: DefId,
        args: Vec<Operand<'tcx>>,
        is_cleanup: bool,
        after_call: bool,
    ) {
        self.instrumentation_points
            .borrow_mut()
            .push(InstrumentationPoint {
                loc: Location {
                    block,
                    statement_index,
                },
                func,
                args,
                is_cleanup,
                after_call,
            })
    }

    /// Inserts a call to `func`.
    ///
    /// The call will be inserted before the statement at index `statement_idx`
    /// in `block`. If `statement_idx` is the number of statements in the block,
    /// the call will be inserted at the end of the block.
    ///
    /// `func` must not unwind, as it will have no cleanup destination. Returns
    /// the successor basic block and the local slot for the inserted call's
    /// return value.
    fn insert_call(
        &mut self,
        block: BasicBlock,
        statement_index: usize,
        func: DefId,
        mut args: Vec<Operand<'tcx>>,
        is_cleanup: bool,
    ) -> (BasicBlock, Local) {
        let (blocks, locals) = self.body.basic_blocks_and_local_decls_mut();

        let successor_stmts = blocks[block].statements.split_off(statement_index);
        let successor_terminator = blocks[block].terminator.take();
        let successor_block = blocks.push(BasicBlockData {
            statements: successor_stmts,
            terminator: successor_terminator,
            is_cleanup: blocks[block].is_cleanup,
        });

        for arg in &mut args {
            if let Some((cast_stmt, cast_local)) = cast_ptr_to_usize(self.tcx, locals, &arg) {
                *arg = cast_local;
                blocks[block].statements.insert(statement_index, cast_stmt);
            }
        }

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

        (successor_block, ret_local)
    }

    fn do_instrumentation(&mut self) {
        // Sort by reverse location so that we can split blocks without
        // perturbing future statement indices
        self.instrumentation_points
            .get_mut()
            .sort_unstable_by(|a, b| b.loc.cmp(&a.loc).then(b.after_call.cmp(&a.after_call)));

        let points: Vec<_> = self.instrumentation_points.get_mut().drain(..).collect();
        for point in points {
            let InstrumentationPoint {
                loc,
                func,
                mut args,
                is_cleanup,
                after_call,
            } = point;

            // Add the MIR location as the first argument to the instrumentation function
            let loc_idx = self.get_mir_loc_idx(loc);
            args.insert(0, self.make_loc_const(loc_idx));

            let (blocks, locals) = self.body.basic_blocks_and_local_decls_mut();
            let mut extra_statement = None;
            if after_call {
                let call = blocks[loc.block].terminator_mut();
                let ret_value = if let TerminatorKind::Call {
                    destination: Some((place, next_block)),
                    args,
                    ..
                } = &mut call.kind
                {
                    // Make the call operands copies so we don't reuse a moved value
                    dbg!("Replacing args with copies", &args);
                    args.iter_mut().for_each(|arg| *arg = arg.to_copy());

                    Operand::Copy(*place)
                } else {
                    panic!(
                        "Expected a call terminator in block to instrument, found: {:?}",
                        call
                    );
                };

                if let Some((cast_stmt, cast_local)) =
                    cast_ptr_to_usize(self.tcx, locals, &ret_value)
                {
                    extra_statement = Some(cast_stmt);
                    args.push(cast_local);
                } else {
                    args.push(ret_value);
                }
            }

            let (successor_block, _) =
                self.insert_call(loc.block, loc.statement_index, func, args, is_cleanup);

            let (blocks, locals) = self.body.basic_blocks_and_local_decls_mut();
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

                if let Some(stmt) = extra_statement {
                    blocks[successor_block].statements.push(stmt);
                }
            }

            if is_cleanup {
                blocks[successor_block].is_cleanup = true;
            }
        }
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
        for (id, block) in self.body.basic_blocks().iter_enumerated() {
            match &block.terminator {
                Some(Terminator {
                    kind: TerminatorKind::Call { func, args, .. },
                    ..
                }) => {
                    if let ty::FnDef(def_id, _) = func.ty(self.body, self.tcx).kind() {
                        let fn_name = self.tcx.item_name(*def_id);
                        if HOOK_FUNCTIONS.contains(&fn_name.as_str()) {
                            let func_def_id = self
                                .find_instrumentation_def(fn_name)
                                .expect("Could not find instrumentation hook function");
                            self.add_instrumentation(
                                id,
                                block.statements.len(),
                                func_def_id,
                                args.clone(),
                                false,
                                true,
                            );
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn instrument_ptr_derefs(&mut self) {
        let mut ptr_derefs = PtrUseVisitor {
            body: &self.body,
            uses: vec![],
        };
        ptr_derefs.visit_body(self.body);

        let instrument_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_deref"))
            .expect("Could not find pointer deref hook");

        for (place, loc) in ptr_derefs.uses {
            self.add_instrumentation(
                loc.block,
                loc.statement_index,
                instrument_fn,
                vec![Operand::Copy(place)],
                false,
                false,
            );
        }
    }

    fn instrument_ptr_assignments(&mut self) {
        let instrument_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_assign"))
            .expect("Could not find pointer deref hook");

        for (block, block_data) in self.body.basic_blocks().iter_enumerated() {
            for (statement_index, statement) in block_data.statements.iter().enumerate() {
                if let StatementKind::Assign(assign) = &statement.kind {
                    let place = assign.0;
                    let local = &self.body.local_decls[place.local];
                    if local.is_user_variable() && local.ty.is_unsafe_ptr() {
                        self.add_instrumentation(
                            block,
                            statement_index + 1,
                            instrument_fn,
                            vec![Operand::Copy(place)],
                            false,
                            false,
                        );
                    }
                }
            }
        }
    }

    fn instrument_ptr_args(&mut self) {
        let instrument_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_arg"))
            .expect("Could not find pointer deref hook");

        for (block, block_data) in self.body.basic_blocks().iter_enumerated() {
            if let TerminatorKind::Call { args, .. } = &block_data.terminator().kind {
                for arg in args {
                    if let Some(place) = arg.place() {
                        if self.body.local_decls[place.local].ty.is_unsafe_ptr() {
                            self.add_instrumentation(
                                block,
                                block_data.statements.len(),
                                instrument_fn,
                                vec![Operand::Copy(place)],
                                false,
                                false,
                            );
                        }
                    }
                }
            }
        }
    }

    fn instrument_ptr_returns(&mut self) {
        let instrument_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_ret"))
            .expect("Could not find pointer deref hook");

        for (block, block_data) in self.body.basic_blocks().iter_enumerated() {
            if let TerminatorKind::Return = &block_data.terminator().kind {
                let place = Place::return_place();
                if self.body.local_decls[place.local].ty.is_unsafe_ptr() {
                    self.add_instrumentation(
                        block,
                        block_data.statements.len(),
                        instrument_fn,
                        vec![Operand::Copy(place)],
                        false,
                        false,
                    );
                }
            }
        }
    }

    fn make_loc_const(&self, idx: u32) -> Operand<'tcx> {
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
