use anyhow::Context;
use c2rust_analysis_rt::HOOK_FUNCTIONS;
use c2rust_analysis_rt::{Metadata, MirLoc, MirLocId};
use indexmap::IndexSet;
use log::debug;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::visit::{PlaceContext, Visitor};
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, Body, CastKind, Constant, Local, LocalDecl, Location, Operand,
    Place, PlaceElem, ProjectionElem, Rvalue, SourceInfo, Statement, StatementKind, Terminator,
    TerminatorKind, START_BLOCK,
};
use rustc_middle::ty::{self, ParamEnv, TyCtxt};
use rustc_span::def_id::{DefId, DefPathHash, CRATE_DEF_INDEX};
use rustc_span::{Symbol, DUMMY_SP};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::mem;
use std::path::Path;
use std::sync::Mutex;

pub struct InstrumentMemoryOps {
    mir_locs: Mutex<IndexSet<MirLoc>>,
    functions: Mutex<HashMap<c2rust_analysis_rt::DefPathHash, String>>,
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
        let function_name = tcx.item_name(body_did);
        debug!("Instrumenting function {}", function_name);

        self.functions.lock().unwrap().insert(
            tcx.def_path_hash(body_did).0.as_value().into(),
            tcx.item_name(body_did).to_string(),
        );
        debug!("Body before instrumentation: {:#?}", body);
        instrument(self, tcx, body, body_did);
        debug!("Body after instrumentation: {:#?}", body);
    }

    /// Finish instrumentation and write out metadata to `metadata_file_path`.
    pub fn finalize(&self, metadata_file_path: &Path) -> anyhow::Result<()> {
        let mut locs = self.mir_locs.lock().unwrap();
        let mut functions = self.functions.lock().unwrap();
        let locs: Vec<MirLoc> = locs.drain(..).collect();
        let functions: HashMap<c2rust_analysis_rt::DefPathHash, String> =
            functions.drain().collect();
        let metadata_file =
            File::create(metadata_file_path).context("Could not open metadata file")?;
        let metadata = Metadata { locs, functions };
        bincode::serialize_into(metadata_file, &metadata)
            .context("Location serialization failed")?;
        Ok(())
    }

    /// Get the unique index corresponding to a particular MIR location.
    ///
    /// Returned indices will not be sorted in any particular order, but are
    /// unique and constant across the entire lifetime of this instrumentation
    /// instance.
    fn get_mir_loc_idx(&self, body_def: DefPathHash, location: Location) -> MirLocId {
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
    tcx: TyCtxt<'tcx>,
    body: &'a Body<'tcx>,
    runtime_crate_did: DefId,

    instrumentation_points: RefCell<Vec<InstrumentationPoint<'tcx>>>,
}

impl<'a, 'tcx: 'a> FunctionInstrumenter<'a, 'tcx> {
    /// Queues insertion of a call to `func`
    ///
    /// The call will be inserted before the statement at index `statement_idx`
    /// in `block`. If `statement_idx` is the number of statements in the block,
    /// the call will be inserted at the end of the block.
    ///
    /// `func` must not unwind, as it will have no cleanup destination.
    fn add_instrumentation(
        &self,
        loc: Location,
        func: DefId,
        args: Vec<Operand<'tcx>>,
        is_cleanup: bool,
        after_call: bool,
    ) {
        self.instrumentation_points
            .borrow_mut()
            .push(InstrumentationPoint {
                loc,
                func,
                args,
                is_cleanup,
                after_call,
            })
    }

    fn into_instrumentation_points(mut self) -> Vec<InstrumentationPoint<'tcx>> {
        // Sort by reverse location so that we can split blocks without
        // perturbing future statement indices
        self.instrumentation_points
            .get_mut()
            .sort_unstable_by(|a, b| b.loc.cmp(&a.loc).then(b.after_call.cmp(&a.after_call)));
        self.instrumentation_points.into_inner()
    }

    fn find_instrumentation_def(&self, name: Symbol) -> Option<DefId> {
        find_instrumentation_def(self.tcx, self.runtime_crate_did, name)
    }

    fn should_instrument(&self, local: Local, context: PlaceContext) -> bool {
        let is_unsafe_ptr = self.body.local_decls[local].ty.is_unsafe_ptr();
        is_unsafe_ptr && context.is_use()
    }
}

impl<'a, 'tcx: 'a> Visitor<'tcx> for FunctionInstrumenter<'a, 'tcx> {
    fn visit_projection_elem(
        &mut self,
        local: Local,
        proj_base: &[PlaceElem<'tcx>],
        elem: PlaceElem<'tcx>,
        context: PlaceContext,
        location: Location,
    ) {
        let field_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_field"))
            .expect("Could not find pointer field hook");
        let load_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_load"))
            .expect("Could not find pointer load hook");

        let local_decl = &self.body.local_decls[local];
        if self.should_instrument(local, context) && local_decl.ty.is_unsafe_ptr() {
            match elem {
                PlaceElem::Field(field, _) => {
                    self.add_instrumentation(
                        location,
                        field_fn,
                        vec![
                            Operand::Copy(local.into()),
                            make_const(self.tcx, field.as_u32()),
                        ],
                        false,
                        false,
                    );
                }
                PlaceElem::Deref if !context.is_mutating_use() => {
                    self.add_instrumentation(
                        location,
                        load_fn,
                        vec![Operand::Copy(local.into())],
                        false,
                        false,
                    );
                }
                _ => {}
            }
        }

        self.super_projection_elem(local, proj_base, elem, context, location)
    }

    fn visit_statement(&mut self, statement: &Statement<'tcx>, mut location: Location) {
        let copy_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_copy"))
            .expect("Could not find pointer copy hook");
        let store_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_store"))
            .expect("Could not find pointer store hook");

        if let StatementKind::Assign(assign) = &statement.kind {
            let mut place = assign.0;
            let value = &assign.1;
            if place.is_indirect() {
                let mut place_ref = place.as_ref();
                while let Some((cur_ref, proj)) = place_ref.last_projection() {
                    if let ProjectionElem::Deref = proj {
                        place = Place {
                            local: cur_ref.local,
                            projection: self.tcx.intern_place_elems(cur_ref.projection),
                        };
                    }
                    place_ref = cur_ref;
                }
                self.add_instrumentation(
                    location,
                    store_fn,
                    vec![Operand::Copy(place)],
                    false,
                    false,
                );
            } else if value.ty(&self.body.local_decls, self.tcx).is_unsafe_ptr() {
                // We want to insert after the assignment statement so we can
                // use the destination local as our instrumentation argument
                location.statement_index += 1;
                self.add_instrumentation(
                    location,
                    copy_fn,
                    vec![Operand::Copy(place)],
                    false,
                    false,
                );
            }
        }

        self.super_statement(statement, location)
    }

    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, location: Location) {
        let arg_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_arg"))
            .expect("Could not find pointer arg hook");

        let ret_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_ret"))
            .expect("Could not find pointer ret hook");

        match &terminator.kind {
            TerminatorKind::Call { func, args, .. } => {
                for arg in args {
                    if let Some(place) = arg.place() {
                        if self.body.local_decls[place.local].ty.is_unsafe_ptr() {
                            self.add_instrumentation(
                                location,
                                arg_fn,
                                vec![Operand::Copy(place)],
                                false,
                                false,
                            );
                        }
                    }
                }
                if let ty::FnDef(def_id, _) = func.ty(self.body, self.tcx).kind() {
                    let fn_name = self.tcx.item_name(*def_id);
                    if HOOK_FUNCTIONS.contains(&fn_name.as_str()) {
                        let func_def_id = self
                            .find_instrumentation_def(fn_name)
                            .expect("Could not find instrumentation hook function");
                        self.add_instrumentation(location, func_def_id, args.clone(), false, true);
                    }
                }
            }
            TerminatorKind::Return => {
                let place = Place::return_place();
                if self.body.local_decls[place.local].ty.is_unsafe_ptr() {
                    self.add_instrumentation(
                        location,
                        ret_fn,
                        vec![Operand::Copy(place)],
                        false,
                        false,
                    );
                }
            }
            _ => {}
        }

        self.super_terminator(terminator, location);
    }
}

fn find_instrumentation_def(
    tcx: TyCtxt,
    runtime_crate_did: DefId,
    name: Symbol,
) -> Option<DefId> {
    Some(
        tcx.module_children(runtime_crate_did)
            .iter()
            .find(|child| child.ident.name == name)?
            .res
            .def_id(),
    )
}

fn make_const(tcx: TyCtxt, idx: u32) -> Operand {
    Operand::Constant(Box::new(Constant {
        span: DUMMY_SP,
        user_ty: None,
        literal: ty::Const::from_bits(tcx, idx.into(), ParamEnv::empty().and(tcx.types.u32)).into(),
    }))
}

fn instrument<'tcx>(
    state: &InstrumentMemoryOps,
    tcx: TyCtxt<'tcx>,
    body: &mut Body<'tcx>,
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

    let mut instrumenter = FunctionInstrumenter {
        tcx,
        body,
        runtime_crate_did,

        instrumentation_points: RefCell::new(vec![]),
    };
    let main_did = tcx.entry_fn(()).map(|(def_id, _)| def_id);
    instrumenter.visit_body(body);
    do_instrumentation(
        state,
        &instrumenter.into_instrumentation_points(),
        tcx,
        body,
        body_def_hash,
    );

    if Some(body_did) == main_did {
        instrument_entry_fn(tcx, runtime_crate_did, body);
    }
}

fn instrument_entry_fn<'tcx>(tcx: TyCtxt<'tcx>, runtime_crate_did: DefId, body: &mut Body<'tcx>) {
    let init_fn_did =
        find_instrumentation_def(tcx, runtime_crate_did, Symbol::intern("initialize"))
            .expect("Could not find instrumentation context constructor definition");

    let fini_fn_did = find_instrumentation_def(tcx, runtime_crate_did, Symbol::intern("finalize"))
        .expect("Could not find instrumentation context constructor definition");

    let _ = insert_call(tcx, body, START_BLOCK, 0, init_fn_did, vec![]);

    let mut return_blocks = vec![];
    let mut resume_blocks = vec![];
    for (block, block_data) in body.basic_blocks().iter_enumerated() {
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
        let _ = insert_call(tcx, body, block, 0, fini_fn_did, vec![]);
    }
    for block in resume_blocks {
        let _ = insert_call(tcx, body, block, 0, fini_fn_did, vec![]);
    }
}

fn do_instrumentation<'tcx>(
    state: &InstrumentMemoryOps,
    points: &[InstrumentationPoint<'tcx>],
    tcx: TyCtxt<'tcx>,
    body: &mut Body<'tcx>,
    body_def: DefPathHash,
) {
    for point in points {
        let &InstrumentationPoint {
            loc,
            func,
            ref args,
            is_cleanup,
            after_call,
        } = point;
        let mut args = args.clone();

        // Add the MIR location as the first argument to the instrumentation function
        let loc_idx = state.get_mir_loc_idx(body_def, loc);
        args.insert(0, make_const(tcx, loc_idx));

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

                Operand::Copy(*place)
            } else {
                panic!(
                    "Expected a call terminator in block to instrument, found: {:?}",
                    call
                );
            };

            if let Some((casts, cast_local)) = cast_ptr_to_usize(tcx, locals, &ret_value) {
                extra_statements = Some(casts);
                args.push(cast_local);
            } else {
                args.push(ret_value);
            }
        }

        let (successor_block, _) =
            insert_call(tcx, body, loc.block, loc.statement_index, func, args);

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
fn insert_call<'tcx>(
    tcx: TyCtxt<'tcx>,
    body: &mut Body<'tcx>,
    block: BasicBlock,
    statement_index: usize,
    func: DefId,
    mut args: Vec<Operand<'tcx>>,
) -> (BasicBlock, Local) {
    let (blocks, locals) = body.basic_blocks_and_local_decls_mut();

    let successor_stmts = blocks[block].statements.split_off(statement_index);
    let successor_terminator = blocks[block].terminator.take();
    let successor_block = blocks.push(BasicBlockData {
        statements: successor_stmts,
        terminator: successor_terminator,
        is_cleanup: blocks[block].is_cleanup,
    });

    for arg in &mut args {
        if let Some((cast_stmts, cast_local)) = cast_ptr_to_usize(tcx, locals, arg) {
            *arg = cast_local;
            blocks[block]
                .statements
                .splice(statement_index..statement_index, cast_stmts);
        }
    }

    let fn_sig = tcx.fn_sig(func);
    let fn_sig = tcx.liberate_late_bound_regions(func, fn_sig);

    let ret_local = locals.push(LocalDecl::new(fn_sig.output(), DUMMY_SP));
    let func = Operand::function_handle(tcx, func, ty::List::empty(), DUMMY_SP);

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
) -> Option<(Vec<Statement<'tcx>>, Operand<'tcx>)> {
    let arg_ty = arg.ty(locals, tcx);
    if !arg_ty.is_any_ptr() {
        return None;
    }

    let mut new_stmts = vec![];

    let ptr_arg = if !arg_ty.is_unsafe_ptr() {
        let ptr_ty = arg_ty.builtin_deref(false).unwrap();
        let raw_ptr_ty = tcx.mk_ptr(ptr_ty);
        let raw_ptr_local = locals.push(LocalDecl::new(raw_ptr_ty, DUMMY_SP));
        let mut deref = arg
            .place()
            .expect("Can't get the address of a constant");
        let mut projs = Vec::with_capacity(deref.projection.len() + 1);
        projs.extend(deref.projection);
        projs.push(ProjectionElem::Deref);
        deref.projection = tcx.intern_place_elems(&*projs);
        let cast_stmt = Statement {
            source_info: SourceInfo::outermost(DUMMY_SP),
            kind: StatementKind::Assign(Box::new((
                raw_ptr_local.into(),
                Rvalue::AddressOf(ptr_ty.mutbl, deref),
            ))),
        };
        new_stmts.push(cast_stmt);
        Operand::Move(raw_ptr_local.into())
    } else {
        arg.to_copy()
    };

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
    new_stmts.push(cast_stmt);
    Some((new_stmts, casted_arg))
}
