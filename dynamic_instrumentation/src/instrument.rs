use anyhow::Context;
use c2rust_analysis_rt::metadata::Metadata;
use c2rust_analysis_rt::mir_loc::{self, EventMetadata, Func, MirLoc, MirLocId, TransferKind};
use c2rust_analysis_rt::HOOK_FUNCTIONS;
use fs2::FileExt;
use fs_err::OpenOptions;
use indexmap::IndexSet;
use log::{debug, trace};
use rustc_ast::Mutability;
use rustc_index::vec::Idx;
use rustc_middle::mir::visit::{
    MutVisitor, MutatingUseContext, NonMutatingUseContext, PlaceContext, Visitor,
};
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, Body, BorrowKind, ClearCrossCrate, HasLocalDecls, Local, LocalDecl,
    Location, Operand, Place, PlaceElem, ProjectionElem, Rvalue, Safety, SourceInfo, Statement,
    StatementKind, Terminator, TerminatorKind, START_BLOCK,
};
use rustc_middle::ty::{self, Ty, TyCtxt};
use rustc_span::def_id::{DefId, DefPathHash};
use rustc_span::DUMMY_SP;
use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use std::sync::Mutex;

use crate::arg::{ArgKind, InstrumentationArg};
use crate::hooks::Hooks;
use crate::mir_utils::{has_outer_deref, remove_outer_deref, strip_all_deref};
use crate::point::cast_ptr_to_usize;
use crate::point::InstrumentationApplier;
use crate::point::{CheckAddressTakenLocals, CollectInstrumentationPoints, SubAddressTakenLocals};
use crate::util::Convert;

#[derive(Default)]
pub struct Instrumenter {
    mir_locs: Mutex<IndexSet<MirLoc>>,
    functions: Mutex<HashMap<mir_loc::DefPathHash, String>>,
}

impl Instrumenter {
    /// Create a new instrumentation object.
    ///
    /// A single [`Instrumenter`] instance should be shared across the
    /// entire crate being instrumented, as the indexed source locations are
    /// shared and should be global.
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_fn(&self, did: DefId, tcx: TyCtxt) {
        self.functions.lock().unwrap().insert(
            tcx.def_path_hash(did).convert(),
            tcx.item_name(did).to_string(),
        );
    }

    /// Instrument memory operations in-place in the function `body`.
    pub fn instrument_fn<'tcx>(&self, tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>, body_did: DefId) {
        let function_name = tcx.item_name(body_did);
        debug!("Instrumenting function {}", function_name);

        self.add_fn(body_did, tcx);
        debug!("Body before instrumentation: {:#?}", body);
        instrument_body(self, tcx, body, body_did);
        debug!("Body after instrumentation: {:#?}", body);
    }

    /// Finish instrumentation and write out metadata to `metadata_file_path`.
    pub fn finalize(&self, metadata_path: &Path) -> anyhow::Result<()> {
        let mut locs = self.mir_locs.lock().unwrap();
        let mut functions = self.functions.lock().unwrap();
        let locs = locs.drain(..).collect::<Vec<_>>();
        let functions = functions.drain().collect::<HashMap<_, _>>();
        let metadata = Metadata { locs, functions };
        let bytes = bincode::serialize(&metadata).context("Location serialization failed")?;
        let mut file = OpenOptions::new()
            .append(true)
            .write(true)
            .open(metadata_path)
            .context("Could not open metadata file")?;
        file.file().lock_exclusive()?;
        let e = file.write_all(&bytes);
        file.file().unlock()?;
        e?;
        Ok(())
    }

    /// Get the unique index corresponding to a particular MIR location.
    ///
    /// Returned indices will not be sorted in any particular order, but are
    /// unique and constant across the entire lifetime of this instrumentation
    /// instance.
    pub fn get_mir_loc_idx(
        &self,
        body_def: DefPathHash,
        location: Location,
        metadata: EventMetadata,
    ) -> MirLocId {
        let body_def = body_def.convert();
        let fn_name = self
            .functions
            .lock()
            .unwrap()
            .get(&body_def)
            .unwrap()
            .clone();
        let mir_loc = MirLoc {
            func: Func {
                def_path_hash: body_def,
                name: fn_name,
            },
            basic_block_idx: location.block.index(),
            statement_idx: location.statement_index,
            metadata,
        };
        let (idx, _) = self.mir_locs.lock().unwrap().insert_full(mir_loc);
        idx.try_into().unwrap()
    }
}

fn is_shared_or_unsafe_ptr(ty: Ty) -> bool {
    ty.is_unsafe_ptr() || (ty.is_region_ptr() && !ty.is_mutable_ptr())
}

fn is_region_or_unsafe_ptr(ty: Ty) -> bool {
    ty.is_unsafe_ptr() || ty.is_region_ptr()
}

impl<'tcx> Visitor<'tcx> for CheckAddressTakenLocals<'_, 'tcx> {
    fn visit_assign(&mut self, place: &Place<'tcx>, rvalue: &Rvalue<'tcx>, location: Location) {
        let locals = self.local_decls().clone();
        let ctx = self.tcx();

        let _op_ty = |op: &Operand<'tcx>| op.ty(&locals, ctx);
        let place_ty = |p: &Place<'tcx>| p.ty(&locals, ctx).ty;
        let _local_ty = |p: &Place| place_ty(&p.local.into());
        let value_ty = rvalue.ty(self, self.tcx());

        self.super_assign(place, rvalue, location);

        match rvalue {
            _ if !is_region_or_unsafe_ptr(value_ty) => {}

            Rvalue::AddressOf(_, p) | Rvalue::Ref(_, _, p) if !p.is_indirect() => {
                self.address_taken.insert(p.local);
            }
            _ => (),
        }
    }
}

impl<'tcx> MutVisitor<'tcx> for SubAddressTakenLocals<'tcx> {
    fn visit_place(
        &mut self,
        mut place: &mut Place<'tcx>,
        context: PlaceContext,
        location: Location,
    ) {
        // if we have found an address-taken local `_1`, substitute with `(*_2)`
        if let Some(substitute) = self.local_substitute.get(&place.local) {
            if context.is_use()
                && (!context.is_place_assignment()
                    || (context.is_place_assignment()
                        && !place.is_indirect()
                        && place.projection.len() > 0))
                && !context.is_drop()
            && !matches!(
                context,
                PlaceContext::NonMutatingUse(NonMutatingUseContext::Inspect)
            )
            {
                let projection = {
                    let mut v = Vec::with_capacity(place.projection.len() + 1);
                    v.extend([ProjectionElem::Deref]);
                    v.extend(place.projection);
                    self.tcx().intern_place_elems(&v)
                };
                place.local = *substitute;
                place.projection = projection;
            }
        }

        self.super_place(place, context, location)
    }

    fn tcx<'a>(&'a self) -> TyCtxt<'tcx> {
        self.tcx()
    }

    fn visit_body(&mut self, body: &mut Body<'tcx>) {
        let mut addr_taken_locals = Vec::new();

        // for each address-taken local, push a new local that will get assigned
        // its address
        for local in &self.address_taken {
            let arg_ty = Place::from(*local).ty(&body.local_decls, self.tcx()).ty;
            let inner_ty = ty::TypeAndMut {
                ty: arg_ty,
                mutbl: Mutability::Mut,
            };

            let raw_ptr_ty = self.tcx().mk_ptr(inner_ty);
            let raw_ptr_local = body.local_decls.push(LocalDecl::new(raw_ptr_ty, DUMMY_SP));

            self.local_substitute.insert(*local, raw_ptr_local);

            // set original local as mutable
            body.local_decls.get_mut(*local).unwrap().mutability = Mutability::Mut;
        }

        // mark location of address-taken arguments
        for l in 1..body.arg_count + 1 {
            let local = Local::from_u32(l as u32);
            if let Some(substitute) = self.local_substitute.get(&local) {
                println!("marking {local:?} = *{substitute:?}");
                // put into first block, first statement
                addr_taken_locals.push((Place::from(local), BasicBlock::from_u32(0), 0));
            }
        }

        // when the address-taken local is assigned to for the first time, we know it's active,
        // so mark the location of that assignment for the next step
        for (bid, block) in body.basic_blocks().iter_enumerated().rev() {
            for (sid, statement) in block.statements.iter().enumerate().rev() {
                if let StatementKind::Assign(stmt) = &statement.kind {
                    let (ref place, _) = **stmt;
                    if let Some(l) = place.as_local().filter(|l| self.address_taken.contains(l)) {
                        // put just below first assignment
                        println!("placing {place:?} in {bid:?}:{:?}", sid+1);
                        addr_taken_locals.push((*place, bid, sid + 1));
                        // self.address_taken.remove(&l);
                    }
                }
            }
            if let Some(term) = &block.terminator {
                match &term.kind {
                    TerminatorKind::Call {
                        func: _func,
                        args: _args,
                        destination,
                        target,
                        ..
                    } => {
                        if let Some(l) = destination
                            .as_local()
                            .filter(|l| self.address_taken.contains(l))
                        {
                            if let Some(next_block) = target {
                                // put into first statement of following block
                                addr_taken_locals.push((*destination, *next_block, 0));
                                // self.address_taken.remove(&l);
                            }
                        }
                    }
                    TerminatorKind::DropAndReplace {
                        place,
                        value,
                        target,
                        unwind: _,
                    } if value.place().is_some() => {
                        if let Some(local_substitute) =
                            place.as_local().and_then(|l| self.local_substitute.get(&l))
                        {
                            // put into first statement of following block
                            addr_taken_locals.push((*place, *target, 0));
                            self.local_substitute.insert(place.local, *local_substitute);
                        }
                    }
                    _ => (),
                }
            }
        }

        // visit places first, so that they're already re-written and so that we don't accidentally
        // rewrite locals in statements that we're about to insert
        self.super_body(body);

        // sort the insertion locations by statement id descending so that statement insertion
        // of sid N does not shift/invalidate insertion at M where M > N 
        addr_taken_locals.sort_by_key(|(_, _bid, sid)| *sid);
        let addr_taken_locals = addr_taken_locals.into_iter().rev();

        // Add `_y = &raw _x` for each address-taken local _x,
        // just below the original assignment `_x = ...`
        for (place, bid, sid) in addr_taken_locals {
            let addr_of_stmt = Statement {
                source_info: SourceInfo::outermost(DUMMY_SP),
                kind: StatementKind::Assign(Box::new((
                    self.local_substitute
                        .get(&place.local)
                        .cloned()
                        .unwrap()
                        .into(),
                    Rvalue::AddressOf(Mutability::Mut, place),
                ))),
            };

            body.basic_blocks_mut()[bid]
                .statements
                .insert(sid, addr_of_stmt);
        }

        for (bid, block) in body.basic_blocks().iter_enumerated() {
            for (sid, statement) in block.statements.iter().enumerate() {
                println!("{bid:?}:{sid:?} after: {statement:?}");
            }
            if let Some(term) = &block.terminator {
                println!("{bid:?} term after: {term:?}");
            }
        }
    }
}

impl<'tcx> Visitor<'tcx> for CollectInstrumentationPoints<'_, 'tcx> {
    fn visit_place(&mut self, place: &Place<'tcx>, context: PlaceContext, location: Location) {
        self.super_place(place, context, location);

        let field_fn = self.hooks().find("ptr_field");

        let base_ty = self.local_decls()[place.local].ty;

        // Instrument field projections on raw-ptr places
        if is_region_or_unsafe_ptr(base_ty) && context.is_use() {
            for (base, elem) in place.iter_projections() {
                if let PlaceElem::Field(field, _) = elem {
                    let proj_dest = || {
                        // Only the last field projection gets a destination
                        self.assignment()
                            .as_ref()
                            .map(|(dest, _)| dest)
                            .copied()
                            .filter(|_| base.projection.len() == place.projection.len() - 1)
                    };
                    self.loc(location, location, field_fn)
                        .arg_var(place.local)
                        .arg_index_of(field)
                        .source(place)
                        .dest_from(proj_dest)
                        .add_to(self);
                }
            }
        }
    }

    fn visit_assign(&mut self, dest: &Place<'tcx>, value: &Rvalue<'tcx>, location: Location) {
        let copy_fn = self.hooks().find("ptr_copy");
        let addr_local_fn = self.hooks().find("addr_of_local");
        let ptr_contrive_fn = self.hooks().find("ptr_contrive");
        let ptr_to_int_fn = self.hooks().find("ptr_to_int");
        let load_value_fn = self.hooks().find("load_value");
        let store_value_fn = self.hooks().find("store_value");
        let store_fn = self.hooks().find("ptr_store");
        let load_fn = self.hooks().find("ptr_load");

        let dest = *dest;
        self.with_assignment((dest, value.clone()), |this| {
            this.visit_rvalue(value, location)
        });

        let locals = self.local_decls().clone();
        let ctx = self.tcx();

        let op_ty = |op: &Operand<'tcx>| op.ty(&locals, ctx);
        let place_ty = |p: &Place<'tcx>| p.ty(&locals, ctx).ty;
        let local_ty = |p: &Place| place_ty(&p.local.into());
        let value_ty = value.ty(self, self.tcx());

        self.visit_place(
            &dest,
            PlaceContext::MutatingUse(MutatingUseContext::Store),
            location,
        );

        let mut add_load_instr = |p: &Place<'tcx>| {
            self.loc(location, location, load_fn)
                .arg_var(p.local)
                .source(&remove_outer_deref(*p, ctx))
                .add_to(self);
        };

        // add instrumentation for load-from-address operations
        match value {
            Rvalue::Use(Operand::Copy(p) | Operand::Move(p))
                if p.is_indirect() && is_region_or_unsafe_ptr(local_ty(p)) =>
            {
                add_load_instr(p)
            }
            _ => (),
        }

        match value {
            _ if dest.is_indirect() => {
                // Strip all derefs to set base_dest to the pointer that is deref'd
                let base_dest = strip_all_deref(&dest, self.tcx());

                self.loc(location, location, store_fn)
                    .arg_var(base_dest)
                    .source(&remove_outer_deref(dest, self.tcx()))
                    .add_to(self);

                if is_region_or_unsafe_ptr(value_ty) {
                    self.loc(location, location.successor_within_block(), store_value_fn)
                        .arg_var(dest)
                        .source(value)
                        .dest(&dest)
                        .add_to(self);
                }
            }
            Rvalue::Cast(_, Operand::Copy(p) | Operand::Move(p), _)
                if value_ty.is_integral() && !p.is_indirect() =>
            {
                if is_region_or_unsafe_ptr(place_ty(p)) {
                    self.loc(location, location, ptr_to_int_fn)
                        .arg_var(p.local)
                        .source(p)
                        .add_to(self);
                }
            }
            _ if !is_region_or_unsafe_ptr(value_ty) => {}
            Rvalue::AddressOf(_, p) | Rvalue::Ref(_, _, p)
                if has_outer_deref(p)
                    && is_region_or_unsafe_ptr(place_ty(&remove_outer_deref(*p, self.tcx()))) =>
            {
                let source = remove_outer_deref(*p, self.tcx());
                // Instrument which local's address is taken
                self.loc(location, location.successor_within_block(), copy_fn)
                    .arg_var(dest)
                    .source(&source)
                    .dest(&dest)
                    .add_to(self);
            }
            Rvalue::AddressOf(_, p) => {
                // Instrument which local's address is taken
                self.loc(location, location.successor_within_block(), addr_local_fn)
                    .arg_var(dest)
                    .arg_index_of(p.local)
                    .source(p)
                    .dest(&dest)
                    .add_to(self);
            }
            Rvalue::Use(Operand::Copy(p) | Operand::Move(p)) if p.is_indirect() => {
                // We're dereferencing something, the result of which is a reference or pointer
                self.loc(location, location.successor_within_block(), load_value_fn)
                    .arg_var(dest)
                    .dest(&dest)
                    .add_to(self);
            }
            Rvalue::Use(Operand::Constant(..)) => {
                // Track (as copies) assignments that give local names to constants so that code
                // taking references to said constants can refer to these assignments as sources.
                // TODO: should be replaced by AddrOfStatic when support for that is added
                self.loc(location, location.successor_within_block(), copy_fn)
                    .arg_var(dest)
                    .dest(&dest)
                    .debug_mir()
                    .add_to(self);
            }
            Rvalue::Use(Operand::Copy(p) | Operand::Move(p)) => {
                self.loc(location, location.successor_within_block(), copy_fn)
                    .arg_var(dest)
                    .source(p)
                    .dest(&dest)
                    .add_to(self);
            }
            Rvalue::Cast(_, op, _) => {
                let func = if op_ty(op).is_integral() {
                    ptr_contrive_fn
                } else {
                    copy_fn
                };
                self.loc(location, location.successor_within_block(), func)
                    .arg_var(dest)
                    .source(op)
                    .dest(&dest)
                    .add_to(self);
            }
            Rvalue::Ref(_, bkind, p) if !p.is_indirect() => {
                let source = remove_outer_deref(*p, self.tcx());
                if let BorrowKind::Mut { .. } = bkind {
                    // Instrument which local's address is taken
                    self.loc(location, location, addr_local_fn)
                        .arg_addr_of(*p)
                        .arg_index_of(p.local)
                        .source(&source)
                        .dest(&dest)
                        .add_to(self);
                } else {
                    // Instrument immutable borrows by tracing the reference itself
                    self.loc(location, location.successor_within_block(), addr_local_fn)
                        .arg_var(dest)
                        .arg_index_of(p.local)
                        .source(&source)
                        .dest(&dest)
                        .add_to(self);
                };
            }
            _ => (),
        }
    }

    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, location: Location) {
        self.super_terminator(terminator, location);

        let arg_fn = self.hooks().find("ptr_copy");
        let ret_fn = self.hooks().find("ptr_ret");

        match &terminator.kind {
            TerminatorKind::Call {
                func,
                args,
                destination,
                target,
                ..
            } => {
                let mut callee_arg: Place = Local::new(1).into();
                let is_hook = {
                    if let ty::FnDef(def_id, _) = func.ty(self, self.tcx()).kind() {
                        let fn_name = self.tcx().item_name(*def_id);
                        HOOK_FUNCTIONS.contains(&fn_name.as_str())
                    } else {
                        false
                    }
                };
                let func_kind = func.ty(self, self.tcx()).kind();
                let transfer_kind = if let &ty::FnDef(def_id, _) = func_kind {
                    TransferKind::Arg(self.tcx().def_path_hash(def_id).convert())
                } else {
                    TransferKind::None
                };
                if !is_hook {
                    for arg in args {
                        if let Some(place) = arg.place() {
                            let place_ty = place.ty(self, self.tcx()).ty;
                            if is_shared_or_unsafe_ptr(place_ty) {
                                self.loc(location, location, arg_fn)
                                    .arg_var(place)
                                    .source(&place)
                                    .dest(&callee_arg)
                                    .transfer(transfer_kind)
                                    .add_to(self);
                            }
                        }
                        callee_arg.local.increment_by(1);
                    }
                }
                if let (&ty::FnDef(def_id, _), &Some(target)) = (func_kind, target) {
                    trace!("term: {:?}", terminator.kind);
                    let fn_name = self.tcx().item_name(def_id);
                    if HOOK_FUNCTIONS.contains(&fn_name.as_str()) {
                        let func_def_id = self.hooks().find_from_symbol(fn_name);

                        // Hooked function called; trace args
                        self.loc(location, location, func_def_id)
                            .source(args)
                            .dest(destination)
                            .after_call()
                            .transfer(TransferKind::Ret(self.func_hash()))
                            .arg_vars(args.iter().cloned())
                            .add_to(self);
                    } else if is_region_or_unsafe_ptr(destination.ty(self, self.tcx()).ty) {
                        let instrumentation_location = Location {
                            statement_index: 0,
                            block: target,
                        };

                        self.loc(location, instrumentation_location, arg_fn)
                            .source(&0)
                            .dest(destination)
                            .transfer(TransferKind::Ret(
                                self.tcx().def_path_hash(def_id).convert(),
                            ))
                            .arg_var(*destination)
                            .add_to(self);
                    }
                }
            }
            TerminatorKind::Return => {
                let place = Place::return_place();
                if is_region_or_unsafe_ptr(self.local_decls()[place.local].ty) {
                    self.loc(location, location, ret_fn)
                        .arg_var(place)
                        .add_to(self);
                }
            }
            _ => (),
        }
    }
}

fn instrument_body<'a, 'tcx>(
    state: &Instrumenter,
    tcx: TyCtxt<'tcx>,
    body: &'a mut Body<'tcx>,
    body_did: DefId,
) {
    let hooks = Hooks::new(tcx);

    for (i, b) in body.basic_blocks().iter().enumerate() {
        for s in &b.statements {
            println!("bb{i:?} before: {s:?}");
        }
        println!("bb{i:?} before: {:?}", b.terminator())
    }
    println!();

    let address_taken_locals = {
        let mut local_visitor = CheckAddressTakenLocals::new(tcx, body);
        local_visitor.visit_body(body);
        local_visitor.address_taken
    };
    println!("address taken: {:?}", address_taken_locals);

    let mut local_rewriter = SubAddressTakenLocals::new(tcx, address_taken_locals);
    local_rewriter.visit_body(body);
    println!(
        "address taken substitutions: {:?}",
        local_rewriter.local_substitute
    );

    // The local rewriter above rewrites address-taken locals with (*_x) where _x
    // is a pointer, resulting in an unsafe operation. To allow this, set all scopes
    // as unsafe
    for scope in &mut body.source_scopes {
        if let ClearCrossCrate::Set(data) = &mut scope.local_data {
            data.safety = Safety::BuiltinUnsafe;
        }
    }

    // collect instrumentation points
    let mut collector = CollectInstrumentationPoints::new(tcx, hooks, body);
    collector.visit_body(body);
    let points = collector.into_instrumentation_points();

    // insert instrumentation
    let mut applier = InstrumentationApplier::new(state, tcx, body, body_did);
    applier.apply_points(&points);

    // Apply `main`-specific instrumentation if this fn is main
    let main_did = tcx.entry_fn(()).map(|(def_id, _)| def_id);
    if Some(body_did) == main_did {
        instrument_entry_fn(tcx, hooks, body);
    }

    for (i, b) in body.basic_blocks().iter().enumerate() {
        for s in &b.statements {
            println!("bb{i:?} after: {s:?}");
        }
        println!("bb{i:?} after: {:?}", b.terminator())
    }
    println!();
}

/// Add initialization code to the body of a function known to be the binary entrypoint
fn instrument_entry_fn<'tcx>(tcx: TyCtxt<'tcx>, hooks: Hooks, body: &mut Body<'tcx>) {
    let init_fn = hooks.find("initialize");
    let fini_fn = hooks.find("finalize");

    let _ = insert_call(tcx, body, START_BLOCK, 0, init_fn, vec![]);

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
        let _ = insert_call(tcx, body, block, 0, fini_fn, vec![]);
    }
    for block in resume_blocks {
        let _ = insert_call(tcx, body, block, 0, fini_fn, vec![]);
    }
}

/// Inserts a call to `func`.
///
/// The call will be inserted before the statement at index `statement_index` in `block`.
/// If `statement_index` is the number of statements in the block,
/// the call will be inserted at the end of the block.
///
/// `func` must not unwind, as it will have no cleanup destination.
/// Returns the successor basic block and the local slot for the inserted call's return value.
pub fn insert_call<'tcx>(
    tcx: TyCtxt<'tcx>,
    body: &mut Body<'tcx>,
    block: BasicBlock,
    statement_index: usize,
    func: DefId,
    mut args: Vec<InstrumentationArg<'tcx>>,
) -> (BasicBlock, Local) {
    trace!("ST: {:?}", statement_index);

    let blocks = body.basic_blocks.as_mut();
    let locals = &mut body.local_decls;

    let successor_stmts = blocks[block].statements.split_off(statement_index);
    let successor_terminator = blocks[block].terminator.take();
    let successor_block = blocks.push(BasicBlockData {
        statements: successor_stmts,
        terminator: successor_terminator,
        is_cleanup: blocks[block].is_cleanup,
    });

    for arg in &mut args {
        if let Some((cast_stmts, cast_local)) = cast_ptr_to_usize(tcx, locals, arg) {
            *arg = InstrumentationArg::Op(ArgKind::AddressUsize(cast_local));
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
            args: args.iter().map(|arg| arg.inner().clone()).collect(),
            destination: ret_local.into(),
            target: Some(successor_block),
            cleanup: None,
            from_hir_call: true,
            fn_span: DUMMY_SP,
        },
        source_info: SourceInfo::outermost(DUMMY_SP),
    };
    blocks[block].terminator.replace(call);

    (successor_block, ret_local)
}
