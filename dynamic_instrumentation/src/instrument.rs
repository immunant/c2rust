use anyhow::Context;
use c2rust_analysis_rt::metadata::Metadata;
use c2rust_analysis_rt::mir_loc::{EventMetadata, Func, FuncId, MirLoc, MirLocId, TransferKind};
use c2rust_analysis_rt::HOOK_FUNCTIONS;
use fs2::FileExt;
use fs_err::OpenOptions;
use indexmap::IndexSet;
use log::{debug, trace};
use rustc_ast::Mutability;
use rustc_index::vec::Idx;
use rustc_middle::mir::visit::{MutVisitor, MutatingUseContext, PlaceContext, Visitor};
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, Body, BorrowKind, ClearCrossCrate, HasLocalDecls, Local, LocalDecl,
    Location, Operand, Place, PlaceElem, ProjectionElem, Rvalue, Safety, SourceInfo, SourceScope,
    SourceScopeData, Statement, StatementKind, Terminator, TerminatorKind, START_BLOCK,
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
use crate::point::InstrumentationApplier;
use crate::point::{cast_ptr_to_usize, InstrumentationPriority};
use crate::point::{
    CollectAddressTakenLocals, CollectInstrumentationPoints, RewriteAddressTakenLocals,
};
use crate::util::Convert;

#[derive(Default)]
pub struct Instrumenter {
    mir_locs: Mutex<IndexSet<MirLoc>>,
    functions: Mutex<HashMap<FuncId, String>>,
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
            FuncId(tcx.def_path_hash(did).convert()),
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
        let fn_id = FuncId(body_def.convert());
        let fn_name = self.functions.lock().unwrap().get(&fn_id).unwrap().clone();
        let mir_loc = MirLoc {
            func: Func {
                id: fn_id,
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

impl<'tcx> Visitor<'tcx> for CollectAddressTakenLocals<'_, 'tcx> {
    /// Checks the right hand side of each MIR assignment statement for taking the
    /// address of a local, which occurs only in [`Rvalue`]s. This may
    /// happen explicitly with something like [`std::ptr::addr_of`]`!` or
    /// implicitly by taking a reference `&` of a local. Instances/locations
    /// of address-taking are saved for a later walk over the MIR AST with
    /// [`RewriteAddressTakenLocals`].
    fn visit_assign(&mut self, place: &Place<'tcx>, rvalue: &Rvalue<'tcx>, location: Location) {
        self.super_assign(place, rvalue, location);
        let p = match rvalue {
            Rvalue::AddressOf(_, p) | Rvalue::Ref(_, _, p) => p,
            _ => return,
        };
        let value_ty = rvalue.ty(self, self.tcx());
        if is_region_or_unsafe_ptr(value_ty) && p.projection.is_empty() {
            self.address_taken.insert(p.local);
        }
    }
}

impl<'tcx> MutVisitor<'tcx> for RewriteAddressTakenLocals<'tcx> {
    /// Rewrites an address-taken local in terms of its underlying address.
    fn visit_place(
        &mut self,
        mut place: &mut Place<'tcx>,
        context: PlaceContext,
        location: Location,
    ) {
        // If we have found an address-taken local `_x`, substitute with `(*_y)` where `_y`
        // is the address of `_x`.
        if let Some(substitute) = self.local_to_address.get(&place.local) {
            // We only want to rewrite address-taken locals that are not assigned to in
            // a MIR assignment statement, unless the assignment is to one of that local's
            // projections, such as one of its fields. The reason for this is that the local
            // needs to be initialized at least once to make the compiler happy and not throw
            // an error claiming use without initialization (such as when taking its address)
            let is_assignment_to_local_projection = context.is_place_assignment()
                && !place.is_indirect()
                && !place.projection.is_empty();
            let is_non_assignment_use = !context.is_place_assignment();

            if context.is_use()
                && (is_non_assignment_use || is_assignment_to_local_projection)
                // maintain drop semantics for original address-taken local -- the liveness
                // properties of its address are not necessarily the same, and dropping
                // `(*_y)` is undesirable
                && !context.is_drop()
            {
                // add deref
                let projection = {
                    let v = [ProjectionElem::Deref]
                        .into_iter()
                        .chain(place.projection)
                        .collect::<Vec<_>>();
                    self.tcx().intern_place_elems(&v)
                };
                // replace `_x` with `_y`
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
        // for each address-taken local, push a new local that will get assigned
        // its address
        for local in self.address_taken.iter().copied() {
            let arg_ty = Place::from(local).ty(&body.local_decls, self.tcx()).ty;
            let inner_ty = ty::TypeAndMut {
                ty: arg_ty,
                mutbl: Mutability::Mut,
            };

            let raw_ptr_ty = self.tcx().mk_ptr(inner_ty);
            let raw_ptr_local = body.local_decls.push(LocalDecl::new(raw_ptr_ty, DUMMY_SP));

            self.local_to_address.insert(local, raw_ptr_local);

            // set original local as mutable, because it's mutably borrowed when it has
            // its address taken
            body.local_decls.get_mut(local).unwrap().mutability = Mutability::Mut;
        }

        // START ADDRESS-TAKING STATEMENTS
        //
        // The code below appends context-dependent MIR locations to a collection
        // that will later be used as reference for where to insert the first instance
        // of taking the address of an already-determined-to-be address-taken local.
        // E.g. if `_x` is an address-taken local, below is what determines where the
        // statement `_y = &raw _x` will be placed.

        let mut local_address_statements = Vec::new();

        // assign insertion location of address-taken arguments, skipping over
        // return local 0
        for arg_idx in 1..=body.arg_count {
            let arg_local = Local::from_usize(arg_idx);
            if self.local_to_address.get(&arg_local).is_some() {
                // put into first block, first statement
                local_address_statements.push((Place::from(arg_local), Location::START));
            }
        }

        // when the address-taken local is assigned to for the first time, we know it's active,
        // so insert `_y = &raw x` just below that assignment, which is necessary because
        // otherwise the address-taking statement would be taking the address of an uninitialized
        // variable. For assignment statements, place `_y = &raw _x` one statement below. For
        // terminators with a destination to the address-taken local, or drop and replace
        // statements, put the address-taking statement in the first statement of the successor
        // block
        for (bid, block) in body.basic_blocks().iter_enumerated().rev() {
            for (sid, statement) in block.statements.iter().enumerate().rev() {
                if let StatementKind::Assign(stmt) = &statement.kind {
                    let (ref place, _) = **stmt;
                    if place
                        .as_local()
                        .filter(|local| self.address_taken.contains(local))
                        .is_some()
                    {
                        // put just below first assignment
                        local_address_statements.push((
                            *place,
                            Location {
                                block: bid,
                                statement_index: sid + 1,
                            },
                        ));
                    }
                }
            }
            if let Some(term) = &block.terminator {
                match &term.kind {
                    TerminatorKind::Call {
                        func: _,
                        args: _,
                        destination,
                        target,
                        ..
                    } => {
                        if destination
                            .as_local()
                            .filter(|l| self.address_taken.contains(l))
                            .is_some()
                        {
                            if let Some(next_block) = target {
                                // put into first statement of following block
                                local_address_statements.push((
                                    *destination,
                                    Location {
                                        block: *next_block,
                                        statement_index: 0,
                                    },
                                ));
                            }
                        }
                    }
                    TerminatorKind::DropAndReplace {
                        place,
                        value,
                        target,
                        unwind: _,
                    } if value.place().is_some() => {
                        if let Some(local_to_address) =
                            place.as_local().and_then(|l| self.local_to_address.get(&l))
                        {
                            // put into first statement of following block
                            local_address_statements.push((
                                *place,
                                Location {
                                    block: *target,
                                    statement_index: 0,
                                },
                            ));
                            self.local_to_address.insert(place.local, *local_to_address);
                        }
                    }
                    _ => (),
                }
            }
        }

        // END ADDRESS-TAKING STATEMENTS
        // visit places first before inserting `_y = &raw _x`, so that we don't accidentally
        // rewrite `_y = &raw _x` into `y = &raw (*_y)`
        self.super_body(body);

        // sort the insertion locations by statement id descending so that statement insertion
        // for statements with indices N and M, where N < M, does not shift/invalidate index M
        local_address_statements.sort_by_key(|(_, loc)| loc.statement_index);
        let local_address_statements = local_address_statements.into_iter().rev();

        // Add `_y = &raw _x` for each address-taken local _x
        for (addr_taken_local, loc) in local_address_statements {
            let addr_of_stmt = Statement {
                source_info: SourceInfo::outermost(DUMMY_SP),
                kind: StatementKind::Assign(Box::new((
                    self.local_to_address
                        .get(&addr_taken_local.local)
                        .copied()
                        .unwrap()
                        .into(),
                    Rvalue::AddressOf(Mutability::Mut, addr_taken_local),
                ))),
            };

            body.basic_blocks_mut()[loc.block]
                .statements
                .insert(loc.statement_index, addr_of_stmt);
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
        let store_addr_taken_fn = self.hooks().find("ptr_store_addr_taken");
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

        // Instrument assignment to a local that is address-taken, even
        // if its address was taken after this assignment statement
        match self.addr_taken_local_addresses.get(&dest.local) {
            Some(address) if dest.projection.is_empty() => {
                let addr_of_local = Place::from(*address);
                // TODO: this is a hack that places the store_addr_taken_fn
                // after the instrumentation for taking the address of that local,
                // which must be in place prior to this instrumentation.
                let num_statements = self.body.basic_blocks()[location.block].statements.len();
                let store_addr_taken_loc = Location {
                    block: location.block,
                    // +1 to ensure `dest` is in scope
                    // +1 to be placed after address-taking statement
                    statement_index: std::cmp::min(num_statements, location.statement_index + 2),
                };
                self.loc(
                    location,
                    store_addr_taken_loc, // to be placed after address-of-local instrumentation
                    store_addr_taken_fn,
                )
                .arg_addr_of(dest)
                .source(&addr_of_local)
                .add_to(self);
            }
            _ => (),
        }

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
            Rvalue::AddressOf(_, p)
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
                    .instrumentation_priority(InstrumentationPriority::Early)
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
            Rvalue::Ref(_, bkind, p)
                if has_outer_deref(p)
                    && is_region_or_unsafe_ptr(place_ty(&remove_outer_deref(*p, self.tcx()))) =>
            {
                // this is a reborrow or field reference, i.e. _2 = &(*_1)
                let source = remove_outer_deref(*p, self.tcx());
                if let BorrowKind::Mut { .. } = bkind {
                    // Instrument which local's address is taken
                    self.loc(location, location, copy_fn)
                        .arg_addr_of(*p)
                        .source(&source)
                        .dest(&dest)
                        .add_to(self);
                } else {
                    // Instrument immutable borrows by tracing the reference itself
                    self.loc(location, location.successor_within_block(), copy_fn)
                        .arg_var(dest)
                        .source(&source)
                        .dest(&dest)
                        .add_to(self);
                };
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
                        .instrumentation_priority(InstrumentationPriority::Early)
                        .add_to(self);
                } else {
                    // Instrument immutable borrows by tracing the reference itself
                    self.loc(location, location.successor_within_block(), addr_local_fn)
                        .arg_var(dest)
                        .arg_index_of(p.local)
                        .source(&source)
                        .dest(&dest)
                        .instrumentation_priority(InstrumentationPriority::Early)
                        .add_to(self);
                };
            }
            _ => (),
        }
    }

    /// Add an instrumentation to mark the start of the body.
    fn visit_body(&mut self, body: &Body<'tcx>) {
        self.super_body(body);

        if self.instrumentation_points.is_empty() {
            return;
        }
        let body_begin_func = self.hooks().find("mark_begin_body");
        let start_loc = Location::START;
        self.loc(start_loc, start_loc, body_begin_func).add_to(self);
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
                    TransferKind::Arg(FuncId(self.tcx().def_path_hash(def_id).convert()))
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
                            .transfer(TransferKind::Ret(self.func_id()))
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
                            .transfer(TransferKind::Ret(FuncId(
                                self.tcx().def_path_hash(def_id).convert(),
                            )))
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

fn mark_scopes_unsafe(scopes: &mut rustc_index::vec::IndexVec<SourceScope, SourceScopeData>) {
    for scope in scopes {
        if let ClearCrossCrate::Set(data) = &mut scope.local_data {
            data.safety = Safety::BuiltinUnsafe;
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

    let address_taken_locals = {
        let mut local_visitor = CollectAddressTakenLocals::new(tcx, body);
        local_visitor.visit_body(body);
        local_visitor.address_taken
    };

    let local_to_address = {
        let mut local_rewriter = RewriteAddressTakenLocals::new(tcx, address_taken_locals);
        local_rewriter.visit_body(body);
        local_rewriter.local_to_address
    };

    // The local rewriter above rewrites address-taken locals with (*_x) where _x
    // is a pointer, resulting in an unsafe operation. To allow this, set all scopes
    // as unsafe
    mark_scopes_unsafe(&mut body.source_scopes);

    // collect instrumentation points
    let points = {
        let mut collector = CollectInstrumentationPoints::new(tcx, hooks, body, local_to_address);
        collector.visit_body(body);
        collector.into_instrumentation_points()
    };

    // insert instrumentation
    let mut applier = InstrumentationApplier::new(state, tcx, body, body_did);
    applier.apply_points(&points);

    // Apply `main`-specific instrumentation if this fn is main
    let main_did = tcx.entry_fn(()).map(|(def_id, _)| def_id);
    if Some(body_did) == main_did {
        instrument_entry_fn(tcx, hooks, body);
    }
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
