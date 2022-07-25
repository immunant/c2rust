use anyhow::Context;
use c2rust_analysis_rt::metadata::Metadata;
use c2rust_analysis_rt::mir_loc::{
    self, EventMetadata, Func, MirLoc, MirLocId, MirPlace, MirProjection, TransferKind,
};
use c2rust_analysis_rt::HOOK_FUNCTIONS;
use indexmap::IndexSet;
use log::debug;
use rustc_data_structures::fingerprint::Fingerprint;
use rustc_index::vec::{Idx, IndexVec};
use rustc_middle::mir::visit::{MutatingUseContext, PlaceContext, Visitor};
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, Body, BorrowKind, CastKind, Constant, Local, LocalDecl, Location,
    Mutability, Operand, Place, PlaceElem, PlaceRef, ProjectionElem, Rvalue, SourceInfo, Statement,
    StatementKind, Terminator, TerminatorKind, START_BLOCK,
};
use rustc_middle::ty::{self, ParamEnv, TyCtxt, TyS};
use rustc_span::def_id::{DefId, DefPathHash, CRATE_DEF_INDEX};
use rustc_span::{Symbol, DUMMY_SP};
use std::collections::HashMap;
use std::fs::File;
use std::mem;
use std::path::Path;
use std::sync::Mutex;

/// Like [`From`] and [`Into`], but can't `impl` those because of the orphan rule.
trait Convert<T> {
    fn convert(self) -> T;
}

impl Convert<Fingerprint> for mir_loc::Fingerprint {
    fn convert(self) -> Fingerprint {
        let Self(a, b) = self;
        Fingerprint::new(a, b)
    }
}

impl Convert<mir_loc::Fingerprint> for Fingerprint {
    fn convert(self) -> mir_loc::Fingerprint {
        self.as_value().into()
    }
}

impl Convert<DefPathHash> for mir_loc::DefPathHash {
    fn convert(self) -> DefPathHash {
        DefPathHash(self.0.convert())
    }
}

impl Convert<mir_loc::DefPathHash> for DefPathHash {
    fn convert(self) -> mir_loc::DefPathHash {
        mir_loc::DefPathHash(self.0.convert())
    }
}

pub struct InstrumentMemoryOps {
    mir_locs: Mutex<IndexSet<MirLoc>>,
    functions: Mutex<HashMap<mir_loc::DefPathHash, String>>,
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
            tcx.def_path_hash(body_did).convert(),
            tcx.item_name(body_did).to_string(),
        );
        debug!("Body before instrumentation: {:#?}", body);
        instrument_body(self, tcx, body, body_did);
        debug!("Body after instrumentation: {:#?}", body);
    }

    /// Finish instrumentation and write out metadata to `metadata_file_path`.
    pub fn finalize(&self, metadata_file_path: &Path) -> anyhow::Result<()> {
        let mut locs = self.mir_locs.lock().unwrap();
        let mut functions = self.functions.lock().unwrap();
        let locs = locs.drain(..).collect::<Vec<_>>();
        let functions = functions.drain().collect::<HashMap<_, _>>();
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
    fn get_mir_loc_idx(
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

#[derive(Clone, Debug)]
enum ArgKind<'tcx> {
    AddressUsize(Operand<'tcx>),
    Reference(Operand<'tcx>),
    RawPtr(Operand<'tcx>),
}

impl<'tcx> ArgKind<'tcx> {
    fn inner(&self) -> &Operand<'tcx> {
        use ArgKind::*;
        match self {
            AddressUsize(x) => x,
            Reference(x) => x,
            RawPtr(x) => x,
        }
    }

    fn from_type(op: Operand<'tcx>, ty: &ty::Ty<'tcx>) -> Self {
        use ArgKind::*;
        (if ty.is_unsafe_ptr() {
            RawPtr
        } else if ty.is_region_ptr() {
            Reference
        } else if ty.is_integral() {
            AddressUsize
        } else {
            panic!("operand is not of integer-castable type: {:?}", op)
        })(op)
    }
}

#[derive(Clone, Debug)]
enum InstrumentationArg<'tcx> {
    AddrOf(Operand<'tcx>),
    Op(ArgKind<'tcx>),
}

impl<'tcx> InstrumentationArg<'tcx> {
    fn inner(&self) -> &Operand<'tcx> {
        use InstrumentationArg::*;
        match self {
            AddrOf(x) => x,
            Op(x) => x.inner(),
        }
    }
}

#[derive(Clone)]
struct InstrumentationPoint<'tcx> {
    id: usize,
    loc: Location,
    func: DefId,
    args: Vec<InstrumentationArg<'tcx>>,
    is_cleanup: bool,
    after_call: bool,
    metadata: EventMetadata,
}

struct InstrumentationAdder<'a, 'tcx: 'a> {
    tcx: TyCtxt<'tcx>,
    body: &'a Body<'tcx>,
    runtime_crate_did: DefId,

    instrumentation_points: Vec<InstrumentationPoint<'tcx>>,

    assignment: Option<(Place<'tcx>, Rvalue<'tcx>)>,
}

struct InstrumentationBuilder<'a, 'tcx: 'a, S: 'tcx> {
    tcx: TyCtxt<'tcx>,
    body: &'a Body<'tcx>,
    state: S,
}

impl<'a, 'tcx: 'a> InstrumentationAdder<'a, 'tcx> {
    fn loc(
        &self,
        loc: Location,
        func: DefId,
    ) -> InstrumentationBuilder<'a, 'tcx, ReadyToInstrument<'tcx>> {
        InstrumentationBuilder {
            tcx: self.tcx,
            body: self.body,
            state: ReadyToInstrument {
                point: InstrumentationPoint {
                    id: 0,
                    loc,
                    func,
                    args: vec![],
                    is_cleanup: false,
                    after_call: false,
                    metadata: EventMetadata::default(),
                },
            },
        }
    }

    fn into_instrumentation_points(mut self) -> Vec<InstrumentationPoint<'tcx>> {
        // Sort by reverse location so that we can split blocks without
        // perturbing future statement indices
        self.instrumentation_points.sort_unstable_by(|a, b| {
            b.loc
                .cmp(&a.loc)
                .then(b.after_call.cmp(&a.after_call))
                .then(b.id.cmp(&a.id))
        });
        self.instrumentation_points
    }

    fn find_instrumentation_def(&self, name: Symbol) -> Option<DefId> {
        find_instrumentation_def(self.tcx, self.runtime_crate_did, name)
    }

    fn func_hash(&self) -> mir_loc::DefPathHash {
        self.tcx.def_path_hash(self.body.source.def_id()).convert()
    }
}

fn is_shared_or_unsafe_ptr(ty: &TyS) -> bool {
    ty.is_unsafe_ptr() || (ty.is_region_ptr() && !ty.is_mutable_ptr())
}

fn is_region_or_unsafe_ptr(ty: &TyS) -> bool {
    ty.is_unsafe_ptr() || ty.is_region_ptr()
}

fn has_outer_deref(p: &Place) -> bool {
    matches!(
        p.iter_projections().last(),
        Some((_, ProjectionElem::Deref))
    )
}

/// Get the inner-most dereferenced [`Place`].
fn strip_all_deref<'tcx>(p: &Place<'tcx>, tcx: TyCtxt<'tcx>) -> Place<'tcx> {
    let mut base_dest = p.as_ref();
    let mut place_ref = p.clone().as_ref();
    while let Some((cur_ref, proj)) = place_ref.last_projection() {
        if let ProjectionElem::Deref = proj {
            base_dest = cur_ref;
        }
        place_ref = cur_ref;
    }

    Place {
        local: base_dest.local,
        projection: tcx.intern_place_elems(base_dest.projection),
    }
}

/// Used to strip initital deref from projection sequences
fn remove_outer_deref<'tcx>(p: Place<'tcx>, tcx: TyCtxt<'tcx>) -> Place<'tcx> {
    // Remove outer deref if present
    if let PlaceRef {
        local,
        projection: &[ref base @ .., ProjectionElem::Deref],
    } = p.as_ref()
    {
        return Place {
            local,
            projection: tcx.intern_place_elems(base),
        };
    }

    p
}

fn to_mir_place(place: &Place) -> MirPlace {
    MirPlace {
        local: place.local.as_u32().into(),
        projection: place
            .projection
            .iter()
            .map(|p| match p {
                ProjectionElem::Deref => MirProjection::Deref,
                ProjectionElem::Field(field_id, _) => MirProjection::Field(field_id.into()),
                ProjectionElem::Index(local) => MirProjection::Index(local.into()),
                _ => MirProjection::Unsupported,
            })
            .collect(),
    }
}

// gets the one and only input Place, if applicable
fn rv_place<'tcx>(rv: &'tcx Rvalue) -> Option<Place<'tcx>> {
    match rv {
        Rvalue::Use(op) => op.place(),
        Rvalue::Repeat(op, _) => op.place(),
        Rvalue::Ref(_, _, p) => Some(*p),
        // ThreadLocalRef
        Rvalue::AddressOf(_, p) => Some(*p),
        Rvalue::Len(p) => Some(*p),
        Rvalue::Cast(_, op, _) => op.place(),
        // BinaryOp
        // CheckedBinaryOp
        // NullaryOp
        Rvalue::UnaryOp(_, op) => op.place(),
        Rvalue::Discriminant(p) => Some(*p),
        // Aggregate
        Rvalue::ShallowInitBox(op, _) => op.place(),
        _ => None,
    }
}

trait Source {
    fn source(&self) -> Option<MirPlace>;
}

impl Source for Place<'_> {
    fn source(&self) -> Option<MirPlace> {
        Some(to_mir_place(self))
    }
}

impl Source for Operand<'_> {
    fn source(&self) -> Option<MirPlace> {
        self.place().as_ref().and_then(Place::source)
    }
}

impl Source for Vec<Operand<'_>> {
    fn source(&self) -> Option<MirPlace> {
        // TODO: have hook-specific sources
        self.first()
            .and_then(|op| op.place().as_ref().map(to_mir_place))
    }
}

impl Source for Rvalue<'_> {
    fn source(&self) -> Option<MirPlace> {
        rv_place(self).as_ref().and_then(Place::source)
    }
}

impl Source for Local {
    fn source(&self) -> Option<MirPlace> {
        Place::from(*self).source()
    }
}

impl Source for u32 {
    fn source(&self) -> Option<MirPlace> {
        Local::from_u32(*self).source()
    }
}

trait InstrumentationState {}

struct Uninitialized {}
struct NeedsLoc {}
struct ReadyToInstrument<'tcx> {
    point: InstrumentationPoint<'tcx>,
}
impl InstrumentationState for NeedsLoc {}
impl<'tcx> InstrumentationState for ReadyToInstrument<'tcx> {}
impl InstrumentationState for Uninitialized {}

trait IntoOperand<'tcx> {
    fn op<'a>(&'a self, tcx: TyCtxt<'tcx>) -> Operand<'tcx>;
}

impl<'tcx> IntoOperand<'tcx> for Place<'tcx> {
    fn op<'a>(self: &'a Place<'tcx>, _tcx: TyCtxt) -> Operand<'tcx> {
        Operand::Copy(self.clone())
    }
}

impl<'tcx> IntoOperand<'tcx> for u32 {
    fn op<'a>(&'a self, tcx: TyCtxt<'tcx>) -> Operand<'tcx> {
        make_const(tcx, self.clone())
    }
}

impl<'tcx> IntoOperand<'tcx> for Local {
    fn op<'a>(&'a self, tcx: TyCtxt<'tcx>) -> Operand<'tcx> {
        let p: Place = self.clone().into();
        p.op(tcx)
    }
}

impl<'tcx> IntoOperand<'tcx> for Operand<'tcx> {
    fn op(&self, _tcx: TyCtxt<'tcx>) -> Self {
        self.clone()
    }
}

impl<'a, 'tcx: 'a> InstrumentationBuilder<'a, 'tcx, ReadyToInstrument<'tcx>> {
    fn arg_addr_of(&mut self, a: impl IntoOperand<'tcx>) -> &mut Self {
        let op = a.op(self.tcx);
        self.state.point.args.push(InstrumentationArg::AddrOf(op));
        self
    }

    fn arg_var<'c>(&'c mut self, a: impl IntoOperand<'tcx>) -> &'c mut Self {
        let op = a.op(self.tcx);
        let op_ty = op.ty(self.body, self.tcx);
        self.state
            .point
            .args
            .push(InstrumentationArg::Op(ArgKind::from_type(op, &op_ty)));
        self
    }

    fn args<'c>(&'c mut self, args: &Vec<Operand<'tcx>>) -> &'c mut Self {
        for a in args {
            self.arg_var(a.clone());
        }
        self
    }

    fn cleanup<'c>(&'c mut self) -> &'c mut Self {
        self.state.point.is_cleanup = true;
        self
    }

    fn after_call<'c>(&'c mut self) -> &'c mut Self {
        self.state.point.after_call = true;
        self
    }

    fn source<'c, T: Source>(&'c mut self, s: &T) -> &'c mut Self {
        self.state.point.metadata.source = s.source();
        self
    }

    fn source_from<'c, F, S: Source>(&'c mut self, f: F) -> &'c mut Self
    where
        F: Fn() -> Option<S>,
    {
        if let Some(s) = f() {
            self.state.point.metadata.source = s.source();
        }
        self
    }

    fn dest<'c>(&'c mut self, p: &Place) -> &'c mut Self {
        self.state.point.metadata.destination = Some(to_mir_place(p));
        self
    }

    fn dest_from<'c, F>(&'c mut self, f: F) -> &'c mut Self
    where
        F: Fn() -> Option<Place<'tcx>>,
    {
        if let Some(p) = f() {
            self.state.point.metadata.destination = Some(to_mir_place(&p));
        }
        self
    }

    fn transfer<'c>(&'c mut self, t: TransferKind) -> &'c mut Self {
        self.state.point.metadata.transfer_kind = t;
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
    /// [`func`]: Self::func
    /// [`statement_idx`]: Location::statement_index
    fn add(&mut self, adder: &mut InstrumentationAdder<'a, 'tcx>) {
        self.state.point.id = adder.instrumentation_points.len();
        adder.instrumentation_points.push(self.state.point.clone());
    }
}

impl<'a, 'tcx: 'a> Visitor<'tcx> for InstrumentationAdder<'a, 'tcx> {
    fn visit_place<'v>(
        &'v mut self,
        place: &Place<'tcx>,
        context: PlaceContext,
        location: Location,
    ) {
        self.super_place(place, context, location);

        let field_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_field"))
            .expect("Could not find pointer field hook");

        let base_ty = self.body.local_decls[place.local].ty;

        // Instrument field projections on raw-ptr places
        if is_region_or_unsafe_ptr(base_ty) && context.is_use() {
            for (base, elem) in place.iter_projections() {
                if let PlaceElem::Field(field, _) = elem {
                    let proj_dest = || {
                        // Only the last field projection gets a destination
                        self.assignment
                            .as_ref()
                            .map(|(dest, _)| dest)
                            .copied()
                            .filter(|_| base.projection.len() == place.projection.len() - 1)
                    };
                    self.loc(location, field_fn)
                        .arg_var(place.local)
                        .arg_var(field.as_u32())
                        .source(place)
                        .dest_from(proj_dest)
                        .add(self);
                }
            }
        }
    }

    fn visit_assign<'v>(
        &'v mut self,
        dest: &Place<'tcx>,
        value: &Rvalue<'tcx>,
        mut location: Location,
    ) {
        let copy_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_copy"))
            .expect("Could not find pointer copy hook");
        let addr_local_fn = self
            .find_instrumentation_def(Symbol::intern("addr_of_local"))
            .expect("Could not find addr_of_local hook");
        let ptr_contrive_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_contrive"))
            .expect("Could not find addr_of_local hook");
        let ptr_to_int_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_to_int"))
            .expect("Could not find addr_of_local hook");
        let load_value_fn = self
            .find_instrumentation_def(Symbol::intern("load_value"))
            .expect("Could not find pointer load hook");
        let store_value_fn = self
            .find_instrumentation_def(Symbol::intern("store_value"))
            .expect("Could not find pointer load hook");
        let store_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_store"))
            .expect("Could not find pointer store hook");
        let load_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_load"))
            .expect("Could not find pointer load hook");

        let dest = *dest;
        self.assignment = Some((dest, value.clone()));
        self.visit_rvalue(value, location);
        self.assignment = None;

        let locals = self.body.local_decls.clone();
        let ctx = self.tcx;

        let op_ty = |op: &Operand<'tcx>| op.ty(&locals, ctx);
        let place_ty = |p: &Place<'tcx>| p.ty(&locals, ctx).ty;
        let local_ty = |p: &Place| place_ty(&p.local.into());
        let value_ty = value.ty(&self.body.local_decls, self.tcx);

        self.visit_place(
            &dest,
            PlaceContext::MutatingUse(MutatingUseContext::Store),
            location,
        );

        let mut add_load_instr = |p: &Place<'tcx>| {
            self.loc(location, load_fn)
                .arg_var(p.local)
                .source(&remove_outer_deref(*p, ctx))
                .add(self);
        };

        // add instrumentation for load-from-address operations
        match value {
            Rvalue::Use(Operand::Copy(p) | Operand::Move(p)) if p.is_indirect() => {
                add_load_instr(p)
            }
            Rvalue::AddressOf(_, p) if !local_ty(p).is_region_ptr() && p.is_indirect() => {
                add_load_instr(p)
            }
            _ => (),
        }

        match value {
            _ if dest.is_indirect() => {
                // Strip all derefs to set base_dest to the pointer that is deref'd
                let base_dest = strip_all_deref(&dest, self.tcx);

                self.loc(location, store_fn)
                    .arg_var(base_dest)
                    .source(&remove_outer_deref(dest, self.tcx))
                    .add(self);

                if is_region_or_unsafe_ptr(value_ty) {
                    location.statement_index += 1;
                    self.loc(location, store_value_fn)
                        .arg_var(dest)
                        .source(value)
                        .dest(&dest)
                        .add(self);
                }
            }
            Rvalue::Cast(_, Operand::Copy(p) | Operand::Move(p), _)
                if value_ty.is_integral() && !p.is_indirect() =>
            {
                if is_region_or_unsafe_ptr(place_ty(p)) {
                    self.loc(location, ptr_to_int_fn)
                        .arg_var(p.local)
                        .source(p)
                        .add(self);
                }
            }
            _ if !is_region_or_unsafe_ptr(value_ty) => {}
            Rvalue::AddressOf(_, p) => {
                location.statement_index += 1;
                // Instrument which local's address is taken
                self.loc(location, addr_local_fn)
                    .arg_var(dest)
                    .arg_var(p.local.as_u32())
                    .source(p)
                    .dest(&dest)
                    .add(self);
            }
            Rvalue::Use(Operand::Copy(p) | Operand::Move(p)) if p.is_indirect() => {
                location.statement_index += 1;
                // We're dereferencing something, the result of which is a reference or pointer
                self.loc(location, load_value_fn)
                    .arg_var(dest)
                    .dest(&dest)
                    .add(self);
            }
            Rvalue::Use(Operand::Copy(p) | Operand::Move(p)) => {
                location.statement_index += 1;
                self.loc(location, copy_fn)
                    .arg_var(dest)
                    .source(p)
                    .dest(&dest)
                    .add(self);
            }
            Rvalue::Cast(_, op, _) => {
                let func = if op_ty(op).is_integral() {
                    ptr_contrive_fn
                } else {
                    copy_fn
                };
                location.statement_index += 1;
                self.loc(location, func)
                    .arg_var(dest)
                    .source(op)
                    .dest(&dest)
                    .add(self);
            }
            Rvalue::Ref(_, bkind, p) if has_outer_deref(p) => {
                // this is a reborrow or field reference, i.e. _2 = &(*_1)
                let source = remove_outer_deref(*p, self.tcx);
                if let BorrowKind::Mut { .. } = bkind {
                    // Instrument which local's address is taken
                    self.loc(location, copy_fn)
                        .arg_addr_of(*p)
                        .source(&source)
                        .dest(&dest)
                        .add(self);
                } else {
                    // Instrument immutable borrows by tracing the reference itself
                    location.statement_index += 1;
                    self.loc(location, copy_fn)
                        .arg_var(dest)
                        .source(&source)
                        .dest(&dest)
                        .add(self);
                };
            }
            Rvalue::Ref(_, bkind, p) if !p.is_indirect() => {
                // this is a reborrow or field reference, i.e. _2 = &(*_1)
                let source = remove_outer_deref(*p, self.tcx);
                if let BorrowKind::Mut { .. } = bkind {
                    // Instrument which local's address is taken
                    self.loc(location, addr_local_fn)
                        .arg_addr_of(*p)
                        .arg_var(p.local.as_u32())
                        .source(&source)
                        .dest(&dest)
                        .add(self);
                } else {
                    // Instrument immutable borrows by tracing the reference itself
                    location.statement_index += 1;
                    self.loc(location, addr_local_fn)
                        .arg_var(dest)
                        .arg_var(p.local.as_u32())
                        .source(&source)
                        .dest(&dest)
                        .add(self);
                };
            }
            _ => (),
        }
    }

    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, mut location: Location) {
        self.super_terminator(terminator, location);

        let arg_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_copy"))
            .expect("Could not find pointer arg hook");

        let ret_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_ret"))
            .expect("Could not find pointer ret hook");

        match &terminator.kind {
            TerminatorKind::Call {
                func,
                args,
                destination,
                ..
            } => {
                let mut callee_arg: Place = Local::new(1).into();
                let is_hook = {
                    if let ty::FnDef(def_id, _) = func.ty(self.body, self.tcx).kind() {
                        let fn_name = self.tcx.item_name(*def_id);
                        HOOK_FUNCTIONS.contains(&fn_name.as_str())
                    } else {
                        false
                    }
                };
                let func_kind = func.ty(self.body, self.tcx).kind();
                let transfer_kind = if let &ty::FnDef(def_id, _) = func_kind {
                    TransferKind::Arg(self.tcx.def_path_hash(def_id).convert())
                } else {
                    TransferKind::None
                };
                if !is_hook {
                    for arg in args {
                        if let Some(place) = arg.place() {
                            let place_ty = place.ty(self.body, self.tcx).ty;
                            if is_shared_or_unsafe_ptr(place_ty) {
                                self.loc(location, arg_fn)
                                    .arg_var(place)
                                    .source(&place)
                                    .dest(&callee_arg)
                                    .transfer(transfer_kind)
                                    .add(self);
                            }
                        }
                        callee_arg.local.increment_by(1);
                    }
                }
                if let &ty::FnDef(def_id, _) = func_kind {
                    if destination.is_some() {
                        let (dest_place, dest_block) = destination.unwrap();
                        println!("term: {:?}", terminator.kind);
                        let fn_name = self.tcx.item_name(def_id);
                        if HOOK_FUNCTIONS.contains(&fn_name.as_str()) {
                            let func_def_id = self
                                .find_instrumentation_def(fn_name)
                                .expect("Could not find instrumentation hook function");

                            // Hooked function called; trace args
                            self.loc(location, func_def_id)
                                .source(args)
                                .dest(&dest_place)
                                .after_call()
                                .transfer(TransferKind::Ret(self.func_hash()))
                                .args(args)
                                .add(self);
                        } else if is_region_or_unsafe_ptr(
                            dest_place.ty(&self.body.local_decls, self.tcx).ty,
                        ) {
                            location.statement_index = 0;
                            location.block = dest_block;

                            self.loc(location, arg_fn)
                                .source(&0)
                                .dest(&dest_place)
                                .transfer(TransferKind::Ret(
                                    self.tcx.def_path_hash(def_id).convert(),
                                ))
                                .arg_var(destination.unwrap().0)
                                .add(self);
                        }
                    }
                }
            }
            TerminatorKind::Return => {
                let place = Place::return_place();
                if is_region_or_unsafe_ptr(self.body.local_decls[place.local].ty) {
                    self.loc(location, ret_fn).arg_var(place).add(self);
                }
            }
            _ => (),
        }
    }
}

fn find_instrumentation_def(tcx: TyCtxt, runtime_crate_did: DefId, name: Symbol) -> Option<DefId> {
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

fn instrument_body<'a, 'tcx>(
    state: &InstrumentMemoryOps,
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

    let mut collect_points = InstrumentationAdder {
        tcx,
        body,
        runtime_crate_did,
        instrumentation_points: vec![],
        assignment: None,
    };
    collect_points.visit_body(body);

    apply_instrumentation(
        state,
        &collect_points.into_instrumentation_points(),
        tcx,
        body,
        body_def_hash,
    );

    // Apply `main`-specific instrumentation if this fn is main
    let main_did = tcx.entry_fn(()).map(|(def_id, _)| def_id);
    if Some(body_did) == main_did {
        instrument_entry_fn(tcx, runtime_crate_did, body);
    }
}

/// Add initialization code to the body of a function known to be the binary entrypoint
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

/// Rewrite the body to apply the specified instrumentation points
fn apply_instrumentation<'a, 'tcx>(
    state: &InstrumentMemoryOps,
    points: &[InstrumentationPoint<'tcx>],
    tcx: TyCtxt<'tcx>,
    body: &'a mut Body<'tcx>,
    body_def: DefPathHash,
) {
    for point in points {
        let &InstrumentationPoint {
            id: _id,
            loc,
            func,
            ref args,
            is_cleanup,
            after_call,
            ref metadata,
        } = point;
        let mut args = args.clone();

        if let TransferKind::Arg(def_path_hash) = metadata.transfer_kind {
            let callee_id = tcx.def_path_hash_to_def_id(def_path_hash.convert(), &mut || {
                panic!("cannot find DefId of callee func hash")
            });
            state.functions.lock().unwrap().insert(
                tcx.def_path_hash(callee_id).convert(),
                tcx.item_name(callee_id).to_string(),
            );
        }

        // Add the MIR location as the first argument to the instrumentation function
        let loc_idx = state.get_mir_loc_idx(body_def, loc, metadata.clone());
        args.insert(
            0,
            InstrumentationArg::Op(ArgKind::AddressUsize(make_const(tcx, loc_idx))),
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
    mut args: Vec<InstrumentationArg<'tcx>>,
) -> (BasicBlock, Local) {
    println!("ST: {:?}", statement_index);
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
    arg: &InstrumentationArg<'tcx>,
) -> Option<(Vec<Statement<'tcx>>, Operand<'tcx>)> {
    let mut new_stmts = vec![];

    let arg_ty = arg.inner().ty(locals, tcx);

    let ptr = match arg {
        // If we were given an address as a usize, no conversion is necessary
        InstrumentationArg::Op(ArgKind::AddressUsize(_arg)) => {
            assert!(
                arg_ty.is_integral() || arg_ty.is_unit(),
                "{:?}: {:?} is not of integral or unit type",
                arg,
                arg_ty
            );
            return None;
        }
        // From a reference `r`, cast through raw ptr to usize: `r as *mut _ as usize`
        InstrumentationArg::Op(ArgKind::Reference(arg)) => {
            assert!(arg_ty.is_region_ptr());
            let inner_ty = arg_ty.builtin_deref(false).unwrap();

            let raw_ptr_ty = tcx.mk_ptr(inner_ty);
            let raw_ptr_local = locals.push(LocalDecl::new(raw_ptr_ty, DUMMY_SP));

            let mut deref = arg.place().expect("Can't get the address of a constant");
            let mut projs = Vec::with_capacity(deref.projection.len() + 1);
            projs.extend(deref.projection);
            projs.push(ProjectionElem::Deref);
            deref.projection = tcx.intern_place_elems(&*projs);
            let cast_stmt = Statement {
                source_info: SourceInfo::outermost(DUMMY_SP),
                kind: StatementKind::Assign(Box::new((
                    raw_ptr_local.into(),
                    Rvalue::AddressOf(inner_ty.mutbl, deref),
                ))),
            };
            new_stmts.push(cast_stmt);
            Operand::Move(raw_ptr_local.into())
        }
        // From a raw pointer `r`, cast: `r as usize`
        InstrumentationArg::Op(ArgKind::RawPtr(arg)) => {
            assert!(
                arg_ty.is_unsafe_ptr(),
                "{:?}: {:?} is not an unsafe ptr",
                arg,
                arg_ty
            );
            arg.to_copy()
        }
        // From a place to which a reference is also constructed, create a raw
        // ptr with `addr_of!`
        InstrumentationArg::AddrOf(arg) => {
            let arg_place = arg.place().expect("Can't get the address of a constant");
            let arg_place = remove_outer_deref(arg_place, tcx);

            let arg_ty = arg_place.ty(locals, tcx).ty;
            let inner_ty = ty::TypeAndMut {
                ty: arg_ty,
                mutbl: Mutability::Not,
            };

            let raw_ptr_ty = tcx.mk_ptr(inner_ty);
            let raw_ptr_local = locals.push(LocalDecl::new(raw_ptr_ty, DUMMY_SP));

            let addr_of_stmt = Statement {
                source_info: SourceInfo::outermost(DUMMY_SP),
                kind: StatementKind::Assign(Box::new((
                    raw_ptr_local.into(),
                    Rvalue::AddressOf(inner_ty.mutbl, arg_place),
                ))),
            };
            new_stmts.push(addr_of_stmt);
            Operand::Move(raw_ptr_local.into())
        }
    };

    // Cast the raw ptr to a usize before passing to the
    // instrumentation function
    let usize_ty = tcx.mk_mach_uint(ty::UintTy::Usize);
    let casted_local = locals.push(LocalDecl::new(usize_ty, DUMMY_SP));
    let casted_arg = Operand::Move(casted_local.into());
    let cast_stmt = Statement {
        source_info: SourceInfo::outermost(DUMMY_SP),
        kind: StatementKind::Assign(Box::new((
            casted_local.into(),
            Rvalue::Cast(CastKind::Misc, ptr, usize_ty),
        ))),
    };
    new_stmts.push(cast_stmt);
    Some((new_stmts, casted_arg))
}
