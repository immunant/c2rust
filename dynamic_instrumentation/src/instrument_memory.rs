use anyhow::Context;
use c2rust_analysis_rt::metadata::Metadata;
use c2rust_analysis_rt::mir_loc::{
    self, EventMetadata, Func, MirLoc, MirLocId, MirPlace, MirProjection, TransferKind,
};
use c2rust_analysis_rt::HOOK_FUNCTIONS;
use indexmap::IndexSet;
use log::debug;
use rustc_data_structures::fingerprint::Fingerprint;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::visit::{MutatingUseContext, PlaceContext, Visitor};
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, Body, BorrowKind, CastKind, Constant, Local, LocalDecl, Location,
    Mutability, Operand, Place, PlaceElem, ProjectionElem, Rvalue, SourceInfo, Statement,
    StatementKind, Terminator, TerminatorKind, START_BLOCK,
};
use rustc_middle::ty::{self, ParamEnv, TyCtxt, TyS};
use rustc_span::def_id::{DefId, DefPathHash, CRATE_DEF_INDEX};
use rustc_span::{Symbol, DUMMY_SP};
use std::cell::RefCell;
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
enum InstrumentationOperand<'tcx> {
    AddressUsize(Operand<'tcx>),
    Reference(Operand<'tcx>),
    RawPtr(Operand<'tcx>),
    Place(Operand<'tcx>),
}

impl<'tcx> InstrumentationOperand<'tcx> {
    fn inner(&self) -> Operand<'tcx> {
        use InstrumentationOperand::*;
        match self {
            AddressUsize(x) => x,
            Reference(x) => x,
            RawPtr(x) => x,
            Place(x) => x,
        }
        .clone()
    }

    fn from_type(op: Operand<'tcx>, ty: &ty::Ty<'tcx>) -> Self {
        use InstrumentationOperand::*;
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

struct InstrumentationPoint<'tcx> {
    id: usize,
    loc: Location,
    func: DefId,
    args: Vec<InstrumentationOperand<'tcx>>,
    is_cleanup: bool,
    after_call: bool,
    metadata: EventMetadata,
}

struct CollectFunctionInstrumentationPoints<'a, 'tcx: 'a> {
    tcx: TyCtxt<'tcx>,
    body: &'a Body<'tcx>,
    runtime_crate_did: DefId,

    instrumentation_points: RefCell<Vec<InstrumentationPoint<'tcx>>>,

    assignment: Option<(Place<'a>, Rvalue<'a>)>,
}

impl<'a, 'tcx: 'a> CollectFunctionInstrumentationPoints<'a, 'tcx> {
    /// Queues insertion of a call to `func`
    ///
    /// The call will be inserted before the statement at index `statement_idx`
    /// in `block`. If `statement_idx` is the number of statements in the block,
    /// the call will be inserted at the end of the block.
    ///
    /// `func` must not unwind, as it will have no cleanup destination.
    fn add_instrumentation_point(
        &self,
        loc: Location,
        func: DefId,
        args: Vec<InstrumentationOperand<'tcx>>,
        is_cleanup: bool,
        after_call: bool,
        metadata: EventMetadata,
    ) {
        let id = self.instrumentation_points.borrow().len();
        self.instrumentation_points
            .borrow_mut()
            .push(InstrumentationPoint {
                id,
                loc,
                func,
                args,
                is_cleanup,
                after_call,
                metadata,
            })
    }

    fn into_instrumentation_points(mut self) -> Vec<InstrumentationPoint<'tcx>> {
        // Sort by reverse location so that we can split blocks without
        // perturbing future statement indices
        self.instrumentation_points
            .get_mut()
            .sort_unstable_by(|a, b| {
                b.loc
                    .cmp(&a.loc)
                    .then(b.after_call.cmp(&a.after_call))
                    .then(b.id.cmp(&a.id))
            });
        self.instrumentation_points.into_inner()
    }

    fn find_instrumentation_def(&self, name: Symbol) -> Option<DefId> {
        find_instrumentation_def(self.tcx, self.runtime_crate_did, name)
    }

    fn is_shared_or_unsafe_ptr(&self, ty: &TyS<'tcx>) -> bool {
        ty.is_unsafe_ptr() || (ty.is_region_ptr() && !ty.is_mutable_ptr())
    }

    fn func_hash(&self) -> mir_loc::DefPathHash {
        self.tcx.def_path_hash(self.body.source.def_id()).convert()
    }
}

fn has_outer_deref(p: &Place) -> bool {
    matches!(
        p.iter_projections().last(),
        Some((_, ProjectionElem::Deref))
    )
}

fn sans_last_projection<'tcx>(p: &Place<'tcx>, tcx: TyCtxt<'tcx>) -> Option<Place<'tcx>> {
    let Place { local, projection } = *p;
    projection.split_last().map(|(_last, rest)| Place {
        local,
        projection: tcx.intern_place_elems(rest),
    })
}

/// Get the inner-most dereferenced [`Place`].
fn strip_all_deref<'tcx>(p: &Place<'tcx>, tcx: TyCtxt<'tcx>) -> Place<'tcx> {
    let mut base_dest = *p;
    let mut place_ref = base_dest.as_ref();
    while let Some((cur_ref, proj)) = place_ref.last_projection() {
        if let ProjectionElem::Deref = proj {
            base_dest = Place {
                local: cur_ref.local,
                projection: tcx.intern_place_elems(cur_ref.projection),
            };
        }
        place_ref = cur_ref;
    }

    base_dest
}

/// Used to strip initital deref from projection sequences
fn remove_outer_deref<'tcx>(p: &Place<'tcx>, tcx: TyCtxt<'tcx>) -> Place<'tcx> {
    // Remove outer deref if present
    match p.as_ref().last_projection() {
        Some((_, ProjectionElem::Deref)) => {
            let sans_proj =
                sans_last_projection(p, tcx).expect("expected but did not find deref projection");
            sans_proj
        }
        _ => *p,
    }
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

impl<'a, 'tcx: 'a> Visitor<'tcx> for CollectFunctionInstrumentationPoints<'a, 'tcx> {
    fn visit_place(&mut self, place: &Place<'tcx>, context: PlaceContext, location: Location) {
        self.super_place(place, context, location);
        let _field_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_field"))
            .expect("Could not find pointer field hook");
        let load_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_load"))
            .expect("Could not find pointer load hook");

        let base_ty = self.body.local_decls[place.local].ty;
        // Instrument field projections on raw-ptr places
        if self.is_shared_or_unsafe_ptr(base_ty) && context.is_use() && !place.projection.is_empty()
        {
            for (pid, (_base, elem)) in place.iter_projections().enumerate() {
                if let PlaceElem::Field(field, _) = elem {
                    let field_fn = self
                        .find_instrumentation_def(Symbol::intern("ptr_field"))
                        .expect("Could not find pointer field hook");

                    let destination = if pid == place.projection.len() - 1 {
                        self.assignment
                            .as_ref()
                            .map(|(dest, _rval)| to_mir_place(dest))
                    } else {
                        None
                    };

                    // Projecting a field; trace the local of the original place as well as the field idx
                    self.add_instrumentation_point(
                        location,
                        field_fn,
                        vec![
                            InstrumentationOperand::from_type(
                                Operand::Copy(place.local.into()),
                                &self.body.local_decls[place.local].ty,
                            ),
                            InstrumentationOperand::AddressUsize(make_const(
                                self.tcx,
                                field.as_u32(),
                            )),
                        ],
                        false,
                        false,
                        EventMetadata {
                            source: Some(to_mir_place(place)),
                            destination,
                            transfer_kind: TransferKind::None,
                        },
                    );
                }
            }

            if place.is_indirect()
                && !context.is_mutating_use()
                // reborrows do not apply, i.e. _2 = &(*_1)
                && !matches!(self.assignment, Some((_, Rvalue::Ref(..))))
                // copying address of dereferenced value does not apply, i.e. _2 = &raw (*_1)
                && !matches!(self.assignment, Some((_, Rvalue::AddressOf(..))) if base_ty.is_region_ptr())
            {
                // Place is loading from a raw ptr; trace the raw ptr
                self.add_instrumentation_point(
                    location,
                    load_fn,
                    vec![InstrumentationOperand::from_type(
                        Operand::Copy(place.local.into()),
                        &self.body.local_decls[place.local].ty,
                    )],
                    false,
                    false,
                    EventMetadata {
                        source: Some(to_mir_place(place)),
                        destination: None,
                        transfer_kind: TransferKind::None,
                    },
                );
            }
        }
    }

    fn visit_statement(&mut self, statement: &Statement<'tcx>, mut location: Location) {
        let copy_fn = self
            .find_instrumentation_def(Symbol::intern("ptr_copy"))
            .expect("Could not find pointer copy hook");
        let ref_copy_fn = self
            .find_instrumentation_def(Symbol::intern("ref_copy"))
            .expect("Could not find ref copy hook");
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

        if let StatementKind::Assign(assign) = &statement.kind {
            println!("statement: {:?}", statement);
            let dest = assign.0;
            let value = &assign.1;

            self.visit_place(
                &dest,
                PlaceContext::MutatingUse(MutatingUseContext::Store),
                location,
            );

            self.assignment = Some(*assign.to_owned());
            self.visit_rvalue(value, location);
            self.assignment = None;

            let dest_ty = dest.ty(&self.body.local_decls, self.tcx).ty;
            let value_ty = value.ty(&self.body.local_decls, self.tcx);

            if dest.is_indirect() {
                // Strip all derefs to set base_dest to the pointer that is deref'd
                let base_dest = strip_all_deref(&dest, self.tcx);

                // Storing a raw pointer in an indirect destination; trace the destination
                let base_dest_ty = base_dest.ty(&self.body.local_decls, self.tcx).ty;
                self.add_instrumentation_point(
                    location,
                    store_fn,
                    vec![InstrumentationOperand::from_type(
                        Operand::Copy(base_dest),
                        &base_dest_ty,
                    )],
                    false,
                    false,
                    EventMetadata {
                        source: Some(to_mir_place(&dest)),
                        destination: None,
                        transfer_kind: TransferKind::None,
                    },
                );
                if self.is_shared_or_unsafe_ptr(value_ty) {
                    let mut loc = location;
                    loc.statement_index += 1;
                    // Stored value is a raw pointer; trace the destination
                    self.add_instrumentation_point(
                        loc,
                        store_value_fn,
                        vec![InstrumentationOperand::from_type(
                            Operand::Copy(dest),
                            &dest_ty,
                        )],
                        false,
                        false,
                        EventMetadata {
                            source: rv_place(value).map(|p| to_mir_place(&p)),
                            destination: Some(to_mir_place(&dest)),
                            transfer_kind: TransferKind::None,
                        },
                    );
                }
            } else if value_ty.is_integral() {
                if let Rvalue::Cast(_, op, _) = value {
                    if let Some(p) = op.place() {
                        if !p.is_indirect()
                            && self
                                .is_shared_or_unsafe_ptr(p.ty(&self.body.local_decls, self.tcx).ty)
                        {
                            // Cast from raw pointer to usize; trace the pointer being casted
                            self.add_instrumentation_point(
                                location,
                                ptr_to_int_fn,
                                vec![InstrumentationOperand::from_type(
                                    Operand::Copy(p.local.into()),
                                    &self.body.local_decls[p.local].ty,
                                )],
                                false,
                                false,
                                EventMetadata {
                                    source: Some(to_mir_place(&p)),
                                    destination: None,
                                    transfer_kind: TransferKind::None,
                                },
                            );
                        }
                    }
                }
            } else if self.is_shared_or_unsafe_ptr(value_ty) {
                match value {
                    Rvalue::Use(Operand::Copy(p))
                    | Rvalue::Use(Operand::Move(p))
                    | Rvalue::Ref(_, _, p) => {
                        location.statement_index += 1;
                        if p.is_indirect() && !matches!(value, Rvalue::Ref(..)) {
                            // We're dereferencing something, the result of which is another pointer;
                            // trace the destination
                            self.add_instrumentation_point(
                                location,
                                load_value_fn,
                                vec![InstrumentationOperand::from_type(
                                    Operand::Copy(dest),
                                    &dest_ty,
                                )],
                                false,
                                false,
                                EventMetadata {
                                    source: None,
                                    destination: Some(to_mir_place(&dest)),
                                    transfer_kind: TransferKind::None,
                                },
                            );
                        } else {
                            let source = if has_outer_deref(p) {
                                // this statement is a reborrow, i.e. _2 = &(*_1),
                                // so count this as a copy with _1 as a source
                                remove_outer_deref(p, self.tcx)
                            } else {
                                *p
                            };
                            // Copying a pointer or reference to another place; trace the destination
                            self.add_instrumentation_point(
                                location,
                                copy_fn,
                                vec![InstrumentationOperand::from_type(
                                    Operand::Copy(dest),
                                    &dest_ty,
                                )],
                                false,
                                false,
                                EventMetadata {
                                    source: Some(to_mir_place(&source)),
                                    destination: Some(to_mir_place(&dest)),
                                    transfer_kind: TransferKind::None,
                                },
                            );
                        }
                    }
                    Rvalue::Cast(_, p, _) => {
                        location.statement_index += 1;
                        if p.ty(&self.body.local_decls, self.tcx).is_integral() {
                            // Casting integer to a pointer; trace the destination
                            self.add_instrumentation_point(
                                location,
                                ptr_contrive_fn,
                                vec![InstrumentationOperand::from_type(
                                    Operand::Copy(dest),
                                    &dest_ty,
                                )],
                                false,
                                false,
                                EventMetadata {
                                    source: p.place().as_ref().map(to_mir_place),
                                    destination: Some(to_mir_place(&dest)),
                                    transfer_kind: TransferKind::None,
                                },
                            );
                        } else {
                            // Casting between pointer types; trace the destination
                            self.add_instrumentation_point(
                                location,
                                copy_fn,
                                vec![InstrumentationOperand::from_type(
                                    Operand::Copy(dest),
                                    &dest_ty,
                                )],
                                false,
                                false,
                                EventMetadata {
                                    source: p.place().as_ref().map(to_mir_place),
                                    destination: Some(to_mir_place(&dest)),
                                    transfer_kind: TransferKind::None,
                                },
                            );
                        }
                    }
                    Rvalue::AddressOf(_, p) => {
                        // Instrument which local's address is taken
                        location.statement_index += 1;
                        self.add_instrumentation_point(
                            location,
                            addr_local_fn,
                            vec![
                                InstrumentationOperand::RawPtr(Operand::Copy(dest)),
                                InstrumentationOperand::AddressUsize(make_const(
                                    self.tcx,
                                    p.local.as_u32(),
                                )),
                            ],
                            false,
                            false,
                            EventMetadata {
                                source: Some(to_mir_place(p)),
                                destination: Some(to_mir_place(&dest)),
                                transfer_kind: TransferKind::None,
                            },
                        );
                    }
                    _ => (),
                }
            } else if value_ty.is_region_ptr() {
                if let Rvalue::Ref(_, bkind, p) = value {
                    let instr_operand = if let BorrowKind::Mut { .. } = bkind {
                        InstrumentationOperand::Place(Operand::Copy(*p))
                    } else {
                        // Instrument immutable borrows by tracing the reference itself
                        location.statement_index += 1;
                        InstrumentationOperand::Reference(Operand::Copy(dest))
                    };
                    self.add_instrumentation_point(
                        location,
                        ref_copy_fn,
                        vec![instr_operand],
                        false,
                        false,
                        EventMetadata {
                            source: Some(to_mir_place(p)),
                            destination: Some(to_mir_place(&dest)),
                            transfer_kind: TransferKind::None,
                        },
                    );
                }
            }
        } else {
            self.super_statement(statement, location);
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
                let mut arg_local = mir_loc::Local { index: 1 };
                let is_hook = {
                    if let ty::FnDef(def_id, _) = func.ty(self.body, self.tcx).kind() {
                        let fn_name = self.tcx.item_name(*def_id);
                        HOOK_FUNCTIONS.contains(&fn_name.as_str())
                    } else {
                        false
                    }
                };
                let transfer_kind =
                    if let &ty::FnDef(def_id, _) = func.ty(self.body, self.tcx).kind() {
                        TransferKind::Arg(self.tcx.def_path_hash(def_id).convert())
                    } else {
                        TransferKind::None
                    };
                if !is_hook {
                    for arg in args {
                        if let Some(place) = arg.place() {
                            if self.is_shared_or_unsafe_ptr(place.ty(self.body, self.tcx).ty) {
                                println!(
                                    "visiting terminator arg {:?}, {:?}, {:?}, {:?}",
                                    place,
                                    place.ty(self.body, self.tcx).ty,
                                    InstrumentationOperand::from_type(
                                        Operand::Copy(place),
                                        &place.ty(self.body, self.tcx).ty,
                                    ),
                                    terminator
                                );
                                // Block terminator is a non-hook fn call; trace any raw-ptr args
                                self.add_instrumentation_point(
                                    location,
                                    arg_fn,
                                    vec![InstrumentationOperand::from_type(
                                        Operand::Copy(place),
                                        &place.ty(self.body, self.tcx).ty,
                                    )],
                                    false,
                                    false,
                                    EventMetadata {
                                        source: Some(to_mir_place(&place)),
                                        destination: Some(MirPlace {
                                            local: arg_local,
                                            projection: vec![],
                                        }),
                                        transfer_kind,
                                    },
                                );
                            }
                        }
                        arg_local.index += 1;
                    }
                }
                if let &ty::FnDef(def_id, _) = func.ty(self.body, self.tcx).kind() {
                    if destination.is_some() {
                        println!("term: {:?}", terminator.kind);
                        let fn_name = self.tcx.item_name(def_id);
                        // println!(
                        //     "visiting func {:?}, {:?}",
                        //     fn_name,
                        //     self.tcx.def_path_hash(*def_id)
                        // );
                        if HOOK_FUNCTIONS.contains(&fn_name.as_str()) {
                            let func_def_id = self
                                .find_instrumentation_def(fn_name)
                                .expect("Could not find instrumentation hook function");

                            // Hooked function called; trace args
                            self.add_instrumentation_point(
                                location,
                                func_def_id,
                                args.iter()
                                    .map(|arg| {
                                        InstrumentationOperand::from_type(
                                            arg.clone(),
                                            &arg.ty(self.body, self.tcx),
                                        )
                                    })
                                    .collect(),
                                false,
                                /* after_call: */ true,
                                EventMetadata {
                                    // TODO: hook-specific sources
                                    source: args
                                        .clone()
                                        .iter()
                                        .map(|op| to_mir_place(&op.place().unwrap()))
                                        .next(),
                                    // FIXME: hooks have sources
                                    destination: destination.map(|d| to_mir_place(&d.0)),
                                    transfer_kind: TransferKind::Ret(self.func_hash()),
                                },
                            );
                        } else if self.is_shared_or_unsafe_ptr(
                            destination
                                .unwrap()
                                .0
                                .ty(&self.body.local_decls, self.tcx)
                                .ty,
                        ) {
                            location.statement_index = 0;
                            location.block = destination.unwrap().1;
                            // Non-hooked fn with raw-ptr result called; trace destination
                            self.add_instrumentation_point(
                                location,
                                arg_fn,
                                vec![InstrumentationOperand::from_type(
                                    Operand::Copy(destination.unwrap().0),
                                    &destination
                                        .unwrap()
                                        .0
                                        .ty(&self.body.local_decls, self.tcx)
                                        .ty,
                                )],
                                false,
                                false,
                                EventMetadata {
                                    source: Some(to_mir_place(&Local::from_u32(0).into())),
                                    destination: destination.map(|d| to_mir_place(&d.0)),
                                    transfer_kind: TransferKind::Ret(
                                        self.tcx.def_path_hash(def_id).convert(),
                                    ),
                                },
                            );
                        }
                    }
                }
            }
            TerminatorKind::Return => {
                let place = Place::return_place();
                if self.is_shared_or_unsafe_ptr(self.body.local_decls[place.local].ty) {
                    // Function returned raw ptr; trace the return place
                    self.add_instrumentation_point(
                        location,
                        ret_fn,
                        vec![InstrumentationOperand::from_type(
                            Operand::Copy(place),
                            &place.ty(self.body, self.tcx).ty,
                        )],
                        false,
                        false,
                        EventMetadata::default(),
                    );
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

fn instrument_body<'tcx>(
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

    let mut collect_points = CollectFunctionInstrumentationPoints {
        tcx,
        body,
        runtime_crate_did,

        instrumentation_points: RefCell::new(vec![]),
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
fn apply_instrumentation<'tcx>(
    state: &InstrumentMemoryOps,
    points: &[InstrumentationPoint<'tcx>],
    tcx: TyCtxt<'tcx>,
    body: &mut Body<'tcx>,
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
            InstrumentationOperand::AddressUsize(make_const(tcx, loc_idx)),
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
                // The return type of a hooked fn is always a raw ptr or unit
                if place_ty.is_unit() {
                    // It's somewhat wrong to call unit an AddressUsize, but it has the pass-through
                    // semantics we want
                    InstrumentationOperand::AddressUsize(Operand::Copy(*place))
                } else {
                    assert!(place_ty.is_unsafe_ptr());
                    InstrumentationOperand::RawPtr(Operand::Copy(*place))
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
                args.push(InstrumentationOperand::AddressUsize(cast_local));
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
    mut args: Vec<InstrumentationOperand<'tcx>>,
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
            *arg = InstrumentationOperand::AddressUsize(cast_local);
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
            args: args.iter().map(|arg| arg.inner()).collect(),
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
    arg: &InstrumentationOperand<'tcx>,
) -> Option<(Vec<Statement<'tcx>>, Operand<'tcx>)> {
    let mut new_stmts = vec![];

    let arg_ty = arg.inner().ty(locals, tcx);

    let ptr = match arg {
        // If we were given an address as a usize, no conversion is necessary
        InstrumentationOperand::AddressUsize(_arg) => {
            assert!(
                arg_ty.is_integral() || arg_ty.is_unit(),
                "{:?}: {:?} is not of integral or unit type",
                arg,
                arg_ty
            );
            return None;
        }
        // From a reference `r`, cast through raw ptr to usize: `r as *mut _ as usize`
        InstrumentationOperand::Reference(arg) => {
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
        InstrumentationOperand::RawPtr(arg) => {
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
        InstrumentationOperand::Place(arg) => {
            let arg_place = arg.place().expect("Can't get the address of a constant");
            let arg_place = remove_outer_deref(&arg_place, tcx);

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
