#![feature(rustc_private)]
extern crate either;
extern crate rustc_arena;
extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_type_ir;

use crate::borrowck::{AdtMetadata, FieldMetadata, OriginArg, OriginParam};
use crate::context::{
    AnalysisCtxt, AnalysisCtxtData, FlagSet, GlobalAnalysisCtxt, GlobalAssignment, LFnSig, LTy,
    LTyCtxt, LocalAssignment, PermissionSet, PointerId,
};
use crate::dataflow::DataflowConstraints;
use crate::equiv::{GlobalEquivSet, LocalEquivSet};
use crate::labeled_ty::LabeledTyCtxt;
use crate::log::init_logger;
use crate::util::Callee;
use assert_matches::assert_matches;
use indexmap::IndexSet;
use labeled_ty::LabeledTy;
use rustc_ast::Mutability;
use rustc_hir::def::DefKind;
use rustc_hir::def_id::{DefId, LocalDefId};
use rustc_index::vec::IndexVec;
use rustc_middle::mir::visit::Visitor;
use rustc_middle::mir::{
    AggregateKind, BindingForm, Body, Constant, LocalDecl, LocalInfo, LocalKind, Location, Operand,
    Rvalue, StatementKind,
};
use rustc_middle::ty::tls;
use rustc_middle::ty::{GenericArgKind, Ty, TyCtxt, TyKind, WithOptConstParam};
use rustc_span::Span;
use rustc_type_ir::RegionKind::{ReEarlyBound, ReStatic};
use std::collections::{HashMap, HashSet};
use std::env;
use std::fmt::{Debug, Display};
use std::ops::{Deref, DerefMut, Index};

mod borrowck;
mod c_void_casts;
mod context;
mod dataflow;
mod equiv;
mod labeled_ty;
mod log;
mod pointer_id;
mod rewrite;
mod trivial;
mod type_desc;
mod util;

/// A wrapper around `T` that dynamically tracks whether it's initialized or not.
/// [`RefCell`][std::cell::RefCell] dynamically tracks borrowing and panics if the rules are
/// violated at run time; `MaybeUnset` dynamically tracks initialization and similarly panics if
/// the value is accessed while unset.
#[derive(Clone, Copy, Debug)]
struct MaybeUnset<T>(Option<T>);

impl<T> Default for MaybeUnset<T> {
    fn default() -> MaybeUnset<T> {
        MaybeUnset(None)
    }
}

impl<T> MaybeUnset<T> {
    pub fn set(&mut self, x: T) {
        if self.0.is_some() {
            panic!("value is already set");
        }
        self.0 = Some(x);
    }

    pub fn clear(&mut self) {
        if self.0.is_none() {
            panic!("value is already cleared");
        }
        self.0 = None;
    }

    pub fn get(&self) -> &T {
        self.0.as_ref().expect("value is not set")
    }

    pub fn get_mut(&mut self) -> &mut T {
        self.0.as_mut().expect("value is not set")
    }

    pub fn take(&mut self) -> T {
        self.0.take().expect("value is not set")
    }
}

impl<T> Deref for MaybeUnset<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.get()
    }
}

impl<T> DerefMut for MaybeUnset<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.get_mut()
    }
}

fn construct_adt_metadata<'tcx>(tcx: TyCtxt<'tcx>) -> AdtMetadataTable {
    let struct_dids: Vec<_> = tcx
        .hir_crate_items(())
        .definitions()
        .filter_map(|ldid: LocalDefId| {
            use DefKind::*;
            let did = ldid.to_def_id();
            if matches!(tcx.def_kind(did), Struct | Enum | Union) {
                return Some(did);
            }

            None
        })
        .collect();

    let mut adt_metadata_table = AdtMetadataTable {
        table: HashMap::new(),
        struct_dids,
    };

    // Gather known lifetime parameters for each struct
    for struct_did in &adt_metadata_table.struct_dids {
        let struct_ty = tcx.type_of(struct_did);
        if let TyKind::Adt(adt_def, substs) = struct_ty.kind() {
            adt_metadata_table
                .table
                .insert(adt_def.did(), AdtMetadata::default());
            eprintln!("gathering known lifetimes for {adt_def:?}");
            for sub in substs.iter() {
                if let GenericArgKind::Lifetime(r) = sub.unpack() {
                    eprintln!("\tfound lifetime {r:?} in {adt_def:?}");
                    assert_matches!(r.kind(), ReEarlyBound(eb) => {
                        let _ = adt_metadata_table
                        .table
                        .entry(adt_def.did())
                        .and_modify(|metadata| {
                            metadata.lifetime_params.insert(OriginParam::Actual(eb));
                        });
                    });
                }
            }
        } else {
            panic!("{struct_ty:?} is not a struct");
        }
    }

    let ltcx = LabeledTyCtxt::<'tcx, &[OriginArg<'tcx>]>::new(tcx);
    let mut loop_count = 0;
    loop {
        /*
            This loop iterates over all structs and gathers metadata for each.
            If there were no recursive or mutually-recursive data structures,
            this loop would only need one iteration to complete. To support
            recursive and mutually-recursive structs, the loop iterates until
            the metadata gathered for each struct reaches a fixed point.
        */
        loop_count += 1;
        assert!(loop_count < 1000);

        eprintln!("---- running fixed point struct field analysis iteration #{loop_count:?} ----");
        let old_adt_metadata = adt_metadata_table.table.clone();
        let mut next_hypo_origin_id = 0;

        // for each struct, gather lifetime information (actual and hypothetical)
        for struct_did in &adt_metadata_table.struct_dids {
            let adt_def = tcx.adt_def(struct_did);
            eprintln!("gathering lifetimes and lifetime parameters for {adt_def:?}");
            for field in adt_def.all_fields() {
                let field_ty: Ty = tcx.type_of(field.did);
                eprintln!("\t{adt_def:?}.{:}", field.name);
                let field_origin_args = ltcx.label(field_ty, &mut |ty| {
                    let mut field_origin_args = IndexSet::new();
                    match ty.kind() {
                        TyKind::RawPtr(ty) => {
                            eprintln!(
                                "\t\tfound pointer that requires hypothetical lifetime: *{:}",
                                if let Mutability::Mut = ty.mutbl {
                                    "mut"
                                } else {
                                    "const"
                                }
                            );
                            adt_metadata_table
                                .table
                                .entry(*struct_did)
                                .and_modify(|adt| {
                                    let origin_arg = OriginArg::Hypothetical(next_hypo_origin_id);
                                    let origin_param =
                                        OriginParam::Hypothetical(next_hypo_origin_id);
                                    eprintln!(
                                        "\t\t\tinserting origin {origin_param:?} into {adt_def:?}"
                                    );

                                    adt.lifetime_params.insert(origin_param);
                                    next_hypo_origin_id += 1;
                                    field_origin_args.insert(origin_arg);
                                });
                        }
                        TyKind::Ref(reg, _ty, _mutability) => {
                            eprintln!("\t\tfound reference field lifetime: {reg:}");
                            assert_matches!(reg.kind(), ReEarlyBound(..) | ReStatic);
                            let origin_arg = OriginArg::Actual(*reg);
                            adt_metadata_table
                                .table
                                .entry(*struct_did)
                                .and_modify(|adt| {
                                    if let ReEarlyBound(eb) = reg.kind() {
                                        eprintln!("\t\t\tinserting origin {eb:?} into {adt_def:?}");
                                        adt.lifetime_params.insert(OriginParam::Actual(eb));
                                    }

                                    field_origin_args.insert(origin_arg);
                                });
                        }
                        TyKind::Adt(adt_field, substs) => {
                            eprintln!("\t\tfound ADT field base type: {adt_field:?}");
                            for sub in substs.iter() {
                                if let GenericArgKind::Lifetime(r) = sub.unpack() {
                                    eprintln!("\tfound field lifetime {r:?} in {adt_def:?}.{adt_field:?}");
                                    eprintln!("\t\t\tinserting {adt_field:?} lifetime param {r:?} into {adt_def:?}.{:} lifetime parameters", field.name);
                                    assert_matches!(r.kind(), ReEarlyBound(..) | ReStatic);
                                    field_origin_args.insert(OriginArg::Actual(r));
                                }
                            }
                            if let Some(adt_field_metadata) =
                                adt_metadata_table.table.get(&adt_field.did()).cloned()
                            {
                                // add a metadata entry for the struct field matching the metadata entry
                                // for the struct definition of said field
                                adt_metadata_table
                                    .table
                                    .insert(field.did, adt_field_metadata.clone());

                                for adt_field_lifetime_param in adt_field_metadata.lifetime_params.iter() {
                                    adt_metadata_table.table.entry(*struct_did).and_modify(|adt| {
                                        if let OriginParam::Hypothetical(h) = adt_field_lifetime_param {
                                            eprintln!("\t\t\tbubbling {adt_field:?} origin {adt_field_lifetime_param:?} up into {adt_def:?} origins");
                                            field_origin_args.insert(OriginArg::Hypothetical(*h));
                                            adt.lifetime_params.insert(*adt_field_lifetime_param);
                                        }
                                    });
                                }
                            }
                        }
                        _ => (),
                    }

                    if field_origin_args.is_empty() {
                        return &[];
                    }
                    let field_origin_args: Vec<_> = field_origin_args.into_iter().collect();
                    ltcx.arena().alloc_slice(&field_origin_args[..])
                });

                adt_metadata_table
                    .table
                    .entry(*struct_did)
                    .and_modify(|adt| {
                        adt.field_info.insert(
                            field.did,
                            FieldMetadata {
                                origin_args: field_origin_args,
                            },
                        );
                    });
            }

            eprintln!();
        }

        if adt_metadata_table.table == old_adt_metadata {
            eprintln!("reached a fixed point in struct lifetime reconciliation\n");
            break;
        }
    }

    adt_metadata_table
}

pub struct AdtMetadataTable<'tcx> {
    pub table: HashMap<DefId, AdtMetadata<'tcx>>,
    pub struct_dids: Vec<DefId>,
}

impl<'tcx> Debug for AdtMetadataTable<'tcx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_string(lty: LabeledTy<'_, &[OriginArg]>) -> String {
            let args: Vec<String> = lty.args.iter().map(|t| fmt_string(t)).collect();
            use rustc_type_ir::TyKind::*;
            match lty.kind() {
                Ref(..) | RawPtr(..) => {
                    format!("&{:?} {:}", lty.label[0], args[0])
                }
                Adt(adt, _) => {
                    let mut s = format!("{adt:?}");
                    let params = lty
                        .label
                        .iter()
                        .map(|p| format!("{:?}", p))
                        .into_iter()
                        .chain(args.into_iter())
                        .collect::<Vec<_>>()
                        .join(",");

                    if !params.is_empty() {
                        s.push('<');
                        s.push_str(&params);
                        s.push('>');
                    }
                    s
                }
                Tuple(_) => {
                    format!("({:})", args.join(","))
                }
                _ => format!("{:?}", lty.ty),
            }
        }

        tls::with_opt(|tcx| {
            let tcx = tcx.unwrap();
            for k in &self.struct_dids {
                let adt = &self.table[k];
                write!(f, "struct {:}", tcx.item_name(*k))?;
                write!(f, "<")?;
                let lifetime_params_str = adt
                    .lifetime_params
                    .iter()
                    .map(|p| format!("{:?}", p))
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, "{lifetime_params_str:}")?;
                writeln!(f, "> {{")?;
                for (fdid, fmeta) in &adt.field_info {
                    write!(f, "\t{:}: ", tcx.item_name(*fdid))?;
                    let field_string_lty = fmt_string(fmeta.origin_args);

                    write!(f, "{field_string_lty:}")?;

                    writeln!(f)?;
                }

                writeln!(f, "}}\n")?;
            }
            writeln!(f)
        })
    }
}

/// Determine if a [`Constant`] is a (byte-)string literal.
///
/// The current implementation is super hacky and just uses [`Constant`]'s [`Display`] `impl`,
/// but I'll soon replace it with a more robust (but complex) version
/// using pretty much the same way the [`Display`] `impl` does it.
///
/// [`Display`]: std::fmt::Display
fn is_string_literal(c: &Constant) -> bool {
    let s = c.to_string();
    s.ends_with('"') && {
        let s = match s.strip_prefix("const ") {
            Some(s) => s,
            None => return false,
        };
        s.starts_with('"') || s.starts_with("b\"")
    }
}

/// Label string literals, including byte string literals.
///
/// String literals are const refs (refs that are constant).
/// We only handle string literals here,
/// as handling all const refs requires disambiguating which const refs
/// are purely inline and don't reference any other named items
/// vs. named const refs, and inline aggregate const refs that include named const refs.
///
/// String literals we know are purely inline,
/// so they do not need [`DataflowConstraints`] to their pointee (unlike their named counterparts),
/// because the values cannot be accessed elsewhere,
/// and their permissions are predetermined (see [`PermissionSet::STRING_LITERAL`]).
fn label_string_literals<'tcx>(
    acx: &mut AnalysisCtxt<'_, 'tcx>,
    c: &Constant<'tcx>,
    loc: Location,
) -> Option<LTy<'tcx>> {
    let ty = c.ty();
    if !ty.is_ref() {
        return None;
    }
    if is_string_literal(c) {
        acx.string_literal_locs.push(loc);
        Some(acx.assign_pointer_ids(ty))
    } else {
        None
    }
}

fn label_rvalue_tys<'tcx>(acx: &mut AnalysisCtxt<'_, 'tcx>, mir: &Body<'tcx>) {
    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        for (i, stmt) in bb_data.statements.iter().enumerate() {
            let (_, rv) = match &stmt.kind {
                StatementKind::Assign(x) => &**x,
                _ => continue,
            };

            let loc = Location {
                statement_index: i,
                block: bb,
            };

            if acx.c_void_casts.should_skip_stmt(loc) {
                continue;
            }

            let lty = match rv {
                Rvalue::Aggregate(ref kind, ref _ops) => match **kind {
                    AggregateKind::Array(elem_ty) => {
                        let elem_lty = acx.assign_pointer_ids(elem_ty);
                        let array_ty = rv.ty(acx, acx.tcx());
                        let args = acx.lcx().mk_slice(&[elem_lty]);
                        acx.lcx().mk(array_ty, args, PointerId::NONE)
                    }
                    AggregateKind::Adt(..) => {
                        let adt_ty = rv.ty(acx, acx.tcx());
                        acx.assign_pointer_ids(adt_ty)
                    }
                    AggregateKind::Tuple => {
                        let tuple_ty = rv.ty(acx, acx.tcx());
                        acx.assign_pointer_ids(tuple_ty)
                    }
                    _ => continue,
                },
                Rvalue::Cast(_, _, ty) => acx.assign_pointer_ids(*ty),
                Rvalue::Use(Operand::Constant(c)) => match label_string_literals(acx, c, loc) {
                    Some(lty) => lty,
                    None => continue,
                },
                _ => continue,
            };

            acx.rvalue_tys.insert(loc, lty);
        }
    }
}

fn run(tcx: TyCtxt) {
    let mut gacx = GlobalAnalysisCtxt::new(tcx);
    let mut func_info = HashMap::new();

    /// Local information, specific to a single function.  Many of the data structures we use for
    /// the pointer analysis have a "global" part that's shared between all functions and a "local"
    /// part that's specific to the function being analyzed; this struct contains only the local
    /// parts.  The different fields are set, used, and cleared at various points below.
    #[derive(Default)]
    struct FuncInfo<'tcx> {
        /// Local analysis context data, such as [`LTy`]s for all MIR locals.  Combine with the
        /// [`GlobalAnalysisCtxt`] to get a complete [`AnalysisCtxt`] for use within this function.
        acx_data: MaybeUnset<AnalysisCtxtData<'tcx>>,
        /// Dataflow constraints gathered from the body of this function.  These are used for
        /// propagating `READ`/`WRITE`/`OFFSET_ADD` and similar permissions.
        dataflow: MaybeUnset<DataflowConstraints>,
        /// Local equivalence-class information.  Combine with the [`GlobalEquivSet`] to get a
        /// complete [`EquivSet`], which assigns an equivalence class to each [`PointerId`] that
        /// appears in the function.  Used for renumbering [`PointerId`]s.
        local_equiv: MaybeUnset<LocalEquivSet>,
        /// Local part of the permission/flag assignment.  Combine with the [`GlobalAssignment`] to
        /// get a complete [`Assignment`] for this function, which maps every [`PointerId`] in this
        /// function to a [`PermissionSet`] and [`FlagSet`].
        lasn: MaybeUnset<LocalAssignment>,
    }

    // Follow a postorder traversal, so that callers are visited after their callees.  This means
    // callee signatures will usually be up to date when we visit the call site.
    let all_fn_ldids = fn_body_owners_postorder(tcx);
    eprintln!("callgraph traversal order:");
    for &ldid in &all_fn_ldids {
        eprintln!("  {:?}", ldid);
    }

    // Assign global `PointerId`s for all pointers that appear in function signatures.
    for &ldid in &all_fn_ldids {
        let sig = tcx.fn_sig(ldid.to_def_id());
        let sig = tcx.erase_late_bound_regions(sig);

        let inputs = sig
            .inputs()
            .iter()
            .map(|&ty| gacx.assign_pointer_ids(ty))
            .collect::<Vec<_>>();
        let inputs = gacx.lcx.mk_slice(&inputs);
        let output = gacx.assign_pointer_ids(sig.output());

        let lsig = LFnSig { inputs, output };
        gacx.fn_sigs.insert(ldid.to_def_id(), lsig);
    }

    // Collect all `static` items.
    let all_static_dids = all_static_items(tcx);
    eprintln!("statics:");
    for &did in &all_static_dids {
        eprintln!("  {:?}", did);
    }

    // Assign global `PointerId`s for types of `static` items.
    assert!(gacx.static_tys.is_empty());
    gacx.static_tys = HashMap::with_capacity(all_static_dids.len());
    for &did in &all_static_dids {
        gacx.assign_pointer_to_static(did);
    }

    // Label the field types of each struct.
    for ldid in tcx.hir_crate_items(()).definitions() {
        let did = ldid.to_def_id();
        use DefKind::*;
        if !matches!(tcx.def_kind(did), Struct | Enum | Union) {
            continue;
        }
        gacx.assign_pointer_to_fields(did);
    }

    // Initial pass to assign local `PointerId`s and gather equivalence constraints, which state
    // that two pointer types must be converted to the same reference type.  Some additional data
    // computed during this the process is kept around for use in later passes.
    let mut global_equiv = GlobalEquivSet::new(gacx.num_pointers());
    for &ldid in &all_fn_ldids {
        let ldid_const = WithOptConstParam::unknown(ldid);
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();
        let lsig = *gacx.fn_sigs.get(&ldid.to_def_id()).unwrap();

        let mut acx = gacx.function_context(&mir);

        // Assign PointerIds to local types
        assert!(acx.local_tys.is_empty());
        acx.local_tys = IndexVec::with_capacity(mir.local_decls.len());
        for (local, decl) in mir.local_decls.iter_enumerated() {
            let lty = match mir.local_kind(local) {
                LocalKind::Var | LocalKind::Temp => acx.assign_pointer_ids(decl.ty),
                LocalKind::Arg => {
                    debug_assert!(local.as_usize() >= 1 && local.as_usize() <= mir.arg_count);
                    lsig.inputs[local.as_usize() - 1]
                }
                LocalKind::ReturnPointer => lsig.output,
            };
            let l = acx.local_tys.push(lty);
            assert_eq!(local, l);

            let ptr = acx.new_pointer();
            let l = acx.addr_of_local.push(ptr);
            assert_eq!(local, l);
        }

        label_rvalue_tys(&mut acx, &mir);

        // Compute local equivalence classes and dataflow constraints.
        let (dataflow, equiv_constraints) = dataflow::generate_constraints(&acx, &mir);
        let mut local_equiv = LocalEquivSet::new(acx.num_pointers());
        let mut equiv = global_equiv.and_mut(&mut local_equiv);
        for (a, b) in equiv_constraints {
            equiv.unify(a, b);
        }

        let mut info = FuncInfo::default();
        info.acx_data.set(acx.into_data());
        info.dataflow.set(dataflow);
        info.local_equiv.set(local_equiv);
        func_info.insert(ldid, info);
    }

    // Remap pointers based on equivalence classes, so all members of an equivalence class now use
    // the same `PointerId`.
    let (global_counter, global_equiv_map) = global_equiv.renumber();
    eprintln!("global_equiv_map = {global_equiv_map:?}");
    gacx.remap_pointers(&global_equiv_map, global_counter);

    for &ldid in &all_fn_ldids {
        let info = func_info.get_mut(&ldid).unwrap();
        let (local_counter, local_equiv_map) = info.local_equiv.renumber(&global_equiv_map);
        eprintln!("local_equiv_map = {local_equiv_map:?}");
        info.acx_data.remap_pointers(
            gacx.lcx,
            global_equiv_map.and(&local_equiv_map),
            local_counter,
        );
        info.dataflow
            .remap_pointers(global_equiv_map.and(&local_equiv_map));
        info.local_equiv.clear();
    }

    // Compute permission and flag assignments.

    let mut gasn =
        GlobalAssignment::new(gacx.num_pointers(), PermissionSet::UNIQUE, FlagSet::empty());
    for info in func_info.values_mut() {
        let num_pointers = info.acx_data.num_pointers();
        let lasn = LocalAssignment::new(num_pointers, PermissionSet::UNIQUE, FlagSet::empty());
        info.lasn.set(lasn);
    }

    let adt_metadata = construct_adt_metadata(tcx);
    eprintln!("=== ADT Metadata ===");
    eprintln!("{adt_metadata:?}");

    let mut loop_count = 0;
    loop {
        // Loop until the global assignment reaches a fixpoint.  The inner loop also runs until a
        // fixpoint, but it only considers a single function at a time.  The inner loop for one
        // function can affect other functions by updating the `GlobalAssignment`, so we also need
        // the outer loop, which runs until the `GlobalAssignment` converges as well.
        loop_count += 1;
        let old_gasn = gasn.clone();
        for &ldid in &all_fn_ldids {
            let info = func_info.get_mut(&ldid).unwrap();
            let ldid_const = WithOptConstParam::unknown(ldid);
            let name = tcx.item_name(ldid.to_def_id());
            let mir = tcx.mir_built(ldid_const);
            let mir = mir.borrow();

            let field_tys = gacx.field_tys.clone();
            let acx = gacx.function_context_with_data(&mir, info.acx_data.take());
            let mut asn = gasn.and(&mut info.lasn);

            // `dataflow.propagate` and `borrowck_mir` both run until the assignment converges on a
            // fixpoint, so there's no need to do multiple iterations here.
            info.dataflow.propagate(&mut asn.perms_mut());

            borrowck::borrowck_mir(
                &acx,
                &info.dataflow,
                &mut asn.perms_mut(),
                name.as_str(),
                &mir,
                &adt_metadata,
                field_tys,
            );

            info.acx_data.set(acx.into_data());
        }

        if gasn == old_gasn {
            break;
        }
    }
    eprintln!("reached fixpoint in {} iterations", loop_count);

    // Print results for each function in `all_fn_ldids`, going in declaration order.  Concretely,
    // we iterate over `body_owners()`, which is a superset of `all_fn_ldids`, and filter based on
    // membership in `func_info`, which contains an entry for each ID in `all_fn_ldids`.
    let mut all_rewrites = Vec::new();
    for ldid in tcx.hir().body_owners() {
        // Skip any body owners that aren't present in `func_info`, and also get the info itself.
        let info = match func_info.get_mut(&ldid) {
            Some(x) => x,
            None => continue,
        };
        let ldid_const = WithOptConstParam::unknown(ldid);
        let name = tcx.item_name(ldid.to_def_id());
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();
        let acx = gacx.function_context_with_data(&mir, info.acx_data.take());
        let mut asn = gasn.and(&mut info.lasn);
        info.dataflow.propagate_cell(&mut asn);

        acx.check_string_literal_perms(&asn);

        // Print labeling and rewrites for the current function.

        eprintln!("\nfinal labeling for {:?}:", name);
        let lcx1 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
        let lcx2 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
        for (local, decl) in mir.local_decls.iter_enumerated() {
            print_labeling_for_var(
                lcx1,
                lcx2,
                format_args!("{:?} ({})", local, describe_local(tcx, decl)),
                acx.addr_of_local[local],
                acx.local_tys[local],
                &asn.perms(),
                &asn.flags(),
            );
        }

        eprintln!("\ntype assignment for {:?}:", name);
        rewrite::dump_rewritten_local_tys(&acx, &asn, &mir, describe_local);

        eprintln!();
        let hir_body_id = tcx.hir().body_owned_by(ldid);
        let expr_rewrites = rewrite::gen_expr_rewrites(&acx, &asn, &mir, hir_body_id);
        let ty_rewrites = rewrite::gen_ty_rewrites(&acx, &asn, &mir, ldid);
        // Print rewrites
        eprintln!(
            "\ngenerated {} expr rewrites + {} ty rewrites for {:?}:",
            expr_rewrites.len(),
            ty_rewrites.len(),
            name
        );
        for &(span, ref rw) in expr_rewrites.iter().chain(ty_rewrites.iter()) {
            eprintln!("  {}: {}", describe_span(tcx, span), rw);
        }
        all_rewrites.extend(expr_rewrites);
        all_rewrites.extend(ty_rewrites);
    }

    // Print results for `static` items.
    eprintln!("\nfinal labeling for static items:");
    let lcx1 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
    let lcx2 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
    for (did, lty) in gacx.static_tys.iter() {
        let name = tcx.item_name(*did);
        print_labeling_for_var(
            lcx1,
            lcx2,
            format_args!("{name:?}"),
            gacx.addr_of_static[&did],
            lty,
            &gasn.perms,
            &gasn.flags,
        );
    }

    let static_rewrites = rewrite::gen_static_rewrites(&gacx, &gasn);
    eprintln!("generated {} static rewrites:", static_rewrites.len());
    for &(span, ref rw) in &static_rewrites {
        eprintln!("    {}: {}", describe_span(gacx.tcx, span), rw);
    }
    all_rewrites.extend(static_rewrites);

    // Apply rewrite to all functions at once.
    rewrite::apply_rewrites(tcx, all_rewrites);
}

trait AssignPointerIds<'tcx> {
    fn lcx(&self) -> LTyCtxt<'tcx>;

    fn new_pointer(&mut self) -> PointerId;

    fn assign_pointer_ids(&mut self, ty: Ty<'tcx>) -> LTy<'tcx> {
        self.lcx().label(ty, &mut |ty| match ty.kind() {
            TyKind::Ref(_, _, _) | TyKind::RawPtr(_) => self.new_pointer(),
            _ => PointerId::NONE,
        })
    }
}

impl<'tcx> AssignPointerIds<'tcx> for GlobalAnalysisCtxt<'tcx> {
    fn lcx(&self) -> LTyCtxt<'tcx> {
        self.lcx
    }

    fn new_pointer(&mut self) -> PointerId {
        self.new_pointer()
    }
}

impl<'tcx> AssignPointerIds<'tcx> for AnalysisCtxt<'_, 'tcx> {
    fn lcx(&self) -> LTyCtxt<'tcx> {
        self.lcx()
    }

    fn new_pointer(&mut self) -> PointerId {
        self.new_pointer()
    }
}

fn describe_local(tcx: TyCtxt, decl: &LocalDecl) -> String {
    let mut span = decl.source_info.span;
    if let Some(ref info) = decl.local_info {
        if let LocalInfo::User(ref binding_form) = **info {
            let binding_form = binding_form.as_ref().assert_crate_local();
            if let BindingForm::Var(ref v) = *binding_form {
                span = v.pat_span;
            }
        }
    }
    describe_span(tcx, span)
}

fn describe_span(tcx: TyCtxt, span: Span) -> String {
    let s = tcx
        .sess
        .source_map()
        .span_to_snippet(span.source_callsite())
        .unwrap();
    let s = {
        let mut s2 = String::new();
        for word in s.split_ascii_whitespace() {
            if !s2.is_empty() {
                s2.push(' ');
            }
            s2.push_str(word);
        }
        s2
    };

    let (src1, src2, src3) = if s.len() > 20 {
        (&s[..15], " ... ", &s[s.len() - 5..])
    } else {
        (&s[..], "", "")
    };
    let line = tcx.sess.source_map().lookup_char_pos(span.lo()).line;
    format!("{}: {}{}{}", line, src1, src2, src3)
}

fn print_labeling_for_var<'tcx>(
    lcx1: LabeledTyCtxt<'tcx, PermissionSet>,
    lcx2: LabeledTyCtxt<'tcx, FlagSet>,
    desc: impl Display,
    addr_of_ptr: PointerId,
    lty: LTy<'tcx>,
    perms: &impl Index<PointerId, Output = PermissionSet>,
    flags: &impl Index<PointerId, Output = FlagSet>,
) {
    let addr_of1 = perms[addr_of_ptr];
    let ty1 = lcx1.relabel(lty, &mut |lty| {
        if lty.label == PointerId::NONE {
            PermissionSet::empty()
        } else {
            perms[lty.label]
        }
    });
    eprintln!("{}: addr_of = {:?}, type = {:?}", desc, addr_of1, ty1);

    let addr_of2 = flags[addr_of_ptr];
    let ty2 = lcx2.relabel(lty, &mut |lty| {
        if lty.label == PointerId::NONE {
            FlagSet::empty()
        } else {
            flags[lty.label]
        }
    });
    eprintln!(
        "{}: addr_of flags = {:?}, type flags = {:?}",
        desc, addr_of2, ty2
    );

    let addr_of3 = addr_of_ptr;
    let ty3 = lty;
    eprintln!("{}: addr_of = {:?}, type = {:?}", desc, addr_of3, ty3);
}

/// Return `LocalDefId`s for all `static`s.
fn all_static_items(tcx: TyCtxt) -> Vec<DefId> {
    let mut order = Vec::new();

    for root_ldid in tcx.hir().body_owners() {
        match tcx.def_kind(root_ldid) {
            DefKind::Fn | DefKind::AssocFn | DefKind::AnonConst | DefKind::Const => continue,
            DefKind::Static(_) => {}
            dk => panic!(
                "unexpected def_kind {:?} for body_owner {:?}",
                dk, root_ldid
            ),
        }
        order.push(root_ldid.to_def_id())
    }

    order
}

/// Return all `LocalDefId`s for all `fn`s that are `body_owners`, ordered according to a postorder
/// traversal of the graph of references between bodies.
fn fn_body_owners_postorder(tcx: TyCtxt) -> Vec<LocalDefId> {
    let mut seen = HashSet::new();
    let mut order = Vec::new();
    enum Visit {
        Pre(LocalDefId),
        Post(LocalDefId),
    }
    let mut stack = Vec::new();

    for root_ldid in tcx.hir().body_owners() {
        if seen.contains(&root_ldid) {
            continue;
        }

        match tcx.def_kind(root_ldid) {
            DefKind::Fn | DefKind::AssocFn => {}
            DefKind::AnonConst | DefKind::Const | DefKind::Static(_) => continue,
            dk => panic!(
                "unexpected def_kind {:?} for body_owner {:?}",
                dk, root_ldid
            ),
        }

        stack.push(Visit::Pre(root_ldid));
        while let Some(visit) = stack.pop() {
            match visit {
                Visit::Pre(ldid) => {
                    if seen.insert(ldid) {
                        stack.push(Visit::Post(ldid));
                        for_each_callee(tcx, ldid, |callee_ldid| {
                            stack.push(Visit::Pre(callee_ldid));
                        });
                    }
                }
                Visit::Post(ldid) => {
                    order.push(ldid);
                }
            }
        }
    }

    order
}

fn for_each_callee(tcx: TyCtxt, ldid: LocalDefId, f: impl FnMut(LocalDefId)) {
    let ldid_const = WithOptConstParam::unknown(ldid);
    let mir = tcx.mir_built(ldid_const);
    let mir = mir.borrow();
    let mir: &Body = &mir;

    struct CalleeVisitor<'a, 'tcx, F> {
        tcx: TyCtxt<'tcx>,
        mir: &'a Body<'tcx>,
        f: F,
    }

    impl<'tcx, F: FnMut(LocalDefId)> Visitor<'tcx> for CalleeVisitor<'_, 'tcx, F> {
        fn visit_operand(&mut self, operand: &Operand<'tcx>, _location: Location) {
            let ty = operand.ty(self.mir, self.tcx);
            let def_id = match util::ty_callee(self.tcx, ty) {
                Callee::LocalDef { def_id, .. } => def_id,
                _ => return,
            };
            let ldid = match def_id.as_local() {
                Some(x) => x,
                None => return,
            };
            if self.tcx.is_foreign_item(def_id) {
                return;
            }
            if !matches!(self.tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn) {
                return;
            }
            (self.f)(ldid);
        }
    }

    CalleeVisitor { tcx, mir, f }.visit_body(mir);
}

struct AnalysisCallbacks;

impl rustc_driver::Callbacks for AnalysisCallbacks {
    fn after_expansion<'tcx>(
        &mut self,
        _compiler: &rustc_interface::interface::Compiler,
        queries: &'tcx rustc_interface::Queries<'tcx>,
    ) -> rustc_driver::Compilation {
        queries.global_ctxt().unwrap().peek_mut().enter(|tcx| {
            run(tcx);
        });
        rustc_driver::Compilation::Continue
    }
}

fn main() -> rustc_interface::interface::Result<()> {
    init_logger();
    let args = env::args().collect::<Vec<_>>();
    rustc_driver::RunCompiler::new(&args, &mut AnalysisCallbacks).run()
}
