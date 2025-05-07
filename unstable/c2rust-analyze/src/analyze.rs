use crate::annotate::AnnotationBuffer;
use crate::borrowck;
use crate::context::{
    self, AnalysisCtxt, AnalysisCtxtData, Assignment, DontRewriteFieldReason, DontRewriteFnReason,
    DontRewriteStaticReason, FlagSet, GlobalAnalysisCtxt, LFnSig, LTy, LTyCtxt, PermissionSet,
    PointerId, PointerInfo,
};
use crate::dataflow;
use crate::dataflow::DataflowConstraints;
use crate::equiv::GlobalEquivSet;
use crate::equiv::LocalEquivSet;
use crate::labeled_ty::LabeledTyCtxt;
use crate::last_use::{self, LastUse};
use crate::panic_detail;
use crate::panic_detail::PanicDetail;
use crate::pointee_type;
use crate::pointee_type::PointeeTypes;
use crate::pointer_id::GlobalPointerTable;
use crate::pointer_id::LocalPointerTable;
use crate::pointer_id::PointerTable;
use crate::recent_writes::RecentWrites;
use crate::rewrite;
use crate::type_desc;
use crate::type_desc::Ownership;
use crate::util;
use crate::util::Callee;
use crate::util::TestAttr;
use c2rust_pdg::graph::Graphs;
use c2rust_pdg::info::NodeInfo;
use log::{debug, info, warn};
use rustc_hir::def::DefKind;
use rustc_hir::def_id::CrateNum;
use rustc_hir::def_id::DefId;
use rustc_hir::def_id::DefIndex;
use rustc_hir::def_id::LocalDefId;
use rustc_hir::definitions::DefPathData;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::visit::{PlaceContext, Visitor};
use rustc_middle::mir::{
    AggregateKind, BindingForm, Body, Constant, Local, LocalDecl, LocalInfo, LocalKind, Location,
    Operand, Place, PlaceElem, PlaceRef, Rvalue, StatementKind,
};
use rustc_middle::ty::GenericArgKind;
use rustc_middle::ty::Ty;
use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::TyKind;
use rustc_middle::ty::WithOptConstParam;
use rustc_span::{Span, Symbol};
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Write as _;
use std::fs::File;
use std::io;
use std::io::BufRead;
use std::io::BufReader;
use std::iter;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::Index;
use std::panic::AssertUnwindSafe;
use std::path::Path;
use std::str::FromStr;

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
    #[track_caller]
    pub fn set(&mut self, x: T) {
        if self.0.is_some() {
            panic!("value is already set");
        }
        self.0 = Some(x);
    }

    #[track_caller]
    pub fn clear(&mut self) {
        if self.0.is_none() {
            panic!("value is already cleared");
        }
        self.0 = None;
    }

    #[track_caller]
    pub fn get(&self) -> &T {
        self.0.as_ref().expect("value is not set")
    }

    #[track_caller]
    pub fn get_mut(&mut self) -> &mut T {
        self.0.as_mut().expect("value is not set")
    }

    #[track_caller]
    pub fn take(&mut self) -> T {
        self.0.take().expect("value is not set")
    }

    pub fn is_set(&self) -> bool {
        self.0.is_some()
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

            let _g = panic_detail::set_current_span(stmt.source_info.span);

            let lty = match rv {
                Rvalue::Ref(..) | Rvalue::AddressOf(..) => {
                    let ty = rv.ty(acx, acx.tcx());
                    acx.assign_pointer_ids(ty)
                }
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
                Rvalue::Cast(_, _, ty) => {
                    acx.assign_pointer_ids_with_info(*ty, PointerInfo::ANNOTATED)
                }
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

/// Set flags in `acx.ptr_info` based on analysis of the `mir`.  This is used for `PointerInfo`
/// flags that represent non-local properties or other properties that can't be set easily when the
/// `PointerId` is first allocated.
fn update_pointer_info<'tcx>(acx: &mut AnalysisCtxt<'_, 'tcx>, mir: &Body<'tcx>) {
    // For determining whether a local should have `NOT_TEMPORARY_REF`, we look for the code
    // pattern that rustc generates when lowering `&x` and `&mut x` expressions.  This normally
    // consists of a `LocalKind::Temp` local that's initialized with `_1 = &mut ...;` or a similar
    // statement and that isn't modified or overwritten anywhere else.  Thus, we keep track of
    // which locals appear on the LHS of such statements and also the number of places in which a
    // local is (possibly) written.
    //
    // Note this currently detects only one of the temporaries in `&mut &mut x`, so we may need to
    // make these checks more precise at some point.
    let mut write_count = HashMap::with_capacity(mir.local_decls.len());
    let mut rhs_is_ref = HashSet::new();

    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        for (i, stmt) in bb_data.statements.iter().enumerate() {
            let (pl, rv) = match &stmt.kind {
                StatementKind::Assign(x) => &**x,
                _ => continue,
            };

            debug!(
                "update_pointer_info: visit assignment: {:?}[{}]: {:?}",
                bb, i, stmt
            );

            if !pl.is_indirect() {
                // This is a write directly to `pl.local`.
                *write_count.entry(pl.local).or_insert(0) += 1;
                debug!("  record write to LHS {:?}", pl);
            }

            let ref_pl = match *rv {
                Rvalue::Ref(_rg, _kind, pl) => Some(pl),
                Rvalue::AddressOf(_mutbl, pl) => Some(pl),
                _ => None,
            };
            if let Some(ref_pl) = ref_pl {
                // For simplicity, we consider taking the address of a local to be a write.  We
                // expect this not to happen for the sorts of temporary refs we're looking for.
                if !ref_pl.is_indirect() {
                    debug!("  record write to ref target {:?}", ref_pl);
                    *write_count.entry(ref_pl.local).or_insert(0) += 1;
                }

                rhs_is_ref.insert(pl.local);
            }
        }
    }

    for local in mir.local_decls.indices() {
        let is_temp_ref = mir.local_kind(local) == LocalKind::Temp
            && write_count.get(&local).copied().unwrap_or(0) == 1
            && rhs_is_ref.contains(&local);

        if let Some(ptr) = acx.ptr_of(local) {
            if !is_temp_ref {
                acx.ptr_info_mut()[ptr].insert(PointerInfo::NOT_TEMPORARY_REF);
            }
        }
    }
}

fn foreign_mentioned_tys(tcx: TyCtxt) -> HashSet<DefId> {
    let mut foreign_mentioned_tys = HashSet::new();
    for ty in tcx
        .hir_crate_items(())
        .foreign_items()
        .map(|item| item.def_id.to_def_id())
        .filter_map(|did| match tcx.def_kind(did) {
            DefKind::Fn | DefKind::AssocFn => Some(tcx.mk_fn_ptr(tcx.fn_sig(did))),
            DefKind::Static(_) => Some(tcx.type_of(did)),
            _ => None,
        })
    {
        walk_adts(tcx, ty, &mut |did| foreign_mentioned_tys.insert(did));
    }
    foreign_mentioned_tys
}

/// Walks the type `ty` and applies a function `f` to it if it's an ADT
/// `f` gets applied recursively to `ty`'s generic types and fields (if applicable)
/// If `f` returns false, the fields of the ADT are not recursed into.
/// Otherwise, the function will naturally terminate given that the generic types
/// of a type are finite in length.
/// We only look for ADTs rather than other FFI-crossing types because ADTs
/// are the only nominal ones, which are the ones that we may rewrite.
fn walk_adts<'tcx, F>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>, f: &mut F)
where
    F: FnMut(DefId) -> bool,
{
    for arg in ty.walk() {
        if let GenericArgKind::Type(ty) = arg.unpack() {
            if let TyKind::Adt(adt_def, _) = ty.kind() {
                if !f(adt_def.did()) {
                    continue;
                }
                for field in adt_def.all_fields() {
                    let field_ty = tcx.type_of(field.did);
                    walk_adts(tcx, field_ty, f);
                }
            }
        }
    }
}

fn mark_foreign_fixed<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    asn: &mut Assignment,
    tcx: TyCtxt<'tcx>,
) {
    // FIX the inputs and outputs of function declarations in extern blocks
    for (did, lsig) in gacx.fn_sigs.iter() {
        if tcx.is_foreign_item(did) {
            make_sig_fixed(asn, lsig);
        }
    }

    // FIX the types of static declarations in extern blocks
    for (did, lty) in gacx.static_tys.iter() {
        if tcx.is_foreign_item(did) {
            make_ty_fixed(asn, lty);

            // Also fix the `addr_of_static` permissions.
            let ptr = gacx.addr_of_static[did];
            asn.flags[ptr].insert(FlagSet::FIXED);
        }
    }

    // FIX the fields of structs mentioned in extern blocks
    for adt_did in &gacx.adt_metadata.struct_dids {
        if gacx.foreign_mentioned_tys.contains(adt_did) {
            let adt_def = tcx.adt_def(adt_did);
            let fields = adt_def.all_fields();
            for field in fields {
                let field_lty = gacx.field_ltys[&field.did];
                debug!(
                    "adding FIXED permission for {adt_did:?} field {:?}",
                    field.did
                );
                make_ty_fixed(asn, field_lty);
            }
        }
    }
}

fn mark_all_statics_fixed<'tcx>(gacx: &mut GlobalAnalysisCtxt<'tcx>, asn: &mut Assignment) {
    for (did, lty) in gacx.static_tys.iter() {
        make_ty_fixed(asn, lty);

        // Also fix the `addr_of_static` permissions.
        let ptr = gacx.addr_of_static[did];
        asn.flags[ptr].insert(FlagSet::FIXED);
    }
}

fn mark_all_structs_fixed<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    asn: &mut Assignment,
    tcx: TyCtxt<'tcx>,
) {
    for adt_did in &gacx.adt_metadata.struct_dids {
        let adt_def = tcx.adt_def(adt_did);
        let fields = adt_def.all_fields();
        for field in fields {
            let field_lty = gacx.field_ltys[&field.did];
            make_ty_fixed(asn, field_lty);
        }
    }
}

fn parse_def_id(s: &str) -> Result<DefId, String> {
    // DefId debug output looks like `DefId(0:1 ~ alias1[0dc4]::{use#0})`.  The ` ~ name` part may
    // be omitted if the name/DefPath info is not available at the point in the compiler where the
    // `DefId` was printed.
    let orig_s = s;
    let s = s
        .strip_prefix("DefId(")
        .ok_or("does not start with `DefId(`")?;
    let s = s.strip_suffix(')').ok_or("does not end with `)`")?;
    let s = match s.find(" ~ ") {
        Some(i) => &s[..i],
        None => s,
    };
    let i = s
        .find(':')
        .ok_or("does not contain `:` in `CrateNum:DefIndex` part")?;

    let krate_str = &s[..i];
    let index_str = &s[i + 1..];
    let krate = u32::from_str(krate_str).map_err(|_| "failed to parse CrateNum")?;
    let index = u32::from_str(index_str).map_err(|_| "failed to parse DefIndex")?;
    let def_id = DefId {
        krate: CrateNum::from_u32(krate),
        index: DefIndex::from_u32(index),
    };

    let rendered = format!("{:?}", def_id);
    if rendered != orig_s {
        return Err(format!(
            "path mismatch: after parsing input {}, obtained a different path {:?}",
            orig_s, def_id
        ));
    }

    Ok(def_id)
}

fn read_defs_list(defs: &mut HashSet<DefId>, path: &str) -> io::Result<()> {
    let f = BufReader::new(File::open(path)?);
    for (i, line) in f.lines().enumerate() {
        let line = line?;
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        let def_id = parse_def_id(line).unwrap_or_else(|e| {
            panic!("failed to parse {} line {}: {}", path, i + 1, e);
        });
        defs.insert(def_id);
    }
    Ok(())
}

/// Examine each `DefId` in the crate, and add to `fixed_defs` any that doesn't match at least one
/// prefix in `prefixes`.  For example, if `prefixes` is `foo,bar::baz`, only `foo`, `bar::baz`,
/// and their descendants will be eligible for rewriting; all other `DefId`s will be added to
/// `fixed_defs`.
fn check_rewrite_path_prefixes(tcx: TyCtxt, fixed_defs: &mut HashSet<DefId>, prefixes: &str) {
    let hir = tcx.hir();
    let prefixes: HashSet<Vec<Symbol>> = prefixes
        .split(',')
        // Exclude empty paths.  This allows for leading/trailing commas or double commas within
        // the list, which may result when building the list programmatically.
        .filter(|prefix| !prefix.is_empty())
        .map(|prefix| prefix.split("::").map(Symbol::intern).collect::<Vec<_>>())
        .collect();
    let sym_impl = Symbol::intern("{impl}");
    // Buffer for accumulating the path to a particular def.
    let mut path_buf = Vec::with_capacity(10);
    for ldid in tcx.hir_crate_items(()).definitions() {
        let def_path = hir.def_path(ldid);

        // Traverse `def_path`, adding each `Symbol` to `path_buf`.  We check after each push
        // whether `path_buf` matches something in `prefixes`, which has the effect of checking
        // every prefix of the path of `ldid`.
        path_buf.clear();
        let mut matched = false;
        for ddpd in &def_path.data {
            match ddpd.data {
                // We ignore these when building the `Symbol` path.
                DefPathData::CrateRoot
                | DefPathData::ForeignMod
                | DefPathData::Use
                | DefPathData::GlobalAsm
                | DefPathData::ClosureExpr
                | DefPathData::Ctor
                | DefPathData::AnonConst
                | DefPathData::ImplTrait => continue,
                DefPathData::TypeNs(sym)
                | DefPathData::ValueNs(sym)
                | DefPathData::MacroNs(sym)
                | DefPathData::LifetimeNs(sym) => {
                    path_buf.push(sym);
                }
                DefPathData::Impl => {
                    path_buf.push(sym_impl);
                }
            }
            if prefixes.contains(&path_buf) {
                matched = true;
                break;
            }
        }

        if !matched {
            fixed_defs.insert(ldid.to_def_id());
        }
    }
}

fn get_fixed_defs(tcx: TyCtxt) -> io::Result<HashSet<DefId>> {
    let mut fixed_defs = HashSet::new();
    if let Ok(path) = env::var("C2RUST_ANALYZE_FIXED_DEFS_LIST") {
        read_defs_list(&mut fixed_defs, &path)?;
    }
    if let Ok(prefixes) = env::var("C2RUST_ANALYZE_REWRITE_PATHS") {
        check_rewrite_path_prefixes(tcx, &mut fixed_defs, &prefixes);
    }
    Ok(fixed_defs)
}

fn get_force_rewrite_defs() -> io::Result<HashSet<DefId>> {
    let mut force_rewrite = HashSet::new();
    if let Ok(path) = env::var("C2RUST_ANALYZE_FORCE_REWRITE_LIST") {
        read_defs_list(&mut force_rewrite, &path)?;
    }
    Ok(force_rewrite)
}

fn get_skip_pointee_defs() -> io::Result<HashSet<DefId>> {
    let mut skip_pointee = HashSet::new();
    if let Ok(path) = env::var("C2RUST_ANALYZE_SKIP_POINTEE_LIST") {
        read_defs_list(&mut skip_pointee, &path)?;
    }
    Ok(skip_pointee)
}

fn get_rewrite_mode(tcx: TyCtxt, pointwise_fn_ldid: Option<LocalDefId>) -> rewrite::UpdateFiles {
    let mut update_files = rewrite::UpdateFiles::No;
    if let Ok(val) = env::var("C2RUST_ANALYZE_REWRITE_MODE") {
        match val.as_str() {
            "none" => {}
            "inplace" => {
                update_files = rewrite::UpdateFiles::InPlace;
            }
            "alongside" => {
                update_files = rewrite::UpdateFiles::Alongside;
            }
            "pointwise" => {
                let pointwise_fn_ldid = pointwise_fn_ldid.expect(
                    "C2RUST_ANALYZE_REWRITE_MODE=pointwise, \
                            but pointwise_fn_ldid is unset?",
                );
                let pointwise_fn_name = tcx.item_name(pointwise_fn_ldid.to_def_id());
                update_files = rewrite::UpdateFiles::AlongsidePointwise(pointwise_fn_name);
            }
            _ => panic!("bad value {:?} for C2RUST_ANALYZE_REWRITE_MODE", val),
        }
    }
    update_files
}

/// Local information, specific to a single function.  Many of the data structures we use for
/// the pointer analysis have a "global" part that's shared between all functions and a "local"
/// part that's specific to the function being analyzed; this struct contains only the local
/// parts.  The different fields are set, used, and cleared at various points below.
#[derive(Clone, Default)]
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
    /// Constraints on pointee types gathered from the body of this function.
    pointee_constraints: MaybeUnset<pointee_type::ConstraintSet<'tcx>>,
    /// Local part of pointee type sets.
    local_pointee_types: MaybeUnset<LocalPointerTable<PointeeTypes<'tcx>>>,
    /// Table for looking up the most recent write to a given local.
    recent_writes: MaybeUnset<RecentWrites>,
    /// Analysis result indicating which uses of each local are actually the last use of that
    /// local.
    last_use: MaybeUnset<LastUse>,
}

fn run(tcx: TyCtxt) {
    debug!("all defs:");
    for ldid in tcx.hir_crate_items(()).definitions() {
        //debug!("{:?} @ {:?}", ldid, tcx.source_span(ldid));
        debug!("{:?}", ldid);
        if tcx.def_kind(ldid) == DefKind::Struct {
            let adt_def = tcx.adt_def(ldid);
            for field in &adt_def.non_enum_variant().fields {
                debug!("{:?}", field.did);
            }
        }
    }

    // Load the list of fixed defs early, so any errors are reported immediately.
    let fixed_defs = get_fixed_defs(tcx).unwrap();

    let rewrite_pointwise = env::var("C2RUST_ANALYZE_REWRITE_MODE")
        .ok()
        .map_or(false, |val| val == "pointwise");

    let mut gacx = GlobalAnalysisCtxt::new(tcx);
    let mut func_info = HashMap::new();

    // Follow a postorder traversal, so that callers are visited after their callees.  This means
    // callee signatures will usually be up to date when we visit the call site.
    let all_fn_ldids = fn_body_owners_postorder(tcx);
    debug!("callgraph traversal order:");
    for &ldid in &all_fn_ldids {
        debug!("  {:?}", ldid);
    }

    gacx.force_rewrite = get_force_rewrite_defs().unwrap();
    eprintln!("{} force_rewrite defs", gacx.force_rewrite.len());
    let mut xs = gacx.force_rewrite.iter().copied().collect::<Vec<_>>();
    xs.sort();
    for x in xs {
        eprintln!("{:?}", x);
    }

    populate_field_users(&mut gacx, &all_fn_ldids);

    // ----------------------------------
    // Label all global and local types
    // ----------------------------------

    assign_pointer_ids(&mut gacx, &mut func_info, &all_fn_ldids);

    // Compute hypothetical region data for all ADTs and functions.  This can only be done after
    // all field types are labeled.
    gacx.construct_region_metadata();

    // ----------------------------------
    // Run early analyses
    // ----------------------------------

    do_recent_writes(&gacx, &mut func_info, &all_fn_ldids);
    do_last_use(&gacx, &mut func_info, &all_fn_ldids);

    // ----------------------------------
    // Remap `PointerId`s by equivalence class
    // ----------------------------------

    // Initial pass to gather equivalence constraints, which state that two pointer types must be
    // converted to the same reference type.  Some additional data computed during this the process
    // is kept around for use in later passes.
    let global_equiv = build_equiv_constraints(&mut gacx, &mut func_info, &all_fn_ldids);

    // Remap pointers based on equivalence classes, so all members of an equivalence class now use
    // the same `PointerId`.
    let (global_counter, global_equiv_map) = global_equiv.renumber();
    debug!("global_equiv_map = {global_equiv_map:?}");
    gacx.remap_pointers(&global_equiv_map, global_counter.num_pointers());

    let mut local_counter = global_counter.into_local();
    for &ldid in &all_fn_ldids {
        // Note that we apply this `PointerId` remapping even for failed functions.

        let info = func_info.get_mut(&ldid).unwrap();
        let (local_base, local_count, local_equiv_map) = info
            .local_equiv
            .renumber(&global_equiv_map, &mut local_counter);
        debug!("local_equiv_map = {local_equiv_map:?}");
        info.acx_data.remap_pointers(
            &mut gacx,
            global_equiv_map.and(&local_equiv_map),
            local_base,
            local_count,
        );
        if info.dataflow.is_set() {
            info.dataflow
                .remap_pointers(global_equiv_map.and(&local_equiv_map));
        }
        info.local_equiv.clear();
    }

    // ----------------------------------
    // Infer pointee types
    // ----------------------------------

    // This runs after equivalence class remapping because it lets us get better pointee results in
    // pointer-to-pointer cases without implementing full type unification.

    let global_pointee_types = do_pointee_type(&mut gacx, &mut func_info, &all_fn_ldids);
    debug_print_pointee_types(
        &mut gacx,
        &mut func_info,
        &all_fn_ldids,
        &global_pointee_types,
    );

    // ----------------------------------
    // Compute dataflow constraints
    // ----------------------------------

    build_dataflow_constraints(
        &mut gacx,
        &mut func_info,
        &all_fn_ldids,
        &global_pointee_types,
    );

    // ----------------------------------
    // Build initial assignment
    // ----------------------------------

    // Compute permission and flag assignments.

    fn should_make_fixed(info: PointerInfo) -> bool {
        // We group ref types into three categories:
        //
        // 1. Explicitly annotated as a reference by the user (`REF` and `ANNOTATED`)
        // 2. Inferred as ref by the compiler, excluding temporaries (`REF` and
        //    `NOT_TEMPORARY_REF`, but not `ANNOTATED`)
        // 3. Temporary refs (`REF` but not `ANNOTATED` or `NOT_TEMPORARY_REF`)
        //
        // Currently, we apply the `FIXED` flag to categories 1 and 2.
        info.contains(PointerInfo::REF)
            && (info.contains(PointerInfo::ANNOTATED)
                || info.contains(PointerInfo::NOT_TEMPORARY_REF))
    }

    // track all types mentioned in extern blocks, we
    // don't want to rewrite those
    gacx.foreign_mentioned_tys = foreign_mentioned_tys(tcx);

    const INITIAL_PERMS: PermissionSet = PermissionSet::union_all([
        PermissionSet::UNIQUE,
        PermissionSet::NON_NULL,
        PermissionSet::HEAP,
        PermissionSet::STACK,
    ]);
    const INITIAL_FLAGS: FlagSet = FlagSet::empty();

    let mut asn = Assignment::new(gacx.num_total_pointers(), INITIAL_PERMS, INITIAL_FLAGS);
    let mut updates_forbidden = GlobalPointerTable::new(gacx.num_total_pointers());

    for (ptr, &info) in gacx.ptr_info().iter() {
        if should_make_fixed(info) {
            asn.flags[ptr].insert(FlagSet::FIXED);
        }
        if info.contains(PointerInfo::ADDR_OF_LOCAL) {
            // `addr_of_local` is always a stack pointer, though it should be rare for the
            // `ADDR_OF_LOCAL` flag to appear on a global `PointerId`.
            asn.perms[ptr].remove(PermissionSet::HEAP);
        }
    }

    mark_foreign_fixed(&mut gacx, &mut asn, tcx);

    if rewrite_pointwise {
        // In pointwise mode, we restrict rewriting to a single fn at a time.  All statics and
        // struct fields are marked `FIXED` so they won't be rewritten.
        mark_all_statics_fixed(&mut gacx, &mut asn);
        mark_all_structs_fixed(&mut gacx, &mut asn, tcx);
    }

    for (ptr, perms) in gacx.known_fn_ptr_perms() {
        let existing_perms = &mut asn.perms[ptr];
        existing_perms.remove(INITIAL_PERMS);
        assert_eq!(*existing_perms, PermissionSet::empty());
        *existing_perms = perms;
    }

    for info in func_info.values_mut() {
        for (ptr, &info) in info.acx_data.local_ptr_info().iter() {
            if should_make_fixed(info) {
                asn.flags[ptr].insert(FlagSet::FIXED);
            }
            if info.contains(PointerInfo::ADDR_OF_LOCAL) {
                // `addr_of_local` is always a stack pointer.  This will be propagated
                // automatically through dataflow whenever the address of the local is taken.
                asn.perms[ptr].remove(PermissionSet::HEAP);
            }
        }
    }

    let skip_borrowck_everywhere = env::var("C2RUST_ANALYZE_SKIP_BORROWCK").as_deref() == Ok("1");

    // Load permission info from PDG
    let pdg_compare = env::var("C2RUST_ANALYZE_COMPARE_PDG").as_deref() == Ok("1");
    // In compare mode, we load the PDG for comparison after analysis, not before.
    if !pdg_compare {
        if let Some(pdg_file_path) = std::env::var_os("PDG_FILE") {
            pdg_update_permissions(
                &mut gacx,
                &all_fn_ldids,
                &mut func_info,
                &mut asn,
                &mut updates_forbidden,
                skip_borrowck_everywhere,
                pdg_file_path,
            );
        }
    }

    // Items in the "fixed defs" list have all pointers in their types set to `FIXED`.  For
    // testing, putting #[c2rust_analyze_test::fixed_signature] on an item has the same effect.
    for ldid in tcx.hir_crate_items(()).definitions() {
        // TODO (HACK): `Clone::clone` impls are omitted from `fn_sigs` and cause a panic below.
        if is_impl_clone(tcx, ldid.to_def_id()) {
            continue;
        }

        let def_fixed = fixed_defs.contains(&ldid.to_def_id())
            || util::has_test_attr(tcx, ldid, TestAttr::FixedSignature);
        match tcx.def_kind(ldid.to_def_id()) {
            DefKind::Fn | DefKind::AssocFn if def_fixed => {
                let lsig = match gacx.fn_sigs.get(&ldid.to_def_id()) {
                    Some(x) => x,
                    None => panic!("missing fn_sig for {:?}", ldid),
                };
                make_sig_fixed(&mut asn, lsig);
                gacx.dont_rewrite_fns
                    .add(ldid.to_def_id(), DontRewriteFnReason::USER_REQUEST);
            }

            DefKind::Struct | DefKind::Enum | DefKind::Union => {
                let adt_def = tcx.adt_def(ldid);
                for field in adt_def.all_fields() {
                    // Each field can be separately listed in `fixed_defs` or annotated with the
                    // attribute to cause it to be marked FIXED.  If the whole ADT is
                    // listed/annotated, then every field is marked FIXED.
                    let field_fixed = def_fixed
                        || fixed_defs.contains(&ldid.to_def_id())
                        || field.did.as_local().map_or(false, |ldid| {
                            util::has_test_attr(tcx, ldid, TestAttr::FixedSignature)
                        });
                    if field_fixed {
                        let lty = match gacx.field_ltys.get(&field.did) {
                            Some(&x) => x,
                            None => panic!("missing field_lty for {:?}", ldid),
                        };
                        make_ty_fixed(&mut asn, lty);
                        gacx.dont_rewrite_fields
                            .add(field.did, DontRewriteFieldReason::USER_REQUEST);
                    }
                }
            }

            DefKind::Static(_) if def_fixed => {
                let lty = match gacx.static_tys.get(&ldid.to_def_id()) {
                    Some(&x) => x,
                    None => panic!("missing static_ty for {:?}", ldid),
                };
                make_ty_fixed(&mut asn, lty);

                let ptr = match gacx.addr_of_static.get(&ldid.to_def_id()) {
                    Some(&x) => x,
                    None => panic!("missing addr_of_static for {:?}", ldid),
                };
                if !ptr.is_none() {
                    asn.flags[ptr].insert(FlagSet::FIXED);
                }
                gacx.dont_rewrite_statics
                    .add(ldid.to_def_id(), DontRewriteStaticReason::USER_REQUEST);
            }

            _ => {}
        }
    }

    // ----------------------------------
    // Run dataflow solver and borrowck analysis
    // ----------------------------------

    apply_test_attr_fail_before_analysis(&mut gacx, &all_fn_ldids);
    apply_test_attr_force_non_null_args(&mut gacx, &all_fn_ldids, &mut asn, &mut updates_forbidden);

    debug!("=== ADT Metadata ===");
    debug!("{:?}", gacx.adt_metadata);

    let mut loop_count = 0;
    loop {
        // Loop until the global assignment reaches a fixpoint.  The inner loop also runs until a
        // fixpoint, but it only considers a single function at a time.  The inner loop for one
        // function can affect other functions by updating the `Assignment`, so we also need the
        // outer loop, which runs until the `Assignment` converges as well.
        loop_count += 1;
        let old_gasn = asn.perms.as_slice()[..gacx.num_global_pointers()].to_owned();

        for &ldid in &all_fn_ldids {
            if gacx.fn_analysis_invalid(ldid.to_def_id()) {
                continue;
            }

            let skip_borrowck =
                skip_borrowck_everywhere || util::has_test_attr(tcx, ldid, TestAttr::SkipBorrowck);

            let info = func_info.get_mut(&ldid).unwrap();
            let ldid_const = WithOptConstParam::unknown(ldid);
            let name = tcx.item_name(ldid.to_def_id());
            let mir = tcx.mir_built(ldid_const);
            let mir = mir.borrow();

            let field_ltys = gacx.field_ltys.clone();
            let acx = gacx.function_context_with_data(&mir, info.acx_data.take());

            let r = panic_detail::catch_unwind(AssertUnwindSafe(|| {
                // `dataflow.propagate` and `borrowck_mir` both run until the assignment converges
                // on a fixpoint, so there's no need to do multiple iterations here.
                info.dataflow.propagate(&mut asn.perms, &updates_forbidden);

                if !skip_borrowck {
                    borrowck::borrowck_mir(
                        &acx,
                        &info.dataflow,
                        asn.perms_mut(),
                        &updates_forbidden,
                        name.as_str(),
                        &mir,
                        field_ltys,
                    );
                }
            }));

            info.acx_data.set(acx.into_data());

            match r {
                Ok(()) => {}
                Err(pd) => {
                    gacx.mark_fn_failed(
                        ldid.to_def_id(),
                        DontRewriteFnReason::BORROWCK_INVALID,
                        pd,
                    );
                    continue;
                }
            }
        }

        let mut num_changed = 0;
        for (i, &old) in old_gasn.iter().enumerate() {
            let ptr = PointerId::global(i as u32);

            if skip_borrowck_everywhere {
                asn.perms[ptr].insert(PermissionSet::UNIQUE);
            }

            let new = asn.perms[ptr];
            if old != new {
                let added = new & !old;
                let removed = old & !new;
                let kept = old & new;
                debug!(
                    "changed {:?}: added {:?}, removed {:?}, kept {:?}",
                    ptr, added, removed, kept
                );
                num_changed += 1;
            }
        }
        debug!(
            "iteration {}: {} global pointers changed",
            loop_count, num_changed
        );

        if asn.perms.as_slice()[..gacx.num_global_pointers()] == old_gasn {
            break;
        }
    }
    info!("reached fixpoint in {} iterations", loop_count);

    // Do final processing on each function.
    for &ldid in &all_fn_ldids {
        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            continue;
        }

        let info = func_info.get_mut(&ldid).unwrap();
        let ldid_const = WithOptConstParam::unknown(ldid);
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();
        let acx = gacx.function_context_with_data(&mir, info.acx_data.take());

        let r = panic_detail::catch_unwind(AssertUnwindSafe(|| {
            // Add the CELL permission to pointers that need it.
            info.dataflow.propagate_cell(&mut asn);

            acx.check_string_literal_perms(&asn);
        }));

        info.acx_data.set(acx.into_data());

        match r {
            Ok(()) => {}
            Err(pd) => {
                gacx.mark_fn_failed(
                    ldid.to_def_id(),
                    DontRewriteFnReason::MISC_ANALYSIS_INVALID,
                    pd,
                );
                continue;
            }
        }
    }

    // Check that these perms haven't changed.
    let mut known_perm_error_ptrs = HashSet::new();
    for (ptr, perms) in gacx.known_fn_ptr_perms() {
        if asn.perms[ptr] != perms {
            known_perm_error_ptrs.insert(ptr);
            warn!(
                "known permissions changed for PointerId {ptr:?}: {perms:?} -> {:?}",
                asn.perms[ptr]
            );
        }
    }

    let mut known_perm_error_fns = HashSet::new();
    for (&def_id, lsig) in &gacx.fn_sigs {
        if !tcx.is_foreign_item(def_id) {
            continue;
        }
        for lty in lsig.inputs_and_output().flat_map(|lty| lty.iter()) {
            let ptr = lty.label;
            if !ptr.is_none() && known_perm_error_ptrs.contains(&ptr) {
                known_perm_error_fns.insert(def_id);
                warn!("known permissions changed for {def_id:?}: {lsig:?}");
                break;
            }
        }
    }

    // PDG comparison mode: skip all normal rewriting, and instead add annotations describing
    // places where the static analysis and PDG differ.
    if pdg_compare {
        // For each `PointerId`, this records whether we saw a null pointer stored in a location
        // annotated with that `PointerId` and whether we saw at least one non-null pointer stored
        // in such a location.
        let mut observations = HashMap::<(Option<LocalDefId>, PointerId), (bool, bool)>::new();

        let pdg_file_path = std::env::var_os("PDG_FILE")
            .unwrap_or_else(|| panic!("must set PDG_FILE for PDG comparison mode"));
        pdg_update_permissions_with_callback(
            &mut gacx,
            &all_fn_ldids,
            &mut func_info,
            &mut asn,
            &mut updates_forbidden,
            pdg_file_path,
            |_asn, _updates_forbidden, ldid, ptr, ptr_is_global, _node_info, node_is_non_null| {
                let parent = if ptr_is_global { None } else { Some(ldid) };
                let obs = observations.entry((parent, ptr)).or_insert((false, false));
                if node_is_non_null {
                    obs.1 = true;
                } else {
                    obs.0 = true;
                }
            },
        );

        let mut ann = AnnotationBuffer::new(tcx);
        // Generate comparison annotations for all functions.
        for ldid in tcx.hir().body_owners() {
            // Skip any body owners that aren't present in `func_info`, and also get the info
            // itself.
            let info = match func_info.get_mut(&ldid) {
                Some(x) => x,
                None => continue,
            };

            if !info.acx_data.is_set() {
                continue;
            }

            let ldid_const = WithOptConstParam::unknown(ldid);
            let mir = tcx.mir_built(ldid_const);
            let mir = mir.borrow();
            let acx = gacx.function_context_with_data(&mir, info.acx_data.take());

            // Generate inline annotations for pointer-typed locals
            for (local, decl) in mir.local_decls.iter_enumerated() {
                let span = local_span(decl);
                let mut ptrs = Vec::new();
                let ty_str = context::print_ty_with_pointer_labels(acx.local_tys[local], |ptr| {
                    if ptr.is_none() {
                        return String::new();
                    }
                    ptrs.push(ptr);
                    format!("{{{}}}", ptr)
                });
                // Pointers where static analysis reports `NON_NULL` but dynamic reports nullable.
                let mut static_non_null_ptrs = Vec::new();
                // Pointers where dynamic analysis reports `NON_NULL` but static reports nullable.
                let mut dynamic_non_null_ptrs = Vec::new();
                for ptr in ptrs {
                    let static_non_null: bool = asn.perms()[ptr].contains(PermissionSet::NON_NULL);
                    let parent = if acx.ptr_is_global(ptr) {
                        None
                    } else {
                        Some(ldid)
                    };
                    let dynamic_non_null: Option<bool> = observations
                        .get(&(parent, ptr))
                        .map(|&(saw_null, saw_non_null)| saw_non_null && !saw_null);
                    if dynamic_non_null.is_none() || dynamic_non_null == Some(static_non_null) {
                        // No conflict between static and dynamic results.
                        continue;
                    }
                    if static_non_null {
                        static_non_null_ptrs.push(ptr);
                    } else {
                        dynamic_non_null_ptrs.push(ptr);
                    }
                }
                if static_non_null_ptrs.is_empty() && dynamic_non_null_ptrs.is_empty() {
                    continue;
                }
                ann.emit(span, format_args!("typeof({:?}) = {}", local, ty_str));
                if !static_non_null_ptrs.is_empty() {
                    ann.emit(
                        span,
                        format_args!("  static NON_NULL: {:?}", static_non_null_ptrs),
                    );
                }
                if !dynamic_non_null_ptrs.is_empty() {
                    ann.emit(
                        span,
                        format_args!("  dynamic NON_NULL: {:?}", dynamic_non_null_ptrs),
                    );
                }
            }

            info.acx_data.set(acx.into_data());
        }

        let annotations = ann.finish();
        rewrite::apply_rewrites(tcx, Vec::new(), annotations, rewrite::UpdateFiles::No);
        return;
    }

    if env::var("C2RUST_ANALYZE_DEBUG_LAST_USE").is_ok() {
        let mut ann = AnnotationBuffer::new(tcx);
        debug_annotate_last_use(&gacx, &func_info, &all_fn_ldids, &mut ann);
        let annotations = ann.finish();
        let update_files = get_rewrite_mode(tcx, None);
        eprintln!("update mode = {:?}", update_files);
        rewrite::apply_rewrites(tcx, Vec::new(), annotations, update_files);
        eprintln!("finished writing last_use annotations - exiting");
        return;
    }

    if !rewrite_pointwise {
        run2(
            None,
            tcx,
            gacx,
            asn,
            &global_pointee_types,
            func_info,
            &all_fn_ldids,
            &fixed_defs,
            &known_perm_error_fns,
        );
    } else {
        for &ldid in &all_fn_ldids {
            run2(
                Some(ldid),
                tcx,
                gacx.clone(),
                asn.clone(),
                &global_pointee_types,
                func_info.clone(),
                &all_fn_ldids,
                &fixed_defs,
                &known_perm_error_fns,
            );
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn run2<'tcx>(
    pointwise_fn_ldid: Option<LocalDefId>,
    tcx: TyCtxt<'tcx>,
    mut gacx: GlobalAnalysisCtxt<'tcx>,
    mut asn: Assignment,
    global_pointee_types: &GlobalPointerTable<PointeeTypes<'tcx>>,
    mut func_info: HashMap<LocalDefId, FuncInfo<'tcx>>,
    all_fn_ldids: &Vec<LocalDefId>,
    fixed_defs: &HashSet<DefId>,
    known_perm_error_fns: &HashSet<DefId>,
) {
    // ----------------------------------
    // Generate rewrites
    // ----------------------------------

    // Regenerate region metadata, with hypothetical regions only in places where we intend to
    // introduce refs.
    gacx.construct_region_metadata_filtered(|lty| {
        let ptr = lty.label;
        if ptr.is_none() {
            return false;
        }
        let flags = asn.flags[ptr];
        if flags.contains(FlagSet::FIXED) {
            return false;
        }
        let perms = asn.perms[ptr];
        let desc = type_desc::perms_to_desc(lty.ty, perms, flags);
        match desc.own {
            Ownership::Imm | Ownership::Cell | Ownership::Mut => true,
            Ownership::Raw | Ownership::RawMut | Ownership::Rc | Ownership::Box => false,
        }
    });

    // For testing, putting #[c2rust_analyze_test::fail_before_rewriting] on a function marks it as
    // failed at this point.
    for &ldid in all_fn_ldids {
        let mut should_mark_failed = false;
        if util::has_test_attr(tcx, ldid, TestAttr::FailBeforeRewriting) {
            should_mark_failed = true;
        }
        if let Some(pointwise_fn_ldid) = pointwise_fn_ldid {
            // In pointwise mode, mark all functions except `pointwise_fn_ldid` as failed to
            // prevent rewriting.
            if ldid != pointwise_fn_ldid {
                should_mark_failed = true;
            }
        }
        if should_mark_failed {
            gacx.mark_fn_failed(
                ldid.to_def_id(),
                DontRewriteFnReason::FAKE_INVALID_FOR_TESTING,
                PanicDetail::new("explicit fail_before_rewriting for testing".to_owned()),
            );
        }
    }

    // Buffer debug output for each function.  Grouping together all the different types of info
    // for a single function makes FileCheck tests easier to write.
    let mut func_reports = HashMap::<LocalDefId, String>::new();

    // Buffer for annotations, which are inserted inline as comments when rewriting.
    let mut ann = AnnotationBuffer::new(tcx);

    // Generate rewrites for all functions.
    let mut all_rewrites = Vec::new();

    let mut manual_shim_casts = rewrite::ManualShimCasts::No;
    if let Ok(val) = env::var("C2RUST_ANALYZE_USE_MANUAL_SHIMS") {
        if val == "1" {
            manual_shim_casts = rewrite::ManualShimCasts::Yes;
        }
    }
    let manual_shim_casts = manual_shim_casts;

    // It may take multiple tries to reach a state where all rewrites succeed.
    for i in 0.. {
        assert!(i < 100);
        func_reports.clear();
        all_rewrites.clear();
        info!("--- start rewriting ---");

        // Update non-rewritten items first.  This has two purposes.  First, it clears the
        // `new_keys()` lists, which we check at the end of the loop to see whether we've reached a
        // fixpoint.  Second, doing this adds the `FIXED` flag to pointers that we shouldn't
        // rewrite, such as pointers in the signatures of non-rewritten functions.
        process_new_dont_rewrite_items(&mut gacx, &mut asn);

        for &ldid in all_fn_ldids {
            if gacx.dont_rewrite_fn(ldid.to_def_id()) {
                continue;
            }

            let info = func_info.get_mut(&ldid).unwrap();
            let ldid_const = WithOptConstParam::unknown(ldid);
            let name = tcx.item_name(ldid.to_def_id());
            let mir = tcx.mir_built(ldid_const);
            let mir = mir.borrow();
            let mut acx = gacx.function_context_with_data(&mir, info.acx_data.take());
            let pointee_types = global_pointee_types.and(info.local_pointee_types.get());

            let r = panic_detail::catch_unwind(AssertUnwindSafe(|| {
                if util::has_test_attr(tcx, ldid, TestAttr::SkipRewrite) {
                    return;
                }
                if fixed_defs.contains(&ldid.to_def_id()) {
                    return;
                }

                let hir_body_id = tcx.hir().body_owned_by(ldid);
                let expr_rewrites = rewrite::gen_expr_rewrites(
                    &mut acx,
                    &asn,
                    pointee_types,
                    &info.last_use,
                    ldid.to_def_id(),
                    &mir,
                    hir_body_id,
                );
                let ty_rewrites = rewrite::gen_ty_rewrites(&acx, &asn, pointee_types, &mir, ldid);
                // Print rewrites
                let report = func_reports.entry(ldid).or_default();
                writeln!(
                    report,
                    "generated {} expr rewrites + {} ty rewrites for {:?}:",
                    expr_rewrites.len(),
                    ty_rewrites.len(),
                    name
                )
                .unwrap();
                for &(span, ref rw) in expr_rewrites.iter().chain(ty_rewrites.iter()) {
                    writeln!(report, "  {}: {}", describe_span(tcx, span), rw).unwrap();
                }
                writeln!(report).unwrap();
                all_rewrites.extend(expr_rewrites);
                all_rewrites.extend(ty_rewrites);
            }));

            info.acx_data.set(acx.into_data());

            match r {
                Ok(()) => {}
                Err(pd) => {
                    gacx.mark_fn_failed(ldid.to_def_id(), DontRewriteFnReason::REWRITE_INVALID, pd);
                    continue;
                }
            }
        }

        // This call never panics, which is important because this is the fallback if the more
        // sophisticated analysis and rewriting above did panic.
        let (shim_call_rewrites, shim_fn_def_ids) = rewrite::gen_shim_call_rewrites(&gacx, &asn);
        all_rewrites.extend(shim_call_rewrites);

        // Generate shims for functions that need them.
        for def_id in shim_fn_def_ids {
            let r = panic_detail::catch_unwind(AssertUnwindSafe(|| {
                all_rewrites.push(rewrite::gen_shim_definition_rewrite(
                    &gacx,
                    &asn,
                    def_id,
                    manual_shim_casts,
                ));
            }));
            match r {
                Ok(()) => {}
                Err(pd) => {
                    gacx.mark_fn_failed(def_id, DontRewriteFnReason::SHIM_GENERATION_FAILED, pd);
                    continue;
                }
            }
        }

        // Exit the loop upon reaching a fixpoint.
        let any_new_dont_rewrite_keys = !gacx.dont_rewrite_fns.new_keys().is_empty()
            || !gacx.dont_rewrite_statics.new_keys().is_empty()
            || !gacx.dont_rewrite_fields.new_keys().is_empty();
        if !any_new_dont_rewrite_keys {
            break;
        }
    }

    // Generate rewrites for statics
    let mut static_rewrites = Vec::new();
    for (&def_id, &ptr) in gacx.addr_of_static.iter() {
        if fixed_defs.contains(&def_id) {
            continue;
        }
        static_rewrites.extend(rewrite::gen_static_rewrites(tcx, &asn, def_id, ptr));
    }
    let mut statics_report = String::new();
    writeln!(
        statics_report,
        "generated {} static rewrites:",
        static_rewrites.len()
    )
    .unwrap();
    for &(span, ref rw) in &static_rewrites {
        writeln!(
            statics_report,
            "    {}: {}",
            describe_span(gacx.tcx, span),
            rw
        )
        .unwrap();
    }
    all_rewrites.extend(static_rewrites);

    // Generate rewrites for ADTs
    let mut adt_reports = HashMap::<DefId, String>::new();
    for &def_id in gacx.adt_metadata.table.keys() {
        if gacx.foreign_mentioned_tys.contains(&def_id) {
            debug!("Avoiding rewrite for foreign-mentioned type: {def_id:?}");
            continue;
        }
        if fixed_defs.contains(&def_id) {
            continue;
        }

        let adt_rewrites = rewrite::gen_adt_ty_rewrites(&gacx, &asn, global_pointee_types, def_id);
        let report = adt_reports.entry(def_id).or_default();
        writeln!(
            report,
            "generated {} ADT rewrites for {:?}:",
            adt_rewrites.len(),
            def_id
        )
        .unwrap();
        for &(span, ref rw) in &adt_rewrites {
            writeln!(report, "    {}: {}", describe_span(gacx.tcx, span), rw).unwrap();
        }
        all_rewrites.extend(adt_rewrites);
    }

    // ----------------------------------
    // Print reports for tests and debugging
    // ----------------------------------

    // Print analysis results for each function in `all_fn_ldids`, going in declaration order.
    // Concretely, we iterate over `body_owners()`, which is a superset of `all_fn_ldids`, and
    // filter based on membership in `func_info`, which contains an entry for each ID in
    // `all_fn_ldids`.
    for ldid in tcx.hir().body_owners() {
        // Skip any body owners that aren't present in `func_info`, and also get the info itself.
        let info = match func_info.get_mut(&ldid) {
            Some(x) => x,
            None => continue,
        };

        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            continue;
        }

        let ldid_const = WithOptConstParam::unknown(ldid);
        let name = tcx.item_name(ldid.to_def_id());
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();
        let acx = gacx.function_context_with_data(&mir, info.acx_data.take());
        let pointee_types = global_pointee_types.and(info.local_pointee_types.get());

        // Print labeling and rewrites for the current function.

        debug!("\nfinal labeling for {:?}:", name);
        let lcx1 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
        let lcx2 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
        for (local, decl) in mir.local_decls.iter_enumerated() {
            print_labeling_for_var(
                lcx1,
                lcx2,
                format_args!("{:?} ({})", local, describe_local(tcx, decl)),
                acx.addr_of_local[local],
                acx.local_tys[local],
                asn.perms(),
                asn.flags(),
            );
        }

        debug!("\ntype assignment for {:?}:", name);
        rewrite::dump_rewritten_local_tys(&acx, &asn, pointee_types, &mir, describe_local);

        if let Some(report) = func_reports.remove(&ldid) {
            debug!("{}", report);
        }

        info.acx_data.set(acx.into_data());
    }

    // Generate annotations for all functions.
    for ldid in tcx.hir().body_owners() {
        // Skip any body owners that aren't present in `func_info`, and also get the info itself.
        let info = match func_info.get_mut(&ldid) {
            Some(x) => x,
            None => continue,
        };

        if !info.acx_data.is_set() {
            continue;
        }

        let ldid_const = WithOptConstParam::unknown(ldid);
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();
        let acx = gacx.function_context_with_data(&mir, info.acx_data.take());

        let mut emit_lty_annotations = |span, lty: LTy, desc: &str| {
            let mut ptrs = Vec::new();
            let ty_str = context::print_ty_with_pointer_labels(lty, |ptr| {
                if ptr.is_none() {
                    return String::new();
                }
                ptrs.push(ptr);
                format!("{{{}}}", ptr)
            });
            if ptrs.is_empty() {
                return;
            }
            // TODO: emit pointee_types when nontrivial
            ann.emit(span, format_args!("typeof({}) = {}", desc, ty_str));
            for ptr in ptrs {
                ann.emit(
                    span,
                    format_args!("  {} = {:?}, {:?}", ptr, asn.perms()[ptr], asn.flags()[ptr]),
                );
            }
        };

        // Generate inline annotations for pointer-typed locals
        for (local, decl) in mir.local_decls.iter_enumerated() {
            let span = local_span(decl);
            // TODO: emit addr_of when it's nontrivial
            let desc = format!("{:?}", local);
            emit_lty_annotations(span, acx.local_tys[local], &desc);
        }

        for (&loc, &rv_lty) in &acx.rvalue_tys {
            // `loc` must refer to a statement.  Terminators don't have `Rvalue`s and thus never
            // appear in `rvalue_tys`.
            let stmt = mir.stmt_at(loc).either(|stmt| stmt, |_term| unreachable!());
            let span = stmt.source_info.span;
            emit_lty_annotations(span, rv_lty, &format!("{:?}", stmt));
        }

        info.acx_data.set(acx.into_data());
    }

    // Annotate begin/end of each def.  This is used by extract_working_defs.py to locate defs that
    // were rewritten successfully.
    if env::var("C2RUST_ANALYZE_ANNOTATE_DEF_SPANS").as_deref() == Ok("1") {
        for ldid in tcx.hir_crate_items(()).definitions() {
            let span = tcx.source_span(ldid);
            ann.emit(span.shrink_to_lo(), format_args!("start of def {ldid:?}"));
            ann.emit(span.shrink_to_hi(), format_args!("end of def {ldid:?}"));
        }
    }

    // Print results for `static` items.
    debug!("\nfinal labeling for static items:");
    let lcx1 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
    let lcx2 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
    let mut static_dids = gacx.static_tys.keys().cloned().collect::<Vec<_>>();
    static_dids.sort();
    for did in static_dids {
        let lty = gacx.static_tys[&did];
        let name = tcx.item_name(did);
        print_labeling_for_var(
            lcx1,
            lcx2,
            format_args!("{name:?}"),
            gacx.addr_of_static[&did],
            lty,
            &asn.perms,
            &asn.flags,
        );
    }
    debug!("\n{statics_report}");

    // Print results for ADTs and fields
    debug!("\nfinal labeling for fields:");
    let mut field_dids = gacx.field_ltys.keys().cloned().collect::<Vec<_>>();
    field_dids.sort();
    for did in field_dids {
        let field_lty = gacx.field_ltys[&did];
        let name = tcx.item_name(did);
        let pid = field_lty.label;
        if pid != PointerId::NONE {
            let ty_perms = asn.perms[pid];
            let ty_flags = asn.flags[pid];
            debug!("{name:}: ({pid}) perms = {ty_perms:?}, flags = {ty_flags:?}");
        }

        // Emit annotations for fields
        let span = match tcx.def_ident_span(did) {
            Some(x) => x,
            None => {
                warn!("field {:?} has no def_ident_span to annotate", did);
                continue;
            }
        };
        let mut ptrs = Vec::new();
        let ty_str = context::print_ty_with_pointer_labels(field_lty, |ptr| {
            if ptr.is_none() {
                return String::new();
            }
            ptrs.push(ptr);
            format!("{{{}}}", ptr)
        });
        if ptrs.is_empty() {
            continue;
        }
        ann.emit(span, format_args!("typeof({}) = {}", name, ty_str));
        for ptr in ptrs {
            ann.emit(
                span,
                format_args!("  {} = {:?}, {:?}", ptr, asn.perms[ptr], asn.flags[ptr]),
            );
        }
    }

    let mut adt_dids = gacx.adt_metadata.table.keys().cloned().collect::<Vec<_>>();
    adt_dids.sort();
    for did in adt_dids {
        if let Some(report) = adt_reports.remove(&did) {
            debug!("\n{}", report);
        }
    }

    // ----------------------------------
    // Apply rewrites
    // ----------------------------------

    let annotations = ann.finish();

    // Apply rewrite to all functions at once.
    let update_files = get_rewrite_mode(tcx, pointwise_fn_ldid);
    rewrite::apply_rewrites(tcx, all_rewrites, annotations, update_files);

    // ----------------------------------
    // Report caught panics
    // ----------------------------------

    // Report errors that were caught previously
    debug!("\nerror details:");
    for ldid in tcx.hir().body_owners() {
        if let Some(detail) = gacx.fns_failed.get(&ldid.to_def_id()) {
            if !detail.has_backtrace() {
                continue;
            }
            debug!("\nerror in {:?}:{}", ldid, detail.to_string_full());
        }
    }

    debug!("\nerror summary:");
    fn sorted_def_ids(it: impl IntoIterator<Item = DefId>) -> Vec<DefId> {
        let mut v = it.into_iter().collect::<Vec<_>>();
        v.sort();
        v
    }
    for def_id in sorted_def_ids(gacx.dont_rewrite_fns.keys()) {
        let opt_detail = gacx.fns_failed.get(&def_id);
        let flags = gacx.dont_rewrite_fns.get(def_id);
        assert!(opt_detail.is_some() || !flags.is_empty());
        let detail_str = match opt_detail {
            Some(detail) => detail.to_string_short(),
            None => "(no panic)".into(),
        };
        debug!("analysis of {def_id:?} failed: {flags:?}, {detail_str}");
    }

    for def_id in sorted_def_ids(gacx.dont_rewrite_statics.keys()) {
        let flags = gacx.dont_rewrite_statics.get(def_id);
        debug!("analysis of {def_id:?} failed: {flags:?}");
    }

    for def_id in sorted_def_ids(gacx.dont_rewrite_fields.keys()) {
        let flags = gacx.dont_rewrite_fields.get(def_id);
        debug!("analysis of {def_id:?} failed: {flags:?}");
    }

    info!(
        "\nsaw errors in {} / {} functions",
        gacx.fns_failed.len(),
        all_fn_ldids.len()
    );

    if !known_perm_error_fns.is_empty() {
        info!(
            "saw permission errors in {} known fns",
            known_perm_error_fns.len()
        );
    }
}

fn assign_pointer_ids<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    func_info: &mut HashMap<LocalDefId, FuncInfo<'tcx>>,
    all_fn_ldids: &[LocalDefId],
) {
    let tcx = gacx.tcx;

    // Global items: functions

    // Assign global `PointerId`s for all pointers that appear in function signatures.
    for &ldid in all_fn_ldids {
        let sig = tcx.fn_sig(ldid.to_def_id());
        let sig = tcx.erase_late_bound_regions(sig);

        // All function signatures are fully annotated.
        let inputs = sig
            .inputs()
            .iter()
            .map(|&ty| gacx.assign_pointer_ids_with_info(ty, PointerInfo::ANNOTATED))
            .collect::<Vec<_>>();
        let inputs = gacx.lcx.mk_slice(&inputs);
        let output = gacx.assign_pointer_ids_with_info(sig.output(), PointerInfo::ANNOTATED);
        let c_variadic = sig.c_variadic;

        let lsig = LFnSig {
            inputs,
            output,
            c_variadic,
        };
        gacx.fn_sigs.insert(ldid.to_def_id(), lsig);
    }

    // Foreign function signatures
    for did in tcx
        .hir_crate_items(())
        .foreign_items()
        .map(|item| item.def_id.to_def_id())
        .filter(|did| matches!(tcx.def_kind(did), DefKind::Fn | DefKind::AssocFn))
    {
        let sig = tcx.erase_late_bound_regions(tcx.fn_sig(did));
        let inputs = sig
            .inputs()
            .iter()
            .map(|&ty| gacx.assign_pointer_ids_with_info(ty, PointerInfo::ANNOTATED))
            .collect::<Vec<_>>();

        let inputs = gacx.lcx.mk_slice(&inputs);
        let output = gacx.assign_pointer_ids_with_info(sig.output(), PointerInfo::ANNOTATED);
        let c_variadic = sig.c_variadic;

        let lsig = LFnSig {
            inputs,
            output,
            c_variadic,
        };
        gacx.fn_sigs.insert(did, lsig);
    }

    // Global items: statics

    // Collect all `static` items.
    let all_static_dids = all_static_items(tcx);
    debug!("statics:");
    for &did in &all_static_dids {
        debug!("  {:?}", did);
    }

    // Assign global `PointerId`s for types of `static` items.
    assert!(gacx.static_tys.is_empty());
    gacx.static_tys = HashMap::with_capacity(all_static_dids.len());
    for &did in &all_static_dids {
        gacx.assign_pointer_to_static(did);
    }

    // Global items: ADTs

    // Label the field types of each struct.
    for ldid in tcx.hir_crate_items(()).definitions() {
        let did = ldid.to_def_id();
        use DefKind::*;
        if !matches!(tcx.def_kind(did), Struct | Enum | Union) {
            continue;
        }
        gacx.assign_pointer_to_fields(did);
    }

    // Local variables

    let mut next_base = gacx.ptr_info().next_index();
    eprintln!("global pointerid range: {} .. {}", 0, next_base);
    for &ldid in all_fn_ldids {
        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            continue;
        }

        let ldid_const = WithOptConstParam::unknown(ldid);
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();
        let lsig = *gacx.fn_sigs.get(&ldid.to_def_id()).unwrap();

        let mut acx = gacx.function_context(&mir, next_base);

        let r = panic_detail::catch_unwind(AssertUnwindSafe(|| {
            // Assign PointerIds to local types
            assert!(acx.local_tys.is_empty());
            acx.local_tys = IndexVec::with_capacity(mir.local_decls.len());
            for (local, decl) in mir.local_decls.iter_enumerated() {
                // TODO: set PointerInfo::ANNOTATED for the parts of the type with user annotations
                let lty = match mir.local_kind(local) {
                    LocalKind::Var | LocalKind::Temp => acx.assign_pointer_ids(decl.ty),
                    LocalKind::Arg
                        if lsig.c_variadic && local.as_usize() - 1 == lsig.inputs.len() =>
                    {
                        // This is the hidden VaList<'a> argument at the end
                        // of the argument list of a variadic function. It does not
                        // appear in lsig.inputs, so we handle it separately here.
                        acx.assign_pointer_ids(decl.ty)
                    }
                    LocalKind::Arg => {
                        debug_assert!(local.as_usize() >= 1 && local.as_usize() <= mir.arg_count);
                        lsig.inputs[local.as_usize() - 1]
                    }
                    LocalKind::ReturnPointer => lsig.output,
                };
                let l = acx.local_tys.push(lty);
                assert_eq!(local, l);

                let ptr = acx.new_pointer(PointerInfo::ADDR_OF_LOCAL);
                let l = acx.addr_of_local.push(ptr);
                assert_eq!(local, l);
            }

            label_rvalue_tys(&mut acx, &mir);
            update_pointer_info(&mut acx, &mir);
        }));

        next_base = acx.local_ptr_info().next_index();
        eprintln!(
            "local pointerid range: {} .. {}",
            acx.local_ptr_base(),
            next_base
        );

        let mut info = FuncInfo::default();
        info.acx_data.set(acx.into_data());
        func_info.insert(ldid, info);

        match r {
            Ok(()) => {}
            Err(pd) => {
                gacx.mark_fn_failed(
                    ldid.to_def_id(),
                    DontRewriteFnReason::MISC_ANALYSIS_INVALID,
                    pd,
                );
                continue;
            }
        }
    }

    gacx.set_num_total_pointers(next_base as usize);
}

pub trait AssignPointerIds<'tcx> {
    fn lcx(&self) -> LTyCtxt<'tcx>;

    fn new_pointer(&mut self, info: PointerInfo) -> PointerId;

    fn assign_pointer_ids(&mut self, ty: Ty<'tcx>) -> LTy<'tcx> {
        self.assign_pointer_ids_with_info(ty, PointerInfo::empty())
    }

    fn assign_pointer_ids_with_info(
        &mut self,
        ty: Ty<'tcx>,
        base_ptr_info: PointerInfo,
    ) -> LTy<'tcx> {
        self.lcx().label(ty, &mut |ty| match ty.kind() {
            TyKind::Ref(_, _, _) => self.new_pointer(base_ptr_info | PointerInfo::REF),
            TyKind::RawPtr(_) => self.new_pointer(base_ptr_info),
            _ => PointerId::NONE,
        })
    }
}

impl<'tcx> AssignPointerIds<'tcx> for GlobalAnalysisCtxt<'tcx> {
    fn lcx(&self) -> LTyCtxt<'tcx> {
        self.lcx
    }

    fn new_pointer(&mut self, info: PointerInfo) -> PointerId {
        self.new_pointer(info)
    }
}

impl<'tcx> AssignPointerIds<'tcx> for AnalysisCtxt<'_, 'tcx> {
    fn lcx(&self) -> LTyCtxt<'tcx> {
        self.lcx()
    }

    fn new_pointer(&mut self, info: PointerInfo) -> PointerId {
        self.new_pointer(info)
    }
}

/// Run the `recent_writes` analysis, which computes the most recent write to each MIR local at
/// each program point.  This can then be used to reconstruct the expression that's currently
/// stored in the local.  For example, we use this to detect whether the size argument of `memcpy`
/// is `mem::size_of::<T>()` for some `T`.
fn do_recent_writes<'tcx>(
    gacx: &GlobalAnalysisCtxt<'tcx>,
    func_info: &mut HashMap<LocalDefId, FuncInfo<'tcx>>,
    all_fn_ldids: &[LocalDefId],
) {
    let tcx = gacx.tcx;
    for &ldid in all_fn_ldids {
        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            continue;
        }

        let ldid_const = WithOptConstParam::unknown(ldid);
        let info = func_info.get_mut(&ldid).unwrap();
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();

        // This is very straightforward because it doesn't need an `AnalysisCtxt` and never fails.
        info.recent_writes.set(RecentWrites::new(&mir));
    }
}

fn do_last_use<'tcx>(
    gacx: &GlobalAnalysisCtxt<'tcx>,
    func_info: &mut HashMap<LocalDefId, FuncInfo<'tcx>>,
    all_fn_ldids: &[LocalDefId],
) {
    let tcx = gacx.tcx;
    for &ldid in all_fn_ldids {
        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            continue;
        }

        let ldid_const = WithOptConstParam::unknown(ldid);
        let info = func_info.get_mut(&ldid).unwrap();
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();

        // This is very straightforward because it doesn't need an `AnalysisCtxt` and never fails.
        info.last_use.set(last_use::calc_last_use(&mir));
    }
}

fn debug_annotate_last_use<'tcx>(
    gacx: &GlobalAnalysisCtxt<'tcx>,
    func_info: &HashMap<LocalDefId, FuncInfo<'tcx>>,
    all_fn_ldids: &[LocalDefId],
    ann: &mut AnnotationBuffer,
) {
    let tcx = gacx.tcx;
    for &ldid in all_fn_ldids {
        let ldid_const = WithOptConstParam::unknown(ldid);
        let info = match func_info.get(&ldid) {
            Some(x) => x,
            None => continue,
        };
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();

        if !info.last_use.is_set() {
            continue;
        }
        let last_use = info.last_use.get();
        let mut last_use = last_use.iter().collect::<Vec<_>>();
        last_use.sort();
        for (loc, which, local) in last_use {
            let span = mir
                .stmt_at(loc)
                .either(|stmt| stmt.source_info.span, |term| term.source_info.span);
            ann.emit(
                span,
                format!(
                    "{which:?}: last use of {} {local:?} ({})",
                    if mir.local_kind(local) == LocalKind::Temp {
                        "temporary"
                    } else {
                        "local"
                    },
                    describe_local(tcx, &mir.local_decls[local]),
                ),
            );
        }
    }
}

/// Run the `pointee_type` analysis, which tries to determine the actual type of data that each
/// pointer can point to.  This is particularly important for `void*` pointers, which are typically
/// cast to a different type before use.
fn do_pointee_type<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    func_info: &mut HashMap<LocalDefId, FuncInfo<'tcx>>,
    all_fn_ldids: &[LocalDefId],
) -> GlobalPointerTable<PointeeTypes<'tcx>> {
    let tcx = gacx.tcx;
    let mut global_pointee_types =
        GlobalPointerTable::<PointeeTypes>::new(gacx.num_global_pointers());
    let mut pointee_vars = pointee_type::VarTable::default();

    let skip_pointee = get_skip_pointee_defs().unwrap();

    for &ldid in all_fn_ldids {
        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            continue;
        }

        let ldid_const = WithOptConstParam::unknown(ldid);
        let info = func_info.get_mut(&ldid).unwrap();
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();
        let acx = gacx.function_context_with_data(&mir, info.acx_data.take());

        let r = if !skip_pointee.contains(&ldid.to_def_id()) {
            panic_detail::catch_unwind(AssertUnwindSafe(|| {
                pointee_type::generate_constraints(&acx, &mir, &mut pointee_vars)
            }))
        } else {
            // For defs in the skip_pointee set, build an empty constraint set.
            Ok(pointee_type::ConstraintSet::default())
        };

        let local_pointee_types = LocalPointerTable::new(acx.local_ptr_base(), acx.num_pointers());
        info.acx_data.set(acx.into_data());

        match r {
            Ok(pointee_constraints) => {
                info.pointee_constraints.set(pointee_constraints);
            }
            Err(pd) => {
                gacx.mark_fn_failed(ldid.to_def_id(), DontRewriteFnReason::POINTEE_INVALID, pd);
                assert!(gacx.fn_analysis_invalid(ldid.to_def_id()));
            }
        }

        info.local_pointee_types.set(local_pointee_types);
    }

    // Iterate pointee constraints to a fixpoint.
    let mut loop_count = 0;
    loop {
        // Loop until the global assignment reaches a fixpoint.  The inner loop also runs until a
        // fixpoint, but it only considers a single function at a time.  The inner loop for one
        // function can affect other functions by updating `global_pointee_types`, so we also need
        // the outer loop, which runs until the global type sets converge as well.
        loop_count += 1;
        // We shouldn't need more iterations than the longest acyclic path through the callgraph.
        assert!(loop_count <= 1000);
        let old_global_pointee_types = global_pointee_types.clone();

        for &ldid in all_fn_ldids {
            if gacx.fn_analysis_invalid(ldid.to_def_id()) {
                continue;
            }

            let info = func_info.get_mut(&ldid).unwrap();

            let pointee_constraints = info.pointee_constraints.get();
            let pointee_types = global_pointee_types.and_mut(info.local_pointee_types.get_mut());
            pointee_type::solve_constraints(pointee_constraints, &pointee_vars, pointee_types);
        }

        if global_pointee_types == old_global_pointee_types {
            break;
        }
    }

    global_pointee_types
}

fn debug_print_pointee_types<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    func_info: &mut HashMap<LocalDefId, FuncInfo<'tcx>>,
    all_fn_ldids: &[LocalDefId],
    global_pointee_types: &GlobalPointerTable<PointeeTypes<'tcx>>,
) {
    let tcx = gacx.tcx;
    // Print results for debugging
    for &ldid in all_fn_ldids {
        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            continue;
        }

        let ldid_const = WithOptConstParam::unknown(ldid);
        let info = func_info.get_mut(&ldid).unwrap();
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();

        let acx = gacx.function_context_with_data(&mir, info.acx_data.take());
        let name = tcx.item_name(ldid.to_def_id());
        let pointee_types = global_pointee_types.and(info.local_pointee_types.get());
        print_function_pointee_types(&acx, name, &mir, pointee_types);

        info.acx_data.set(acx.into_data());
    }
}

/// Compute equivalence constraints.  This builds local and global equivalence sets, which map each
/// pointer to an equivalence-class representative.
fn build_equiv_constraints<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    func_info: &mut HashMap<LocalDefId, FuncInfo<'tcx>>,
    all_fn_ldids: &[LocalDefId],
) -> GlobalEquivSet {
    let tcx = gacx.tcx;

    let mut global_equiv = GlobalEquivSet::new(gacx.num_global_pointers());
    for &ldid in all_fn_ldids {
        let info = func_info.get_mut(&ldid).unwrap();
        let mut local_equiv =
            LocalEquivSet::new(info.acx_data.local_ptr_base(), info.acx_data.num_pointers());

        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            // Even on failure, we set a blank `local_equiv`.  This is necessary because we apply
            // renumbering to all functions, even those where analysis has failed.
            info.local_equiv.set(local_equiv);
            continue;
        }

        let ldid_const = WithOptConstParam::unknown(ldid);
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();

        let acx = gacx.function_context_with_data(&mir, info.acx_data.take());
        let recent_writes = info.recent_writes.get();

        // Compute local equivalence classes and dataflow constraints.
        let r = panic_detail::catch_unwind(AssertUnwindSafe(|| {
            dataflow::generate_equiv_constraints(&acx, &mir, recent_writes)
        }));
        match r {
            Ok(equiv_constraints) => {
                let mut equiv = global_equiv.and_mut(&mut local_equiv);
                for (a, b) in equiv_constraints {
                    equiv.unify(a, b);
                }
            }
            Err(pd) => {
                acx.gacx.mark_fn_failed(
                    ldid.to_def_id(),
                    DontRewriteFnReason::DATAFLOW_INVALID,
                    pd,
                );
            }
        };

        info.acx_data.set(acx.into_data());
        info.local_equiv.set(local_equiv);
    }

    global_equiv
}

/// Compute dataflow constraints.  This doesn't try to solve the dataflow constraints yet.  This
/// function doesn't return anything because there are no global dataflow constraints; all dataflow
/// constraints are function-local and are stored in that function's `FuncInfo`.
fn build_dataflow_constraints<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    func_info: &mut HashMap<LocalDefId, FuncInfo<'tcx>>,
    all_fn_ldids: &[LocalDefId],
    global_pointee_types: &GlobalPointerTable<PointeeTypes<'tcx>>,
) {
    let tcx = gacx.tcx;

    for &ldid in all_fn_ldids {
        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            continue;
        }

        let ldid_const = WithOptConstParam::unknown(ldid);
        let info = func_info.get_mut(&ldid).unwrap();
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();

        let acx = gacx.function_context_with_data(&mir, info.acx_data.take());
        let recent_writes = info.recent_writes.get();
        let pointee_types = global_pointee_types.and(info.local_pointee_types.get());

        // Compute local equivalence classes and dataflow constraints.
        let r = panic_detail::catch_unwind(AssertUnwindSafe(|| {
            dataflow::generate_constraints(&acx, &mir, recent_writes, pointee_types)
        }));
        match r {
            Ok(dataflow) => {
                info.dataflow.set(dataflow);
            }
            Err(pd) => {
                acx.gacx.mark_fn_failed(
                    ldid.to_def_id(),
                    DontRewriteFnReason::DATAFLOW_INVALID,
                    pd,
                );
            }
        };

        info.acx_data.set(acx.into_data());
    }
}

fn make_ty_fixed(asn: &mut Assignment, lty: LTy) {
    for lty in lty.iter() {
        let ptr = lty.label;
        if !ptr.is_none() {
            asn.flags[ptr].insert(FlagSet::FIXED);
        }
    }
}

fn make_sig_fixed(asn: &mut Assignment, lsig: &LFnSig) {
    for lty in lsig.inputs.iter().copied().chain(iter::once(lsig.output)) {
        make_ty_fixed(asn, lty);
    }
}

/// For testing, putting #[c2rust_analyze_test::fail_before_analysis] on a function marks it as
/// failed at this point.
fn apply_test_attr_fail_before_analysis(
    gacx: &mut GlobalAnalysisCtxt,
    all_fn_ldids: &[LocalDefId],
) {
    let tcx = gacx.tcx;
    for &ldid in all_fn_ldids {
        if !util::has_test_attr(tcx, ldid, TestAttr::FailBeforeAnalysis) {
            continue;
        }
        gacx.mark_fn_failed(
            ldid.to_def_id(),
            DontRewriteFnReason::FAKE_INVALID_FOR_TESTING,
            PanicDetail::new("explicit fail_before_analysis for testing".to_owned()),
        );
    }
}

/// For testing, putting #[c2rust_analyze_test::force_non_null_args] on a function marks its
/// arguments as `NON_NULL` and also adds `NON_NULL` to the `updates_forbidden` mask.
fn apply_test_attr_force_non_null_args(
    gacx: &mut GlobalAnalysisCtxt,
    all_fn_ldids: &[LocalDefId],
    asn: &mut Assignment,
    updates_forbidden: &mut GlobalPointerTable<PermissionSet>,
) {
    let tcx = gacx.tcx;
    for &ldid in all_fn_ldids {
        if !util::has_test_attr(tcx, ldid, TestAttr::ForceNonNullArgs) {
            continue;
        }

        let lsig = &gacx.fn_sigs[&ldid.to_def_id()];
        for arg_lty in lsig.inputs {
            for lty in arg_lty.iter() {
                let ptr = lty.label;
                if !ptr.is_none() {
                    asn.perms_mut()[ptr].insert(PermissionSet::NON_NULL);
                    updates_forbidden[ptr].insert(PermissionSet::NON_NULL);
                }
            }
        }
    }
}

fn pdg_update_permissions<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    all_fn_ldids: &[LocalDefId],
    func_info: &mut HashMap<LocalDefId, FuncInfo<'tcx>>,
    asn: &mut Assignment,
    updates_forbidden: &mut GlobalPointerTable<PermissionSet>,
    skip_borrowck_everywhere: bool,
    pdg_file_path: impl AsRef<Path>,
) {
    let allow_unsound =
        env::var("C2RUST_ANALYZE_PDG_ALLOW_UNSOUND").map_or(false, |val| &val == "1");

    pdg_update_permissions_with_callback(
        gacx,
        all_fn_ldids,
        func_info,
        asn,
        updates_forbidden,
        pdg_file_path,
        |asn, updates_forbidden, _ldid, ptr, _ptr_is_global, node_info, node_is_non_null| {
            let old_perms = asn.perms()[ptr];
            let mut perms = old_perms;
            if !node_is_non_null {
                perms.remove(PermissionSet::NON_NULL);
            } else if allow_unsound {
                perms.insert(PermissionSet::NON_NULL);
                // Unsound update: if we have never seen a NULL for
                // this local in the PDG, prevent the static analysis
                // from changing that permission.
                updates_forbidden[ptr].insert(PermissionSet::NON_NULL);
            }

            if let Some(node_info) = node_info {
                if node_info.flows_to.load.is_some() {
                    perms.insert(PermissionSet::READ);
                }
                if node_info.flows_to.store.is_some() {
                    perms.insert(PermissionSet::WRITE);
                }
                if node_info.flows_to.pos_offset.is_some() {
                    perms.insert(PermissionSet::OFFSET_ADD);
                }
                if node_info.flows_to.neg_offset.is_some() {
                    perms.insert(PermissionSet::OFFSET_SUB);
                }
                if !node_info.unique && !skip_borrowck_everywhere {
                    perms.remove(PermissionSet::UNIQUE);
                }
            }

            if perms != old_perms {
                let added = perms & !old_perms;
                let removed = old_perms & !perms;
                let kept = old_perms & perms;
                eprintln!(
                    "pdg: changed {:?}: added {:?}, removed {:?}, kept {:?}",
                    ptr, added, removed, kept
                );

                asn.perms_mut()[ptr] = perms;
            }
        },
    );
}

/// Load PDG from `pdg_file_path` and update permissions.
///
/// Each time a pointer's permissions are changed, this function calls `callback(ptr, old, new)`
/// where `ptr` is the `PointerId` in question, `old` is the old `PermissionSet`, and `new` is the
/// new one.
fn pdg_update_permissions_with_callback<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    all_fn_ldids: &[LocalDefId],
    func_info: &mut HashMap<LocalDefId, FuncInfo<'tcx>>,
    asn: &mut Assignment,
    updates_forbidden: &mut GlobalPointerTable<PermissionSet>,
    pdg_file_path: impl AsRef<Path>,
    mut callback: impl FnMut(
        &mut Assignment,
        &mut GlobalPointerTable<PermissionSet>,
        LocalDefId,
        PointerId,
        bool,
        Option<&NodeInfo>,
        bool,
    ),
) {
    let tcx = gacx.tcx;

    let f = std::fs::File::open(pdg_file_path).unwrap();
    let graphs: Graphs = bincode::deserialize_from(f).unwrap();

    let mut known_nulls = HashSet::new();
    for g in &graphs.graphs {
        for n in &g.nodes {
            let dest_pl = match n.dest.as_ref() {
                Some(x) => x,
                None => {
                    continue;
                }
            };
            if !dest_pl.projection.is_empty() {
                continue;
            }
            let dest = dest_pl.local;
            let dest = Local::from_u32(dest.index);
            if g.is_null {
                known_nulls.insert((n.function.id, dest));
            }
        }
    }

    let mut func_def_path_hash_to_ldid = HashMap::new();
    for &ldid in all_fn_ldids {
        let def_path_hash: (u64, u64) = tcx.def_path_hash(ldid.to_def_id()).0.as_value();
        eprintln!("def_path_hash {:?} = {:?}", def_path_hash, ldid);
        func_def_path_hash_to_ldid.insert(def_path_hash, ldid);
    }

    for g in &graphs.graphs {
        for n in &g.nodes {
            let def_path_hash: (u64, u64) = n.function.id.0.into();
            let ldid = match func_def_path_hash_to_ldid.get(&def_path_hash) {
                Some(&x) => x,
                None => {
                    eprintln!(
                        "pdg: unknown DefPathHash {:?} for function {:?}",
                        n.function.id, n.function.name
                    );
                    continue;
                }
            };
            let info = func_info.get_mut(&ldid).unwrap();
            let ldid_const = WithOptConstParam::unknown(ldid);
            let mir = tcx.mir_built(ldid_const);
            let mir = mir.borrow();
            let acx = gacx.function_context_with_data(&mir, info.acx_data.take());

            let dest_pl = match n.dest.as_ref() {
                Some(x) => x,
                None => {
                    info.acx_data.set(acx.into_data());
                    continue;
                }
            };
            if !dest_pl.projection.is_empty() {
                info.acx_data.set(acx.into_data());
                continue;
            }
            let dest = dest_pl.local;
            let dest = Local::from_u32(dest.index);

            if acx.local_tys.get(dest).is_none() {
                eprintln!(
                    "pdg: {}: local {:?} appears as dest, but is out of bounds",
                    n.function.name, dest
                );
                info.acx_data.set(acx.into_data());
                continue;
            }
            let ptr = match acx.ptr_of(dest) {
                Some(x) => x,
                None => {
                    eprintln!(
                        "pdg: {}: local {:?} appears as dest, but has no PointerId",
                        n.function.name, dest
                    );
                    info.acx_data.set(acx.into_data());
                    continue;
                }
            };

            let ptr_is_global = acx.ptr_is_global(ptr);
            callback(
                asn,
                updates_forbidden,
                ldid,
                ptr,
                ptr_is_global,
                n.info.as_ref(),
                !known_nulls.contains(&(n.function.id, dest)),
            );

            info.acx_data.set(acx.into_data());
        }
    }
}

fn local_span(decl: &LocalDecl) -> Span {
    let mut span = decl.source_info.span;
    if let Some(ref info) = decl.local_info {
        if let LocalInfo::User(ref binding_form) = **info {
            let binding_form = binding_form.as_ref().assert_crate_local();
            if let BindingForm::Var(ref v) = *binding_form {
                span = v.pat_span;
            }
        }
    }
    span
}

fn describe_local(tcx: TyCtxt, decl: &LocalDecl) -> String {
    let span = local_span(decl);
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
    debug!("{}: addr_of = {:?}, type = {:?}", desc, addr_of1, ty1);

    let addr_of2 = flags[addr_of_ptr];
    let ty2 = lcx2.relabel(lty, &mut |lty| {
        if lty.label == PointerId::NONE {
            FlagSet::empty()
        } else {
            flags[lty.label]
        }
    });
    debug!(
        "{}: addr_of flags = {:?}, type flags = {:?}",
        desc, addr_of2, ty2
    );

    let addr_of3 = addr_of_ptr;
    let ty3 = lty;
    debug!("{}: addr_of = {:?}, type = {:?}", desc, addr_of3, ty3);
}

fn print_function_pointee_types<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    name: impl Display,
    mir: &Body<'tcx>,
    pointee_types: PointerTable<PointeeTypes<'tcx>>,
) {
    debug!("\npointee types for {}", name);
    for (local, decl) in mir.local_decls.iter_enumerated() {
        debug!(
            "{:?} ({}): addr_of = {:?}, type = {:?}",
            local,
            describe_local(acx.tcx(), decl),
            acx.addr_of_local[local],
            acx.local_tys[local]
        );

        let mut all_pointer_ids = Vec::new();
        if !acx.addr_of_local[local].is_none() {
            all_pointer_ids.push(acx.addr_of_local[local]);
        }
        acx.local_tys[local].for_each_label(&mut |ptr| {
            if !ptr.is_none() {
                all_pointer_ids.push(ptr);
            }
        });

        for ptr in all_pointer_ids {
            let tys = &pointee_types[ptr];
            if tys.tys.is_empty() {
                continue;
            }
            debug!("  pointer {:?}: {:?}", ptr, tys.tys);
        }
    }
}

/// Return `LocalDefId`s for all `static`s.
fn all_static_items(tcx: TyCtxt) -> Vec<DefId> {
    let mut order = Vec::new();

    for root_ldid in tcx.hir_crate_items(()).definitions() {
        match tcx.def_kind(root_ldid) {
            DefKind::Static(_) => {}
            _ => continue,
        }
        order.push(root_ldid.to_def_id())
    }

    order
}

fn is_impl_clone(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    let clone_trait_def_id = match tcx.lang_items().clone_trait() {
        Some(def_id) => def_id,
        None => return false,
    };
    if let Some(impl_def_id) = tcx.impl_of_method(def_id) {
        if let Some(trait_ref) = tcx.impl_trait_ref(impl_def_id) {
            return trait_ref.def_id == clone_trait_def_id;
        }
    }
    false
}

/// Return all `LocalDefId`s for all `fn`s that are `body_owners`, ordered according to a postorder
/// traversal of the graph of references between bodies.  Also returns the callgraph itself, in the
/// form of a map from callee `LocalDefId` to a set of caller `LocalDefId`s.
pub(super) fn fn_body_owners_postorder(tcx: TyCtxt) -> Vec<LocalDefId> {
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
            DefKind::Fn | DefKind::AssocFn => {
                if is_impl_clone(tcx, root_ldid.to_def_id()) {
                    continue;
                }
            }
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

/// Call `f` for each field mentioned in a place projection within the body of `ldid`.
fn for_each_field_use(tcx: TyCtxt, ldid: LocalDefId, f: impl FnMut(DefId)) {
    let ldid_const = WithOptConstParam::unknown(ldid);
    let mir = tcx.mir_built(ldid_const);
    let mir = mir.borrow();
    let mir: &Body = &mir;

    struct FieldUseVisitor<'a, 'tcx, F> {
        tcx: TyCtxt<'tcx>,
        mir: &'a Body<'tcx>,
        f: F,
    }

    impl<'tcx, F: FnMut(DefId)> Visitor<'tcx> for FieldUseVisitor<'_, 'tcx, F> {
        fn visit_place(
            &mut self,
            place: &Place<'tcx>,
            _context: PlaceContext,
            _location: Location,
        ) {
            for (i, elem) in place.projection.iter().enumerate() {
                let field_idx = match elem {
                    PlaceElem::Field(x, _) => x,
                    _ => continue,
                };
                // Build a `PlaceRef` with all the projections up to, but not including, `elem`.
                let place_ref = PlaceRef {
                    local: place.local,
                    projection: &place.projection[..i],
                };
                let adt_pty = place_ref.ty(self.mir, self.tcx);
                let adt_def = match adt_pty.ty.ty_adt_def() {
                    Some(x) => x,
                    // `PlaceElem::Field` also works on tuple types, which don't have an `AdtDef`.
                    None => continue,
                };
                let variant_def = match adt_pty.variant_index {
                    None => adt_def.non_enum_variant(),
                    Some(i) => adt_def.variant(i),
                };
                let field_def = &variant_def.fields[field_idx.index()];
                (self.f)(field_def.did);
            }
        }
    }

    FieldUseVisitor { tcx, mir, f }.visit_body(mir);
}

/// Populate `gacx.field_users` and `gacx.fn_fields_used`.
fn populate_field_users(gacx: &mut GlobalAnalysisCtxt, fn_ldids: &[LocalDefId]) {
    let mut field_users = HashMap::new();
    let mut seen = HashSet::new();

    for &ldid in fn_ldids {
        let mut fn_fields = Vec::new();
        let mut fn_seen = HashSet::new();
        for_each_field_use(gacx.tcx, ldid, |field_def_id| {
            if let Some(field_ldid) = field_def_id.as_local() {
                if seen.insert((field_ldid, ldid)) {
                    field_users
                        .entry(field_ldid)
                        .or_insert_with(Vec::new)
                        .push(ldid);
                }

                if fn_seen.insert(field_ldid) {
                    fn_fields.push(field_ldid);
                }
            }
        });
        gacx.fn_fields_used.insert(ldid, fn_fields);
    }

    for (k, v) in field_users {
        gacx.field_users.insert(k, v);
    }
}

/// Call `take_new_keys()` on `gacx.dont_rewrite_{fns,statics,fields}` and process the results.
/// This involves adding `FIXED` to some pointers and maybe propagating `DontRewrite` flags to
/// other items.
fn process_new_dont_rewrite_items(gacx: &mut GlobalAnalysisCtxt, asn: &mut Assignment) {
    for i in 0.. {
        assert!(i < 20);
        let mut found_any = false;

        for did in gacx.dont_rewrite_fns.take_new_keys() {
            if gacx.force_rewrite.contains(&did) {
                eprintln!("process_new_dont_rewrite_items: mark sig of {did:?} fixed: {:?} - IGNORED due to force_rewrite", gacx.dont_rewrite_fns.get(did));
                continue;
            }
            found_any = true;
            eprintln!(
                "process_new_dont_rewrite_items: mark sig of {did:?} fixed: {:?}",
                gacx.dont_rewrite_fns.get(did)
            );
            let lsig = &gacx.fn_sigs[&did];
            make_sig_fixed(asn, lsig);

            let ldid = match did.as_local() {
                Some(x) => x,
                None => continue,
            };

            for &field_ldid in gacx.fn_fields_used.get(ldid) {
                if gacx.force_rewrite.contains(&field_ldid.to_def_id()) {
                    eprintln!("process_new_dont_rewrite_items: mark field {field_ldid:?} fixed: user {did:?} is not rewritten - IGNORED due to force_rewrite");
                    continue;
                }
                eprintln!("process_new_dont_rewrite_items: mark field {field_ldid:?} fixed: user {did:?} is not rewritten");
                gacx.dont_rewrite_fields.add(
                    field_ldid.to_def_id(),
                    DontRewriteFieldReason::NON_REWRITTEN_USE,
                );
            }

            // TODO: callers/callees
        }

        for did in gacx.dont_rewrite_statics.take_new_keys() {
            if gacx.force_rewrite.contains(&did) {
                eprintln!("process_new_dont_rewrite_items: mark static {did:?} fixed: {:?} - IGNORED due to force_rewrite", gacx.dont_rewrite_statics.get(did));
                continue;
            }
            found_any = true;
            eprintln!(
                "process_new_dont_rewrite_items: mark static {did:?} fixed: {:?}",
                gacx.dont_rewrite_statics.get(did)
            );
            let lty = gacx.static_tys[&did];
            make_ty_fixed(asn, lty);
        }

        for did in gacx.dont_rewrite_fields.take_new_keys() {
            if gacx.force_rewrite.contains(&did) {
                eprintln!("process_new_dont_rewrite_items: mark field {did:?} fixed: {:?} - IGNORED due to force_rewrite", gacx.dont_rewrite_fields.get(did));
                continue;
            }
            found_any = true;
            eprintln!(
                "process_new_dont_rewrite_items: mark field {did:?} fixed: {:?}",
                gacx.dont_rewrite_fields.get(did)
            );
            let lty = gacx.field_ltys[&did];
            make_ty_fixed(asn, lty);
        }

        // The previous steps can cause more items to become non-rewritten.  Keep going until
        // there's no more work to do.
        if !found_any {
            break;
        }
    }
}

pub struct AnalysisCallbacks;

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
