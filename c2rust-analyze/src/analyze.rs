use crate::annotate::AnnotationBuffer;
use crate::borrowck;
use crate::context::{
    self, AnalysisCtxt, AnalysisCtxtData, DontRewriteFieldReason, DontRewriteFnReason,
    DontRewriteStaticReason, FlagSet, GlobalAnalysisCtxt, GlobalAssignment, LFnSig, LTy, LTyCtxt,
    LocalAssignment, PermissionSet, PointerId, PointerInfo,
};
use crate::dataflow;
use crate::dataflow::DataflowConstraints;
use crate::equiv::GlobalEquivSet;
use crate::equiv::LocalEquivSet;
use crate::labeled_ty::LabeledTyCtxt;
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
use ::log::warn;
use c2rust_pdg::graph::Graphs;
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

            eprintln!(
                "update_pointer_info: visit assignment: {:?}[{}]: {:?}",
                bb, i, stmt
            );

            if !pl.is_indirect() {
                // This is a write directly to `pl.local`.
                *write_count.entry(pl.local).or_insert(0) += 1;
                eprintln!("  record write to LHS {:?}", pl);
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
                    eprintln!("  record write to ref target {:?}", ref_pl);
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

pub(super) fn gather_foreign_sigs<'tcx>(gacx: &mut GlobalAnalysisCtxt<'tcx>, tcx: TyCtxt<'tcx>) {
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
        let lsig = LFnSig { inputs, output };
        gacx.fn_sigs.insert(did, lsig);
    }
}

fn mark_foreign_fixed<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    gasn: &mut GlobalAssignment,
    tcx: TyCtxt<'tcx>,
) {
    // FIX the inputs and outputs of function declarations in extern blocks
    for (did, lsig) in gacx.fn_sigs.iter() {
        if tcx.is_foreign_item(did) {
            make_sig_fixed(gasn, lsig);
        }
    }

    // FIX the types of static declarations in extern blocks
    for (did, lty) in gacx.static_tys.iter() {
        if tcx.is_foreign_item(did) {
            make_ty_fixed(gasn, lty);

            // Also fix the `addr_of_static` permissions.
            let ptr = gacx.addr_of_static[&did];
            gasn.flags[ptr].insert(FlagSet::FIXED);
        }
    }

    // FIX the fields of structs mentioned in extern blocks
    for adt_did in &gacx.adt_metadata.struct_dids {
        if gacx.foreign_mentioned_tys.contains(adt_did) {
            let adt_def = tcx.adt_def(adt_did);
            let fields = adt_def.all_fields();
            for field in fields {
                let field_lty = gacx.field_ltys[&field.did];
                eprintln!(
                    "adding FIXED permission for {adt_did:?} field {:?}",
                    field.did
                );
                make_ty_fixed(gasn, field_lty);
            }
        }
    }
}

fn mark_all_statics_fixed<'tcx>(gacx: &mut GlobalAnalysisCtxt<'tcx>, gasn: &mut GlobalAssignment) {
    for (did, lty) in gacx.static_tys.iter() {
        make_ty_fixed(gasn, lty);

        // Also fix the `addr_of_static` permissions.
        let ptr = gacx.addr_of_static[&did];
        gasn.flags[ptr].insert(FlagSet::FIXED);
    }
}

fn mark_all_structs_fixed<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    gasn: &mut GlobalAssignment,
    tcx: TyCtxt<'tcx>,
) {
    for adt_did in &gacx.adt_metadata.struct_dids {
        let adt_def = tcx.adt_def(adt_did);
        let fields = adt_def.all_fields();
        for field in fields {
            let field_lty = gacx.field_ltys[&field.did];
            make_ty_fixed(gasn, field_lty);
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
    let s = s.strip_suffix(")").ok_or("does not end with `)`")?;
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
    if &rendered != s {
        return Err(format!(
            "path mismatch: after parsing input {}, obtained a different path {:?}",
            orig_s, def_id
        ));
    }

    Ok(def_id)
}

fn read_fixed_defs_list(fixed_defs: &mut HashSet<DefId>, path: &str) -> io::Result<()> {
    let f = BufReader::new(File::open(path)?);
    for (i, line) in f.lines().enumerate() {
        let line = line?;
        let line = line.trim();
        if line.len() == 0 || line.starts_with('#') {
            continue;
        }

        let def_id = parse_def_id(&line).unwrap_or_else(|e| {
            panic!("failed to parse {} line {}: {}", path, i + 1, e);
        });
        fixed_defs.insert(def_id);
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
        .filter(|prefix| prefix.len() > 0)
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
        read_fixed_defs_list(&mut fixed_defs, &path)?;
    }
    if let Ok(prefixes) = env::var("C2RUST_ANALYZE_REWRITE_PATHS") {
        check_rewrite_path_prefixes(tcx, &mut fixed_defs, &prefixes);
    }
    Ok(fixed_defs)
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
    /// Local part of the permission/flag assignment.  Combine with the [`GlobalAssignment`] to
    /// get a complete [`Assignment`] for this function, which maps every [`PointerId`] in this
    /// function to a [`PermissionSet`] and [`FlagSet`].
    lasn: MaybeUnset<LocalAssignment>,
    /// Local part of the `updates_forbidden` mask.
    l_updates_forbidden: MaybeUnset<LocalPointerTable<PermissionSet>>,
    /// Constraints on pointee types gathered from the body of this function.
    pointee_constraints: MaybeUnset<pointee_type::ConstraintSet<'tcx>>,
    /// Local part of pointee type sets.
    local_pointee_types: MaybeUnset<LocalPointerTable<PointeeTypes<'tcx>>>,
    /// Table for looking up the most recent write to a given local.
    recent_writes: MaybeUnset<RecentWrites>,
}

fn run(tcx: TyCtxt) {
    eprintln!("all defs:");
    for ldid in tcx.hir_crate_items(()).definitions() {
        eprintln!("{:?}", ldid);
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
    eprintln!("callgraph traversal order:");
    for &ldid in &all_fn_ldids {
        eprintln!("  {:?}", ldid);
    }

    populate_field_users(&mut gacx, &all_fn_ldids);

    // ----------------------------------
    // Label all global types
    // ----------------------------------

    // Assign global `PointerId`s for all pointers that appear in function signatures.
    for &ldid in &all_fn_ldids {
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

        let lsig = LFnSig { inputs, output };
        gacx.fn_sigs.insert(ldid.to_def_id(), lsig);
    }

    gather_foreign_sigs(&mut gacx, tcx);

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

    // Compute hypothetical region data for all ADTs and functions.  This can only be done after
    // all field types are labeled.
    gacx.construct_region_metadata();

    // ----------------------------------
    // Infer pointee types
    // ----------------------------------

    for &ldid in &all_fn_ldids {
        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            continue;
        }

        let ldid_const = WithOptConstParam::unknown(ldid);
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();
        let lsig = *gacx.fn_sigs.get(&ldid.to_def_id()).unwrap();

        let mut acx = gacx.function_context(&mir);

        let r = panic_detail::catch_unwind(AssertUnwindSafe(|| {
            // Assign PointerIds to local types
            assert!(acx.local_tys.is_empty());
            acx.local_tys = IndexVec::with_capacity(mir.local_decls.len());
            for (local, decl) in mir.local_decls.iter_enumerated() {
                // TODO: set PointerInfo::ANNOTATED for the parts of the type with user annotations
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

                let ptr = acx.new_pointer(PointerInfo::empty());
                let l = acx.addr_of_local.push(ptr);
                assert_eq!(local, l);
            }

            label_rvalue_tys(&mut acx, &mir);
            update_pointer_info(&mut acx, &mir);

            pointee_type::generate_constraints(&acx, &mir)
        }));

        let mut info = FuncInfo::default();
        let local_pointee_types = LocalPointerTable::new(acx.num_pointers());
        info.acx_data.set(acx.into_data());

        match r {
            Ok(pointee_constraints) => {
                info.pointee_constraints.set(pointee_constraints);
            }
            Err(pd) => {
                gacx.mark_fn_failed(ldid.to_def_id(), DontRewriteFnReason::POINTEE_INVALID, pd);
            }
        }

        info.local_pointee_types.set(local_pointee_types);
        info.recent_writes.set(RecentWrites::new(&mir));
        func_info.insert(ldid, info);
    }

    // Iterate pointee constraints to a fixpoint.
    let mut global_pointee_types = GlobalPointerTable::<PointeeTypes>::new(gacx.num_pointers());
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

        // Clear the `incomplete` flags for all global pointers.  See comment in
        // `pointee_types::solve::solve_constraints`.
        for (_, tys) in global_pointee_types.iter_mut() {
            tys.incomplete = false;
        }

        for &ldid in &all_fn_ldids {
            if gacx.fn_analysis_invalid(ldid.to_def_id()) {
                continue;
            }

            let info = func_info.get_mut(&ldid).unwrap();

            let pointee_constraints = info.pointee_constraints.get();
            let pointee_types = global_pointee_types.and_mut(info.local_pointee_types.get_mut());
            pointee_type::solve_constraints(pointee_constraints, pointee_types);
        }

        if global_pointee_types == old_global_pointee_types {
            break;
        }
    }

    // Print results for debugging
    for &ldid in &all_fn_ldids {
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

    // ----------------------------------
    // Compute dataflow constraints
    // ----------------------------------

    // Initial pass to assign local `PointerId`s and gather equivalence constraints, which state
    // that two pointer types must be converted to the same reference type.  Some additional data
    // computed during this the process is kept around for use in later passes.
    let mut global_equiv = GlobalEquivSet::new(gacx.num_pointers());
    for &ldid in &all_fn_ldids {
        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            continue;
        }

        let info = func_info.get_mut(&ldid).unwrap();
        let ldid_const = WithOptConstParam::unknown(ldid);
        let mir = tcx.mir_built(ldid_const);
        let mir = mir.borrow();

        let acx = gacx.function_context_with_data(&mir, info.acx_data.take());
        let recent_writes = info.recent_writes.get();
        let pointee_types = global_pointee_types.and(info.local_pointee_types.get());

        let r = panic_detail::catch_unwind(AssertUnwindSafe(|| {
            dataflow::generate_constraints(&acx, &mir, recent_writes, pointee_types)
        }));

        let (dataflow, equiv_constraints) = match r {
            Ok(x) => x,
            Err(pd) => {
                info.acx_data.set(acx.into_data());
                gacx.mark_fn_failed(ldid.to_def_id(), DontRewriteFnReason::DATAFLOW_INVALID, pd);
                continue;
            }
        };

        // Compute local equivalence classes and dataflow constraints.
        let mut local_equiv = LocalEquivSet::new(acx.num_pointers());
        let mut equiv = global_equiv.and_mut(&mut local_equiv);
        for (a, b) in equiv_constraints {
            equiv.unify(a, b);
        }

        info.acx_data.set(acx.into_data());
        info.dataflow.set(dataflow);
        info.local_equiv.set(local_equiv);
    }

    // ----------------------------------
    // Remap `PointerId`s by equivalence class
    // ----------------------------------

    // Remap pointers based on equivalence classes, so all members of an equivalence class now use
    // the same `PointerId`.
    let (global_counter, global_equiv_map) = global_equiv.renumber();
    eprintln!("global_equiv_map = {global_equiv_map:?}");
    pointee_type::remap_pointers_global(
        &mut global_pointee_types,
        &global_equiv_map,
        &global_counter,
    );
    gacx.remap_pointers(&global_equiv_map, global_counter);

    for &ldid in &all_fn_ldids {
        if gacx.fn_analysis_invalid(ldid.to_def_id()) {
            continue;
        }

        let info = func_info.get_mut(&ldid).unwrap();
        let (local_counter, local_equiv_map) = info.local_equiv.renumber(&global_equiv_map);
        eprintln!("local_equiv_map = {local_equiv_map:?}");
        pointee_type::remap_pointers_local(
            &mut global_pointee_types,
            &mut info.local_pointee_types,
            global_equiv_map.and(&local_equiv_map),
            &local_counter,
        );
        info.acx_data.remap_pointers(
            &mut gacx,
            global_equiv_map.and(&local_equiv_map),
            local_counter,
        );
        info.dataflow
            .remap_pointers(global_equiv_map.and(&local_equiv_map));
        info.local_equiv.clear();
    }

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

    const INITIAL_PERMS: PermissionSet =
        PermissionSet::union_all([PermissionSet::UNIQUE, PermissionSet::NON_NULL]);
    const INITIAL_FLAGS: FlagSet = FlagSet::empty();

    let mut gasn = GlobalAssignment::new(gacx.num_pointers(), INITIAL_PERMS, INITIAL_FLAGS);
    let mut g_updates_forbidden = GlobalPointerTable::new(gacx.num_pointers());

    for (ptr, &info) in gacx.ptr_info().iter() {
        if should_make_fixed(info) {
            gasn.flags[ptr].insert(FlagSet::FIXED);
        }
    }

    mark_foreign_fixed(&mut gacx, &mut gasn, tcx);

    if rewrite_pointwise {
        // In pointwise mode, we restrict rewriting to a single fn at a time.  All statics and
        // struct fields are marked `FIXED` so they won't be rewritten.
        mark_all_statics_fixed(&mut gacx, &mut gasn);
        mark_all_structs_fixed(&mut gacx, &mut gasn, tcx);
    }

    for (ptr, perms) in gacx.known_fn_ptr_perms() {
        let existing_perms = &mut gasn.perms[ptr];
        existing_perms.remove(INITIAL_PERMS);
        assert_eq!(*existing_perms, PermissionSet::empty());
        *existing_perms = perms;
    }

    for info in func_info.values_mut() {
        let num_pointers = info.acx_data.num_pointers();
        let mut lasn = LocalAssignment::new(num_pointers, INITIAL_PERMS, INITIAL_FLAGS);
        let l_updates_forbidden = LocalPointerTable::new(num_pointers);

        for (ptr, &info) in info.acx_data.local_ptr_info().iter() {
            if should_make_fixed(info) {
                lasn.flags[ptr].insert(FlagSet::FIXED);
            }
        }

        info.lasn.set(lasn);
        info.l_updates_forbidden.set(l_updates_forbidden);
    }

    // Load permission info from PDG
    let mut func_def_path_hash_to_ldid = HashMap::new();
    for &ldid in &all_fn_ldids {
        let def_path_hash: (u64, u64) = tcx.def_path_hash(ldid.to_def_id()).0.as_value();
        eprintln!("def_path_hash {:?} = {:?}", def_path_hash, ldid);
        func_def_path_hash_to_ldid.insert(def_path_hash, ldid);
    }

    if let Some(pdg_file_path) = std::env::var_os("PDG_FILE") {
        let f = std::fs::File::open(pdg_file_path).unwrap();
        let graphs: Graphs = bincode::deserialize_from(f).unwrap();

        for g in &graphs.graphs {
            for n in &g.nodes {
                let def_path_hash: (u64, u64) = n.function.id.0.into();
                let ldid = match func_def_path_hash_to_ldid.get(&def_path_hash) {
                    Some(&x) => x,
                    None => {
                        panic!(
                            "pdg: unknown DefPathHash {:?} for function {:?}",
                            n.function.id, n.function.name
                        );
                    }
                };
                let info = func_info.get_mut(&ldid).unwrap();
                let ldid_const = WithOptConstParam::unknown(ldid);
                let mir = tcx.mir_built(ldid_const);
                let mir = mir.borrow();
                let acx = gacx.function_context_with_data(&mir, info.acx_data.take());
                let mut asn = gasn.and(&mut info.lasn);

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

                let node_info = match n.info.as_ref() {
                    Some(x) => x,
                    None => {
                        eprintln!(
                            "pdg: {}: node with dest {:?} is missing NodeInfo",
                            n.function.name, dest
                        );
                        info.acx_data.set(acx.into_data());
                        continue;
                    }
                };

                let old_perms = asn.perms()[ptr];
                let mut perms = old_perms;
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
                if !node_info.unique {
                    perms.remove(PermissionSet::UNIQUE);
                }
                // TODO: PermissionSet::NON_NULL

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

                info.acx_data.set(acx.into_data());
            }
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
                make_sig_fixed(&mut gasn, lsig);
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
                        make_ty_fixed(&mut gasn, lty);
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
                make_ty_fixed(&mut gasn, lty);

                let ptr = match gacx.addr_of_static.get(&ldid.to_def_id()) {
                    Some(&x) => x,
                    None => panic!("missing addr_of_static for {:?}", ldid),
                };
                if !ptr.is_none() {
                    gasn.flags[ptr].insert(FlagSet::FIXED);
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
    apply_test_attr_force_non_null_args(
        &mut gacx,
        &all_fn_ldids,
        &mut func_info,
        &mut gasn,
        &mut g_updates_forbidden,
    );

    eprintln!("=== ADT Metadata ===");
    eprintln!("{:?}", gacx.adt_metadata);

    let mut loop_count = 0;
    loop {
        // Loop until the global assignment reaches a fixpoint.  The inner loop also runs until a
        // fixpoint, but it only considers a single function at a time.  The inner loop for one
        // function can affect other functions by updating the `GlobalAssignment`, so we also need
        // the outer loop, which runs until the `GlobalAssignment` converges as well.
        loop_count += 1;
        let old_gasn = gasn.clone();

        for &ldid in &all_fn_ldids {
            if gacx.fn_analysis_invalid(ldid.to_def_id()) {
                continue;
            }

            let info = func_info.get_mut(&ldid).unwrap();
            let ldid_const = WithOptConstParam::unknown(ldid);
            let name = tcx.item_name(ldid.to_def_id());
            let mir = tcx.mir_built(ldid_const);
            let mir = mir.borrow();

            let field_ltys = gacx.field_ltys.clone();
            let acx = gacx.function_context_with_data(&mir, info.acx_data.take());
            let mut asn = gasn.and(&mut info.lasn);
            let updates_forbidden = g_updates_forbidden.and(&info.l_updates_forbidden);

            let r = panic_detail::catch_unwind(AssertUnwindSafe(|| {
                // `dataflow.propagate` and `borrowck_mir` both run until the assignment converges
                // on a fixpoint, so there's no need to do multiple iterations here.
                info.dataflow
                    .propagate(&mut asn.perms_mut(), &updates_forbidden);

                borrowck::borrowck_mir(
                    &acx,
                    &info.dataflow,
                    &mut asn.perms_mut(),
                    &updates_forbidden,
                    name.as_str(),
                    &mir,
                    field_ltys,
                );
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
        for (ptr, &old) in old_gasn.perms.iter() {
            let new = gasn.perms[ptr];
            if old != new {
                let added = new & !old;
                let removed = old & !new;
                let kept = old & new;
                eprintln!(
                    "changed {:?}: added {:?}, removed {:?}, kept {:?}",
                    ptr, added, removed, kept
                );
                num_changed += 1;
            }
        }
        eprintln!(
            "iteration {}: {} global pointers changed",
            loop_count, num_changed
        );

        if gasn == old_gasn {
            break;
        }
    }
    eprintln!("reached fixpoint in {} iterations", loop_count);

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
        let mut asn = gasn.and(&mut info.lasn);

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
        if gasn.perms[ptr] != perms {
            known_perm_error_ptrs.insert(ptr);
            warn!(
                "known permissions changed for PointerId {ptr:?}: {perms:?} -> {:?}",
                gasn.perms[ptr]
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

    if !rewrite_pointwise {
        run2(
            None,
            tcx,
            gacx,
            gasn,
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
                gasn.clone(),
                &global_pointee_types,
                func_info.clone(),
                &all_fn_ldids,
                &fixed_defs,
                &known_perm_error_fns,
            );
        }
    }
}

fn run2<'tcx>(
    pointwise_fn_ldid: Option<LocalDefId>,
    tcx: TyCtxt<'tcx>,
    mut gacx: GlobalAnalysisCtxt<'tcx>,
    mut gasn: GlobalAssignment,
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
        let flags = gasn.flags[ptr];
        if flags.contains(FlagSet::FIXED) {
            return false;
        }
        let perms = gasn.perms[ptr];
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
        eprintln!("\n--- start rewriting ---");

        // Update non-rewritten items first.  This has two purposes.  First, it clears the
        // `new_keys()` lists, which we check at the end of the loop to see whether we've reached a
        // fixpoint.  Second, doing this adds the `FIXED` flag to pointers that we shouldn't
        // rewrite, such as pointers in the signatures of non-rewritten functions.
        process_new_dont_rewrite_items(&mut gacx, &mut gasn);

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
            let asn = gasn.and(&mut info.lasn);
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
        let (shim_call_rewrites, shim_fn_def_ids) = rewrite::gen_shim_call_rewrites(&gacx, &gasn);
        all_rewrites.extend(shim_call_rewrites);

        // Generate shims for functions that need them.
        for def_id in shim_fn_def_ids {
            let r = panic_detail::catch_unwind(AssertUnwindSafe(|| {
                all_rewrites.push(rewrite::gen_shim_definition_rewrite(
                    &gacx,
                    &gasn,
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
        static_rewrites.extend(rewrite::gen_static_rewrites(tcx, &gasn, def_id, ptr));
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
            eprintln!("Avoiding rewrite for foreign-mentioned type: {def_id:?}");
            continue;
        }
        if fixed_defs.contains(&def_id) {
            continue;
        }

        let adt_rewrites =
            rewrite::gen_adt_ty_rewrites(&gacx, &gasn, &global_pointee_types, def_id);
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
        let asn = gasn.and(&mut info.lasn);
        let pointee_types = global_pointee_types.and(info.local_pointee_types.get());

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
        rewrite::dump_rewritten_local_tys(&acx, &asn, pointee_types, &mir, describe_local);

        eprintln!();
        if let Some(report) = func_reports.remove(&ldid) {
            eprintln!("{}", report);
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
        let asn = gasn.and(&mut info.lasn);

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
            if ptrs.is_empty() {
                continue;
            }
            // TODO: emit addr_of when it's nontrivial
            // TODO: emit pointee_types when nontrivial
            ann.emit(span, format_args!("typeof({:?}) = {}", local, ty_str));
            for ptr in ptrs {
                ann.emit(
                    span,
                    format_args!("  {} = {:?}, {:?}", ptr, asn.perms()[ptr], asn.flags()[ptr]),
                );
            }
        }

        info.acx_data.set(acx.into_data());
    }

    // Print results for `static` items.
    eprintln!("\nfinal labeling for static items:");
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
            &gasn.perms,
            &gasn.flags,
        );
    }
    eprintln!("\n{statics_report}");

    // Print results for ADTs and fields
    eprintln!("\nfinal labeling for fields:");
    let mut field_dids = gacx.field_ltys.keys().cloned().collect::<Vec<_>>();
    field_dids.sort();
    for did in field_dids {
        let field_lty = gacx.field_ltys[&did];
        let name = tcx.item_name(did);
        let pid = field_lty.label;
        if pid != PointerId::NONE {
            let ty_perms = gasn.perms[pid];
            let ty_flags = gasn.flags[pid];
            eprintln!("{name:}: ({pid}) perms = {ty_perms:?}, flags = {ty_flags:?}");
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
        if ptrs.len() == 0 {
            continue;
        }
        ann.emit(span, format_args!("typeof({}) = {}", name, ty_str));
        for ptr in ptrs {
            ann.emit(
                span,
                format_args!("  {} = {:?}, {:?}", ptr, gasn.perms[ptr], gasn.flags[ptr]),
            );
        }
    }

    let mut adt_dids = gacx.adt_metadata.table.keys().cloned().collect::<Vec<_>>();
    adt_dids.sort();
    for did in adt_dids {
        if let Some(report) = adt_reports.remove(&did) {
            eprintln!("\n{}", report);
        }
    }

    // ----------------------------------
    // Apply rewrites
    // ----------------------------------

    let annotations = ann.finish();

    // Apply rewrite to all functions at once.
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
    rewrite::apply_rewrites(tcx, all_rewrites, annotations, update_files);

    // ----------------------------------
    // Report caught panics
    // ----------------------------------

    // Report errors that were caught previously
    eprintln!("\nerror details:");
    for ldid in tcx.hir().body_owners() {
        if let Some(detail) = gacx.fns_failed.get(&ldid.to_def_id()) {
            if !detail.has_backtrace() {
                continue;
            }
            eprintln!("\nerror in {:?}:\n{}", ldid, detail.to_string_full());
        }
    }

    eprintln!("\nerror summary:");
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
        eprintln!("analysis of {def_id:?} failed: {flags:?}, {detail_str}");
    }

    for def_id in sorted_def_ids(gacx.dont_rewrite_statics.keys()) {
        let flags = gacx.dont_rewrite_statics.get(def_id);
        eprintln!("analysis of {def_id:?} failed: {flags:?}");
    }

    for def_id in sorted_def_ids(gacx.dont_rewrite_fields.keys()) {
        let flags = gacx.dont_rewrite_fields.get(def_id);
        eprintln!("analysis of {def_id:?} failed: {flags:?}");
    }

    eprintln!(
        "\nsaw errors in {} / {} functions",
        gacx.fns_failed.len(),
        all_fn_ldids.len()
    );

    if known_perm_error_fns.len() > 0 {
        eprintln!(
            "saw permission errors in {} known fns",
            known_perm_error_fns.len()
        );
    }
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

fn make_ty_fixed(gasn: &mut GlobalAssignment, lty: LTy) {
    for lty in lty.iter() {
        let ptr = lty.label;
        if !ptr.is_none() {
            gasn.flags[ptr].insert(FlagSet::FIXED);
        }
    }
}

fn make_sig_fixed(gasn: &mut GlobalAssignment, lsig: &LFnSig) {
    for lty in lsig.inputs.iter().copied().chain(iter::once(lsig.output)) {
        make_ty_fixed(gasn, lty);
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
    func_info: &mut HashMap<LocalDefId, FuncInfo>,
    gasn: &mut GlobalAssignment,
    g_updates_forbidden: &mut GlobalPointerTable<PermissionSet>,
) {
    let tcx = gacx.tcx;
    for &ldid in all_fn_ldids {
        if !util::has_test_attr(tcx, ldid, TestAttr::ForceNonNullArgs) {
            continue;
        }

        let info = func_info.get_mut(&ldid).unwrap();
        let mut asn = gasn.and(&mut info.lasn);
        let mut updates_forbidden = g_updates_forbidden.and_mut(&mut info.l_updates_forbidden);

        let lsig = &gacx.fn_sigs[&ldid.to_def_id()];
        for arg_lty in lsig.inputs.iter().copied() {
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

fn print_function_pointee_types<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    name: impl Display,
    mir: &Body<'tcx>,
    pointee_types: PointerTable<PointeeTypes<'tcx>>,
) {
    eprintln!("\npointee types for {}", name);
    for (local, decl) in mir.local_decls.iter_enumerated() {
        eprintln!(
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
            if tys.ltys.len() == 0 && !tys.incomplete {
                continue;
            }
            eprintln!(
                "  pointer {:?}: {:?}{}",
                ptr,
                tys.ltys,
                if tys.incomplete { " (INCOMPLETE)" } else { "" }
            );
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
fn process_new_dont_rewrite_items(gacx: &mut GlobalAnalysisCtxt, gasn: &mut GlobalAssignment) {
    for i in 0.. {
        assert!(i < 20);
        let mut found_any = false;

        for did in gacx.dont_rewrite_fns.take_new_keys() {
            found_any = true;
            let lsig = &gacx.fn_sigs[&did];
            make_sig_fixed(gasn, lsig);

            let ldid = match did.as_local() {
                Some(x) => x,
                None => continue,
            };

            for &field_ldid in gacx.fn_fields_used.get(ldid) {
                gacx.dont_rewrite_fields.add(
                    field_ldid.to_def_id(),
                    DontRewriteFieldReason::NON_REWRITTEN_USE,
                );
            }

            // TODO: callers/callees
        }

        for did in gacx.dont_rewrite_statics.take_new_keys() {
            found_any = true;
            let lty = gacx.static_tys[&did];
            make_ty_fixed(gasn, lty);
        }

        for did in gacx.dont_rewrite_fields.take_new_keys() {
            found_any = true;
            let lty = gacx.field_ltys[&did];
            make_ty_fixed(gasn, lty);
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
