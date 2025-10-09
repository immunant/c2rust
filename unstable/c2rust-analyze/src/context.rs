use crate::analyze::fn_body_owners_postorder;
use crate::analyze::AssignPointerIds;
use crate::borrowck::{AdtMetadata, FieldMetadata, OriginArg, OriginParam};
use crate::known_fn::{all_known_fns, KnownFn};
use crate::labeled_ty::{LabeledTy, LabeledTyCtxt};
use crate::panic_detail::PanicDetail;
use crate::pointer_id::{GlobalPointerTable, LocalPointerTable, PointerTable, PointerTableMut};
use crate::util::{self, describe_rvalue, PhantomLifetime, RvalueDesc};
use assert_matches::assert_matches;
use bitflags::bitflags;
use indexmap::IndexSet;
use log::*;
use rustc_ast::Mutability;
use rustc_hir::def::DefKind;
use rustc_hir::def_id::{DefId, LocalDefId};
use rustc_index::vec::IndexVec;
use rustc_middle::mir::interpret::{self, AllocId, ConstValue, GlobalAlloc};
use rustc_middle::mir::{
    Body, Constant, ConstantKind, Field, HasLocalDecls, Local, LocalDecls, Location, Operand,
    Place, PlaceElem, PlaceRef, Rvalue,
};
use rustc_middle::ty::tls;
use rustc_middle::ty::AdtDef;
use rustc_middle::ty::DefIdTree;
use rustc_middle::ty::FieldDef;
use rustc_middle::ty::GenericArgKind;
use rustc_middle::ty::GenericParamDefKind;
use rustc_middle::ty::Instance;
use rustc_middle::ty::RegionKind;
use rustc_middle::ty::Ty;
use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::TyKind;
use rustc_type_ir::RegionKind::{ReEarlyBound, ReStatic};
use std::collections::hash_map::{Entry, HashMap};
use std::collections::HashSet;
use std::fmt::{Debug, Write as _};
use std::hash::Hash;
use std::mem;
use std::ops::{BitOr, Index, Range};

bitflags! {
    /// Permissions are created such that we allow dropping permissions in any assignment.
    /// This means removing a permission from a pointer's [`PermissionSet`]
    /// must allow the pointer to take on more values, not restrict it to fewer values.
    ///
    /// That's why, for example [`UNIQUE`] is named as such,
    /// as opposed to something like `ALIASED` (a pointer capability),
    /// as removing [`UNIQUE`] (`&mut`) allows more values to be taken on (`&`).
    ///
    /// Currently, we assume that all pointers are valid or null
    /// (see [the `std::ptr` safety docs](https://doc.rust-lang.org/std/ptr/index.html#safety)).
    /// We do not yet (here) consider unaligned or cast-from-integer pointers.
    ///
    /// [`UNIQUE`]: Self::UNIQUE
    #[derive(Default)]
    pub struct PermissionSet: u16 {
        /// The value(s) accessible through this pointer can be read.
        const READ = 0x0001;

        /// The value(s) accessible through this pointer can be written.
        const WRITE = 0x0002;

        /// This pointer is unique: using an alias not derived from this
        /// pointer invalidates this pointer, after which it is not valid to use.
        const UNIQUE = 0x0004;

        /// This pointer is linear-typed.  Copying a `LINEAR` pointer to another `LINEAR` location
        /// moves the pointer and invalidates the source of the copy.  (However, a
        /// copy-and-downcast to a non-`LINEAR` location is a borrow, which does not invalidate the
        /// source pointer.)
        const LINEAR = 0x0008;

        /// This pointer can be offset in the positive direction.
        ///
        /// Offsetting the pointer in an unknown direction requires both `OFFSET_ADD` and
        /// `OFFSET_SUB`.  Offsetting by zero requires neither `OFFSET_ADD` nor `OFFSET_SUB`.
        const OFFSET_ADD = 0x0010;

        /// This pointer can be offset in the negative direction.
        const OFFSET_SUB = 0x0020;

        /// This pointer can be freed.
        const FREE = 0x0040;

        /// This pointer is non-null.
        ///
        /// [`NON_NULL`] is set (or not) when the pointer is created,
        /// and it flows forward along dataflow edges.
        ///
        /// The following should be set to [`NON_NULL`]:
        /// * the results of [`Rvalue::Ref`] and [`Rvalue::AddressOf`]
        /// * the result of a known function like [`_.offset`] that never returns null pointers
        ///
        /// The following should not be set to [`NON_NULL`]:
        /// * [`core::ptr::null`]
        /// * [`core::ptr::null_mut`]
        /// * [`core::ptr::from_exposed_addr`] with the constant `0`
        /// * `0 as * {const,mut} _`, a cast from the constant `0` to a pointer type
        /// * anything else
        ///
        /// Non-zero but invalid pointers, such as those produced by:
        /// * [`core::ptr::invalid`]
        /// * [`core::ptr::invalid_mut`]
        /// * [`NonNull::dangling`]
        /// * [`core::ptr::from_exposed_addr`]
        /// * int-to-ptr casts though `as` or [`core::mem::transmute`]
        ///
        /// will be [`NON_NULL`], but for now,
        /// we do not consider and do not allow such non-null invalid pointers at all.
        ///
        /// [`NON_NULL`] pointers will become references, e.x. `&T`.\
        /// Non-[`NON_NULL`] pointers will become [`Option<&T>`].
        ///
        /// Casts/transitions from [`NON_NULL`] to non-[`NON_NULL`] will become [`Some(_)`].\
        /// Casts/transitions from non-[`NON_NULL`] to [`NON_NULL`] will become [`_.unwrap()`].
        ///
        /// [`_.is_null()`] on a [`NON_NULL`] pointer will become [`false`].\
        /// [`_.is_null()`] on a non-[`NON_NULL`] pointer will become [`_.is_some()`].
        ///
        /// Constant null pointers, like those produced by:
        /// * [`core::ptr::null`]
        /// * [`core::ptr::null_mut`]
        /// * [`core::ptr::from_exposed_addr`] with the constant `0`
        /// * `0 as * {const,mut} _`, a cast from the constant `0` to a pointer type
        ///
        /// will become [`None`].
        ///
        /// [`NON_NULL`]: Self::NON_NULL
        /// [`READ`]: Self::READ
        /// [`WRITE`]: Self::WRITE
        /// [`_.offset`]: core::ptr::offset
        /// [`NonNull::dangling`]: core::ptr::NonNull::dangling
        /// [`Some(_)`]: Some
        /// [`_.unwrap()`]: Option::unwrap
        /// [`_.is_null()`]: core::ptr::is_null
        /// [`_.is_some()`]: Option::is_some
        const NON_NULL = 0x0080;

        /// This pointer points to the heap (or is null).
        ///
        /// `HEAP` and `STACK` form a four-element lattice.  `HEAP` means definitely heap, `STACK`
        /// means definitely stack, neither means it could be either (top), and both means the
        /// pointer is either definitely null or comes from an unknown source (bottom).
        const HEAP = 0x0100;

        /// This pointer points to the stack or to static memory (or is null).
        ///
        /// Currently we distinguish stack/static vs heap, but don't distinguish stack vs static.
        /// The reason is that heap pointers can be rewritten to `Box<T>`, but stack and static
        /// pointers both cannot.
        const STACK = 0x0200;
    }
}

impl PermissionSet {
    pub const NONE: Self = Self::empty();

    /// Mask representing all ways of using a pointer.  If a pointer's permissions don't contain
    /// any of the bits in this mask, then the pointer is unused.
    pub const USED: Self = Self::from_bits_truncate(
        Self::READ.bits
            | Self::WRITE.bits
            | Self::OFFSET_ADD.bits
            | Self::OFFSET_SUB.bits
            | Self::FREE.bits,
    );

    pub const fn union_all<const N: usize>(a: [Self; N]) -> Self {
        let mut this = Self::empty();
        let mut i = 0;
        while i < N {
            this = this.union(a[i]);
            i += 1;
        }
        this
    }

    /// The permissions for a (byte-)string literal.
    //
    // `union_all` is used here since it's a `const fn`, unlike `BitOr::bitor`.
    pub const STRING_LITERAL: Self = Self::union_all([Self::READ, Self::OFFSET_ADD, Self::STACK]);

    /// Negative permissions for a (byte-)string literal.  These permissions should be absent from
    /// all string literals, contrary to the defaults for most pointers.
    pub const STRING_LITERAL_NEGATIVE: Self = Self::union_all([Self::HEAP]);
}

bitflags! {
    /// Additional flags describing a given pointer type.  These are mainly derived from
    /// `PermissionSet`, but don't follow the normal subtyping rules and propagation algorithm.
    #[derive(Default)]
    pub struct FlagSet: u16 {
        /// The pointee type is wrapped in `Cell`.  This is tracked separately from the
        /// `PermissionSet` since it depends on the past/future uses of the pointer in an unusual
        /// way, and it can't be freely discarded (or its inverse freely added) as is the case for
        /// everything in `PermissionSet`.
        const CELL = 0x0001;

        /// This pointer's type is fixed; rewrites must not change it.  This is used for all safe
        /// references (which we assume already have the correct types), for raw pointers that
        /// cross an FFI boundary, and for arguments and return values of functions we can't
        /// rewrite.
        const FIXED = 0x0002;
    }
}

bitflags! {
    /// Flags indicating reasons why a function isn't being rewritten.
    #[derive(Default)]
    pub struct DontRewriteFnReason: u16 {
        /// The user requested that this function be left unchanged.
        const USER_REQUEST = 1 << 0;
        /// The function contains an unsupported int-to-pointer cast.
        const INT_TO_PTR_CAST = 1 << 1;
        /// The function calls an extern function that has no safe rewrite.
        const EXTERN_CALL = 1 << 2;
        /// The function calls another local function that isn't being rewritten.
        const NON_REWRITTEN_CALLEE = 1 << 3;
        /// The function uses `Cell` pointers whose types are too complex for the current rewrite
        /// rules.
        const COMPLEX_CELL = 1 << 4;
        /// The function contains a pointer-to-pointer cast that isn't covered by rewrite rules.
        const PTR_TO_PTR_CAST = 1 << 5;
        /// The function dereferences a pointer that would remain raw after rewriting.
        const RAW_PTR_DEREF = 1 << 6;
        /// Calling this function from non-rewritten code requires a shim, but shim generation
        /// failed.
        const SHIM_GENERATION_FAILED = 1 << 7;

        /// Pointee analysis results for this function are invalid.
        const POINTEE_INVALID = 1 << 10;
        /// Dataflow analysis results for this function are invalid.
        const DATAFLOW_INVALID = 1 << 11;
        /// Borrowcheck/Polonius analysis results for this function are invalid.
        const BORROWCK_INVALID = 1 << 12;
        /// Results of some other analysis for this function are invalid.
        const MISC_ANALYSIS_INVALID = 1 << 13;
        /// The set of rewrites generated for this function is invalid or incomplete.
        const REWRITE_INVALID = 1 << 14;
        /// Analysis results for this function are valid, but were marked as invalid anyway in
        /// order to test error recovery.
        const FAKE_INVALID_FOR_TESTING = 1 << 15;

        const ANALYSIS_INVALID_MASK = Self::POINTEE_INVALID.bits
            | Self::DATAFLOW_INVALID.bits
            | Self::BORROWCK_INVALID.bits
            | Self::MISC_ANALYSIS_INVALID.bits
            | Self::FAKE_INVALID_FOR_TESTING.bits;
    }
}

bitflags! {
    /// Flags indicating reasons why a static isn't being rewritten.
    #[derive(Default)]
    pub struct DontRewriteStaticReason: u16 {
        /// The user requested that this static be left unchanged.
        const USER_REQUEST = 0x0001;
        /// The static is used in a function that isn't being rewritten.
        const NON_REWRITTEN_USE = 0x0002;
    }
}

bitflags! {
    /// Flags indicating reasons why an ADT field isn't being rewritten.
    #[derive(Default)]
    pub struct DontRewriteFieldReason: u16 {
        /// The user requested that this field be left unchanged.
        const USER_REQUEST = 0x0001;
        /// The field is used in a function that isn't being rewritten.
        const NON_REWRITTEN_USE = 0x0002;
    }
}

pub use crate::pointer_id::PointerId;

pub type LTy<'tcx> = LabeledTy<'tcx, PointerId>;
pub type LTyCtxt<'tcx> = LabeledTyCtxt<'tcx, PointerId>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LFnSig<'tcx> {
    pub inputs: &'tcx [LTy<'tcx>],
    pub output: LTy<'tcx>,
    pub c_variadic: bool,
}

impl<'tcx> LFnSig<'tcx> {
    pub fn inputs_and_output(&self) -> impl Iterator<Item = LTy<'tcx>> {
        self.inputs.iter().copied().chain([self.output])
    }
}

bitflags! {
    /// Basic information about a pointer, computed with minimal analysis before running `dataflow`
    /// or `borrowck`.
    ///
    /// When `PointerId`s are merged into a single `PointerId` per equivalence class, the
    /// `PointerInfo` of each resulting `PointerId` is the union of the `PointerInfo`s of all the
    /// members of the class.  Thus, flags describing properties of the declaration that produced a
    /// given `PointerId` should be formulated as "at least one declaration has property X", as a
    /// single `PointerId` may correspond to several declarations.
    #[derive(Default)]
    pub struct PointerInfo: u16 {
        /// This `PointerId` was generated for a `TyKind::Ref`.
        const REF = 0x0001;

        /// At least one declaration that produced this `PointerId` used an explicit type
        /// annotation.
        const ANNOTATED = 0x0002;

        /// This `PointerId` has at least one local declaration that is not a temporary reference
        /// arising from an `&x` or `&mut x` expression in the source.
        const NOT_TEMPORARY_REF = 0x0004;

        /// This `PointerId` appeared as the `addr_of_local` `PointerId` for at least one local.
        const ADDR_OF_LOCAL = 0x0008;
    }
}

#[derive(Clone, Default)]
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
                let other_param_names = tcx.generics_of(k).params.iter().filter_map(|p| {
                    if !matches!(p.kind, GenericParamDefKind::Lifetime) {
                        Some(p.name.to_ident_string())
                    } else {
                        None
                    }
                });

                write!(f, "struct {:}", tcx.item_name(*k))?;
                if !adt.lifetime_params.is_empty() {
                    write!(f, "<")?;
                    let lifetime_params_str = adt
                        .lifetime_params
                        .iter()
                        .map(|p| format!("{:?}", p))
                        .chain(other_param_names)
                        .collect::<Vec<_>>()
                        .join(",");
                    write!(f, "{lifetime_params_str:}")?;
                    write!(f, ">")?;
                }
                writeln!(f, " {{")?;

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

#[derive(Clone)]
pub struct GlobalAnalysisCtxt<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub lcx: LTyCtxt<'tcx>,

    ptr_info: GlobalPointerTable<PointerInfo>,
    /// Total number of global and local pointers across all functions.
    num_total_pointers: usize,

    pub fn_sigs: HashMap<DefId, LFnSig<'tcx>>,
    pub fn_fields_used: MultiMap<LocalDefId, LocalDefId>,

    /// A map of all [`KnownFn`]s as determined by [`all_known_fns`].
    ///
    /// The key is the [`KnownFn`]'s [`name`],
    /// which is its symbol/link name in the binary.
    ///
    /// [`name`]: KnownFn::name
    known_fns: HashMap<&'static str, &'static KnownFn>,

    pub dont_rewrite_fns: FlagMap<DefId, DontRewriteFnReason>,
    pub dont_rewrite_statics: FlagMap<DefId, DontRewriteStaticReason>,
    pub dont_rewrite_fields: FlagMap<DefId, DontRewriteFieldReason>,
    /// Never mark these defs as `FIXED`, regardless of what `DontRewriteReason`s they might
    /// acquire.
    pub force_rewrite: HashSet<DefId>,

    /// `DefId`s of functions where analysis failed, and a [`PanicDetail`] explaining the reason
    /// for each failure.
    pub fns_failed: HashMap<DefId, PanicDetail>,

    pub field_ltys: HashMap<DefId, LTy<'tcx>>,
    pub field_users: MultiMap<LocalDefId, LocalDefId>,

    pub static_tys: HashMap<DefId, LTy<'tcx>>,
    pub addr_of_static: HashMap<DefId, PointerId>,

    pub adt_metadata: AdtMetadataTable<'tcx>,

    pub fn_origins: FnOriginMap<'tcx>,

    pub foreign_mentioned_tys: HashSet<DefId>,
}

pub struct AnalysisCtxt<'a, 'tcx> {
    pub gacx: &'a mut GlobalAnalysisCtxt<'tcx>,

    ptr_info: LocalPointerTable<PointerInfo>,

    pub local_decls: &'a LocalDecls<'tcx>,
    pub local_tys: IndexVec<Local, LTy<'tcx>>,
    pub addr_of_local: IndexVec<Local, PointerId>,
    /// Types for certain [`Rvalue`]s.  Some `Rvalue`s introduce fresh [`PointerId`]s; to keep
    /// those `PointerId`s consistent, the `Rvalue`'s type must be stored rather than recomputed on
    /// the fly.
    pub rvalue_tys: HashMap<Location, LTy<'tcx>>,

    /// [`Location`]s of (byte-)string literal [`rvalue_tys`](Self::rvalue_tys).
    pub string_literal_locs: Vec<Location>,
}

impl<'a, 'tcx> AnalysisCtxt<'_, 'tcx> {
    pub fn string_literal_tys(&'a self) -> impl Iterator<Item = LTy<'tcx>> + 'a {
        self.string_literal_locs
            .iter()
            .map(|loc| self.rvalue_tys[loc])
    }

    pub fn string_literal_perms(
        &'a self,
    ) -> impl Iterator<Item = (PointerId, PermissionSet, PermissionSet)> + PhantomLifetime<'tcx> + 'a
    {
        self.string_literal_tys().map(|lty| {
            (
                lty.label,
                PermissionSet::STRING_LITERAL,
                PermissionSet::STRING_LITERAL_NEGATIVE,
            )
        })
    }

    pub fn check_string_literal_perms(&self, asn: &Assignment) {
        for lty in self.string_literal_tys() {
            let ptr = lty.label;
            let expected_perms = PermissionSet::STRING_LITERAL;
            let mut actual_perms = asn.perms()[ptr];
            // Ignore `UNIQUE` and `NON_NULL` as they get automatically added to all permissions
            // and then removed later if it can't apply.  We don't care about `UNIQUE` or
            // `NON_NULL` for const refs, so just unset it here.
            actual_perms.set(PermissionSet::UNIQUE, false);
            actual_perms.set(PermissionSet::NON_NULL, false);
            assert_eq!(expected_perms, actual_perms);
        }
    }
}

#[derive(Clone)]
pub struct AnalysisCtxtData<'tcx> {
    ptr_info: LocalPointerTable<PointerInfo>,
    local_tys: IndexVec<Local, LTy<'tcx>>,
    addr_of_local: IndexVec<Local, PointerId>,
    rvalue_tys: HashMap<Location, LTy<'tcx>>,
    string_literal_locs: Vec<Location>,
}

#[derive(Clone)]
pub struct FnSigOrigins<'tcx> {
    pub origin_params: Vec<OriginParam>,
    pub inputs: Vec<LabeledTy<'tcx, &'tcx [OriginArg<'tcx>]>>,
    pub output: LabeledTy<'tcx, &'tcx [OriginArg<'tcx>]>,
}

#[derive(Clone, Default)]
pub struct FnOriginMap<'tcx> {
    pub fn_info: HashMap<DefId, FnSigOrigins<'tcx>>,
}

fn fn_origin_args_params<'tcx>(
    tcx: TyCtxt<'tcx>,
    adt_metadata_table: &AdtMetadataTable,
) -> FnOriginMap<'tcx> {
    let fn_dids = fn_body_owners_postorder(tcx);

    let mut fn_info = HashMap::new();

    for fn_did in fn_dids {
        let fn_ty = tcx.type_of(fn_did);

        // gather existing OriginParams
        let mut origin_params = vec![];
        if let TyKind::FnDef(_, substs) = fn_ty.kind() {
            for sub in substs.iter() {
                if let GenericArgKind::Lifetime(re) = sub.unpack() {
                    if let RegionKind::ReEarlyBound(eb) = re.kind() {
                        origin_params.push(OriginParam::Actual(eb))
                    }
                }
            }
        }

        let mut arg_origin_args = vec![];

        // gather new and existing OriginArgs and push new OriginParams
        let sig = tcx.erase_late_bound_regions(tcx.fn_sig(fn_did));
        let ltcx = LabeledTyCtxt::<'tcx, &[OriginArg<'tcx>]>::new(tcx);
        let mut next_hypo_origin_id = 0;
        let mut origin_lty = |ty: Ty<'tcx>| {
            ltcx.label(ty, &mut |ty| {
                let mut origin_args = vec![];
                match ty.kind() {
                    TyKind::RawPtr(_ty) => {
                        origin_args.push(OriginArg::Hypothetical(next_hypo_origin_id));
                        origin_params.push(OriginParam::Hypothetical(next_hypo_origin_id));
                        next_hypo_origin_id += 1;
                    }
                    TyKind::Ref(reg, _ty, _mutability) => {
                        origin_args.push(OriginArg::Actual(*reg));
                    }
                    TyKind::Adt(adt_def, substs) => {
                        for sub in substs.iter() {
                            if let GenericArgKind::Lifetime(r) = sub.unpack() {
                                origin_args.push(OriginArg::Actual(r));
                            }
                        }
                        if let Some(adt_metadata) =
                            adt_metadata_table.table.get(&adt_def.did()).cloned()
                        {
                            for adt_param in adt_metadata.lifetime_params.iter() {
                                if let OriginParam::Hypothetical(_) = adt_param {
                                    let origin_param =
                                        OriginParam::Hypothetical(next_hypo_origin_id);
                                    origin_params.push(origin_param);
                                    origin_args.push(OriginArg::Hypothetical(next_hypo_origin_id));
                                    next_hypo_origin_id += 1;
                                }
                            }
                        }
                    }
                    _ => (),
                }

                if origin_args.is_empty() {
                    return &[];
                }
                let origin_args: Vec<_> = origin_args.into_iter().collect();
                ltcx.arena().alloc_slice(&origin_args[..])
            })
        };
        for ty in sig.inputs().iter() {
            let arg_lty = origin_lty(*ty);
            arg_origin_args.push(arg_lty);
        }

        let output = origin_lty(sig.output());

        fn_info.insert(
            fn_did.to_def_id(),
            FnSigOrigins {
                origin_params,
                inputs: arg_origin_args,
                output,
            },
        );
    }

    FnOriginMap { fn_info }
}

/// Build the `AdtMetadataTable`, which records the region parameters to be added to each ADT.  A
/// region parameter will be created for each raw pointer where `needs_region(lty)` returns `true`.
fn construct_adt_metadata<'tcx>(
    tcx: TyCtxt<'tcx>,
    field_ltys: &HashMap<DefId, LTy<'tcx>>,
    mut needs_region: impl FnMut(LTy<'tcx>) -> bool,
) -> AdtMetadataTable<'tcx> {
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

    // Gather existing lifetime parameters for each struct
    for struct_did in &adt_metadata_table.struct_dids {
        let struct_ty = tcx.type_of(struct_did);
        if let TyKind::Adt(adt_def, substs) = struct_ty.kind() {
            adt_metadata_table
                .table
                .insert(adt_def.did(), AdtMetadata::default());
            let metadata = adt_metadata_table.table.get_mut(&adt_def.did()).unwrap();
            debug!("gathering known lifetimes for {adt_def:?}");
            for sub in substs.iter() {
                if let GenericArgKind::Lifetime(r) = sub.unpack() {
                    debug!("\nfound lifetime {r:?} in {adt_def:?}");
                    assert_matches!(r.kind(), ReEarlyBound(eb) => {
                        metadata.lifetime_params.insert(OriginParam::Actual(eb));
                    });
                }
            }
        } else {
            panic!("{struct_ty:?} is not a struct");
        }
    }

    // TODO: Build dependency graph of ADTs and work by depth-first traversal of strongly-connected
    // components.  Currently, if `Foo` contains a pointer and `Bar` contains two `Foo`s, `Bar`
    // gets one region param that is used for both pointers.  Since this case is acyclic, we could
    // instead give `Bar` two region params and pass a different one to each `Foo`.  Within each
    // SCC, we need to use the first approach, otherwise every ADT might require an infinite number
    // of regions.  But for mentions of ADTs in other SCCs, we can use the second approach, which
    // is more flexible.

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

        info!("---- running fixed point struct field analysis iteration #{loop_count:?} ----");
        let old_adt_metadata = adt_metadata_table.table.clone();
        let mut next_hypo_origin_id = 0;

        // for each struct, gather lifetime information (actual and hypothetical)
        for struct_did in &adt_metadata_table.struct_dids {
            let adt_def = tcx.adt_def(struct_did);
            debug!("gathering lifetimes and lifetime parameters for {adt_def:?}");
            for field in adt_def.all_fields() {
                let field_lty = field_ltys
                    .get(&field.did)
                    .unwrap_or_else(|| panic!("missing field_ltys entry for {:?}", field.did));
                debug!("\t{adt_def:?}.{:}", field.name);
                let field_origin_args = ltcx.relabel(field_lty, &mut |lty| {
                    let mut field_origin_args = IndexSet::new();
                    match lty.kind() {
                        TyKind::RawPtr(ty) => {
                            if needs_region(lty) {
                                debug!(
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
                                        debug!(
                                            "\t\t\tinserting origin {origin_param:?} into {adt_def:?}"
                                        );

                                        adt.lifetime_params.insert(origin_param);
                                        next_hypo_origin_id += 1;
                                        field_origin_args.insert(origin_arg);
                                    });
                            }
                        }
                        TyKind::Ref(reg, _ty, _mutability) => {
                            debug!("\t\tfound reference field lifetime: {reg:}");
                            assert_matches!(reg.kind(), ReEarlyBound(..) | ReStatic);
                            let origin_arg = OriginArg::Actual(*reg);
                            adt_metadata_table
                                .table
                                .entry(*struct_did)
                                .and_modify(|adt| {
                                    if let ReEarlyBound(eb) = reg.kind() {
                                        debug!("\t\tinserting origin {eb:?} into {adt_def:?}");
                                        adt.lifetime_params.insert(OriginParam::Actual(eb));
                                    }

                                    field_origin_args.insert(origin_arg);
                                });
                        }
                        TyKind::Adt(adt_field, substs) => {
                            debug!("\t\tfound ADT field base type: {adt_field:?}");
                            for sub in substs.iter() {
                                if let GenericArgKind::Lifetime(r) = sub.unpack() {
                                    debug!("\tfound field lifetime {r:?} in {adt_def:?}.{adt_field:?}");
                                    debug!("\t\tinserting {adt_field:?} lifetime param {r:?} into {adt_def:?}.{:} lifetime parameters", field.name);
                                    assert_matches!(r.kind(), ReEarlyBound(..) | ReStatic);
                                    field_origin_args.insert(OriginArg::Actual(r));
                                }
                            }
                            if let Some(adt_field_metadata) =
                                adt_metadata_table.table.get(&adt_field.did()).cloned()
                            {
                                for adt_field_lifetime_param in adt_field_metadata.lifetime_params.iter() {
                                    adt_metadata_table.table.entry(*struct_did).and_modify(|adt| {
                                        if let OriginParam::Hypothetical(h) = adt_field_lifetime_param {
                                            debug!("\t\tbubbling {adt_field:?} origin {adt_field_lifetime_param:?} up into {adt_def:?} origins");
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

            debug!("");
        }

        if adt_metadata_table.table == old_adt_metadata {
            info!("reached a fixed point in struct lifetime reconciliation\n");
            break;
        }
    }

    adt_metadata_table
}

impl<'tcx> GlobalAnalysisCtxt<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> GlobalAnalysisCtxt<'tcx> {
        GlobalAnalysisCtxt {
            tcx,
            lcx: LabeledTyCtxt::new(tcx),
            ptr_info: GlobalPointerTable::empty(),
            num_total_pointers: 0,
            fn_sigs: HashMap::new(),
            fn_fields_used: MultiMap::new(),
            known_fns: all_known_fns()
                .iter()
                .map(|known_fn| (known_fn.name, known_fn))
                .collect(),
            dont_rewrite_fns: FlagMap::new(),
            dont_rewrite_statics: FlagMap::new(),
            dont_rewrite_fields: FlagMap::new(),
            force_rewrite: HashSet::new(),
            fns_failed: HashMap::new(),
            field_ltys: HashMap::new(),
            field_users: MultiMap::new(),
            static_tys: HashMap::new(),
            addr_of_static: HashMap::new(),
            adt_metadata: AdtMetadataTable::default(),
            fn_origins: FnOriginMap::default(),
            foreign_mentioned_tys: HashSet::new(),
        }
    }

    /// Initialize `self.adt_metadata` and `self.fn_origins`.  This requires that all field types
    /// in the crate have already been labeled in `field_ltys`.
    pub fn construct_region_metadata(&mut self) {
        debug_assert!(self.adt_metadata.table.is_empty());
        debug_assert!(self.fn_origins.fn_info.is_empty());
        self.construct_region_metadata_filtered(|_| true);
    }

    pub fn construct_region_metadata_filtered(&mut self, filter: impl FnMut(LTy<'tcx>) -> bool) {
        self.adt_metadata = construct_adt_metadata(self.tcx, &self.field_ltys, filter);
        self.fn_origins = fn_origin_args_params(self.tcx, &self.adt_metadata);
    }

    pub fn function_context<'a>(
        &'a mut self,
        mir: &'a Body<'tcx>,
        local_ptr_base: u32,
    ) -> AnalysisCtxt<'a, 'tcx> {
        AnalysisCtxt::new(self, mir, local_ptr_base)
    }

    pub fn function_context_with_data<'a>(
        &'a mut self,
        mir: &'a Body<'tcx>,
        data: AnalysisCtxtData<'tcx>,
    ) -> AnalysisCtxt<'a, 'tcx> {
        AnalysisCtxt::from_data(self, mir, data)
    }

    pub fn new_pointer(&mut self, info: PointerInfo) -> PointerId {
        self.ptr_info.push(info)
    }

    pub fn num_global_pointers(&self) -> usize {
        self.ptr_info.len()
    }

    pub fn num_total_pointers(&self) -> usize {
        self.num_total_pointers
    }

    pub fn set_num_total_pointers(&mut self, n: usize) {
        assert_eq!(
            self.num_total_pointers, 0,
            "num_total_pointers has already been set"
        );
        self.num_total_pointers = n;
    }

    pub fn ptr_info(&self) -> &GlobalPointerTable<PointerInfo> {
        &self.ptr_info
    }

    /// Update all [`PointerId`]s in `self`, replacing each `p` with `map[p]`.  Also sets the "next
    /// [`PointerId`]" counter to `counter`.  `map` and `counter` are usually computed together via
    /// [`GlobalEquivSet::renumber`][crate::equiv::GlobalEquivSet::renumber].
    pub fn remap_pointers(&mut self, map: &GlobalPointerTable<PointerId>, count: usize) {
        let GlobalAnalysisCtxt {
            tcx: _,
            lcx,
            ref mut ptr_info,
            num_total_pointers: _,
            ref mut fn_sigs,
            fn_fields_used: _,
            known_fns: _,
            dont_rewrite_fns: _,
            dont_rewrite_statics: _,
            dont_rewrite_fields: _,
            force_rewrite: _,
            fns_failed: _,
            ref mut field_ltys,
            field_users: _,
            ref mut static_tys,
            ref mut addr_of_static,
            adt_metadata: _,
            fn_origins: _,
            foreign_mentioned_tys: _,
        } = *self;

        *ptr_info = remap_global_ptr_info(ptr_info, map, count);

        for sig in fn_sigs.values_mut() {
            sig.inputs = lcx.mk_slice(
                &sig.inputs
                    .iter()
                    .map(|&lty| remap_lty_pointers(lcx, map, lty))
                    .collect::<Vec<_>>(),
            );
            sig.output = remap_lty_pointers(lcx, map, sig.output);
        }

        for labeled_field in field_ltys.values_mut() {
            *labeled_field = remap_lty_pointers(lcx, map, labeled_field);
        }

        for labeled_static in static_tys.values_mut() {
            *labeled_static = remap_lty_pointers(lcx, map, labeled_static);
        }

        for ptr in addr_of_static.values_mut() {
            if !ptr.is_none() {
                *ptr = map[*ptr];
            }
        }
    }

    pub fn assign_pointer_to_static(&mut self, did: DefId) {
        trace!("assign_pointer_to_static({:?})", did);
        // Statics always have full type annotations.
        let lty = self.assign_pointer_ids_with_info(self.tcx.type_of(did), PointerInfo::ANNOTATED);
        let ptr = self.new_pointer(PointerInfo::empty());
        self.static_tys.insert(did, lty);
        self.addr_of_static.insert(did, ptr);
    }

    pub fn assign_pointer_to_field(&mut self, field: &FieldDef) {
        let lty =
            self.assign_pointer_ids_with_info(self.tcx.type_of(field.did), PointerInfo::ANNOTATED);
        self.field_ltys.insert(field.did, lty);
    }

    pub fn assign_pointer_to_fields(&mut self, did: DefId) {
        for field in self.tcx.adt_def(did).all_fields() {
            self.assign_pointer_to_field(field);
        }
    }

    pub fn mark_fn_failed(&mut self, did: DefId, reason: DontRewriteFnReason, detail: PanicDetail) {
        self.dont_rewrite_fns.add(did, reason);
        // Insert `detail` if there isn't yet an entry for this `DefId`.
        self.fns_failed.entry(did).or_insert(detail);
    }

    /// Iterate over the `DefId`s of all functions that should skip rewriting.
    pub fn iter_fns_skip_rewrite(&self) -> impl Iterator<Item = DefId> + '_ {
        self.dont_rewrite_fns.keys()
    }

    pub fn known_fn(&self, def_id: DefId) -> Option<&'static KnownFn> {
        let symbol = self.tcx.symbol_name(Instance::mono(self.tcx, def_id));
        self.known_fns.get(symbol.name).copied()
    }

    /// Determine the [`PermissionSet`]s that should constrain [`PointerId`]s
    /// contained in the signatures of [`KnownFn`]s.
    ///
    /// This is determined by iterating through the [`LFnSig`]s in `self.fn_sigs`,
    /// filtering out the foreign ones ([`gather_foreign_sigs`] adds them to `fn_sigs`),
    /// looking up the [`KnownFn`] for that foreign `fn`, if it exists,
    /// and then `flat_map`ping that to each [`KnownFn::ptr_perms`].
    ///
    /// [`gather_foreign_sigs`]: crate::analyze::gather_foreign_sigs
    pub fn known_fn_ptr_perms<'a>(
        &'a self,
    ) -> impl Iterator<Item = (PointerId, PermissionSet)> + PhantomLifetime<'tcx> + 'a {
        self.fn_sigs
            .iter()
            .filter(|(def_id, _)| {
                self.tcx.def_kind(self.tcx.parent(**def_id)) == DefKind::ForeignMod
            })
            .filter_map(|(&def_id, fn_sig)| Some((fn_sig, self.known_fn(def_id)?)))
            .flat_map(|(fn_sig, known_fn)| known_fn.ptr_perms(fn_sig))
    }

    /// Check whether the function with the given `def_id` has been marked as non-rewritable.
    pub fn dont_rewrite_fn(&self, def_id: DefId) -> bool {
        self.dont_rewrite_fns.contains(def_id)
    }
    /// Check whether analysis failed for the function with the given `def_id`.
    pub fn fn_analysis_invalid(&self, def_id: DefId) -> bool {
        self.dont_rewrite_fns
            .get(def_id)
            .intersects(DontRewriteFnReason::ANALYSIS_INVALID_MASK)
    }

    pub fn ptr_is_global(&self, ptr: PointerId) -> bool {
        self.ptr_info.contains(ptr)
    }
}

impl<'a, 'tcx> AnalysisCtxt<'a, 'tcx> {
    pub fn new(
        gacx: &'a mut GlobalAnalysisCtxt<'tcx>,
        mir: &'a Body<'tcx>,
        local_ptr_base: u32,
    ) -> AnalysisCtxt<'a, 'tcx> {
        AnalysisCtxt {
            gacx,
            ptr_info: LocalPointerTable::empty(local_ptr_base),
            local_decls: &mir.local_decls,
            local_tys: IndexVec::new(),
            addr_of_local: IndexVec::new(),
            rvalue_tys: HashMap::new(),
            string_literal_locs: Default::default(),
        }
    }

    pub fn from_data(
        gacx: &'a mut GlobalAnalysisCtxt<'tcx>,
        mir: &'a Body<'tcx>,
        data: AnalysisCtxtData<'tcx>,
    ) -> AnalysisCtxt<'a, 'tcx> {
        let AnalysisCtxtData {
            ptr_info,
            local_tys,
            addr_of_local,
            rvalue_tys,
            string_literal_locs,
        } = data;
        AnalysisCtxt {
            gacx,
            ptr_info,
            local_decls: &mir.local_decls,
            local_tys,
            addr_of_local,
            rvalue_tys,
            string_literal_locs,
        }
    }

    pub fn into_data(self) -> AnalysisCtxtData<'tcx> {
        AnalysisCtxtData {
            ptr_info: self.ptr_info,
            local_tys: self.local_tys,
            addr_of_local: self.addr_of_local,
            rvalue_tys: self.rvalue_tys,
            string_literal_locs: self.string_literal_locs,
        }
    }

    pub fn tcx(&self) -> TyCtxt<'tcx> {
        self.gacx.tcx
    }

    pub fn lcx(&self) -> LTyCtxt<'tcx> {
        self.gacx.lcx
    }

    pub fn new_pointer(&mut self, info: PointerInfo) -> PointerId {
        self.ptr_info.push(info)
    }

    pub fn num_pointers(&self) -> usize {
        self.ptr_info.len()
    }

    pub fn _ptr_info(&self) -> PointerTable<PointerInfo> {
        self.gacx.ptr_info.and(&self.ptr_info)
    }

    pub fn ptr_info_mut(&mut self) -> PointerTableMut<PointerInfo> {
        self.gacx.ptr_info.and_mut(&mut self.ptr_info)
    }

    pub fn local_ptr_info(&self) -> &LocalPointerTable<PointerInfo> {
        &self.ptr_info
    }

    pub fn local_ptr_base(&self) -> u32 {
        self.ptr_info.base()
    }

    pub fn ptr_is_global(&self, ptr: PointerId) -> bool {
        self.gacx.ptr_is_global(ptr)
    }

    pub fn type_of<T: TypeOf<'tcx>>(&self, x: T) -> LTy<'tcx> {
        x.type_of(self)
    }

    pub fn ptr_of<T: TypeOf<'tcx>>(&self, x: T) -> Option<PointerId> {
        let ptr = self.type_of(x).label;
        if ptr == PointerId::NONE {
            None
        } else {
            Some(ptr)
        }
    }

    /// Returns the [`LTy`] of an [`Rvalue`]
    pub fn type_of_rvalue(&self, rv: &Rvalue<'tcx>, loc: Location) -> LTy<'tcx> {
        if let Some(lty) = self.rvalue_tys.get(&loc) {
            return lty;
        }

        self.derived_type_of_rvalue(rv)
    }

    /// Returns a boolean indicating whether or
    /// not the `Rvalue` is a reference or pointer containing a field projection.
    /// For example, the following [`Rvalue`]s will satisfy that criteria:
    /// - let r1 = std::ptr::addr_of!(x.field);
    /// - let r2 = &x.field;
    /// - let r3 = &(*p).field;
    /// The following will NOT satisfy that criteria:
    /// - let r1 = x.field;
    /// - let r2 = x.field + y;
    pub fn has_field_projection(&self, rv: &Rvalue<'tcx>) -> bool {
        if let Some(desc) = describe_rvalue(rv) {
            match desc {
                RvalueDesc::Project { proj, .. } | RvalueDesc::AddrOfLocal { proj, .. } => {
                    for p in proj {
                        if let PlaceElem::Field(..) = p {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    /// In some cases, we can compute an `LTy` for an `Rvalue` that uses `PointerId`s derived from
    /// the `LTy`s of other value, rather than using the `PointerId`s assigned in `rvalue_tys`.
    /// For example, suppose we have this code:
    ///
    /// ```ignore
    /// let p: & /*p0*/ MyStruct = ...;
    /// let q: & /*p2*/ i32 = &(*p).field: & /*p1*/ i32;
    /// ```
    ///
    /// The type ascription on the rvalue `&(*p).field` represents the entry in `rvalue_tys`, which
    /// uses `PointerId` `p1`.  However, we can derive another type for this rvalue from the type
    /// of `p`: since `p` has type `& /*p0*/ MyStruct`, the projection `&(*p).field` can be given
    /// the type `& /*p0*/ i32`, using the same `PointerId` `p0`.  Calling `type_of_rvalue` will
    /// return the type assigned in `rvalue_tys`, which uses `p1`, whereas this method will return
    /// the derived type using `p0`.  Certain cases in `dataflow::type_check` will establish
    /// constraints relating the two types.
    pub fn derived_type_of_rvalue(&self, rv: &Rvalue<'tcx>) -> LTy<'tcx> {
        if let Some(desc) = describe_rvalue(rv) {
            let ty = rv.ty(self, self.tcx());
            if matches!(ty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)) {
                let (pointee_lty, proj, ptr) = match desc {
                    RvalueDesc::Project {
                        base,
                        proj,
                        mutbl: _,
                    } => {
                        let base_lty = self.type_of(base);
                        debug!(
                            "rvalue = {:?}, desc = {:?}, base_lty = {:?}",
                            rv, desc, base_lty
                        );
                        (
                            self.projection_lty(base_lty, &PlaceElem::Deref),
                            proj,
                            base_lty.label,
                        )
                    }
                    RvalueDesc::AddrOfLocal {
                        local,
                        proj,
                        mutbl: _,
                    } => (self.type_of(local), proj, self.addr_of_local[local]),
                };

                let mut pointee_lty = pointee_lty;
                for p in proj {
                    pointee_lty = self.projection_lty(pointee_lty, p);
                }

                let ty = rv.ty(self, self.tcx());
                let pointee_ty = match *ty.kind() {
                    TyKind::Ref(_, ty, _) => ty,
                    TyKind::RawPtr(tm) => tm.ty,
                    _ => unreachable!(
                        "got RvalueDesc for non-pointer Rvalue {:?} (of type {:?})",
                        rv, ty,
                    ),
                };
                assert_eq!(
                    self.tcx().erase_regions(pointee_ty),
                    self.tcx().erase_regions(pointee_lty.ty)
                );

                let args = self.lcx().mk_slice(&[pointee_lty]);
                return self.lcx().mk(ty, args, ptr);
            }
        }

        let ty = match *rv {
            Rvalue::Use(ref op) => self.type_of(op),
            Rvalue::CopyForDeref(pl) => self.type_of(pl),
            Rvalue::Repeat(ref op, _) => {
                let op_lty = self.type_of(op);
                let ty = rv.ty(self, self.tcx());
                assert!(matches!(ty.kind(), TyKind::Array(..)));
                let args = self.lcx().mk_slice(&[op_lty]);
                self.lcx().mk(ty, args, PointerId::NONE)
            }
            Rvalue::Ref(..) | Rvalue::AddressOf(..) => {
                unreachable!("should be handled by describe_rvalue case above")
            }
            Rvalue::ThreadLocalRef(..) => todo!("type_of ThreadLocalRef"),
            Rvalue::Cast(..) => panic!("Cast should be present in rvalue_tys"),
            Rvalue::Len(..)
            | Rvalue::BinaryOp(..)
            | Rvalue::CheckedBinaryOp(..)
            | Rvalue::NullaryOp(..)
            | Rvalue::UnaryOp(..)
            | Rvalue::Discriminant(..) => {
                let ty = rv.ty(self, self.tcx());
                label_no_pointers(self, ty)
            }
            Rvalue::Aggregate(ref _kind, ref _vals) => todo!("type_of Aggregate: rv = {rv:?}"),
            Rvalue::ShallowInitBox(ref _op, _ty) => todo!("type_of ShallowInitBox: rv = {rv:?}"),
        };

        ty
    }

    pub fn projection_lty(&self, lty: LTy<'tcx>, proj: &PlaceElem<'tcx>) -> LTy<'tcx> {
        let projection_lty = |_lty: LTy, adt_def: AdtDef, field: Field| {
            let field_def = &adt_def.non_enum_variant().fields[field.index()];
            let field_def_name = field_def.name;
            debug!("projecting into {adt_def:?}.{field_def_name:}");
            let field_lty: LTy = self.gacx.field_ltys.get(&field_def.did).unwrap_or_else(|| {
                panic!("Could not find {adt_def:?}.{field_def_name:?} in field type map")
            });
            field_lty
        };
        util::lty_project(lty, proj, projection_lty)
    }
}

impl<'tcx> AnalysisCtxtData<'tcx> {
    /// Update all [`PointerId`]s in `self`, replacing each `p` with `map[p]`.  Also sets the "next
    /// [`PointerId`]" counter to `counter`.  `map` and `counter` are usually computed together via
    /// [`LocalEquivSet::renumber`][crate::equiv::LocalEquivSet::renumber].
    pub fn remap_pointers(
        &mut self,
        gacx: &mut GlobalAnalysisCtxt<'tcx>,
        map: PointerTable<PointerId>,
        local_base: u32,
        local_count: usize,
    ) {
        let lcx = gacx.lcx;

        let Self {
            ptr_info,
            local_tys,
            addr_of_local,
            rvalue_tys,
            string_literal_locs: _,
        } = self;

        *ptr_info =
            remap_local_ptr_info(ptr_info, &mut gacx.ptr_info, &map, local_base, local_count);

        for lty in local_tys {
            *lty = remap_lty_pointers(lcx, &map, lty);
        }

        for ptr in addr_of_local {
            if !ptr.is_none() {
                *ptr = map[*ptr];
            }
        }

        for lty in rvalue_tys.values_mut() {
            *lty = remap_lty_pointers(lcx, &map, lty);
        }
    }

    pub fn local_ptr_info(&self) -> &LocalPointerTable<PointerInfo> {
        &self.ptr_info
    }

    pub fn num_pointers(&self) -> usize {
        self.ptr_info.len()
    }

    pub fn local_ptr_base(&self) -> u32 {
        self.ptr_info.base()
    }
}

/// For every [`PointerId`] `p` that appears in `lty`, replace `p` with `map[p]` (except that
/// [`PointerId::NONE`] is left unchanged) and return the updated `LTy`.
fn remap_lty_pointers<'tcx, T>(lcx: LTyCtxt<'tcx>, map: &T, lty: LTy<'tcx>) -> LTy<'tcx>
where
    T: Index<PointerId, Output = PointerId>,
{
    lcx.relabel(lty, &mut |inner_lty| {
        if inner_lty.label.is_none() {
            PointerId::NONE
        } else {
            map[inner_lty.label]
        }
    })
}

/// Renumber the keys of `ptr_info`, producing a new table.  For a new `PointerId` `q`, the
/// `PointerInfo` in the output is computed by merging the values of `ptr_info[p]` for all `p`
/// where `map[p] == q`.
fn remap_global_ptr_info(
    ptr_info: &GlobalPointerTable<PointerInfo>,
    map: &GlobalPointerTable<PointerId>,
    num_pointers: usize,
) -> GlobalPointerTable<PointerInfo> {
    let mut new_info = GlobalPointerTable::<PointerInfo>::new(num_pointers);
    for (old, &new) in map.iter() {
        new_info[new] |= ptr_info[old];
    }
    new_info
}

/// Renumber the keys of `old_local_ptr_info`, producing a new local table.  `new_global_ptr_info`
/// must already be remapped with `remap_global_ptr_info`.
fn remap_local_ptr_info(
    old_local_ptr_info: &LocalPointerTable<PointerInfo>,
    new_global_ptr_info: &mut GlobalPointerTable<PointerInfo>,
    map: &PointerTable<PointerId>,
    base: u32,
    count: usize,
) -> LocalPointerTable<PointerInfo> {
    let mut new_local_ptr_info = LocalPointerTable::<PointerInfo>::new(base, count);
    let mut new_ptr_info = new_global_ptr_info.and_mut(&mut new_local_ptr_info);
    for (old, &new) in map.iter() {
        if !old_local_ptr_info.contains(old) {
            // If `old` is global then `new` is also global, and this remapping was handled already
            // by `remap_global_ptr_info`.
            continue;
        }

        new_ptr_info[new] |= old_local_ptr_info[old];
    }
    new_local_ptr_info
}

impl<'tcx> HasLocalDecls<'tcx> for AnalysisCtxt<'_, 'tcx> {
    fn local_decls(&self) -> &LocalDecls<'tcx> {
        self.local_decls
    }
}

pub trait TypeOf<'tcx> {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx>;
}

impl<'tcx, T: TypeOf<'tcx>> TypeOf<'tcx> for &T {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        (**self).type_of(acx)
    }
}

impl<'tcx> TypeOf<'tcx> for Local {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        acx.local_tys[*self]
    }
}

impl<'tcx> TypeOf<'tcx> for Place<'tcx> {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        acx.type_of(self.as_ref())
    }
}

impl<'tcx> TypeOf<'tcx> for PlaceRef<'tcx> {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        let mut ty = acx.type_of(self.local);
        for proj in self.projection {
            ty = acx.projection_lty(ty, proj);
        }
        ty
    }
}

pub fn const_alloc_id(c: &Constant) -> Option<AllocId> {
    if let ConstantKind::Val(ConstValue::Scalar(interpret::Scalar::Ptr(ptr, _)), _ty) = c.literal {
        return Some(ptr.provenance);
    }
    None
}

pub fn find_static_for_alloc(tcx: &TyCtxt, id: AllocId) -> Option<DefId> {
    match tcx.try_get_global_alloc(id) {
        None => {} //hmm, ok
        Some(GlobalAlloc::Static(did)) => {
            if !tcx.is_foreign_item(did) {
                // local static referenced
                return Some(did);
            }
        }
        Some(_) => {}
    }
    None
}

impl<'tcx> TypeOf<'tcx> for Operand<'tcx> {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        match *self {
            Operand::Move(pl) | Operand::Copy(pl) => acx.type_of(pl),
            Operand::Constant(ref c) => {
                // Constants of pointer type should only be pointers into static allocations.
                // Find the defid of the static and look up the pointer ID in gacx.static_tys
                if c.ty().is_any_ptr() {
                    if let Some(alloc_id) = const_alloc_id(c) {
                        if let Some(did) = find_static_for_alloc(&acx.gacx.tcx, alloc_id) {
                            let lty = acx
                                .gacx
                                .static_tys
                                .get(&did)
                                .cloned()
                                .unwrap_or_else(|| panic!("did {:?} not found", did));
                            let ptr = acx
                                .gacx
                                .addr_of_static
                                .get(&did)
                                .cloned()
                                .unwrap_or_else(|| panic!("did {:?} not found", did));
                            let args = acx.lcx().mk_slice(&[lty]);
                            assert!(matches!(
                                *c.ty().kind(),
                                TyKind::Ref(..) | TyKind::RawPtr(..)
                            ));
                            acx.lcx().mk(c.ty(), args, ptr)
                        } else {
                            panic!("no static found for alloc id {:?}", alloc_id)
                        }
                    } else {
                        panic!("constant was of ptr type but not a Scalar pointing into a static allocation: {:?}", c)
                    }
                } else {
                    label_no_pointers(acx, c.ty())
                }
            }
        }
    }
}

/// Label a type that contains no pointer types by applying `PointerId::NONE` everywhere.  Panics
/// if the type does contain pointers.
pub fn label_no_pointers<'tcx>(acx: &AnalysisCtxt<'_, 'tcx>, ty: Ty<'tcx>) -> LTy<'tcx> {
    acx.lcx().label(ty, &mut |inner_ty| {
        assert!(
            !matches!(inner_ty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)),
            "unexpected pointer type in {:?}",
            ty,
        );
        PointerId::NONE
    })
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Assignment {
    pub perms: GlobalPointerTable<PermissionSet>,
    pub flags: GlobalPointerTable<FlagSet>,
}

impl Assignment {
    pub fn new(len: usize, default_perms: PermissionSet, default_flags: FlagSet) -> Assignment {
        Assignment {
            perms: GlobalPointerTable::from_raw(vec![default_perms; len]),
            flags: GlobalPointerTable::from_raw(vec![default_flags; len]),
        }
    }

    pub fn perms(&self) -> &GlobalPointerTable<PermissionSet> {
        &self.perms
    }

    pub fn perms_mut(&mut self) -> &mut GlobalPointerTable<PermissionSet> {
        &mut self.perms
    }

    pub fn flags(&self) -> &GlobalPointerTable<FlagSet> {
        &self.flags
    }

    pub fn _flags_mut(&mut self) -> &mut GlobalPointerTable<FlagSet> {
        &mut self.flags
    }
}

/// Print an `LTy` as a string, using the provided callback to print the labels on each pointer and
/// reference type.
///
/// Note this completely omits any labels on non-pointer types.
pub fn print_ty_with_pointer_labels<L: Copy>(
    lty: LabeledTy<L>,
    mut f: impl FnMut(L) -> String,
) -> String {
    let mut out = String::new();
    print_ty_with_pointer_labels_into(&mut out, lty, &mut f);
    out
}

pub fn print_ty_with_pointer_labels_into<L: Copy>(
    dest: &mut String,
    lty: LabeledTy<L>,
    f: &mut impl FnMut(L) -> String,
) {
    use rustc_type_ir::TyKind::*;
    match lty.ty.kind() {
        // Types with no arguments
        Bool | Char | Int(_) | Uint(_) | Float(_) | Str | Foreign(_) | Never => {
            write!(dest, "{:?}", lty.ty).unwrap();
        }

        // Types with arguments
        Adt(adt_def, _substs) => {
            write!(dest, "{:?}", adt_def.did()).unwrap();
            if !lty.args.is_empty() {
                dest.push('<');
                // TODO: region args
                for (i, &arg_lty) in lty.args.iter().enumerate() {
                    if i > 0 {
                        dest.push_str(", ");
                    }
                    print_ty_with_pointer_labels_into(dest, arg_lty, f);
                }
                dest.push('>');
            }
        }
        &Array(_elem, len) => {
            dest.push('[');
            print_ty_with_pointer_labels_into(dest, lty.args[0], f);
            write!(dest, "; {:?}]", len).unwrap();
        }
        &Slice(_elem) => {
            dest.push('[');
            print_ty_with_pointer_labels_into(dest, lty.args[0], f);
            dest.push(']');
        }
        RawPtr(mty) => {
            if mty.mutbl == Mutability::Not {
                dest.push_str("*const ");
            } else {
                dest.push_str("*mut ");
            }
            let s = f(lty.label);
            if !s.is_empty() {
                dest.push_str(&s);
                dest.push(' ');
            }
            print_ty_with_pointer_labels_into(dest, lty.args[0], f);
        }
        &Ref(_rg, _ty, mutbl) => {
            let s = f(lty.label);
            if mutbl == Mutability::Not {
                dest.push('&');
                if !s.is_empty() {
                    dest.push(' ');
                }
            } else {
                dest.push_str("&mut ");
            }
            if !s.is_empty() {
                dest.push_str(&s);
                dest.push(' ');
            }
            print_ty_with_pointer_labels_into(dest, lty.args[0], f);
        }
        FnDef(def_id, _substs) => {
            write!(dest, "{:?}", def_id).unwrap();
            if !lty.args.is_empty() {
                dest.push('<');
                // TODO: region args
                for (i, &arg_lty) in lty.args.iter().enumerate() {
                    if i > 0 {
                        dest.push_str(", ");
                    }
                    print_ty_with_pointer_labels_into(dest, arg_lty, f);
                }
                dest.push('>');
            }
        }
        FnPtr(_) => {
            let (ret_lty, arg_ltys) = lty.args.split_last().unwrap();
            dest.push_str("fn(");
            for (i, &arg_lty) in arg_ltys.iter().enumerate() {
                if i > 0 {
                    dest.push_str(", ");
                }
                print_ty_with_pointer_labels_into(dest, arg_lty, f);
            }
            dest.push_str(") -> ");
            print_ty_with_pointer_labels_into(dest, ret_lty, f);
        }
        Tuple(_) => {
            dest.push('(');
            for (i, &arg_lty) in lty.args.iter().enumerate() {
                if i > 0 {
                    dest.push_str(", ");
                }
                print_ty_with_pointer_labels_into(dest, arg_lty, f);
            }
            dest.push(')');
        }

        // Types that aren't actually supported by this code yet
        Dynamic(..) | Closure(..) | Generator(..) | GeneratorWitness(..) | Projection(..)
        | Opaque(..) | Param(..) | Bound(..) | Placeholder(..) | Infer(..) | Error(..) => {
            write!(dest, "unknown:{:?}", lty.ty).unwrap();
        }
    }
}

/// Map for associating flags (such as `DontRewriteFnReason`) with keys (such as `DefId`).
#[derive(Clone, Debug)]
pub struct FlagMap<K, V> {
    /// Stores the current flags for each key.  If no flags are set, the entry is omitted; that is,
    /// for every entry `(k, v)`, it's always the case that `v != V::default()`.
    m: HashMap<K, V>,
    /// Keys that were added to `m` since the last call to `take_new()`.  Specifically, this
    /// includes every `k` for which `get(k)` was `V::default()` but `get(k)` now has a different,
    /// non-default value.
    new: Vec<K>,
}

impl<K, V> FlagMap<K, V>
where
    K: Copy + Hash + Eq,
    V: Copy + Default + Eq + BitOr<Output = V>,
{
    pub fn new() -> FlagMap<K, V> {
        FlagMap {
            m: HashMap::new(),
            new: Vec::new(),
        }
    }

    pub fn add(&mut self, k: K, v: V) {
        if v == V::default() {
            return;
        }
        let cur = self.m.entry(k).or_default();
        if *cur == V::default() {
            self.new.push(k);
        }
        *cur = *cur | v;
    }

    /// Get the current flags for `k`.  Returns `V::default()` if no flags have been set.
    pub fn get(&self, k: K) -> V {
        self.m.get(&k).copied().unwrap_or_default()
    }

    pub fn contains(&self, k: K) -> bool {
        self.m.contains_key(&k)
    }

    pub fn new_keys(&self) -> &[K] {
        &self.new
    }

    pub fn take_new_keys(&mut self) -> Vec<K> {
        mem::take(&mut self.new)
    }

    pub fn keys(&self) -> impl Iterator<Item = K> + '_ {
        self.m.keys().copied()
    }
}

/// A map from keys to lists of values, with a compact representation.
#[derive(Clone, Debug)]
pub struct MultiMap<K, V> {
    defs: HashMap<K, Range<usize>>,
    users: Vec<V>,
}

impl<K, V> MultiMap<K, V>
where
    K: Copy + Hash + Eq + Debug,
{
    pub fn new() -> MultiMap<K, V> {
        MultiMap {
            defs: HashMap::new(),
            users: Vec::new(),
        }
    }

    pub fn insert(&mut self, def: K, users: impl IntoIterator<Item = V>) {
        let e = match self.defs.entry(def) {
            Entry::Vacant(e) => e,
            Entry::Occupied(_) => panic!("duplicate entry for {def:?}"),
        };
        let start = self.users.len();
        self.users.extend(users);
        let end = self.users.len();
        e.insert(start..end);
    }

    pub fn get(&self, def: K) -> &[V] {
        let range = match self.defs.get(&def) {
            Some(x) => x,
            None => return &[],
        };
        &self.users[range.clone()]
    }
}
