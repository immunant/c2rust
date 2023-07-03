use crate::borrowck::{AdtMetadata, FieldMetadata, OriginArg, OriginParam};
use crate::c_void_casts::CVoidCasts;
use crate::labeled_ty::{LabeledTy, LabeledTyCtxt};
use crate::panic_detail::PanicDetail;
use crate::pointer_id::{
    GlobalPointerTable, LocalPointerTable, NextGlobalPointerId, NextLocalPointerId, PointerTable,
    PointerTableMut,
};
use crate::util::{self, describe_rvalue, PhantomLifetime, RvalueDesc};
use crate::AssignPointerIds;
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
use rustc_middle::ty::{
    tls, AdtDef, FieldDef, GenericArgKind, GenericParamDefKind, Ty, TyCtxt, TyKind,
};
use rustc_type_ir::RegionKind::{ReEarlyBound, ReStatic};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::ops::Index;

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
    }
}

impl PermissionSet {
    /// The permissions for a (byte-)string literal.
    //
    // `.union` is used here since it's a `const fn`, unlike `BitOr::bitor`.
    pub const STRING_LITERAL: Self = Self::READ.union(Self::OFFSET_ADD);

    #[cfg(test)]
    pub const fn union_all<const N: usize>(a: [Self; N]) -> Self {
        let mut this = Self::empty();
        let mut i = 0;
        while i < N {
            this = this.union(a[i]);
            i += 1;
        }
        this
    }
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

pub use crate::pointer_id::PointerId;

pub type LTy<'tcx> = LabeledTy<'tcx, PointerId>;
pub type LTyCtxt<'tcx> = LabeledTyCtxt<'tcx, PointerId>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LFnSig<'tcx> {
    pub inputs: &'tcx [LTy<'tcx>],
    pub output: LTy<'tcx>,
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
    }
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
                let other_param_names = tcx.generics_of(k).params.iter().filter_map(|p| {
                    if !matches!(p.kind, GenericParamDefKind::Lifetime) {
                        Some(p.name.to_ident_string())
                    } else {
                        None
                    }
                });
                write!(f, "struct {:}", tcx.item_name(*k))?;
                write!(f, "<")?;
                let lifetime_params_str = adt
                    .lifetime_params
                    .iter()
                    .map(|p| format!("{:?}", p))
                    .chain(other_param_names)
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

pub struct GlobalAnalysisCtxt<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub lcx: LTyCtxt<'tcx>,

    ptr_info: GlobalPointerTable<PointerInfo>,

    /// Map from a function to all of its callers.
    pub fn_callers: HashMap<DefId, Vec<DefId>>,

    pub fn_sigs: HashMap<DefId, LFnSig<'tcx>>,

    /// `DefId`s of functions where analysis failed, and a [`PanicDetail`] explaining the reason
    /// for each failure.
    pub fns_failed: HashMap<DefId, PanicDetail>,

    pub field_ltys: HashMap<DefId, LTy<'tcx>>,

    pub static_tys: HashMap<DefId, LTy<'tcx>>,
    pub addr_of_static: HashMap<DefId, PointerId>,

    pub adt_metadata: AdtMetadataTable<'tcx>,

    pub foreign_mentioned_tys: HashSet<DefId>,
}

pub struct AnalysisCtxt<'a, 'tcx> {
    pub gacx: &'a mut GlobalAnalysisCtxt<'tcx>,

    ptr_info: LocalPointerTable<PointerInfo>,

    pub local_decls: &'a LocalDecls<'tcx>,
    pub local_tys: IndexVec<Local, LTy<'tcx>>,
    pub addr_of_local: IndexVec<Local, PointerId>,
    pub c_void_casts: CVoidCasts<'tcx>,
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
    ) -> impl Iterator<Item = (PointerId, PermissionSet)> + PhantomLifetime<'tcx> + 'a {
        self.string_literal_tys()
            .map(|lty| (lty.label, PermissionSet::STRING_LITERAL))
    }

    pub fn check_string_literal_perms(&self, asn: &Assignment) {
        for lty in self.string_literal_tys() {
            let ptr = lty.label;
            let expected_perms = PermissionSet::STRING_LITERAL;
            let mut actual_perms = asn.perms()[ptr];
            // Ignore `UNIQUE` as it gets automatically added to all permissions
            // and then removed later if it can't apply.
            // We don't care about `UNIQUE` for const refs, so just unset it here.
            actual_perms.set(PermissionSet::UNIQUE, false);
            assert_eq!(expected_perms, actual_perms);
        }
    }
}

pub struct AnalysisCtxtData<'tcx> {
    ptr_info: LocalPointerTable<PointerInfo>,
    local_tys: IndexVec<Local, LTy<'tcx>>,
    addr_of_local: IndexVec<Local, PointerId>,
    c_void_casts: CVoidCasts<'tcx>,
    rvalue_tys: HashMap<Location, LTy<'tcx>>,
    string_literal_locs: Vec<Location>,
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

impl<'tcx> GlobalAnalysisCtxt<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> GlobalAnalysisCtxt<'tcx> {
        GlobalAnalysisCtxt {
            tcx,
            lcx: LabeledTyCtxt::new(tcx),
            ptr_info: GlobalPointerTable::empty(),
            fn_callers: HashMap::new(),
            fn_sigs: HashMap::new(),
            fns_failed: HashMap::new(),
            field_ltys: HashMap::new(),
            static_tys: HashMap::new(),
            addr_of_static: HashMap::new(),
            adt_metadata: construct_adt_metadata(tcx),
            foreign_mentioned_tys: HashSet::new(),
        }
    }

    pub fn function_context<'a>(&'a mut self, mir: &'a Body<'tcx>) -> AnalysisCtxt<'a, 'tcx> {
        AnalysisCtxt::new(self, mir)
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

    pub fn num_pointers(&self) -> usize {
        self.ptr_info.len()
    }

    pub fn ptr_info(&self) -> &GlobalPointerTable<PointerInfo> {
        &self.ptr_info
    }

    /// Update all [`PointerId`]s in `self`, replacing each `p` with `map[p]`.  Also sets the "next
    /// [`PointerId`]" counter to `counter`.  `map` and `counter` are usually computed together via
    /// [`GlobalEquivSet::renumber`][crate::equiv::GlobalEquivSet::renumber].
    pub fn remap_pointers(
        &mut self,
        map: &GlobalPointerTable<PointerId>,
        counter: NextGlobalPointerId,
    ) {
        let GlobalAnalysisCtxt {
            tcx: _,
            lcx,
            ref mut ptr_info,
            fn_callers: _,
            ref mut fn_sigs,
            fns_failed: _,
            ref mut field_ltys,
            ref mut static_tys,
            ref mut addr_of_static,
            adt_metadata: _,
            foreign_mentioned_tys: _,
        } = *self;

        *ptr_info = remap_global_ptr_info(ptr_info, map, counter.num_pointers());

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

    pub fn fn_failed(&mut self, did: DefId) -> bool {
        self.fns_failed.contains_key(&did)
    }

    pub fn mark_fn_failed(&mut self, did: DefId, detail: PanicDetail) {
        if self.fns_failed.contains_key(&did) {
            return;
        }

        self.fns_failed.insert(did, detail);

        // This is the first time marking `did` as failed, so also mark all of its callers.
        let callers = self.fn_callers.get(&did).cloned().unwrap_or(Vec::new());
        for caller in callers {
            self.mark_fn_failed(
                caller,
                PanicDetail::new(format!("analysis failed on callee {:?}", did)),
            );
        }
    }
}

impl<'a, 'tcx> AnalysisCtxt<'a, 'tcx> {
    pub fn new(
        gacx: &'a mut GlobalAnalysisCtxt<'tcx>,
        mir: &'a Body<'tcx>,
    ) -> AnalysisCtxt<'a, 'tcx> {
        let tcx = gacx.tcx;
        AnalysisCtxt {
            gacx,
            ptr_info: LocalPointerTable::empty(),
            local_decls: &mir.local_decls,
            local_tys: IndexVec::new(),
            addr_of_local: IndexVec::new(),
            c_void_casts: CVoidCasts::new(mir, tcx),
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
            c_void_casts,
            rvalue_tys,
            string_literal_locs,
        } = data;
        AnalysisCtxt {
            gacx,
            ptr_info,
            local_decls: &mir.local_decls,
            local_tys,
            addr_of_local,
            c_void_casts,
            rvalue_tys,
            string_literal_locs,
        }
    }

    pub fn into_data(self) -> AnalysisCtxtData<'tcx> {
        AnalysisCtxtData {
            ptr_info: self.ptr_info,
            local_tys: self.local_tys,
            addr_of_local: self.addr_of_local,
            c_void_casts: self.c_void_casts,
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

    pub fn _local_ptr_info(&self) -> &LocalPointerTable<PointerInfo> {
        &self.ptr_info
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

    pub fn type_of_rvalue(&self, rv: &Rvalue<'tcx>, loc: Location) -> LTy<'tcx> {
        if let Some(&lty) = self.rvalue_tys.get(&loc) {
            return lty;
        }

        if let Some(desc) = describe_rvalue(rv) {
            let ty = rv.ty(self, self.tcx());
            if matches!(ty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)) {
                let (pointee_lty, proj, ptr) = match desc {
                    RvalueDesc::Project { base, proj } => {
                        let base_lty = self.type_of(base);
                        eprintln!(
                            "rvalue = {:?}, desc = {:?}, base_lty = {:?}",
                            rv, desc, base_lty
                        );
                        (
                            self.projection_lty(base_lty, &PlaceElem::Deref),
                            proj,
                            base_lty.label,
                        )
                    }
                    RvalueDesc::AddrOfLocal { local, proj } => {
                        (self.type_of(local), proj, self.addr_of_local[local])
                    }
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

        match *rv {
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
        }
    }

    pub fn projection_lty(&self, lty: LTy<'tcx>, proj: &PlaceElem<'tcx>) -> LTy<'tcx> {
        let projection_lty = |_lty: LTy, adt_def: AdtDef, field: Field| {
            let field_def = &adt_def.non_enum_variant().fields[field.index()];
            let field_def_name = field_def.name;
            eprintln!("projecting into {adt_def:?}.{field_def_name:}");
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
        counter: NextLocalPointerId,
    ) {
        let lcx = gacx.lcx;

        let Self {
            ptr_info,
            local_tys,
            addr_of_local,
            c_void_casts: _,
            rvalue_tys,
            string_literal_locs: _,
        } = self;

        *ptr_info =
            remap_local_ptr_info(ptr_info, &mut gacx.ptr_info, &map, counter.num_pointers());

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
    num_pointers: usize,
) -> LocalPointerTable<PointerInfo> {
    let mut new_local_ptr_info = LocalPointerTable::<PointerInfo>::new(num_pointers);
    let mut new_ptr_info = new_global_ptr_info.and_mut(&mut new_local_ptr_info);
    for (old, &new) in map.iter() {
        if old.is_global() {
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

fn const_alloc_id(c: &Constant) -> Option<AllocId> {
    if let ConstantKind::Val(ConstValue::Scalar(interpret::Scalar::Ptr(ptr, _)), _ty) = c.literal {
        return Some(ptr.provenance);
    }
    None
}

fn find_static_for_alloc(tcx: &TyCtxt, id: AllocId) -> Option<DefId> {
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
pub struct GlobalAssignment {
    pub perms: GlobalPointerTable<PermissionSet>,
    pub flags: GlobalPointerTable<FlagSet>,
}

impl GlobalAssignment {
    pub fn new(
        len: usize,
        default_perms: PermissionSet,
        default_flags: FlagSet,
    ) -> GlobalAssignment {
        GlobalAssignment {
            perms: GlobalPointerTable::from_raw(vec![default_perms; len]),
            flags: GlobalPointerTable::from_raw(vec![default_flags; len]),
        }
    }

    pub fn and<'a>(&'a mut self, local: &'a mut LocalAssignment) -> Assignment<'a> {
        Assignment {
            global: self,
            local,
        }
    }
}

pub struct LocalAssignment {
    pub perms: LocalPointerTable<PermissionSet>,
    pub flags: LocalPointerTable<FlagSet>,
}

impl LocalAssignment {
    pub fn new(
        len: usize,
        default_perms: PermissionSet,
        default_flags: FlagSet,
    ) -> LocalAssignment {
        LocalAssignment {
            perms: LocalPointerTable::from_raw(vec![default_perms; len]),
            flags: LocalPointerTable::from_raw(vec![default_flags; len]),
        }
    }
}

pub struct Assignment<'a> {
    pub global: &'a mut GlobalAssignment,
    local: &'a mut LocalAssignment,
}

impl Assignment<'_> {
    pub fn perms(&self) -> PointerTable<PermissionSet> {
        self.global.perms.and(&self.local.perms)
    }

    pub fn perms_mut(&mut self) -> PointerTableMut<PermissionSet> {
        self.global.perms.and_mut(&mut self.local.perms)
    }

    pub fn flags(&self) -> PointerTable<FlagSet> {
        self.global.flags.and(&self.local.flags)
    }

    #[allow(dead_code)]
    pub fn _flags_mut(&mut self) -> PointerTableMut<FlagSet> {
        self.global.flags.and_mut(&mut self.local.flags)
    }

    pub fn all_mut(&mut self) -> (PointerTableMut<PermissionSet>, PointerTableMut<FlagSet>) {
        (
            self.global.perms.and_mut(&mut self.local.perms),
            self.global.flags.and_mut(&mut self.local.flags),
        )
    }
}
