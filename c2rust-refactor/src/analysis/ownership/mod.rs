//! This module contains an analysis to infer ownership information for pointers.  It analyzes code
//! using raw pointers and indicates, for each pointer, whether it appears to be owned, mutably
//! borrowed, or immutably borrowed.  It can also infer ownership-polymorphic function signatures,
//! which handles cases where the original C code used a single accessor for both mutable and
//! immutable access to a field.
//!
//! The analysis operates on constraint sets over "permission variables", which can be take on the
//! concrete permissions "READ", "WRITE", and "MOVE".  The analysis runs in two phases.  First, for
//! each function, it analyzes the function and produces a set of constraints relating variables in
//! the function's signature, variables appearing in static locations (such as struct field types).
//! Since interprocedural information is not available yet, this phase leaves holes where
//! constraints for callee functions can be plugged in.  The second phase fills in holes in
//! function summaries to produce complete summaries that are useful to analysis consumers.  It
//! runs interprocedurally to a fixed point, on each function plugging in the complete summaries of
//! its callees and simplifying to produce a complete summary for the current function.

use std::collections::HashMap;
use std::fmt;
use std::u32;

use arena::SyncDroplessArena;
use log::Level;
use rustc::hir;
use rustc::hir::def_id::{DefId, LOCAL_CRATE};
use rustc::hir::{Mutability, Node};
use rustc::ty::{TyCtxt, TyKind, TypeAndMut, TyS};
use rustc_index::vec::{Idx, IndexVec};
use syntax::ast::IntTy;
use syntax::source_map::Span;

use crate::analysis::labeled_ty::{LabeledTy, LabeledTyCtxt};
use crate::command::CommandState;
use crate::context::HirMap;
use crate::type_map;
use crate::RefactorCtxt;

mod annot;
pub mod constraint;
mod context;
mod inst;
mod inter;
mod intra;
mod mono;
/*
mod mono_filter;
*/
mod debug;

use self::annot::{handle_attrs, handle_marks};
use self::constraint::*;
use self::context::Ctxt;
use self::inst::find_instantiations;
use self::inter::InterCtxt;
use self::intra::IntraCtxt;
use self::mono::compute_all_mono_sigs;
/*
use self::mono_filter::filter_suspicious_monos;
*/
use self::debug::*;

/// A variable index.
///
/// There are multiple kinds of variables using the same index type, so the variable kind must be
/// known by other means to use this effectively.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Var(pub u32);

impl Idx for Var {
    fn new(idx: usize) -> Var {
        assert!(idx as u32 as usize == idx);
        Var(idx as u32)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

impl Var {
    fn next(self) -> Self {
        Var(self.0 + 1)
    }
}

/// A permission variable.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PermVar {
    /// "Static" variables appear in the types of non-function items.  This includes `static` items
    /// as well as `struct`s and other ADTs.  Constraints on static vars are inferred from their
    /// usage inside functions.
    Static(Var),

    /// "Signature" variables appear in the signatures of function items.  Constraints on sig vars
    /// are inferred from the body of the function in question.
    Sig(Var),

    /// "Instantiation" variables appear in the instantiations of function signatures inside other
    /// functions.  They are left intact during the initial summary generation, to be filled in
    /// during a later phase of the analysis.
    Inst(Var),

    /// "Local" variables appear in the types of temporaries.  Constraints on local vars are
    /// produced while analyzing a function, and are simplified away when the function's constraint
    /// generation is done.
    Local(Var),
}

/// A type where pointer type constructors are labeled with permission variables.
pub type LTy<'lty, 'tcx> = LabeledTy<'lty, 'tcx, Option<PermVar>>;
type LFnSig<'lty, 'tcx> = FnSig<'lty, 'tcx, Option<PermVar>>;

/// A generic labeled function signature.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct FnSig<'lty, 'tcx, L: 'lty> {
    pub inputs: &'lty [LabeledTy<'lty, 'tcx, L>],
    pub output: LabeledTy<'lty, 'tcx, L>,
}

/// One of the concrete permission values, READ, WRITE, or MOVE.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum ConcretePerm {
    Read,
    Write,
    Move,
}

impl<'lty, 'tcx, L: fmt::Debug> type_map::Signature<LabeledTy<'lty, 'tcx, L>>
    for FnSig<'lty, 'tcx, L>
{
    fn num_inputs(&self) -> usize {
        self.inputs.len()
    }

    fn input(&self, idx: usize) -> LabeledTy<'lty, 'tcx, L> {
        self.inputs[idx]
    }

    fn output(&self) -> LabeledTy<'lty, 'tcx, L> {
        self.output
    }
}

/// Check if a definition is a `fn` item of some sort.  Note that this does not return true on
/// closures.
fn is_fn(hir_map: &hir::map::Map, def_id: DefId) -> bool {
    let n = match hir_map.get_if_local(def_id) {
        None => return false,
        Some(n) => n,
    };

    match n {
        Node::Item(i) => match i.kind {
            hir::ItemKind::Fn(..) => true,
            _ => false,
        },
        Node::ForeignItem(i) => match i.kind {
            hir::ForeignItemKind::Fn(..) => true,
            _ => false,
        },
        Node::TraitItem(i) => match i.kind {
            hir::TraitItemKind::Method(..) => true,
            _ => false,
        },
        Node::ImplItem(i) => match i.kind {
            hir::ImplItemKind::Method(..) => true,
            _ => false,
        },
        _ => false,
    }
}

/// Run the intraprocedural step of polymorphic signature inference.  Results are written back into
/// the `Ctxt`.
fn analyze_intra<'a, 'tcx, 'lty>(
    cx: &mut Ctxt<'lty, 'tcx>,
    hir_map: &HirMap<'a, 'tcx>,
    tcx: TyCtxt<'tcx>,
) {
    for &def_id in tcx.mir_keys(LOCAL_CRATE).iter() {
        // We currently don't process `static` bodies, even though they do have MIR.
        if !is_fn(hir_map, def_id) {
            continue;
        }

        let mir = tcx.optimized_mir(def_id);

        let mut local_cx = IntraCtxt::new(cx, def_id, mir);
        local_cx.init();

        for (bbid, bb) in mir.basic_blocks().iter_enumerated() {
            local_cx.handle_basic_block(bbid, bb);
        }

        local_cx.finish();
    }
}

/// Add conservative assignments for extern functions that we can't
/// analyze. Results are written back into the first variant for each external
/// function in the `Ctxt`.
fn analyze_externs<'a, 'tcx, 'lty>(cx: &mut Ctxt<'lty, 'tcx>, hir_map: &HirMap<'a, 'tcx>) {
    for (def_id, func_summ) in cx.funcs_mut() {
        if func_summ.cset_provided {
            continue;
        }
        match hir_map.get_if_local(*def_id) {
            Some(Node::ForeignItem(i)) => match i.kind {
                // We only want to consider foreign functions
                hir::ForeignItemKind::Fn(..) => {}
                _ => continue,
            },
            _ => continue,
        }
        for &input in func_summ.sig.inputs {
            if let Some(p) = input.label {
                match input.ty.kind {
                    TyKind::Ref(_, _, Mutability::MutMutable) => {
                        func_summ.sig_cset.add(Perm::Concrete(ConcretePerm::Move), Perm::var(p));
                    }
                    TyKind::RawPtr(TypeAndMut{mutbl: Mutability::MutMutable, ..}) => {
                        func_summ.sig_cset.add(Perm::Concrete(ConcretePerm::Move), Perm::var(p));
                    }
                    _ => {}
                }
            }
        }
        func_summ.cset_provided = true;
    }
}

/// Run the interprocedural step of polymorphic signature inference.  Results are written back into
/// the `Ctxt`.
fn analyze_inter<'lty, 'tcx>(cx: &mut Ctxt<'lty, 'tcx>) {
    let mut inter_cx = InterCtxt::new(cx);
    inter_cx.process();
    inter_cx.finish();
}

fn is_mut_t(ty: &TyS) -> bool {
    if let TyKind::RawPtr(mut_ty) = ty.kind {
        if mut_ty.mutbl == Mutability::MutMutable {
            if let TyKind::Param(param_ty) = mut_ty.ty.kind {
                return param_ty.name.as_str() == "T";
            }
        }
    }

    false
}

/// This function adds permission constraints to builtin functions like ptr.offset()
/// so that ownership analysis can reason about them properly
// TODO: When we want to add more constraints to functions here, we should make this
// more generic
fn register_std_constraints<'a, 'tcx, 'lty>(
    ctxt: &mut Ctxt<'lty, 'tcx>,
    tctxt: TyCtxt<'tcx>,
) {
    for (def_id, func_summ) in ctxt.funcs_mut() {
        let fn_name_path = tctxt.def_path(*def_id).to_string_no_crate();

        // #[ownership_constraints(le(WRITE, _0), le(WRITE, _1), le(_0, _1))]
        // fn offset<T>(self: *mut T, _: isize) -> *mut T;
        if func_summ.sig.inputs.len() == 2 && fn_name_path == "::ptr[0]::{{impl}}[1]::offset[0]" {
            let param0_is_mut_t = is_mut_t(func_summ.sig.inputs[0].ty);
            let param1_is_isize = if let TyKind::Int(int_ty) = func_summ.sig.inputs[1].ty.kind {
                int_ty == IntTy::Isize
            } else {
                false
            };
            let ret_is_mut_t = is_mut_t(func_summ.sig.output.ty);
            if param0_is_mut_t && param1_is_isize && ret_is_mut_t {
                func_summ.cset_provided = true;
                func_summ.sig_cset.add(Perm::SigVar(Var(1)), Perm::SigVar(Var(0)));
            }
        }
    }
}

/// Run the analysis.
pub fn analyze<'lty, 'a: 'lty, 'tcx: 'a>(
    st: &CommandState,
    dcx: &RefactorCtxt<'a, 'tcx>,
    arena: &'lty SyncDroplessArena,
) -> AnalysisResult<'lty, 'tcx> {
    let mut cx = Ctxt::new(dcx.ty_ctxt(), arena);

    // Process the annotations and marks provided by the user.
    handle_attrs(&mut cx, st, dcx);
    handle_marks(&mut cx, st, dcx);

    // Compute polymorphic signatures / constraint sets for each function
    analyze_intra(&mut cx, &dcx.hir_map(), dcx.ty_ctxt());
    // Add constraints for extern functions
    analyze_externs(&mut cx, &dcx.hir_map());
    // Inject constraints for std functions
    register_std_constraints(&mut cx, dcx.ty_ctxt());
    analyze_inter(&mut cx);

    // Compute monomorphic signatures and select instantiations in each function
    compute_all_mono_sigs(&mut cx);
    find_instantiations(&mut cx);

    // Convert results to a more usable format.
    cx.into()
}

/// A type where pointers are labeled with variables.
pub type VTy<'lty, 'tcx> = LabeledTy<'lty, 'tcx, Option<Var>>;
/// A signature where pointers are labeled with variables.
pub type VFnSig<'lty, 'tcx> = FnSig<'lty, 'tcx, Option<Var>>;

/// A type where pointers are labeled with concrete permissions.
pub type PTy<'lty, 'tcx> = LabeledTy<'lty, 'tcx, Option<ConcretePerm>>;
/// A signature where pointers are labeled with concrete permissions.
pub type PFnSig<'lty, 'tcx> = FnSig<'lty, 'tcx, Option<ConcretePerm>>;

/// The collected results of running the analysis.
pub struct AnalysisResult<'lty, 'tcx> {
    /// The permission-labeled type of every non-fn item.  This includes statics, consts, and
    /// struct/enum fields.
    pub statics: HashMap<DefId, PTy<'lty, 'tcx>>,

    /// Results for to each (analysis-level) function.  Note that only the primary variant of each
    /// variant group will have its `DefId` present in this table - look up `variants[&id].func_id`
    /// first if you aren't sure whether a `fn` is a primary variant.
    pub funcs: HashMap<DefId, FunctionResult<'lty, 'tcx>>,

    /// Results for to each variant `fn`.  Every `fn` that was analyzed should have an entry
    /// in this table.
    pub variants: HashMap<DefId, VariantResult>,

    /// Results for each monomorphization of each analysis-level function, indexed by function ID
    /// and monomorphization index.
    pub monos: HashMap<(DefId, usize), MonoResult>,

    /// Arena used to allocate all type wrappers
    arena: &'lty SyncDroplessArena,
}

/// Results specific to an analysis-level function.
#[derive(Debug)]
pub struct FunctionResult<'lty, 'tcx: 'lty> {
    /// Polymorphic function signature.  Each pointer is labeled with a `SigVar`.
    pub sig: VFnSig<'lty, 'tcx>,

    /// Mapping of local pat spans to VTys
    pub locals: HashMap<Span, VTy<'lty, 'tcx>>,

    /// Mapping of local vars to concrete permissions
    pub local_assign: IndexVec<Var, ConcretePerm>,

    pub num_sig_vars: u32,

    /// Constraint set relating `SigVar`s to each other and to concrete permission values.
    pub cset: ConstraintSet<'lty>,

    /// List of variant IDs, for multi-variant functions.  If the function has only a single
    /// variant, this field is `None` and the variant's ID is the same as the function's ID.
    pub variants: Option<Vec<DefId>>,

    /// Number of monomorphizations.  If `self.variants` is not `None`, `num_monos` is equal to the
    /// length of that `Vec`.
    pub num_monos: usize,
}

/// Results specific to a variant `fn`.
///
/// Each variant has a parent `FunctionResult`, identified by the `func_id` field.
#[derive(Debug)]
pub struct VariantResult {
    /// ID of the parent function.
    pub func_id: DefId,

    /// Index of this variant within the parent.  If the parent function has multiple variants,
    /// this is also the index of the monomorphization corresponding to this variant.
    pub index: usize,

    /// All references to other functions that appear in this `fn`.  Usually these are function
    /// calls, but they also occur when a function's address is taken.
    pub func_refs: Vec<FuncRef>,
}

/// A reference to a function.
#[derive(Debug)]
pub struct FuncRef {
    /// Function ID of the callee.  Note this refers to an analysis-level function, even if the
    /// `Expr` in the AST refers to a specific variant.
    pub def_id: DefId,

    /// The location of the reference to this function.  If available, this span will point to a
    /// `Path` expression for the function name, a `MethodCall` expression, or an expression that
    /// invokes an overloaded operator.
    pub span: Option<Span>,
}

/// Results specific to a function monomorphization.
///
/// Each monomorphization has a parent `FunctionResult` and a parent `VariantResult`.  If the
/// function's `variants` field is `None`, then the ID of the variant is the same as the function
/// ID.  Otherwise, the variant ID is found by indexing into `variants` with the index of this
/// monomorphization.
#[derive(Debug)]
pub struct MonoResult {
    /// Suffix to add to the function name when this monomorphization is split into its own `fn`.
    /// Usually something like `"mut"` or `"take"`.  If empty, the original name should be used.
    pub suffix: String,

    /// Assignment of concrete permission values to the signature variables of the function.  All
    /// monomorphizations use the `sig` that appears in their parent `FunctionResult`, but each has
    /// a different assignments to the `SigVar`s.
    ///
    /// The length of `assign` should be equal to the parent `FunctionResult`'s `num_sig_vars`.
    pub assign: IndexVec<Var, ConcretePerm>,

    /// Index of the chosen monomorphization for each function reference.  These entries correspond
    /// to those in the parent `VariantResult`'s `func_refs` field.
    pub callee_mono_idxs: Vec<usize>,
}

impl<'lty, 'tcx> AnalysisResult<'lty, 'tcx> {
    /// Get the function and variant results for a `fn` item-like.
    pub fn fn_results(&self, id: DefId) -> (&FunctionResult<'lty, 'tcx>, &VariantResult) {
        let vr = &self.variants[&id];
        let fr = &self.funcs[&vr.func_id];
        (fr, vr)
    }

    pub fn arena(&self) -> &'lty SyncDroplessArena {
        self.arena
    }
}

impl<'lty, 'tcx> From<Ctxt<'lty, 'tcx>> for AnalysisResult<'lty, 'tcx> {
    /// Extract the useful information from the `Ctxt`, and collect it into an `AnalysisResult`.
    fn from(cx: Ctxt<'lty, 'tcx>) -> AnalysisResult<'lty, 'tcx> {
        let mut statics = HashMap::new();
        let mut funcs = HashMap::new();
        let mut variants = HashMap::new();
        let mut monos = HashMap::new();

        // statics

        let perm_lcx = LabeledTyCtxt::new(cx.arena);
        for (&def_id, &lty) in cx.static_summ.iter() {
            let pty = perm_lcx.relabel(lty, &mut |p| {
                if let Some(PermVar::Static(v)) = *p {
                    Some(cx.static_assign[v])
                } else {
                    None
                }
            });
            statics.insert(def_id, pty);
        }

        // funcs

        let var_lcx = LabeledTyCtxt::new(cx.arena);
        for def_id in cx.func_ids() {
            let func = cx.get_func_summ(def_id);

            let sig = {
                let mut f = |p: &Option<_>| {
                    if let Some(PermVar::Sig(v)) = *p {
                        Some(v)
                    } else {
                        None
                    }
                };
                FnSig {
                    inputs: var_lcx.relabel_slice(func.sig.inputs, &mut f),
                    output: var_lcx.relabel(func.sig.output, &mut f),
                }
            };

            let variant_ids = if func.variant_ids.len() == 1 {
                None
            } else {
                Some(func.variant_ids.clone())
            };

            // LTy -> VTy
            let mut f = |p: &Option<PermVar>| -> Option<Var> {
                if let Some(PermVar::Local(v)) = *p {
                    Some(v)
                } else {
                    None
                }
            };

            let locals = func.locals
                .iter()
                .map(|(&span, lty)| (span, var_lcx.relabel(&lty, &mut f)))
                .collect();

            funcs.insert(
                def_id,
                FunctionResult {
                    sig,
                    locals,
                    num_sig_vars: func.num_sig_vars,
                    cset: func.sig_cset.clone(),
                    variants: variant_ids,
                    num_monos: func.num_monos,
                    local_assign: func.local_assign.clone(),
                },
            );

            // func variants

            for (idx, &var_id) in func.variant_ids.iter().enumerate() {
                let variant = cx.get_variant_summ(var_id);
                let func_refs = variant
                    .insts
                    .iter()
                    .map(|inst| FuncRef {
                        def_id: inst.callee,
                        span: inst.span,
                    })
                    .collect();

                variants.insert(
                    var_id,
                    VariantResult {
                        func_id: def_id,
                        index: idx,
                        func_refs: func_refs,
                    },
                );
            }

            // func monos

            // Assign suffixes if not provided.
            let mut suffixes = Vec::new();
            if func.monos_provided {
                // Do nothing. If monos were provided, we'll use their provided names.
            } else if func.num_monos == 1 {
                // Use the original name.
                suffixes.push(String::new());
            } else {
                /// Default suffixes corresponding to the three concrete permissions.
                static SUFFIX_BASE: [&'static str; 3] = ["", "mut", "take"];
                // If more than one mono tries to use the same default suffix, we need to append a
                // number to disambiguate.
                let mut suffix_count = [0, 0, 0];

                let is_output = mono::infer_outputs(func);

                // Guess a suffix for each mono depending on its output types.  Automatic suffixes look
                // like "", "mut", "take", "2", "mut3", "take4", etc.
                for idx in 0..func.num_monos {
                    let mono = cx.get_mono_summ(def_id, idx);

                    let max_perm = is_output
                        .iter_enumerated()
                        .filter(|&(_, &out)| out)
                        .map(|(v, _)| mono.assign[v])
                        .max()
                        .unwrap_or(ConcretePerm::Read);

                    let idx = max_perm as usize;
                    suffix_count[idx] += 1;
                    let suffix = if suffix_count[idx] == 1 {
                        SUFFIX_BASE[idx].to_owned()
                    } else {
                        format!("{}{}", SUFFIX_BASE[idx], suffix_count[idx])
                    };
                    suffixes.push(suffix);
                }
            }

            for idx in 0..func.num_monos {
                let mono = cx.get_mono_summ(def_id, idx);

                let suffix = if func.monos_provided {
                    mono.suffix.clone()
                } else {
                    suffixes[idx].clone()
                };

                monos.insert(
                    (def_id, idx),
                    MonoResult {
                        suffix,
                        assign: mono.assign.clone(),
                        callee_mono_idxs: mono.callee_mono_idxs.clone(),
                    },
                );
            }
        }

        let Ctxt { arena, .. } = cx;

        AnalysisResult {
            statics,
            funcs,
            variants,
            monos,
            arena,
        }
    }
}

/// Print the analysis results to stderr, for debugging.
pub fn dump_results(dcx: &RefactorCtxt, results: &AnalysisResult) {
    debug!("\n === summary ===");

    let arena = SyncDroplessArena::default();
    let new_lcx = LabeledTyCtxt::new(&arena);
    let format_sig = |sig: VFnSig, assign: &IndexVec<Var, ConcretePerm>| {
        let mut func = |p: &Option<_>| p.as_ref().map(|&v| assign[v]);

        let inputs = new_lcx.relabel_slice(sig.inputs, &mut func);
        let output = new_lcx.relabel(sig.output, &mut func);
        format!("{:?} -> {:?}", pretty_slice(inputs), Pretty(output))
    };

    let path_str = |def_id| dcx.ty_ctxt().def_path(def_id).to_string_no_crate();

    let mut ids = results.statics.keys().cloned().collect::<Vec<_>>();
    ids.sort();
    for id in ids {
        let ty = results.statics[&id];
        debug!("static {} :: {:?}", path_str(id), Pretty(ty));
    }

    let mut ids = results.funcs.keys().cloned().collect::<Vec<_>>();
    ids.sort();
    for id in ids {
        let fr = &results.funcs[&id];

        debug!("func {}:", path_str(id));

        debug!("  sig constraints:");
        if log_enabled!(Level::Debug) {
            for &(a, b) in fr.cset.iter() {
                debug!("    {:?} <= {:?}", a, b);
            }
        }

        if let Some(ref var_ids) = fr.variants {
            for (i, &var_id) in var_ids.iter().enumerate() {
                debug!("  variant {}: {}", i, path_str(var_id));
                let vr = &results.variants[&var_id];

                for (j, func_ref) in vr.func_refs.iter().enumerate() {
                    let callee_fr = &results.funcs[&func_ref.def_id];
                    debug!(
                        "    call #{}: {:?} :: {:?}",
                        j,
                        path_str(func_ref.def_id),
                        callee_fr.sig
                    );
                    debug!("      (at {:?})", func_ref.span);
                }
            }
        } else {
            debug!("  single variant");
            let vr = &results.variants[&id];

            for (j, func_ref) in vr.func_refs.iter().enumerate() {
                let callee_fr = &results.funcs[&func_ref.def_id];
                debug!(
                    "    call #{}: {:?} :: {:?}",
                    j,
                    path_str(func_ref.def_id),
                    callee_fr.sig
                );
                debug!("      (at {:?})", func_ref.span);
            }
        }

        for i in 0..fr.num_monos {
            let mr = &results.monos[&(id, i)];

            let var_id = fr.variants.as_ref().map_or(id, |vars| vars[i]);
            let vr = &results.variants[&var_id];

            debug!(
                "  mono #{} ({:?}): {}",
                i,
                mr.suffix,
                format_sig(fr.sig, &mr.assign)
            );
            for (j, (func_ref, &mono_idx)) in vr
                .func_refs
                .iter()
                .zip(mr.callee_mono_idxs.iter())
                .enumerate()
            {
                let callee_fr = &results.funcs[&func_ref.def_id];
                debug!(
                    "    call #{}: {:?} #{} :: {}",
                    j,
                    path_str(func_ref.def_id),
                    mono_idx,
                    format_sig(
                        callee_fr.sig,
                        &results.monos[&(func_ref.def_id, mono_idx)].assign
                    )
                );
                debug!("      (at {:?})", func_ref.span);
            }
        }
    }
}
