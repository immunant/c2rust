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

use std::cmp;
use std::collections::Bound;
use std::collections::BTreeSet;
use std::collections::hash_map::{HashMap, Entry};
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt;
use std::u32;

use arena::DroplessArena;
use rustc::hir;
use rustc::hir::def_id::{DefId, LOCAL_CRATE};
use rustc::mir::*;
use rustc::mir::tcx::LvalueTy;
use rustc::mir::traversal::{Postorder, ReversePostorder};
use rustc::ty::{Ty, TyS, TyCtxt, Instance, TypeVariants, AdtDef};
use rustc::ty::subst::Substs;
use rustc::ty::fold::{TypeVisitor, TypeFoldable};
use rustc_data_structures::bitvec::BitVector;
use rustc_data_structures::indexed_vec::{IndexVec, Idx};
use syntax::ast;
use syntax::codemap::Span;

use analysis::labeled_ty::{LabeledTy, LabeledTyCtxt};
use command::CommandState;
use driver;
use type_map::{self, TypeSource};


pub mod constraint;
mod context;
mod annot;
mod intra;
mod inter;
mod mono;
mod inst;
/*
mod mono_filter;
*/
mod debug;

use self::constraint::*;
use self::context::Ctxt;
use self::annot::{handle_marks, handle_attrs};
use self::intra::IntraCtxt;
use self::inter::InterCtxt;
use self::mono::compute_all_mono_sigs;
use self::inst::find_instantiations;
/*
use self::mono_filter::filter_suspicious_monos;
*/
use self::debug::*;


/// A permission variable reference.
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

/// A type where pointer type constructors are labeled with permission expressions.
// TODO: Ty labels should only ever include the `Perm::*Var` variants.  Make that a different type.
pub type LTy<'tcx> = LabeledTy<'tcx, Option<Perm<'tcx>>>;
type LFnSig<'tcx> = FnSig<'tcx, Option<Perm<'tcx>>>;

/// A generic labeled function signature.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct FnSig<'tcx, L: 'tcx> {
    pub inputs: &'tcx [LabeledTy<'tcx, L>],
    pub output: LabeledTy<'tcx, L>,
}

/// One of the concrete permission values, READ, WRITE, or MOVE.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum ConcretePerm {
    Read,
    Write,
    Move,
}


impl<'tcx, L: fmt::Debug> type_map::Signature<LabeledTy<'tcx, L>> for FnSig<'tcx, L> {
    fn num_inputs(&self) -> usize {
        self.inputs.len()
    }

    fn input(&self, idx: usize) -> LabeledTy<'tcx, L> {
        self.inputs[idx]
    }

    fn output(&self) -> LabeledTy<'tcx, L> {
        self.output
    }
}


/// Check if a definition is a `fn` item of some sort.  Note that this does not return true on
/// closures.
fn is_fn(hir_map: &hir::map::Map, def_id: DefId) -> bool {
    use rustc::hir::map::Node::*;

    let n = match hir_map.get_if_local(def_id) {
        None => return false,
        Some(n) => n,
    };

    match n {
        NodeItem(i) => match i.node {
            hir::ItemFn(..) => true,
            _ => false,
        },
        NodeForeignItem(i) => match i.node {
            hir::ForeignItemFn(..) => true,
            _ => false,
        },
        NodeTraitItem(i) => match i.node {
            hir::TraitItemKind::Method(..) => true,
            _ => false,
        },
        NodeImplItem(i) => match i.node {
            hir::ImplItemKind::Method(..) => true,
            _ => false,
        },
        _ => false,
    }
}

/// Run the intraprocedural step of polymorphic signature inference.  Results are written back into
/// the `Ctxt`.
fn analyze_intra<'a, 'tcx>(cx: &mut Ctxt<'a, 'tcx>,
                           hir_map: &hir::map::Map<'tcx>,
                           tcx: TyCtxt<'a, 'tcx, 'tcx>) {
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

/// Run the interprocedural step of polymorphic signature inference.  Results are written back into
/// the `Ctxt`.
fn analyze_inter(cx: &mut Ctxt) {
    let mut inter_cx = InterCtxt::new(cx);
    inter_cx.process();
    inter_cx.finish();
}

/// Run the analysis.
pub fn analyze<'a, 'tcx>(st: &CommandState,
                         dcx: &driver::Ctxt<'a, 'tcx>)
                         -> AnalysisResult<'tcx> {
    let mut cx = Ctxt::new(dcx.ty_ctxt(), dcx.ty_arena());

    // Process the annotations and marks provided by the user.
    handle_attrs(&mut cx, st, dcx);
    handle_marks(&mut cx, st, dcx);

    // Compute polymorphic signatures / constraint sets for each function
    analyze_intra(&mut cx, dcx.hir_map(), dcx.ty_ctxt());
    analyze_inter(&mut cx);

    // Compute monomorphic signatures and select instantiations in each function
    compute_all_mono_sigs(&mut cx);
    find_instantiations(&mut cx);

    // Convert results to a more usable format.
    convert_results(&cx)
}


/// A type where pointers are labeled with variables.
pub type VTy<'tcx> = LabeledTy<'tcx, Option<Var>>;
/// A signature where pointers are labeled with variables.
pub type VFnSig<'tcx> = FnSig<'tcx, Option<Var>>;

/// A type where pointers are labeled with concrete permissions.
pub type PTy<'tcx> = LabeledTy<'tcx, Option<ConcretePerm>>;
/// A signature where pointers are labeled with concrete permissions.
pub type PFnSig<'tcx> = FnSig<'tcx, Option<ConcretePerm>>;

/// The collected results of running the analysis.
pub struct AnalysisResult<'tcx> {
    /// The permission-labeled type of every non-fn item.  This includes statics, consts, and
    /// struct/enum fields.
    pub statics: HashMap<DefId, PTy<'tcx>>,

    /// Results for to each (analysis-level) function.  Note that only the primary variant of each
    /// variant group will have its `DefId` present in this table - look up `variants[&id].func_id`
    /// first if you aren't sure whether a `fn` is a primary variant.
    pub funcs: HashMap<DefId, FunctionResult<'tcx>>,

    /// Results for to each variant `fn`.  Every `fn` that was analyzed should have an entry
    /// in this table.
    pub variants: HashMap<DefId, VariantResult>,

    /// Results for each monomorphization of each analysis-level function, indexed by function ID
    /// and monomorphization index.
    pub monos: HashMap<(DefId, usize), MonoResult>,
}

/// Results specific to an analysis-level function.
pub struct FunctionResult<'tcx> {
    /// Polymorphic function signature.  Each pointer is labeled with a `SigVar`.
    pub sig: VFnSig<'tcx>,

    pub num_sig_vars: u32,

    /// Constraint set relating `SigVar`s to each other and to concrete permission values.
    pub cset: ConstraintSet<'tcx>,

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

impl<'tcx> AnalysisResult<'tcx> {
    /// Get the function and variant results for a `fn` item-like.
    pub fn fn_results(&self, id: DefId) -> (&FunctionResult<'tcx>, &VariantResult) {
        let vr = &self.variants[&id];
        let fr = &self.funcs[&vr.func_id];
        (fr, vr)
    }
}

/// Extract the useful information from the `Ctxt`, and collect it into an `AnalysisResult`.
fn convert_results<'a, 'tcx>(cx: &Ctxt<'a, 'tcx>) -> AnalysisResult<'tcx> {
    let mut r = AnalysisResult {
        statics: HashMap::new(),
        funcs: HashMap::new(),
        variants: HashMap::new(),
        monos: HashMap::new(),
    };

    // statics

    let mut perm_lcx = LabeledTyCtxt::new(cx.arena);
    for (&def_id, &lty) in cx.static_summ.iter() {
        let pty = perm_lcx.relabel(lty, &mut |p| {
            if let Some(Perm::StaticVar(v)) = *p {
                Some(cx.static_assign[v])
            } else {
                None
            }
        });
        r.statics.insert(def_id, pty);
    }

    // funcs

    let mut var_lcx = LabeledTyCtxt::new(cx.arena);
    for def_id in cx.func_ids() {
        let func = cx.get_func_summ(def_id);

        let sig = {
            let mut f = |p: &Option<_>| {
                if let Some(Perm::SigVar(v)) = *p {
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

        let variant_ids =
            if func.variant_ids.len() == 1 { None }
            else { Some(func.variant_ids.clone()) };

        r.funcs.insert(def_id, FunctionResult {
            sig: sig,
            num_sig_vars: func.num_sig_vars,
            cset: func.sig_cset.clone(),
            variants: variant_ids,
            num_monos: func.num_monos,
        });


        // func variants

        for (idx, &var_id) in func.variant_ids.iter().enumerate() {
            let variant = cx.get_variant_summ(var_id);
            let func_refs = variant.insts.iter().map(|inst| {
                FuncRef {
                    def_id: inst.callee,
                    span: inst.span,
                }
            }).collect();

            r.variants.insert(var_id, VariantResult {
                func_id: def_id,
                index: idx,
                func_refs: func_refs,
            });
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
            for idx in 0 .. func.num_monos {
                let mono = cx.get_mono_summ(def_id, idx);

                let mut max_perm = is_output.iter_enumerated()
                    .filter(|&(_, &out)| out)
                    .map(|(v, _)| mono.assign[v])
                    .max().unwrap_or(ConcretePerm::Read);

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

        for idx in 0 .. func.num_monos {
            let mono = cx.get_mono_summ(def_id, idx);

            let suffix = 
                if func.monos_provided { mono.suffix.clone() }
                else { suffixes[idx].clone() };

            r.monos.insert((def_id, idx), MonoResult {
                suffix: suffix,
                assign: mono.assign.clone(),
                callee_mono_idxs: mono.callee_mono_idxs.clone(),
            });
        }
    }

    r
}

/// Print the analysis results to stderr, for debugging.
pub fn dump_results(dcx: &driver::Ctxt,
                    results: &AnalysisResult) {
    eprintln!("\n === summary ===");

    let arena = DroplessArena::new();
    let mut new_lcx = LabeledTyCtxt::new(&arena);
    let format_sig = |sig: VFnSig, assign: &IndexVec<Var, ConcretePerm>| {
        let mut func = |p: &Option<_>| p.as_ref().map(|&v| assign[v]);

        let inputs = new_lcx.relabel_slice(sig.inputs, &mut func);
        let output = new_lcx.relabel(sig.output, &mut func);
        format!("{:?} -> {:?}", pretty_slice(inputs), Pretty(output))
    };

    let path_str = |def_id| dcx.ty_ctxt().def_path(def_id).to_string(dcx.ty_ctxt());

    let mut ids = results.statics.keys().cloned().collect::<Vec<_>>();
    ids.sort();
    for id in ids {
        let ty = results.statics[&id];
        eprintln!("static {} :: {:?}", path_str(id), Pretty(ty));
    }

    let mut ids = results.funcs.keys().cloned().collect::<Vec<_>>();
    ids.sort();
    for id in ids {
        let fr = &results.funcs[&id];

        eprintln!("func {}:", path_str(id));

        eprintln!("  sig constraints:");
        for &(a, b) in fr.cset.iter() {
            eprintln!("    {:?} <= {:?}", a, b);
        }

        if let Some(ref var_ids) = fr.variants {
            for (i, &var_id) in var_ids.iter().enumerate() {
                eprintln!("  variant {}: {}", i, path_str(var_id));
                let vr = &results.variants[&var_id];

                for (j, func_ref) in vr.func_refs.iter().enumerate() {
                    let callee_fr = &results.funcs[&func_ref.def_id];
                    eprintln!("    call #{}: {:?} :: {:?}",
                              j, path_str(func_ref.def_id), callee_fr.sig);
                    eprintln!("      (at {:?})", func_ref.span);
                }
            }
        } else {
            eprintln!("  single variant");
            let vr = &results.variants[&id];

            for (j, func_ref) in vr.func_refs.iter().enumerate() {
                let callee_fr = &results.funcs[&func_ref.def_id];
                eprintln!("    call #{}: {:?} :: {:?}",
                          j, path_str(func_ref.def_id), callee_fr.sig);
                eprintln!("      (at {:?})", func_ref.span);
            }
        }

        for i in 0 .. fr.num_monos {
            let mr = &results.monos[&(id, i)];

            let var_id = fr.variants.as_ref().map_or(id, |vars| vars[i]);
            let vr = &results.variants[&var_id];

            eprintln!("  mono #{} ({:?}): {}", i, mr.suffix, format_sig(fr.sig, &mr.assign));
            for (j, (func_ref, &mono_idx)) in
                    vr.func_refs.iter().zip(mr.callee_mono_idxs.iter()).enumerate() {
                let callee_fr = &results.funcs[&func_ref.def_id];
                eprintln!("    call #{}: {:?} #{} :: {}",
                          j, path_str(func_ref.def_id), mono_idx,
                          format_sig(callee_fr.sig,
                                     &results.monos[&(func_ref.def_id, mono_idx)].assign));
                eprintln!("      (at {:?})", func_ref.span);
            }
        }
    }
}
