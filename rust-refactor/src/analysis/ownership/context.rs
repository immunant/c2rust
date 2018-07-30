//! The global analysis context.
//!
//!
//! # Functions, variants, and monomorphizations
//!
//! A function can have one or more variants.  If it has one variant, it may have multiple
//! monomorphizations, and each monomorphization belongs to the sole variant.  If it has multiple
//! variants, then it has exactly as many monos as it has variants, and the n'th mono belongs to
//! the n'th variant.
//!
//!
//! # Summary initialization
//!
//! Summaries for functions and variants in the current crate are created by `annot`, which is
//! aware of `#[ownership_variant_of]` annotations and sets up summaries accordingly.  Other
//! summaries are created on-demand by the `func_summ` and `variant_summ` methods.
//!
//! Summaries for monomorphizations are created either by `annot` (upon encountering an
//! `#[ownership_mono]` attr) or by `mono`, which creates mono summaries for all variants that
//! don't currently have them.  Mono summaries aren't created on-demand because we never query a
//! mono that might not exist.

use std::collections::hash_map::{self, HashMap, Entry};

use arena::SyncDroplessArena;
use rustc::hir::def_id::DefId;
use rustc::ty::{Ty, TyCtxt, TypeVariants};
use rustc_data_structures::indexed_vec::IndexVec;
use syntax::codemap::Span;

use analysis::labeled_ty::LabeledTyCtxt;

use super::{Var, FnSig, LTy, LFnSig, ConcretePerm, PermVar};
use super::constraint::ConstraintSet;
use super::constraint::Perm;


// The following structures describe the functions, variants, and monomorphizations relevant to the
// analysis.  These structures generally start out minimally initialized, and are populated as
// parts of the analysis runs.

pub struct FuncSumm<'tcx> {
    pub sig: LFnSig<'tcx>,
    pub num_sig_vars: u32,

    /// Constraints over signature variables only.
    ///
    /// Populated by `inter`.  May be initialized early by `annot` if an `#[ownership_constraints]`
    /// attr is present on one of the variants - in this case, `cset_provided` will be set.
    pub sig_cset: ConstraintSet<'tcx>,

    /// Was the constraint set provided externally?  (If so, we don't process this function during
    /// `inter`.)
    pub cset_provided: bool,

    pub monos_provided: bool,

    pub variant_ids: Vec<DefId>,
    pub num_monos: usize,
}

pub struct VariantSumm<'tcx> {
    pub func_id: DefId,
    pub variant_idx: usize,

    /// Constraints over static, instantiation (callee), and signature variables.
    ///
    /// Populated by `intra`.
    pub inst_cset: ConstraintSet<'tcx>,

    /// List of instantiations, or references to functions.
    ///
    /// Populated by `intra`.
    pub insts: Vec<Instantiation>,
}

pub struct MonoSumm {
    /// Assignment of concrete permissions to sig vars that produces this monomorphization.
    ///
    /// Populated by `mono` when the `MonoSumm` is created.  If `#[ownership_mono]` attrs are
    /// present on the variant, then the `MonoSumm` may be created (and this field populated)
    /// during `annot`.
    pub assign: IndexVec<Var, ConcretePerm>,

    /// The index of the chosen (callee) monomorphization for each callee in `insts`.
    ///
    /// Populated by `inst`.
    pub callee_mono_idxs: Vec<usize>,

    pub suffix: String,
}

pub struct Instantiation {
    pub callee: DefId,
    pub span: Option<Span>,
    pub first_inst_var: u32,
}


pub struct Ctxt<'a, 'tcx: 'a> {
    pub tcx: TyCtxt<'a, 'tcx, 'tcx>,
    pub lcx: LabeledTyCtxt<'tcx, Option<PermVar>>,
    pub arena: &'tcx SyncDroplessArena,

    /// Types of non-`fn` definitions.  This includes `static`s and also `struct` fields.
    pub static_summ: HashMap<DefId, LTy<'tcx>>,

    /// Assignment of permission values to static vars.  This is only here because this is a
    /// convenient way to communicate it from `annot` to `inter`.
    pub static_assign: IndexVec<Var, ConcretePerm>,

    funcs: HashMap<DefId, FuncSumm<'tcx>>,
    variants: HashMap<DefId, VariantSumm<'tcx>>,
    monos: HashMap<(DefId, usize), MonoSumm>,
}

impl<'a, 'tcx> Ctxt<'a, 'tcx> {
    pub fn new(tcx: TyCtxt<'a, 'tcx, 'tcx>,
               arena: &'tcx SyncDroplessArena) -> Ctxt<'a, 'tcx> {
        Ctxt {
            tcx: tcx,
            lcx: LabeledTyCtxt::new(arena),
            arena: arena,

            static_summ: HashMap::new(),
            static_assign: IndexVec::new(),

            funcs: HashMap::new(),
            variants: HashMap::new(),
            monos: HashMap::new(),
        }
    }

    pub fn static_ty(&mut self, did: DefId) -> LTy<'tcx> {
        let assign = &mut self.static_assign;
        match self.static_summ.entry(did) {
            Entry::Vacant(e) => {
                *e.insert(self.lcx.label(self.tcx.type_of(did), &mut |ty| {
                    match ty.sty {
                        TypeVariants::TyRef(_, _, _) |
                        TypeVariants::TyRawPtr(_) => {
                            let v = assign.push(ConcretePerm::Read);
                            Some(PermVar::Static(v))
                        },
                        _ => None,
                    }
                }))
            },

            Entry::Occupied(e) => *e.get(),
        }
    }

    fn func_summ_impl<'b>(funcs: &'b mut HashMap<DefId, FuncSumm<'tcx>>,
                          variants: &mut HashMap<DefId, VariantSumm<'tcx>>,
                          tcx: TyCtxt<'a, 'tcx, 'tcx>,
                          lcx: &mut LabeledTyCtxt<'tcx, Option<PermVar>>,
                          did: DefId) -> &'b mut FuncSumm<'tcx> {
        match funcs.entry(did) {
            Entry::Vacant(e) => {
                assert!(!variants.contains_key(&did),
                        "tried to create func summ for {:?}, which is already a variant");
                let sig = tcx.fn_sig(did);
                let mut counter = 0;

                let l_sig = {
                    let mut f = |ty: Ty<'tcx>| {
                        match ty.sty {
                            TypeVariants::TyRef(_, _, _) |
                            TypeVariants::TyRawPtr(_) => {
                                let v = Var(counter);
                                counter += 1;
                                Some(PermVar::Sig(v))
                            },
                            _ => None,
                        }
                    };

                    FnSig {
                        inputs: lcx.label_slice(sig.skip_binder().inputs(), &mut f),
                        output: lcx.label(sig.skip_binder().output(), &mut f),
                    }
                };

                let (cset, provided) =
                    if let Some(cset) = preload_constraints(tcx, did, l_sig) {
                        (cset, true)
                    } else {
                        (ConstraintSet::new(), false)
                    };

                variants.insert(did, VariantSumm {
                    func_id: did,
                    variant_idx: 0,
                    inst_cset: ConstraintSet::new(),
                    insts: Vec::new(),
                });

                e.insert(FuncSumm {
                    sig: l_sig,
                    num_sig_vars: counter,
                    sig_cset: cset,
                    cset_provided: provided,
                    monos_provided: false,

                    variant_ids: vec![did],
                    num_monos: 0,
                })
            },

            Entry::Occupied(e) => e.into_mut(),
        }
    }

    pub fn func_summ(&mut self, did: DefId) -> &mut FuncSumm<'tcx> {
        Self::func_summ_impl(&mut self.funcs,
                             &mut self.variants,
                             self.tcx,
                             &mut self.lcx,
                             did)
    }

    pub fn get_func_summ(&self, did: DefId) -> &FuncSumm<'tcx> {
        self.funcs.get(&did).unwrap()
    }

    pub fn func_ids<'b>(&'b self) -> FuncIds<'b, 'tcx> {
        FuncIds {
            inner: self.funcs.keys(),
        }
    }

    pub fn variant_ids<'b>(&'b self) -> VariantIds<'b, 'tcx> {
        VariantIds {
            inner: self.variants.keys(),
        }
    }

    pub fn mono_ids<'b>(&'b self) -> MonoIds<'b> {
        MonoIds {
            inner: self.monos.keys(),
        }
    }

    fn add_variant_impl<'b>(funcs: &'b mut HashMap<DefId, FuncSumm<'tcx>>,
                            variants: &'b mut HashMap<DefId, VariantSumm<'tcx>>,
                            tcx: TyCtxt<'a, 'tcx, 'tcx>,
                            lcx: &mut LabeledTyCtxt<'tcx, Option<PermVar>>,
                            func_did: DefId,
                            variant_did: DefId)
                            -> (&'b mut FuncSumm<'tcx>,
                                &'b mut VariantSumm<'tcx>) {
        let func = Self::func_summ_impl(funcs,
                                        variants,
                                        tcx,
                                        lcx,
                                        func_did);
        if variants.contains_key(&variant_did) {
            // The variant already existed, or was just created by `func_summ_impl`.
            let variant = variants.get_mut(&variant_did).unwrap();
            return (func, variant);
        }

        let v_idx = func.variant_ids.len();
        func.variant_ids.push(variant_did);

        variants.insert(variant_did, VariantSumm {
            func_id: func_did,
            variant_idx: v_idx,
            inst_cset: ConstraintSet::new(),
            insts: Vec::new(),
        });
        let variant = variants.get_mut(&variant_did).unwrap();
        (func, variant)
    }

    pub fn add_variant(&mut self, func_did: DefId, variant_id: DefId)
                       -> (&mut FuncSumm<'tcx>,
                           &mut VariantSumm<'tcx>) {
        Self::add_variant_impl(&mut self.funcs,
                               &mut self.variants,
                               self.tcx,
                               &mut self.lcx,
                               func_did,
                               variant_id)
    }

    pub fn add_mono(&mut self, variant_did: DefId)
                    -> (&mut FuncSumm<'tcx>,
                        &mut VariantSumm<'tcx>,
                        &mut MonoSumm) {
        let (func, variant) = Self::variant_summ_impl(&mut self.funcs,
                                                      &mut self.variants,
                                                      self.tcx,
                                                      &mut self.lcx,
                                                      variant_did);
        let m_idx = func.num_monos;
        func.num_monos += 1;

        self.monos.insert((variant.func_id, m_idx), MonoSumm {
            assign: IndexVec::new(),
            callee_mono_idxs: Vec::new(),
            suffix: String::new(),
        });
        let mono = self.monos.get_mut(&(variant.func_id, m_idx)).unwrap();

        (func, variant, mono)
    }

    fn variant_summ_impl<'b>(funcs: &'b mut HashMap<DefId, FuncSumm<'tcx>>,
                             variants: &'b mut HashMap<DefId, VariantSumm<'tcx>>,
                             tcx: TyCtxt<'a, 'tcx, 'tcx>,
                             lcx: &mut LabeledTyCtxt<'tcx, Option<PermVar>>,
                             variant_did: DefId)
                            -> (&'b mut FuncSumm<'tcx>,
                                &'b mut VariantSumm<'tcx>) {
        if variants.contains_key(&variant_did) {
            let variant = variants.get_mut(&variant_did).unwrap();
            let func = funcs.get_mut(&variant.func_id).unwrap();
            (func, variant)
        } else {
            Self::add_variant_impl(funcs, variants, tcx, lcx, variant_did, variant_did)
        }
    }

    /// Get the variant and function summaries for a `fn`.  The summaries will be created if they
    /// don't already exist.
    pub fn variant_summ(&mut self, variant_did: DefId)
                        -> (&mut FuncSumm<'tcx>, &mut VariantSumm<'tcx>) {
        Self::variant_summ_impl(&mut self.funcs,
                                &mut self.variants,
                                self.tcx,
                                &mut self.lcx,
                                variant_did)
    }

    pub fn get_variant_summ(&self, did: DefId) -> &VariantSumm<'tcx> {
        &self.variants[&did]
    }

    pub fn variant_func_sig(&mut self, variant_did: DefId) -> LFnSig<'tcx> {
        self.variant_summ(variant_did).0.sig
    }

    pub fn first_variant_summ(&mut self, func_did: DefId)
                              -> (&mut FuncSumm<'tcx>,
                                  &mut VariantSumm<'tcx>) {
        let func = self.funcs.get_mut(&func_did).unwrap();
        let variant = self.variants.get_mut(&func.variant_ids[0]).unwrap();
        (func, variant)
    }

    pub fn mono_summ(&mut self, func_did: DefId, mono_idx: usize)
                     -> (&mut FuncSumm<'tcx>,
                         &mut VariantSumm<'tcx>,
                         &mut MonoSumm) {
        let func = self.funcs.get_mut(&func_did).unwrap();

        let variant = if func.variant_ids.len() == 1 {
            self.variants.get_mut(&func.variant_ids[0]).unwrap()
        } else {
            self.variants.get_mut(&func.variant_ids[mono_idx]).unwrap()
        };

        let mono = self.monos.get_mut(&(func_did, mono_idx)).unwrap();

        (func, variant, mono)
    }

    pub fn get_mono_summ(&self, func_did: DefId, idx: usize) -> &MonoSumm {
        &self.monos[&(func_did, idx)]
    }

    /// Get the variant summary for the variant that owns the given mono.
    pub fn get_mono_variant_summ(&self, func_did: DefId, mono_idx: usize) -> &VariantSumm<'tcx> {
        let func = self.get_func_summ(func_did);

        if func.variant_ids.len() == 1 {
            self.get_variant_summ(func.variant_ids[0])
        } else {
            self.get_variant_summ(func.variant_ids[mono_idx])
        }
    }


    pub fn min_perm(&mut self, a: Perm<'tcx>, b: Perm<'tcx>) -> Perm<'tcx> {
        Perm::min(a, b, self.arena)
    }
}

fn preload_constraints<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>,
                                 def_id: DefId,
                                 sig: LFnSig<'tcx>) -> Option<ConstraintSet<'tcx>> {
    let mut cset = ConstraintSet::new();

    let path = tcx.absolute_item_path_str(def_id);
    match &path as &str {
        "core::ptr::<impl *const T>::offset" |
        "core::ptr::<impl *mut T>::offset" => {
            cset.add(Perm::var(sig.output.label.unwrap()),
                     Perm::var(sig.inputs[0].label.unwrap()));
        },

        _ => return None,
    }

    eprintln!("PRELOAD CONSTRAINTS for {:?}", def_id);
    eprintln!("  {:?} -> {:?}", sig.inputs, sig.output);
    for &(a, b) in cset.iter() {
        eprintln!("    {:?} <= {:?}", a, b);
    }

    Some(cset)
}

pub struct FuncIds<'a, 'tcx: 'a> {
    inner: hash_map::Keys<'a, DefId, FuncSumm<'tcx>>,
}

impl<'a, 'tcx> Iterator for FuncIds<'a, 'tcx> {
    type Item = DefId;

    fn next(&mut self) -> Option<DefId> {
        self.inner.next().map(|&x| x)
    }
}


pub struct VariantIds<'a, 'tcx: 'a> {
    inner: hash_map::Keys<'a, DefId, VariantSumm<'tcx>>,
}

impl<'a, 'tcx> Iterator for VariantIds<'a, 'tcx> {
    type Item = DefId;

    fn next(&mut self) -> Option<DefId> {
        self.inner.next().map(|&x| x)
    }
}


pub struct MonoIds<'a> {
    inner: hash_map::Keys<'a, (DefId, usize), MonoSumm>,
}

impl<'a> Iterator for MonoIds<'a> {
    type Item = (DefId, usize);

    fn next(&mut self) -> Option<(DefId, usize)> {
        self.inner.next().map(|&x| x)
    }
}

