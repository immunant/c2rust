//! The global analysis context.
use std::collections::hash_map::{self, HashMap, Entry};

use arena::DroplessArena;
use rustc::hir::def_id::DefId;
use rustc::ty::{Ty, TyCtxt, TypeVariants};
use rustc_data_structures::indexed_vec::IndexVec;

use analysis::labeled_ty::LabeledTyCtxt;

use super::{Var, LTy, LFnSig, ConcretePerm};
use super::{FnSummary, Instantiation};
use super::constraint::ConstraintSet;
use super::constraint::Perm;


pub struct Ctxt<'tcx> {
    pub lcx: LabeledTyCtxt<'tcx, Option<Perm<'tcx>>>,
    pub arena: &'tcx DroplessArena,

    /// Types of non-`fn` definitions.  This includes `static`s and also `struct` fields.
    static_summ: HashMap<DefId, LTy<'tcx>>,

    /// Assignment of permission values to static vars.  This is only here because this is a
    /// convenient way to communicate it from `annot` to `inter`.
    pub static_assign: IndexVec<Var, ConcretePerm>,

    fn_summ: HashMap<DefId, FnSummary<'tcx>>,

    /// Cache of labeled tys generated while processing function bodies.  We may process the same
    /// function multiple times, and it would be nice to avoid allocating a new bunch of `LTy`s
    /// each time around.  In this map, `fn_ty_cache[(did, i)]` is the labeled type produced while
    /// processing `did` when the "next local variable" counter was `i`.  This works as long as
    /// function processing visits MIR nodes in the same order and requests the same types each
    /// time.
    ///
    /// The `u32` in the value is the amount to advance `next_local_var` by, after retrieving a
    /// type from the cache.
    fn_ty_cache: HashMap<(DefId, u32), (LTy<'tcx>, u32)>,
}

impl<'tcx> Ctxt<'tcx> {
    pub fn new(arena: &'tcx DroplessArena) -> Ctxt<'tcx> {
        Ctxt {
            lcx: LabeledTyCtxt::new(arena),
            arena: arena,

            static_summ: HashMap::new(),
            static_assign: IndexVec::new(),

            fn_summ: HashMap::new(),
            fn_ty_cache: HashMap::new(),
        }
    }

    pub fn static_ty<'a, 'gcx>(&mut self, did: DefId, tcx: TyCtxt<'a, 'gcx, 'tcx>) -> LTy<'tcx> {
        let assign = &mut self.static_assign;
        match self.static_summ.entry(did) {
            Entry::Vacant(e) => {
                *e.insert(self.lcx.label(tcx.type_of(did), &mut |ty| {
                    match ty.sty {
                        TypeVariants::TyRef(_, _) |
                        TypeVariants::TyRawPtr(_) => {
                            let v = assign.push(ConcretePerm::Read);
                            Some(Perm::StaticVar(v))
                        },
                        _ => None,
                    }
                }))
            },

            Entry::Occupied(e) => *e.get(),
        }
    }

    pub fn fn_summ<'a, 'gcx>(&mut self,
                             did: DefId,
                             tcx: TyCtxt<'a, 'gcx, 'tcx>) -> &mut FnSummary<'tcx> {
        match self.fn_summ.entry(did) {
            Entry::Vacant(e) => {
                let sig = tcx.fn_sig(did);
                let mut counter = 0;

                let l_sig = {
                    let mut f = |ty: Ty<'tcx>| {
                        match ty.sty {
                            TypeVariants::TyRef(_, _) |
                            TypeVariants::TyRawPtr(_) => {
                                let v = Var(counter);
                                counter += 1;
                                Some(Perm::SigVar(v))
                            },
                            _ => None,
                        }
                    };

                    LFnSig {
                        inputs: self.lcx.label_slice(sig.0.inputs(), &mut f),
                        output: self.lcx.label(sig.0.output(), &mut f),
                    }
                };

                let cset = preload_constraints(tcx, did, l_sig)
                    .unwrap_or_else(ConstraintSet::new);

                e.insert(FnSummary {
                    sig: l_sig,
                    num_sig_vars: counter,
                    cset: cset,
                    inst_cset: ConstraintSet::new(),
                    insts: Vec::new(),
                })
            },

            Entry::Occupied(e) => e.into_mut(),
        }
    }

    pub fn get_fn_summ_imm(&self, did: DefId) -> Option<&FnSummary<'tcx>> {
        self.fn_summ.get(&did)
    }

    pub fn get_fn_summ(&mut self, did: DefId) -> Option<&mut FnSummary<'tcx>> {
        self.fn_summ.get_mut(&did)
    }

    pub fn fn_ids<'a>(&'a self) -> FnIds<'a, 'tcx> {
        FnIds {
            inner: self.fn_summ.keys(),
        }
    }

    pub fn fn_sig<'a, 'gcx>(&mut self, did: DefId, tcx: TyCtxt<'a, 'gcx, 'tcx>) -> LFnSig<'tcx> {
        self.fn_summ(did, tcx).sig
    }

    pub fn local_ty(&mut self, did: DefId, next_local: &mut u32, ty: Ty<'tcx>) -> LTy<'tcx> {
        eprintln!("local_ty key = {:?}, {}", did, *next_local);
        match self.fn_ty_cache.entry((did, *next_local)) {
            Entry::Vacant(e) => {
                let first_local = *next_local;
                let lty = self.lcx.label(ty, &mut |ty| {
                    match ty.sty {
                        TypeVariants::TyRef(_, _) |
                        TypeVariants::TyRawPtr(_) => {
                            let v = Var(*next_local);
                            *next_local += 1;
                            Some(Perm::LocalVar(v))
                        },
                        _ => None,
                    }
                });
                // Avoid ambiguity if no vars were generated.
                if *next_local == first_local {
                    *next_local += 1;
                }
                let advance = *next_local - first_local;
                e.insert((lty, advance)).0
            },

            Entry::Occupied(e) => {
                *next_local += e.get().1;
                e.get().0
            },
        }
    }

    pub fn min_perm(&mut self, a: Perm<'tcx>, b: Perm<'tcx>) -> Perm<'tcx> {
        eprintln!("finding min of {:?} and {:?}", a, b);
        match (a, b) {
            // A few easy cases
            (Perm::Concrete(ConcretePerm::Read), _) |
            (_, Perm::Concrete(ConcretePerm::Read)) => Perm::read(),

            (Perm::Concrete(ConcretePerm::Move), p) => p,
            (p, Perm::Concrete(ConcretePerm::Move)) => p,

            (Perm::Min(ps1), Perm::Min(ps2)) => {
                let mut all = Vec::with_capacity(ps1.len() + ps2.len());
                all.extend(ps1.iter().cloned());
                for &p in ps2 {
                    if !all.contains(&p) {
                        all.push(p);
                    }
                }
                let all =
                    if all.len() == 0 { &[] as &[_] }
                    else { self.arena.alloc_slice(&all) };
                eprintln!("nontrivial min: {:?}", all);
                Perm::Min(all)
            },

            (Perm::Min(ps), p) | (p, Perm::Min(ps)) => {
                if ps.contains(&p) {
                    Perm::Min(ps)
                } else {
                    let mut all = Vec::with_capacity(ps.len() + 1);
                    all.extend(ps.iter().cloned());
                    all.push(p);
                    let all =
                        if all.len() == 0 { &[] as &[_] }
                        else { self.arena.alloc_slice(&all) };
                    eprintln!("nontrivial min: {:?}", all);
                    Perm::Min(all)
                }
            },

            (a, b) => {
                if a == b {
                    a
                } else {
                    let all = self.arena.alloc_slice(&[a, b]);
                    eprintln!("nontrivial min: {:?}", all);
                    Perm::Min(all)
                }
            }
        }
    }
}

fn preload_constraints<'a, 'gcx, 'tcx>(tcx: TyCtxt<'a, 'gcx, 'tcx>,
                                       def_id: DefId,
                                       sig: LFnSig<'tcx>) -> Option<ConstraintSet<'tcx>> {
    let mut cset = ConstraintSet::new();

    let path = tcx.absolute_item_path_str(def_id);
    match &path as &str {
        "core::ptr::<impl *const T>::offset" |
        "core::ptr::<impl *mut T>::offset" => {
            cset.add(sig.output.label.unwrap(),
                     sig.inputs[0].label.unwrap());
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

pub struct FnIds<'a, 'tcx: 'a> {
    inner: hash_map::Keys<'a, DefId, FnSummary<'tcx>>,
}

impl<'a, 'tcx> Iterator for FnIds<'a, 'tcx> {
    type Item = DefId;

    fn next(&mut self) -> Option<DefId> {
        self.inner.next().map(|&x| x)
    }
}

