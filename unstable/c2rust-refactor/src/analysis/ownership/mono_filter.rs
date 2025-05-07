//! Filtering of monomorphizations.  We classify some monomorphizations as "suspicious", and skip
//! generating code for them if they aren't used.  This module examines the monomorphizations and
//! their call sites, and generates the list of monomorphizations to skip.

use log::debug;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

use rustc_hir::def_id::DefId;
use rustc_data_structures::indexed_vec::IndexVec;

use super::{Var, ConcretePerm, Perm};
use super::context::Ctxt;


pub fn filter_suspicious_monos(
    cx: &Ctxt,
    all_mono_sigs: &HashMap<DefId, Vec<IndexVec<Var, ConcretePerm>>>,
    all_inst_sels: &HashMap<(DefId, usize), Vec<usize>>)
    -> HashSet<(DefId, usize)> {


    // (1) Find suspicious mono sigs.
    //
    // It's common for accessor functions to get inferred signatures `READ -> READ`, `WRITE ->
    // WRITE`, and `WRITE -> MOVE`.  This last one is suspicious because the presence of the `READ
    // -> READ` sig suggests that no actual updates are happening to record the removal of the
    // accessed element.
    //
    // The heuristic we currently use to find these cases is to look for fns where one mono sig has
    // a `MOVE` output and the other has a `READ` output in the same position.  Note this doesn't
    // actually look for a `READ` input like the description above said.

    let mut suspicious = HashSet::new();
    let mut all_monos = Vec::new();

    for (&def_id, mono_sigs) in all_mono_sigs {
        let summ = cx.get_fn_summ_imm(def_id).unwrap();

        // Only look at inferred monos - ones provided by attributes are exempt.
        if summ.attr_monos.is_none() {
            let outputs = super::mono::infer_outputs(summ);

            for (v, is_output) in outputs.iter_enumerated() {
                if !is_output {
                    continue;
                }

                if !mono_sigs.iter().any(|assign| assign[v] == ConcretePerm::Read) {
                    continue;
                }

                for (i, assign) in mono_sigs.iter().enumerate() {
                    if assign[v] == ConcretePerm::Move {
                        suspicious.insert((def_id, i));
                        debug!("found suspicious mono: {:?} #{}", def_id, i);
                    }
                }
            }
        }

        for i in 0 .. mono_sigs.len() {
            all_monos.push((def_id, i));
        }
    }


    // (2) Build the call graph.  Suspicious monos that have no calls from non-suspicious monos
    // will be filtered out.

    let mut call_graph = HashMap::new();
    let mut caller_count = HashMap::new();

    for (&(def_id, src_idx), inst_sel) in all_inst_sels {
        let summ = cx.get_fn_summ_imm(def_id).unwrap();

        for (inst, &dest_idx) in summ.insts.iter().zip(inst_sel.iter()) {
            let src = (def_id, src_idx);
            let dest = (inst.callee, dest_idx);
            call_graph.entry(src).or_insert_with(HashSet::new).insert(dest);
            *caller_count.entry(dest).or_insert(0) += 1;
        }
    }


    // (3) Search the callgraph for suspicious monos with only suspicious callers.

    let mut queue = VecDeque::new();
    let mut filter = HashSet::new();

    for &src in &all_monos {
        let count = caller_count.get(&src).map(|&x| x).unwrap_or(0);
        debug!("{:?}: {} callers, suspicious? {}", src, count, suspicious.contains(&src));
        if suspicious.contains(&src) && count == 0 {
            queue.push_back(src);
        }
    }

    while let Some(src) = queue.pop_front() {
        filter.insert(src);

        // `src` has been filtered out.  Decrement the caller count for each of its callees.
        for &dest in call_graph.entry(src).or_insert_with(HashSet::new).iter() {
            let count = caller_count.get_mut(&dest).unwrap();
            *count -= 1;
            if *count == 0 {
                queue.push_back(dest);
            }
        }
    }


    filter
}
