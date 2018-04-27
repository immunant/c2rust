//! This module contains stuff related to choosing better loops in Relooper. The problem is simple:
//! there is a point in the relooper algorithm where we commit to making a `Loop` structure. At that
//! point, we partition blocks into those that go into the loop and those that go after the loop.
//!
//! Relooper gives us a handful of blocks which _must_ go in the loop, but we have some freedom to
//! choose which other blocks we want to also push into the loop.
//!
//! Our choice can be informed by one of two strategies:
//!
//!   * _Do what C does_. We can try to match what C does by keeping track of which blocks in CFG
//!     formed loops in the initial C source. This information needs to be built up when initially
//!     building the CFG .
//!
//!   * _Try to avoid jump-table's_. The more entries we push into the loop, the more we risk
//!     creating a massive loop that starts with a jump table. OTOH, the more entries we put after
//!     the loop, the more likely we are to have a jump table right after the loop.
//!

use super::*;


/// Modifies the `body_blocks`, `follow_blocks`, and `follow_entries` to try to get all of the
/// `desired_body` labels into the body. If it is not possible to do this, returns `false` (and the
/// mutable references passed in cannot be used).
pub fn match_loop_body(
    mut desired_body: HashSet<Label>,
    body_blocks: &mut HashMap<Label, BasicBlock<StructureLabel<StmtOrDecl>, StmtOrDecl>>,
    follow_blocks: &mut HashMap<Label, BasicBlock<StructureLabel<StmtOrDecl>, StmtOrDecl>>,
    follow_entries: &mut HashSet<Label>,
) -> bool {

    // Keep moving `follow_entries` that are also in `desired_body` into the loop's `body_blocks`
    let mut something_happened = true;
    while something_happened {
        something_happened = false;

        let to_move: Vec<Label> = follow_entries.intersection(&desired_body).cloned().collect();

        for following in to_move {
            let bb = if let Some(bb) = follow_blocks.remove(&following) { bb } else { continue; };
            something_happened = true;

            desired_body.remove(&following);

            follow_entries.remove(&following);
            follow_entries.extend(&bb.successors());
            follow_entries.retain(|e| !body_blocks.contains_key(e));
            body_blocks.insert(following, bb);
        }
    }

    desired_body.is_empty()
}


/// Use heuristics to decide which blocks to move into the loop body.
///
///  1. Don't do anything if `follow_entries` is zero or one (since that means whatever
///     follows the loop will be nice looking).
///  2. Otherwise, recursively push into the loop `follow_entries` as long as they have no
///     more than 1 successor (the hope is that some of the chains will join).
///
/// This always succeeds.
pub fn heuristic_loop_body(
    predecessor_map: &HashMap<Label, HashSet<Label>>,
    body_blocks: &mut HashMap<Label, BasicBlock<StructureLabel<StmtOrDecl>, StmtOrDecl>>,
    follow_blocks: &mut HashMap<Label, BasicBlock<StructureLabel<StmtOrDecl>, StmtOrDecl>>,
    follow_entries: &mut HashSet<Label>,
) -> () {
    if follow_entries.len() > 1 {
        for follow_entry in follow_entries.clone().iter() {
            let mut following: Label = *follow_entry;

            loop {
                // If this block might have come from 2 places, give up
                if predecessor_map[&following].len() != 1 {
                    break;
                }

                // Otherwise, move it into the loop
                let bb = if let Some(bb) = follow_blocks.remove(&following) { bb } else { break; };
                let succs = bb.successors();

                body_blocks.insert(following, bb);
                follow_entries.remove(&following);
                follow_entries.extend(&succs);

                // If it has more than one successor, don't try following the successor
                if succs.len() != 1 {
                    break;
                }

                following = *succs.iter().next().unwrap();
            }
        }
    }
}

/// These IDs identify groups of basic blocks corresponding to loops in a CFG.
#[derive(Copy,Clone,PartialEq,Eq,PartialOrd,Ord,Debug,Hash)]
pub struct LoopId(u64);

impl LoopId {
    pub fn new(id: u64) -> LoopId {
        LoopId(id)
    }

    pub fn pretty_print(&self) -> String {
        let &LoopId(loop_id) = self;
        format!("l_{}", loop_id)
    }
}

/// Information about loops in a CFG.
#[derive(Clone,Debug)]
pub struct LoopInfo<Lbl: Hash + Eq> {
    /// Given a node, find the tightest enclosing loop
    node_loops: HashMap<Lbl, LoopId>,

    /// Given a loop, find all the nodes in it, along with the next tighest loop around it.
    loops: HashMap<LoopId, (HashSet<Lbl>, Option<LoopId>)>,
}

impl<Lbl: Hash + Eq + Clone> LoopInfo<Lbl> {
    pub fn new() -> Self {
        LoopInfo { node_loops: HashMap::new(), loops: HashMap::new() }
    }

    /// Find the smallest possible loop that contains all of the items
    pub fn tightest_common_loop<E: Iterator<Item=Lbl>>(&self, mut entries: E) -> Option<LoopId> {
        let first = if let Some(f) = entries.next() { f } else { return None };

        let mut loop_id = if let Some(i) = self.node_loops.get(&first) { *i } else { return None };

        for entry in entries {
            let (ref in_loop, parent_id) = self.loops[&loop_id];
            while !in_loop.contains(&entry) {
                loop_id = if let Some(i) = parent_id { i } else { return None };
            }
        }

        return Some(loop_id);
    }

    /// Filter out any nodes which need to be pruned from the entire CFG due to being unreachable.
    pub fn filter_unreachable(&mut self, reachable: &HashSet<Lbl>) -> () {
        self.node_loops.retain(|lbl, _| reachable.contains(lbl));
        for (_, &mut (ref mut set, _)) in self.loops.iter_mut() {
            set.retain(|lbl| reachable.contains(lbl));
        }
    }

    /// Rewrite nodes to take into account a node remapping. Note that the remapping is usually
    /// going to be very much _not_ injective - the whole point of remapping is to merge some nodes.
    pub fn rewrite_blocks(&mut self, rewrites: &HashMap<Lbl, Lbl>) -> () {
        self.node_loops.retain(|lbl, _| rewrites.get(lbl).is_none());
        for (_, &mut (ref mut set, _)) in self.loops.iter_mut() {
            set.retain(|lbl| rewrites.get(lbl).is_none());
        }
    }

    /// Add in information about a new loop
    pub fn add_loop(&mut self, id: LoopId, contents: HashSet<Lbl>, outer_id: Option<LoopId>) -> () {
        for elem in &contents {
            if !self.node_loops.contains_key(elem) {
                self.node_loops.insert(elem.clone(), id);
            }
        }

        self.loops.insert(id, (contents, outer_id));
    }

    /// Get all of the `LoopId`'s corresponding to loops of increasing size around the given label
    pub fn enclosing_loops(&self, lbl: &Lbl) -> Vec<LoopId> {
        let mut loop_id_opt: Option<LoopId> = self.node_loops.get(lbl).cloned();
        let mut loop_ids = vec![];
        while let Some(loop_id) = loop_id_opt {
            loop_ids.push(loop_id);
            loop_id_opt = self.loops[&loop_id].1;
        }
        loop_ids
    }

    pub fn get_loop_contents<'a>(&'a self, id: LoopId) -> &'a HashSet<Lbl> {
        &self.loops
            .get(&id)
            .expect(&format!("There is no loop with id {:?}", id))
            .0
    }

}
