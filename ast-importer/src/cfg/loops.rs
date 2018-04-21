//! This module contains stuff related to choosing better loops in Relooper. The problem is simple:
//! in the "loop" section of relooper, we can choose how many extra blocks to put in the loop vs.
//! after the loop. There are two competing forces:
//!
//!   * we want to reduce the size of `follow_entries` (0 or 1 is optimal)
//!   * pushing entries into the loop risks forcing making a `Multiple` in it
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