//! This modules handles converting a a control-flow graph `Cfg` into `Vec<Structure>`, optionally
//! simplifying the latter.

use super::*;

/// Convert the CFG into a sequence of structures
pub fn reloop(cfg: Cfg<Label>, simplify_structures: bool) -> Vec<Structure> {

    let entries = cfg.entries;
    let blocks = cfg.nodes
        .into_iter()
        .map(|(lbl, bb)| {
            let terminator = bb.terminator.map_labels(|l| StructureLabel::GoTo(*l));
            (lbl, BasicBlock { body: bb.body, terminator })
        })
        .collect();

    let mut relooped = relooper(entries, blocks);

    if simplify_structures {
        relooped = simplify_structure(relooped)
    }

    relooped
}

/// Recursive helper for `reloop`.
fn relooper(
    entries: HashSet<Label>,
    mut blocks: HashMap<Label, BasicBlock<StructureLabel>>
) -> Vec<Structure> {

    // Find nodes outside the graph pointed to from nodes inside the graph
    fn out_edges(blocks: &HashMap<Label, BasicBlock<StructureLabel>>) -> HashSet<Label> {
        blocks
            .iter()
            .flat_map(|(_, bb)| bb.successors())
            .filter(|lbl| !blocks.contains_key(lbl))
            .collect()
    }

    // Transforms `{1: {'a', 'b'}, 2: {'b'}}` into `{'a': {1}, 'b': {1,2}}`.
    fn flip_edges(map: HashMap<Label, HashSet<Label>>) -> HashMap<Label, HashSet<Label>> {
        let mut flipped_map: HashMap<Label, HashSet<Label>> = HashMap::new();
        for (lbl, vals) in map {
            for val in vals {
                flipped_map.entry(val).or_insert(HashSet::new()).insert(lbl);
            }
        }
        flipped_map
    }

    let reachable_labels: HashSet<Label> = blocks
        .iter()
        .flat_map(|(_, bb)| bb.successors())
        .collect();

    // Split the entry labels into those that some basic block may branch to versus those that
    // none can branch to.
    let (some_branch_to, none_branch_to): (HashSet<Label>, HashSet<Label>) = entries
        .iter()
        .cloned()
        .partition(|entry| reachable_labels.contains(&entry));

    // Split the entry labels into those that are in the current blocks, and those that aren't
    let (present, absent): (HashSet<Label>, HashSet<Label>) = entries
        .iter()
        .cloned()
        .partition(|entry| blocks.contains_key(&entry));

    let strict_reachable_from = {
        let mut successor_map: HashMap<Label, HashSet<Label>> = blocks
            .iter()
            .map(|(lbl, bb)| (*lbl, bb.successors()))
            .collect();

        // Iteratively make this bigger
        loop {
            let new_successor_map: HashMap<Label, HashSet<Label>> = successor_map
                .iter()
                .map(|(lbl, seen)| {
                    let succs = successor_map
                        .iter()
                        .filter_map(|(lbl1, seen1)| {
                            if seen.contains(lbl1) { Some(seen1) } else { None }
                        })
                        .fold(seen.clone(), |l,r| &l | &r);
                    (*lbl, succs)
                })
                .collect();
            if successor_map == new_successor_map {
                break;
            } else {
                successor_map = new_successor_map;
            }
        }

        // Flip edges
        flip_edges(successor_map)
    };

    match (none_branch_to.len(), some_branch_to.len()) {
        // Base case
        (0,0) => vec![],

        // Simple blocks
        (1,0) => {
            let entry = *none_branch_to.iter().next().expect("Should find exactly one entry");

            if let Some(bb) = blocks.remove(&entry) {
                let new_entries = bb.successors();
                let BasicBlock { body, terminator } = bb;

                let mut result = vec![Structure::Simple { entries, body, terminator }];
                result.extend(relooper(new_entries, blocks));
                result
            } else {
                let body = vec![];
                let terminator = Jump(StructureLabel::GoTo(entry));

                vec![Structure::Simple { entries, body, terminator }]
            }
        }

        // Skipping to blocks placed later
        _ if !absent.is_empty() => {
            if present.is_empty() {
                vec![]
            } else {
                let branches = absent.into_iter().map(|lbl| (lbl,vec![])).collect();
                let then = relooper(present, blocks);
                vec![Structure::Multiple { entries, branches, then }]
            }
        }

        // Loops
        (0, _) => {

            let new_returns: HashSet<Label> = strict_reachable_from
                .iter()
                .filter(|&(lbl, reachable)| blocks.contains_key(lbl) && entries.contains(lbl))
                .flat_map(|(_, ref reachable)| reachable.iter())
                .cloned()
                .collect();

            // Partition blocks into those belonging in or after the loop
            let (mut body_blocks, follow_blocks): (HashMap<Label, BasicBlock<StructureLabel>>, HashMap<Label, BasicBlock<StructureLabel>>) = blocks
                .into_iter()
                .partition(|&(ref lbl, _)| new_returns.contains(lbl));

            let follow_entries = out_edges(&body_blocks);

            // Rename some `GoTo`s in the loop body to `ExitTo`s
            for (_, bb) in body_blocks.iter_mut() {
                for lbl in bb.terminator.get_labels_mut() {
                    if let &mut StructureLabel::GoTo(label) = lbl {
                        if entries.contains(&label) || follow_entries.contains(&label) {
                            *lbl = StructureLabel::ExitTo(label)
                        }
                    }
                }
            }

            let children = relooper(entries.clone(), body_blocks);

            let mut result = vec![Structure::Loop { entries, body: children }];
            result.extend(relooper(follow_entries, follow_blocks));
            result
        }

        _ => {

            // Like `strict_reachable_from`, but a entries also reach themselves
            let mut reachable_from: HashMap<Label, HashSet<Label>> = strict_reachable_from;
            for entry in &entries {
                reachable_from.entry(*entry).or_insert(HashSet::new()).insert(*entry);
            }

            // Blocks that are reached by only one label
            let singly_reached: HashMap<Label, HashSet<Label>> = flip_edges(reachable_from
                .into_iter()
                .map(|(lbl, reachable)| (lbl, &reachable & &entries))
                .filter(|&(lbl, ref reachable)| reachable.len() == 1)
                .collect()
            );

            let handled_entries: HashMap<Label, HashMap<Label, BasicBlock<StructureLabel>>> = singly_reached
                .into_iter()
                .map(|(lbl, within)| {
                    let val = blocks
                        .iter()
                        .filter(|&(k, _)| within.contains(k))
                        .map(|(&k, v)| (k, v.clone()))
                        .collect();
                    (lbl, val)
                })
                .collect();

            let unhandled_entries: HashSet<Label> = entries
                .iter()
                .filter(|e| !handled_entries.contains_key(e))
                .cloned()
                .collect();

            let mut handled_blocks: HashMap<Label, BasicBlock<StructureLabel>> = HashMap::new();
            for (_, map) in &handled_entries {
                for (k, v) in map {
                    handled_blocks.entry(*k).or_insert(v.clone());
                }
            }
            let handled_blocks = handled_blocks;

            let follow_blocks: HashMap<Label, BasicBlock<StructureLabel>> = blocks
                .into_iter()
                .filter(|&(lbl, _)| !handled_blocks.contains_key(&lbl))
                .collect();

            let follow_entries: HashSet<Label> = &unhandled_entries | &out_edges(&handled_blocks);

            let mut all_handlers: HashMap<Label, Vec<Structure>> = handled_entries
                .into_iter()
                .map(|(lbl, blocks)| {
                    let entries: HashSet<Label> = vec![lbl].into_iter().collect();
                    (lbl, relooper(entries, blocks))
                })
                .collect();

            let handler_keys: HashSet<Label> = all_handlers.keys().cloned().collect();
            let (then, branches) = if handler_keys == entries {
                let a_key = *all_handlers.keys().next().expect("no handlers found");
                let last_handler = all_handlers.remove(&a_key).expect("just got this key");
                (last_handler, all_handlers)
            } else {
                (vec![], all_handlers)
            };

            let mut result = vec![Structure::Multiple { entries, branches, then }];
            result.extend(relooper(follow_entries, follow_blocks));
            result
        }
    }
}

fn simplify_structure(structures: Vec<Structure>) -> Vec<Structure> {

    // Recursive calls come first
    let structures: Vec<Structure> = structures
        .into_iter()
        .map(|structure: Structure| -> Structure {
            match structure {
                Structure::Loop { entries, body } => {
                    let body = simplify_structure(body);
                    Structure::Loop { entries, body }
                },
                Structure::Multiple { entries, branches, then } => {
                    let branches = branches
                        .into_iter()
                        .map(|(lbl, ss)| (lbl, simplify_structure(ss)))
                        .collect();
                    let then = simplify_structure(then);
                    Structure::Multiple { entries, branches, then }
                }
                simple => simple,

            }
        })
        .collect();

    let mut acc_structures: Vec<Structure> = vec![];
    for structure in structures.iter().rev() {
        match structure {
            &Structure::Simple { ref entries, ref body, ref terminator } => {

                let terminator = if let &Switch { ref expr, ref cases } = terminator {

                    // TODO:
                    //
                    //   1. check if anything but the 'GoTo' case can happen here
                    //   2. see if there is anything to do about being clever about possible values of 'currentBlock'


                    // Here, we group patterns by the label they go to.
                    let mut merged_cases: HashMap<Label, Vec<P<Pat>>> = HashMap::new();
                    for &(ref pats, ref lbl) in cases {
                        match lbl {
                            &StructureLabel::GoTo(lbl) => merged_cases.entry(lbl).or_insert(vec![]).extend(pats.clone()),
                            _ => unimplemented!()
                        }
                    }

                    // When converting these patterns back into a vector, we have to be careful to
                    // preserve their initial order (so that the default pattern doesn't end up on
                    // top).
                    let mut cases_new: Vec<_> = vec![];
                    for &(_, ref lbl) in cases.iter().rev() {
                        let lbl = match lbl {
                            &StructureLabel::GoTo(lbl) => lbl,
                            _ => unimplemented!()
                        };

                        match merged_cases.remove(&lbl) {
                            None => { },
                            Some(pats) => cases_new.push((pats, StructureLabel::GoTo(lbl))),
                        }
                    }
                    cases_new.reverse();

                    Switch { expr: expr.clone(), cases: cases_new }
                } else {
                    terminator.clone()
                };

                match acc_structures.pop() {
                    Some(Structure::Multiple { entries: _, branches, then }) => {
                        let rewrite = |t: &StructureLabel| {
                            if let &StructureLabel::GoTo(ref to) = t {
                                let entries: HashSet<_> = vec![*to].into_iter().collect();
                                let body: Vec<Stmt> = vec![];
                                let terminator = Jump(StructureLabel::GoTo(*to));
                                let first_structure = Structure::Simple { entries, body, terminator };

                                let mut nested: Vec<Structure> = vec![first_structure];
                                nested.extend(branches.get(to).cloned().unwrap_or(then.clone()));

                                StructureLabel::Nested(nested)
                            } else {
                                panic!("simplifyStructure: Simple/Multiple invariants violated")
                            }
                        };

                        let terminator = terminator.map_labels(rewrite);
                        let body = body.clone();
                        let entries = entries.clone();
                        acc_structures.push(Structure::Simple { entries, body, terminator });
                    },
                    possibly_popped => {
                        if let Some(popped) = possibly_popped {
                            acc_structures.push(popped);
                        }

                        let entries = entries.clone();
                        let body = body.clone();
                        let terminator = terminator.clone();
                        acc_structures.push(Structure::Simple { entries, body, terminator });
                    }
                }
            }

            other_structure => acc_structures.push(other_structure.clone()),
        }

    }

    acc_structures.reverse();
    acc_structures
}
