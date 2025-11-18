//! This module contains the relooper algorithm for creating structured control
//! flow from a CFG.
//!
//! Relooper is an algorithm for converting an arbitrary, unstructured
//! control-flow graph (CFG) into the structured control-flow constructs
//! available in Rust. The original relooper algorithm was described in the
//! [Emscripten paper][emscripten], which describes converting a CFG into
//! JavaScript's structured control-flow constructs. The implementation of
//! relooper used here is based on the original algorithm, with the addition of
//! some heuristics and Rust-specific control-flow constructs.
//!
//! [emscripten]: https://dl.acm.org/doi/10.1145/2048147.2048224
//!
//! # Terminology
//!
//! The terms "label", "block", and "node" are sometimes used interchangeably in
//! this file. A label is the unique identifier for a block, and a block is a
//! node in the control-flow graph. In some cases we're working directly with
//! the basic blocks, but in many places when working with the CFG we're dealing
//! only with labels, and the blocks themselves are secondary.
//!
//! # Relooper Algorithm
//!
//! The relooper algorithm works by recursively breaking down sections of the
//! CFG into structured control-flow constructs, partitioning blocks into either
//! `Simple` structures, `Loop` structures, or `Multiple` structures. At each
//! step, we start with a set of basic blocks and information about which of
//! those blocks act as entry points to this portion of the CFG. We then look
//! for ways to break down the CFG into smaller sections that can be represented
//! using structured control-flow constructs.
//!
//! If we have a single entry point, and there are no back edges to that entry,
//! then we can generate a simple structure. `Simple` structures contain a
//! single basic block, and represent a straight-line sequence of instructions.
//! All remaining blocks are then recursively relooped, with the immediate
//! successors of the entry becoming the new entries for the rest of that
//! portion of the CFG.
//!
//! If we have entries with back edges to them, then we can generate a `Loop`
//! structure. Any nodes that can reach the entry become part of the loop body,
//! with any remaining nodes becoming the follow blocks for the loop. The loop's
//! contents are then relooped into the loops's body, and the follow blocks get
//! relooped to be the logic that follows the loop.
//!
//! If we have more than one entry, then we can generate a `Multiple` structure.
//! These are effectively `match` statements, with each entry becoming an arm of
//! the `match`. Blocks that are only reachable by one of the entries (including
//! the entry itself) become the body blocks for that arm of the multiple, and
//! any blocks reachable from more than one entry become the follow blocks. We
//! then recursively reloop each of the branches of the multiple, and then
//! reloop the follow blocks.
//!
//! Note that there are a lot of subtleties to how we choose to partition blocks
//! into these structures. The logic in the relooper implementation contains
//! thorough comments describing what we're doing at each step and why we are
//! making the choices that we do. This module documentation covers some of
//! them, but you'll need to read through the full algorithm to get all of the
//! nuances.
//!
//! # Heuristics
//!
//! When reconstructing structured control flow from a CFG, there are often
//! multiple valid ways to structure the graph. In order to produce Rust code
//! that is as similar to the original C as possible, we have a couple of
//! heuristics that use information from the original C code to guide the
//! restructuring process.
//!
//! Before creating a loop, we first try to match a `Multiple` from the original
//! C. During transpilation we preserve information about where there are
//! branches in the C code along with which CFG nodes are part of those
//! branches, which we can then look up based on the current set of entries. If
//! we find that there is a `Multiple` in the original C that matches our
//! current entries, and the structure of the CFG allows it, we can reproduce
//! the control-flow from the original C. Doing this before creating a loop
//! helps in cases where we have branches with multiple disjoint loops, since
//! the loop analysis does not recognize disjoint loops and will always produce
//! a single loop with a `Multiple` inside of it handling the bodies of what
//! should be separate loops.
//!
//! When creating loops, we also make use of a similar heuristic that tries to
//! recreate the loops that we see in the original C. When partitioning blocks
//! into the loop's body, we first attempt to match an existing loop from the
//! original C. Failing that, we fall back on a heuristic that tries to keep as
//! many blocks as possible in the loop's body, even if they don't strictly
//! belong there according to the original C structure.
//!
//! # Simplification
//!
//! After the relooper algorithm runs, we have an optional simplification pass
//! that attempts to reduce the complexity of the generated control flow
//! structures. This pass can help to eliminate unnecessary nesting and make the
//! final output more readable. It applies two main simplifications:
//!
//! - Merge cases in [`Switch`] terminators if they target the same label. That
//!   means instead of having `1 => goto A, 2 => goto A, 3 => goto B`, we
//!   instead get `1 | 2 => goto A, 3 => goto B`.
//! - Inline `Multiple` structures into preceding `Simple` structures. When a
//!   `Simple` structure with a `Switch` terminator is immediately followed by a
//!   `Multiple`, the branches from the `Multiple` are inlined directly into the
//!   `Switch` cases as `Nested` structures, eliminating the intermediate
//!   `Multiple` and reducing nesting depth.

use super::*;

/// Convert the CFG into a sequence of structures
pub fn reloop(
    cfg: Cfg<Label, StmtOrDecl>, // the control flow graph to reloop
    mut store: DeclStmtStore,    // store of what to do with declarations
    use_c_loop_info: bool,       // use the loop information in the CFG (slower, but better)
    use_c_multiple_info: bool,   // use the multiple information in the CFG (slower, but better)
    live_in: IndexSet<CDeclId>,  // declarations we assume are live going into this graph
) -> (Vec<Stmt>, Vec<Structure<Stmt>>) {
    let entries: IndexSet<Label> = vec![cfg.entries].into_iter().collect();
    let blocks = cfg
        .nodes
        .into_iter()
        .map(|(lbl, bb)| {
            let terminator = bb
                .terminator
                .map_labels(|l| StructureLabel::GoTo(l.clone()));
            (
                lbl,
                BasicBlock {
                    body: bb.body,
                    terminator,
                    defined: bb.defined,
                    live: bb.live,
                    span: bb.span,
                },
            )
        })
        .collect();

    let mut relooped_with_decls: Vec<Structure<StmtOrDecl>> = vec![];
    let loop_info = if use_c_loop_info {
        Some(cfg.loops)
    } else {
        None
    };
    let multiple_info = if use_c_multiple_info {
        Some(cfg.multiples)
    } else {
        None
    };
    let mut state = RelooperState::new(loop_info, multiple_info, live_in);
    state.relooper(entries, blocks, &mut relooped_with_decls, false);

    // These are declarations we need to lift
    let lift_me = state.lifted;

    // These are the statements that emerge from these lifts
    let lifted_stmts: Vec<Stmt> = lift_me
        .iter()
        .flat_map(|&decl: &CDeclId| store.extract_decl(decl).unwrap())
        .collect();

    // We map over the existing structure and flatten everything to `Stmt`
    let relooped: Vec<Structure<Stmt>> = relooped_with_decls
        .into_iter()
        .map(|s| s.place_decls(&lift_me, &mut store))
        .collect();

    (lifted_stmts, relooped)
}

/// This is the state we close over while relooping. It accumulates information about which
/// declarations were supposed to be in scope before they were declared.
struct RelooperState {
    /// scopes of declarations seen so far
    scopes: Vec<IndexSet<CDeclId>>,

    /// Declarations that will have to be lifted to the top of the output
    lifted: IndexSet<CDeclId>,

    /// Information about loops
    loop_info: Option<LoopInfo<Label>>,

    /// Information about multiples
    multiple_info: Option<MultipleInfo<Label>>,
}

impl RelooperState {
    pub fn new(
        loop_info: Option<LoopInfo<Label>>,
        multiple_info: Option<MultipleInfo<Label>>,
        live_in: IndexSet<CDeclId>,
    ) -> Self {
        RelooperState {
            scopes: vec![live_in],
            lifted: IndexSet::new(),
            loop_info,
            multiple_info,
        }
    }

    pub fn open_scope(&mut self) {
        self.scopes.push(IndexSet::new());
    }

    pub fn close_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn in_scope(&self, decl: CDeclId) -> bool {
        self.scopes.iter().any(|scope| scope.contains(&decl))
    }

    pub fn add_to_scope(&mut self, decl: CDeclId) {
        self.scopes
            .last_mut()
            .expect("add_to_scope: no scopes found")
            .insert(decl);
    }

    pub fn add_to_top_scope(&mut self, decl: CDeclId) {
        self.scopes
            .first_mut()
            .expect("add_to_top_scope: no scopes found")
            .insert(decl);
    }
}

/// A set of basic blocks, keyed by their label.
type StructuredBlocks = IndexMap<Label, BasicBlock<StructureLabel<StmtOrDecl>, StmtOrDecl>>;

/// An adjacency list mapping from each label to the set of labels it can reach.
type AdjacencyList = IndexMap<Label, IndexSet<Label>>;

impl RelooperState {
    /// Recursive helper for `reloop`.
    ///
    /// TODO: perhaps manually perform TCO?
    fn relooper(
        &mut self,
        entries: IndexSet<Label>,     // current entry points into the CFG
        mut blocks: StructuredBlocks, // the blocks in the sub-CFG considered
        result: &mut Vec<Structure<StmtOrDecl>>, // the generated structures are appended to this
        _disable_heuristics: bool,
    ) {
        // --------------------------------------
        // Base case

        if entries.is_empty() {
            return;
        }

        // Find all labels reachable via a `GoTo` from the current set of blocks.
        let reachable_labels: IndexSet<Label> =
            blocks.iter().flat_map(|(_, bb)| bb.successors()).collect();

        // Split the entry labels into those that some basic block may goto versus those that none can
        // goto.
        let (some_branch_to, none_branch_to): (IndexSet<Label>, IndexSet<Label>) = entries
            .iter()
            .cloned()
            .partition(|entry| reachable_labels.contains(entry));

        // Calculate predecessor and reachability information:
        //
        // * `predecessor_map` maps from each label to its immediate predecessors in the
        //   current set of blocks.
        // * `strict_reachable_from` maps each label to the set of labels that can reach
        //   it, i.e. its direct and indirect predecessors. Entries do not count as
        //   reaching themselves, and so an entry will only appear as a key if it is the
        //   successor of some block (including itself, if the entry branches directly
        //   to itself).
        //
        // For both of these, some keys may not correspond to any blocks in the current
        // set, since blocks may have successors that are outside the current set. For
        // both of these, labels in the values set will always correspond to blocks in
        // the current set.
        let (predecessor_map, strict_reachable_from) = {
            // Map each of our current blocks to its immediate successors.
            let successor_map = blocks
                .iter()
                .map(|(lbl, bb)| (lbl.clone(), bb.successors()))
                .collect();

            // Calculate the transitive closure of the successor map, i.e. the full set of
            // labels reachable from each block, not including itself. Then flip the edges
            // to get a map from a label to the set of labels that can reach it.
            let strict_reachable_from = flip_edges(transitive_closure(&successor_map));

            // Flip the edges of the successor map to get a map of immediate predecessors
            // for each block.
            let predecessor_map = flip_edges(successor_map);

            (predecessor_map, strict_reachable_from)
        };

        // --------------------------------------
        // Simple cases

        // Handle our simple case of only 1 entry. If there's no back edge to the entry
        // we generate a simple structure, otherwise we make a loop.
        if entries.len() == 1 {
            if some_branch_to.is_empty() {
                let entry = none_branch_to
                    .first()
                    .expect("Should find exactly one entry");

                // If our entry is in our current blocks, emit it as a simple block and then
                // recurse into the rest of the blocks.
                //
                // If our entry is not in our current blocks, then it was previously selected as
                // the follow blocks of a multiple and we are at the end of that branch. In this
                // case we emit an empty simple block that jumps to the entry.
                if let Some(bb) = blocks.swap_remove(entry) {
                    let new_entries = bb.successors();
                    let BasicBlock {
                        body,
                        mut terminator,
                        live,
                        defined,
                        span,
                    } = bb;

                    // Flag declarations for everything that is live going in but not already in scope.
                    //
                    // It is tempting to just place the declarations here, but it isn't that simple:
                    // they may end up also being live but not in scope elsewhere and we should _not_
                    // make a second declaration.
                    for l in live {
                        if !self.in_scope(l) {
                            self.add_to_top_scope(l);
                            self.lifted.insert(l);
                        }
                    }

                    // Being into scope things that are defined here
                    for d in defined {
                        self.add_to_scope(d);
                    }

                    // Rewrite `GoTo`s that don't target our next blocks into `BreakTo`s so that
                    // they correctly jump past the code that naturally follows the simple.
                    for lbl in terminator.get_labels_mut() {
                        if let StructureLabel::GoTo(label) = lbl {
                            if !blocks.contains_key(label) {
                                *lbl = StructureLabel::BreakTo(label.clone())
                            }
                        }
                    }

                    result.push(Structure::Simple {
                        entries,
                        body,
                        span,
                        terminator,
                    });

                    self.relooper(new_entries, blocks, result, false);
                } else {
                    // let body = vec![];
                    // let terminator = Jump(StructureLabel::BreakTo(entry.clone()));

                    // result.push(Structure::Simple {
                    //     entries,
                    //     body,
                    //     span: Span::call_site(),
                    //     terminator,
                    // });
                }
            } else {
                // Our only entry is branched back to, make a loop.
                self.make_loop(
                    &strict_reachable_from,
                    blocks,
                    entries,
                    &predecessor_map,
                    result,
                );
            }
            return;
        }

        // --------------------------------------
        // Skipping to blocks placed later

        // Split the entry labels into those that are in the current blocks, and those that aren't
        let (present, absent): (IndexSet<Label>, IndexSet<Label>) = entries
            .iter()
            .cloned()
            .partition(|entry| blocks.contains_key(entry));

        // Handle the case where we have entries that are not in the current set of
        // blocks.
        //
        // This happens when we create a multiple where a block reachable from multiple
        // entries gets put in the follow blocks. This means that the branches within
        // the multiple that reach the follow block have to do nothing, since they need
        // to exit the multiple to get to that next block. To handle this we create a
        // multiple with no-op branches that correspond to the absent entries, and a
        // then branch with result of relooping the present entries.
        //
        // If we only have absent entries, then there's no other blocks to structure and
        // we can just exit.
        if !absent.is_empty() {
            if !present.is_empty() {
                // let branches = absent.into_iter().map(|lbl| (lbl, vec![])).collect();

                // result.push(Structure::Multiple {
                //     entries,
                //     branches,
                // });

                // UUUHHHHHHHHH idk if this is right. This used to be the `then`
                // block in the multiple, but I have banned `then` and we decide
                // based on the entries if we need the fallthrough case. So I
                // guess this should just become the next structure after the
                // multiple? That sounds right but I'm not 100% sure.
                self.relooper(present, blocks, result, false);
            }

            return;
        }

        // --------------------------------------
        // Loops

        /*
        // Try to match an existing branch point (from the initial C).
        //
        // We do this before creating a loop to better handle the cases where we have
        // multiple entries that are part of disjoint loops. The loop analysis that
        // comes next doesn't recognize disjoint loops and will always produce a single
        // loop structure if there is at least one loop back to an entry. If we have
        // multiple branches with disjoint loops, this means we might generate a single
        // loop with a state machine inside it, merging all loop bodies together and
        // resulting in suboptimal control flow. If the original C had multiple
        // branches, this logic will recreate them, avoiding the problem of merging
        // loops accidentally.
        let mut recognized_c_multiple = false;
        if let Some(ref multiple_info) = self.multiple_info {
            let entries_key = entries.iter().cloned().collect();
            if let Some((join, arms)) = multiple_info.get_multiple(&entries_key) {
                recognized_c_multiple = true;

                // Search through the arms of the C multiple to verify that our current CFG matches
                // its structure.
                'search: for (entry, content) in arms {
                    let mut to_visit: Vec<Label> = vec![entry.clone()];
                    let mut visited: IndexSet<Label> = IndexSet::new();

                    // Walk the graph forwards from the entry, making sure we only see blocks that
                    // are part of the current multiple. If we find any nodes that aren't in
                    // `content` then the CFG differs from the C multiple.
                    //
                    // NOTE: We can't reuse the previous reachability analysis here because it
                    // includes nodes past the join node, which isn't what we want here.
                    //
                    // legaren: Actually I'm not 100% sure about that last part, if we restructure
                    // the logic here a bit we may be able to reuse `strict_reachable_from` or
                    // `transitive_closure`, but I haven't thought through it enough yet and I'd
                    // want to have tests before playing with this logic anyway.
                    while let Some(lbl) = to_visit.pop() {
                        // Stop at things you've already seen or the join block
                        if !visited.insert(lbl.clone()) || lbl == *join {
                            continue;
                        }

                        if let Some(bb) = blocks.get(&lbl) {
                            if !content.contains(&lbl) {
                                recognized_c_multiple = false;
                                break 'search;
                            }

                            to_visit.extend(bb.successors())
                        }
                    }

                    // Check that we've only visited the expected nodes. If we've visited any nodes
                    // not in `content`, then the CFG differs from the C multiple and the search
                    // fails.
                    visited.swap_remove(join);
                    if visited.difference(content).next().is_some() {
                        recognized_c_multiple = false;
                        break 'search;
                    }
                }
            }
        }
        recognized_c_multiple = recognized_c_multiple && !disable_heuristics;
        */

        // --------------------------------------
        // Multiple

        // Like `strict_reachable_from`, but entries also reach themselves.
        let mut reachable_from = strict_reachable_from.clone();
        for entry in &entries {
            reachable_from
                .entry(entry.clone())
                .or_default()
                .insert(entry.clone());
        }

        // Calculate which blocks are reached by only one entry (including the entries
        // themselves).
        //
        // For each label in `reachable_from`, filter its predecessor set to only our
        // entries, then filter down to only the labels reachable from exactly one
        // entry. Then flip edges to give us a map from entries to the set of labels
        // that are only reached from that entry.
        let singly_reached: IndexMap<Label, IndexSet<Label>> = flip_edges(
            reachable_from
                .into_iter()
                .map(|(lbl, reachable)| (lbl, &reachable & &entries))
                .filter(|(_, reachable)| reachable.len() == 1)
                .collect(),
        );

        // If we have any blocks that are only reachable from one entry, then we can
        // create a `Multiple` structure.
        if !singly_reached.is_empty() {
            // Map from entry labels to the set of blocks only reachable from that entry,
            // i.e. `singly_reached` but with the set of reachable labels replaced by the
            // corresponding blocks.
            let handled_entries: IndexMap<Label, StructuredBlocks> = singly_reached
                .into_iter()
                .map(|(lbl, within)| {
                    let val = blocks
                        .iter()
                        .filter(|(k, _)| within.contains(*k))
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect();
                    (lbl, val)
                })
                .collect();

            // Entries that don't have any blocks that are only reachable from themselves.
            let unhandled_entries: IndexSet<Label> = entries
                .iter()
                .filter(|&e| !handled_entries.contains_key(e))
                .cloned()
                .collect();

            // Gather the set of all blocks that are only reachable from one entry
            // (including the entries if they are only reachable from themselves).
            let handled_blocks: StructuredBlocks = handled_entries
                .values()
                .flatten()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();

            // Gather the unhandled blocks, i.e. any blocks that are reachable from more
            // than one entry. These become our "follow" blocks, i.e. the blocks that come
            // after the `Multiple`.
            let follow_blocks: StructuredBlocks = blocks
                .into_iter()
                .filter(|(lbl, _)| !handled_blocks.contains_key(lbl))
                .collect();

            // The entries for the follow blocks are our unhandled entries and any successors
            // of handled blocks that are not themselves handled blocks.
            let follow_entries: IndexSet<Label> = &unhandled_entries | &out_edges(&handled_blocks);

            // Reloop each set of handled blocks into their own structured control flow.
            let all_handlers: IndexMap<_, _> = handled_entries
                .into_iter()
                .map(|(lbl, blocks)| {
                    let entries = indexset![lbl.clone()];

                    let mut structs = vec![];
                    self.open_scope();
                    self.relooper(entries, blocks, &mut structs, false);
                    self.close_scope();

                    (lbl, structs)
                })
                .collect();

            // Disable heuristics when relooping the follow blocks if all of our entries are
            // follow entries.
            //
            // This situation happens when there are back edges to all of our entries, i.e.
            // when all of our entries are part of one or more loops. If we leave heuristics
            // enabled in this case, we risk infinite recursion because we're relooping the
            // same set of blocks with the same set of entries. If there are loops and we
            // ended up here, it means that we skipped creating a loop because we matched a
            // C multiple. Disabling heuristics means we won't try to match a C multiple,
            // meaning we will generate a loop-match instead.
            let disable_heuristics = follow_entries == entries;

            result.push(Structure::Multiple {
                entries,
                branches: all_handlers,
            });

            self.relooper(follow_entries, follow_blocks, result, disable_heuristics);
            return;
        }

        assert!(
            none_branch_to.is_empty(),
            "I think by now all entries have to be branched back to, right?"
        );

        // --------------------------------------
        // Loop fallback

        // We couldn't create a multiple, so create a loop.
        self.make_loop(
            &strict_reachable_from,
            blocks,
            entries,
            &predecessor_map,
            result,
        );
    }

    fn make_loop(
        &mut self,
        strict_reachable_from: &AdjacencyList,
        blocks: StructuredBlocks,
        entries: IndexSet<Label>,
        predecessor_map: &AdjacencyList,
        result: &mut Vec<Structure<StmtOrDecl>>,
    ) {
        // Gather the set of current blocks that can reach one of our entries, not
        // including the entries themselves unless they are the successor of some
        // block (including itself, if the entry explicitly branches to itself).
        //
        // NOTE: At least one entry will be present here since at this point we know
        // that there is an entry that is branched to.
        let new_returns: IndexSet<Label> = strict_reachable_from
            .iter()
            .filter(|&(lbl, _)| blocks.contains_key(lbl) && entries.contains(lbl))
            .flat_map(|(_, reachable)| reachable.iter())
            .cloned()
            .collect();

        // Partition blocks into those belonging in or after the loop
        let (mut body_blocks, mut follow_blocks): (StructuredBlocks, StructuredBlocks) = blocks
            .into_iter()
            .partition(|(lbl, _)| new_returns.contains(lbl) || entries.contains(lbl));

        // Any block that follows a body block but is not itself a body block
        // becomes an entry for the follow blocks.
        let mut follow_entries = out_edges(&body_blocks);

        // If matching an existing loop didn't work, fall back on a heuristic
        loops::heuristic_loop_body(
            &predecessor_map,
            &mut body_blocks,
            &mut follow_blocks,
            &mut follow_entries,
        );

        // Rewrite `GoTo`s that exit the loop body to `ExitTo`s.
        //
        // For our body blocks, rewrite terminator `GoTo` labels to `ExitTo` if they
        // target an entry or a follow entry. These are exits from the loop body, i.e.
        // if they branch back to an entry then it's a `continue`, and if they branch to
        // a follow entry then it's a `break`.
        //
        // This is necessary so that when we reloop the body blocks it doesn't get
        // tripped up by the back/out edges. Without this, the reloop pass for the loop
        // body would see the back edges and decide to produce a loop structure, even
        // though we've already done that. Changing from `GoTo` to `ExitTo` means that
        // the back/out edges don't show up in the list of successors when calculating
        // predecessor and reachability information.
        //
        // TODO: Is picking the first entry as the loop label always correct? Is there a
        // situation where we'd end up with nested loops with the same label? I kinda
        // suspect no, but I'm not sure.
        let loop_label = entries.first().unwrap();
        for bb in body_blocks.values_mut() {
            for lbl in bb.terminator.get_labels_mut() {
                if let StructureLabel::GoTo(label) = lbl.clone() {
                    if entries.contains(&label) {
                        *lbl = StructureLabel::ContinueTo {
                            loop_label: loop_label.clone(),
                            target: label.clone(),
                        };
                    } else if follow_entries.contains(&label) {
                        *lbl = StructureLabel::BreakTo(label.clone());
                    }
                }
            }
        }

        let mut body = vec![];
        self.open_scope();
        self.relooper(entries.clone(), body_blocks, &mut body, false);
        self.close_scope();

        result.push(Structure::Loop { entries, body });
        self.relooper(follow_entries, follow_blocks, result, false);
    }
}

/// Nested precondition: `structures` will contain no `StructureLabel::Nested` terminators.
pub fn simplify_structure<Stmt: Clone>(structures: Vec<Structure<Stmt>>) -> Vec<Structure<Stmt>> {
    // Recursive calls come first
    let structures: Vec<Structure<Stmt>> = structures
        .into_iter()
        .map(|structure: Structure<Stmt>| -> Structure<Stmt> {
            use Structure::*;
            match structure {
                Loop { entries, body } => {
                    let body = simplify_structure(body);
                    Loop { entries, body }
                }
                Multiple { entries, branches } => {
                    let branches = branches
                        .into_iter()
                        .map(|(lbl, ss)| (lbl, simplify_structure(ss)))
                        .collect();
                    Multiple { entries, branches }
                }
                simple => simple,
            }
        })
        .collect();

    let mut acc_structures = Vec::new();

    for structure in structures.iter().rev() {
        match structure {
            Structure::Simple {
                entries,
                body,
                span,
                terminator,
            } => {
                // Here, we ensure that all labels in a terminator are mentioned only once in the
                // terminator.
                let terminator: GenTerminator<StructureLabel<Stmt>> =
                    if let Switch { expr, cases } = terminator {
                        use StructureLabel::*;

                        // Here, we group patterns by the label they go to.
                        type Merged = IndexMap<Label, Vec<Pat>>;
                        let mut merged_goto: Merged = IndexMap::new();
                        let mut merged_break: Merged = IndexMap::new();
                        let mut merged_continue = IndexMap::new();

                        for (pat, lbl) in cases {
                            match lbl {
                                GoTo(lbl) => {
                                    merged_goto
                                        .entry(lbl.clone())
                                        .or_default()
                                        .push(pat.clone());
                                }
                                BreakTo(lbl) => {
                                    merged_break
                                        .entry(lbl.clone())
                                        .or_default()
                                        .push(pat.clone());
                                }
                                ContinueTo { loop_label, target } => {
                                    merged_continue
                                        // TODO: It might be nice to make a struct for the key,
                                        // because currently it's easy to confuse which label is the
                                        // loop label and which is the target.
                                        .entry((loop_label.clone(), target.clone()))
                                        .or_insert(Vec::new())
                                        .push(pat.clone());
                                }
                                _ => panic!("simplify_structure: Nested precondition violated"),
                            };
                        }

                        // When converting these patterns back into a vector, we have to be careful to
                        // preserve their initial order (so that the default pattern doesn't end up on
                        // top).
                        let mut cases_new = Vec::new();
                        for (_, lbl) in cases.iter().rev() {
                            match lbl {
                                GoTo(lbl) => match merged_goto.swap_remove(lbl) {
                                    None => {}
                                    Some(mut pats) => {
                                        let pat = if pats.len() == 1 {
                                            pats.pop().unwrap()
                                        } else {
                                            mk().or_pat(pats)
                                        };
                                        cases_new.push((pat, GoTo(lbl.clone())))
                                    }
                                },
                                BreakTo(lbl) => match merged_break.swap_remove(lbl) {
                                    None => {}
                                    Some(mut pats) => {
                                        let pat = if pats.len() == 1 {
                                            pats.pop().unwrap()
                                        } else {
                                            mk().or_pat(pats)
                                        };
                                        cases_new.push((pat, BreakTo(lbl.clone())))
                                    }
                                },
                                ContinueTo { loop_label, target } => {
                                    if let Some(mut pats) = merged_continue
                                        .swap_remove(&(loop_label.clone(), target.clone()))
                                    {
                                        let pat = if pats.len() == 1 {
                                            pats.pop().unwrap()
                                        } else {
                                            mk().or_pat(pats)
                                        };
                                        cases_new.push((
                                            pat,
                                            ContinueTo {
                                                loop_label: loop_label.clone(),
                                                target: target.clone(),
                                            },
                                        ))
                                    }
                                }
                                _ => panic!("simplify_structure: Nested precondition violated"),
                            };
                        }
                        cases_new.reverse();

                        Switch {
                            expr: expr.clone(),
                            cases: cases_new,
                        }
                    } else {
                        terminator.clone()
                    };

                match acc_structures.pop() {
                    Some(Structure::Multiple {
                        entries: _,
                        branches,
                    }) => {
                        use StructureLabel::*;
                        let rewrite = |t: &StructureLabel<Stmt>| match t {
                            GoTo(to) => {
                                if let Some(branch) = branches.get(to) {
                                    Nested(branch.clone())
                                } else {
                                    GoTo(to.clone())
                                }
                            }

                            BreakTo(to) => {
                                if let Some(branch) = branches.get(to) {
                                    Nested(branch.clone())
                                } else {
                                    BreakTo(to.clone())
                                }
                            }

                            ContinueTo { loop_label, target } => ContinueTo {
                                loop_label: loop_label.clone(),
                                target: target.clone(),
                            },

                            Nested(_) => panic!("simplify_structure: Nested precondition violated"),
                        };

                        let terminator = terminator.map_labels(rewrite);
                        let body = body.clone();
                        let span = *span;
                        let entries = entries.clone();
                        acc_structures.push(Structure::Simple {
                            entries,
                            body,
                            span,
                            terminator,
                        });
                    }
                    possibly_popped => {
                        if let Some(popped) = possibly_popped {
                            acc_structures.push(popped);
                        }

                        let entries = entries.clone();
                        let body = body.clone();
                        let span = *span;
                        let terminator = terminator.clone();
                        acc_structures.push(Structure::Simple {
                            entries,
                            body,
                            span,
                            terminator,
                        });
                    }
                }
            }

            other_structure => acc_structures.push(other_structure.clone()),
        }
    }

    acc_structures.reverse();
    acc_structures
}

/// Find nodes outside the graph pointed to from nodes inside the graph. Note that `ExitTo`
/// is not considered here - only `GoTo`.
fn out_edges(blocks: &StructuredBlocks) -> IndexSet<Label> {
    blocks
        .iter()
        .flat_map(|(_, bb)| bb.successors())
        .filter(|lbl| !blocks.contains_key(lbl))
        .collect()
}

/// Transforms `{1: {'a', 'b'}, 2: {'b'}}` into `{'a': {1}, 'b': {1,2}}`.
fn flip_edges(map: IndexMap<Label, IndexSet<Label>>) -> IndexMap<Label, IndexSet<Label>> {
    let mut flipped_map: IndexMap<Label, IndexSet<Label>> = IndexMap::new();
    for (lbl, vals) in map {
        for val in vals {
            flipped_map.entry(val).or_default().insert(lbl.clone());
        }
    }
    flipped_map
}

/// Calculates the transitive closure of a directed graph via depth-first
/// search.
///
/// Given an adjacency list, represented as a map from each label to its
/// immediate successors, calculate which labels are reachable from each
/// starting label. A label does not count as reachable from itself unless there
/// is a back edge from one of its successors (including if the block is a
/// successor of itself).
fn transitive_closure<V: Clone + Hash + Eq>(
    adjacency_list: &IndexMap<V, IndexSet<V>>,
) -> IndexMap<V, IndexSet<V>> {
    let mut edges: IndexSet<(V, V)> = IndexSet::new();
    let mut to_visit: Vec<(V, V)> = adjacency_list
        .keys()
        .map(|v| (v.clone(), v.clone()))
        .collect();

    while let Some((s, v)) = to_visit.pop() {
        for i in adjacency_list.get(&v).unwrap_or(&IndexSet::new()) {
            if edges.insert((s.clone(), i.clone())) {
                to_visit.push((s.clone(), i.clone()));
            }
        }
    }

    let mut closure: IndexMap<V, IndexSet<V>> = IndexMap::new();
    for (f, t) in edges {
        closure.entry(f).or_default().insert(t);
    }

    closure
}
