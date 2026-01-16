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
    let entries: IndexSet<Label> = vec![cfg.entries.clone()].into_iter().collect();
    let blocks: StructuredBlocks = cfg
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

    let successors = blocks
        .iter()
        .map(|(lbl, bb)| (lbl.clone(), bb.successors()))
        .collect();
    let global_predecessors = flip_edges(successors);
    let domination_sets = flip_edges(compute_dominators(&cfg.entries, &global_predecessors));

    let mut state = RelooperState {
        scopes: vec![live_in],
        lifted: IndexSet::new(),
        loop_info,
        _multiple_info: multiple_info,
        domination_sets,
        global_predecessors,
    };
    state.relooper(entries, blocks, &mut relooped_with_decls);

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

    /// Information about branches.
    ///
    /// Currently unused by the new relooper logic, but we don't want to delete
    /// this just yet because it may still have use.
    _multiple_info: Option<MultipleInfo<Label>>,

    /// The set of nodes dominated by each node in the CFG.
    ///
    /// Node A dominates another node B if all paths to B go through A. This
    /// information is used when building loops to determine which nodes should
    /// go inside the loop, since it tells us which nodes can be inlined within
    /// the branch of another node.
    domination_sets: AdjacencyList,

    /// Global predecessor information.
    ///
    /// Normally we only need local predecessor information, but when we're deciding what
    /// nodes to pull into a loop it's useful to have global predecessor information to
    /// figure out which nodes are merge nodes.
    global_predecessors: AdjacencyList,
}

impl RelooperState {
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

/// A mapping from one label to a set of labels.
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
    ) {
        // If there are no entries or blocks then we are at the end of a branch and
        // there's nothing left to reloop.
        if entries.is_empty() || blocks.is_empty() {
            return;
        }

        let local_successors = blocks
            .iter()
            .map(|(lbl, bb)| (lbl.clone(), bb.successors()))
            .collect();

        // Map from each label to the set of labels that reach it. A label does not
        // count as reaching itself unless there is an explicit back edge to that label.
        //
        // Some keys may not correspond to any blocks in the current set, since blocks
        // may have successors that are outside the current set. Labels in the values
        // set will always correspond to blocks in the current set.
        //
        // NOTE: We need to determine reachability using the current local set of
        // blocks. Global reachability won't work here because when we're inside a loop
        // body we don't want to consider back edges, which we strip before processing
        // the loop body.
        let strict_reachable_from = flip_edges(transitive_closure(&local_successors));

        // Handle our simple case of only 1 entry. If there's no back edge to the entry
        // we generate a `Simple` structure, otherwise we make a loop.
        if entries.len() == 1 {
            let entry = entries.first().unwrap();
            if !strict_reachable_from.contains_key(entry) {
                let bb = blocks
                    .swap_remove(entry)
                    .expect("Entry not present in current blocks");
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
                        *lbl = StructureLabel::BreakTo(label.clone())
                    }
                }

                result.push(Structure::Simple {
                    entries,
                    body,
                    span,
                    terminator,
                });

                self.relooper(new_entries, blocks, result);
            } else {
                self.make_loop(&strict_reachable_from, blocks, entries, result);
            }
            return;
        }

        // Sanity check that we have entries that are in our current set of blocks. We
        // may have entries that aren't present in our current blocks when we're inside
        // the branch of a `Multiple`, but there must always be at least one present
        // entry.
        assert!(
            entries.iter().any(|entry| blocks.contains_key(entry)),
            "No entries are in our current set of blocks, entries: {entries:?}, blocks: {:?}",
            blocks.keys().collect::<Vec<_>>(),
        );

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
        let singly_reached: AdjacencyList = flip_edges(
            reachable_from
                .into_iter()
                .map(|(lbl, reached_from)| (lbl, &reached_from & &entries))
                .filter(|(_, reached_from)| reached_from.len() == 1)
                .collect(),
        );

        // If we have any blocks that are only reachable from one entry, then we can
        // create a `Multiple` structure.
        if !singly_reached.is_empty() {
            // Map from entry labels to the set of blocks only reachable from that entry,
            // i.e. `singly_reached` but with the set of reachable labels replaced by the
            // corresponding blocks. We also filter out any entries that aren't present in
            // our current set of blocks, as we don't want branches for those entries.
            let handled_entries: IndexMap<Label, StructuredBlocks> = singly_reached
                .into_iter()
                .filter(|(lbl, _)| entries.contains(lbl) && blocks.contains_key(lbl))
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
            let branches: IndexMap<_, _> = handled_entries
                .into_iter()
                .map(|(lbl, blocks)| {
                    let entries = indexset![lbl.clone()];

                    let mut structs = vec![];
                    self.open_scope();
                    self.relooper(entries, blocks, &mut structs);
                    self.close_scope();

                    (lbl, structs)
                })
                .collect();

            result.push(Structure::Multiple { entries, branches });

            self.relooper(follow_entries, follow_blocks, result);
            return;
        }

        // If we couldn't make a `Multiple`, we have multiple entries and all entries
        // can be reached by other entries. This means irreducible control flow, which
        // we have to process by making a loop.
        self.make_loop(&strict_reachable_from, blocks, entries, result);
    }

    fn make_loop(
        &mut self,
        strict_reachable_from: &AdjacencyList,
        blocks: StructuredBlocks,
        entries: IndexSet<Label>,
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
        let (mut body_blocks, mut follow_blocks) = blocks
            .into_iter()
            .partition::<StructuredBlocks, _>(|(lbl, _)| {
                new_returns.contains(lbl) || entries.contains(lbl)
            });

        // Any block that follows a body block but is not itself a body block
        // becomes an entry for the follow blocks.
        let mut follow_entries = out_edges(&body_blocks);

        // Try to match an existing loop (from the initial C)
        let mut matched_existing_loop = false;
        if let Some(ref loop_info) = self.loop_info {
            let must_be_in_loop = entries.iter().chain(new_returns.iter()).cloned();
            if let Some(loop_id) = loop_info.tightest_common_loop(must_be_in_loop) {
                // Construct the target group of labels
                let mut desired_body: IndexSet<Label> =
                    loop_info.get_loop_contents(loop_id).clone();
                desired_body.retain(|l| !entries.contains(l));
                desired_body.retain(|l| !new_returns.contains(l));

                // Make copies that we can trash
                let mut body_blocks_copy = body_blocks.clone();
                let mut follow_blocks_copy = follow_blocks.clone();
                let mut follow_entries_copy = follow_entries.clone();

                if loops::match_loop_body(
                    desired_body,
                    &strict_reachable_from,
                    &mut body_blocks_copy,
                    &mut follow_blocks_copy,
                    &mut follow_entries_copy,
                ) {
                    matched_existing_loop = true;

                    body_blocks = body_blocks_copy;
                    follow_blocks = follow_blocks_copy;
                    follow_entries = follow_entries_copy;
                }
            }
        }

        // If there's more than one path out of the loop, we want to try moving
        // additional nodes into the loop to avoid ending up with a multiple after the
        // loop.
        if !matched_existing_loop && follow_entries.len() > 1 {
            // Gather the set of follow entries that only have a single in edge. These can
            // be inlined into a branch in one of the loop body nodes, so we want to pull
            // them into the loop.
            let inlined = follow_entries
                .iter()
                .filter(|&e| self.global_predecessors[e].len() == 1);

            // Move all nodes dominated by an inlined node into the loop. This will include
            // the inlined node since all nodes dominate themself.
            for inlined in inlined {
                for dominated in &self.domination_sets[inlined] {
                    let block = follow_blocks
                        .remove(dominated)
                        .expect("Dominated node not in follow blocks");
                    body_blocks.insert(dominated.clone(), block);
                }
            }

            // Recalculate our follow entries based on which nodes we moved into the loop body.
            follow_entries = out_edges(&body_blocks);
        }

        // Rewrite `GoTo`s that exit the loop body.
        //
        // For our body blocks, rewrite terminator `GoTo` labels if they target an entry
        // or a follow entry. These are exits from the loop body, i.e. if they branch
        // back to an entry then it's a `continue`, and if they branch to a follow entry
        // then it's a `break`. This is necessary so that when we reloop the body blocks
        // it doesn't get tripped up by the back/out edges. Without this, the reloop
        // pass for the loop body would see the back edges and decide to produce a loop
        // structure, even though we've already done that.
        //
        // For back edges we also track which label we're going to use to identify the
        // loop. A loop may have multiple entries (i.e. irreducible control flow), so we
        // need to know which label to use when generating the AST.
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
        self.relooper(entries.clone(), body_blocks, &mut body);
        self.close_scope();

        result.push(Structure::Loop { entries, body });

        self.relooper(follow_entries, follow_blocks, result);
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
fn flip_edges(map: AdjacencyList) -> AdjacencyList {
    let mut flipped_map: AdjacencyList = IndexMap::new();
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

/// Calculate the dominator sets for each node in the CFG.
///
/// A node `d` dominates node `n` if every path from the entry to `n` must pass
/// through `d`. Every node dominates itself.
///
/// Returns a map from each label to the set of labels that dominate it.
fn compute_dominators(entry: &Label, predecessor_map: &AdjacencyList) -> AdjacencyList {
    // All non-entry nodes (nodes that have predecessors, excluding entry itself).
    let nodes: Vec<Label> = predecessor_map
        .keys()
        .filter(|k| *k != entry)
        .cloned()
        .collect();

    // The initial conservative dominator set includes entry + all non-entry nodes.
    let initial_dom: IndexSet<Label> = nodes
        .iter()
        .cloned()
        .chain(std::iter::once(entry.clone()))
        .collect();

    // Entry is dominated only by itself, all others start dominated by all nodes.
    let mut dominators = IndexMap::new();
    dominators.insert(entry.clone(), indexset! { entry.clone() });
    for node in &nodes {
        dominators.insert(node.clone(), initial_dom.clone());
    }

    let mut changed = true;
    while changed {
        changed = false;
        for node in &nodes {
            let preds = &predecessor_map[node];

            let mut new_dom = initial_dom.clone();
            for pred in preds {
                let pred_dom = &dominators[pred];
                new_dom.retain(|x| pred_dom.contains(x));
            }
            new_dom.insert(node.clone()); // A node always dominates itself.

            if dominators.get(node) != Some(&new_dom) {
                dominators.insert(node.clone(), new_dom);
                changed = true;
            }
        }
    }

    dominators
}

#[cfg(test)]
mod tests {
    use super::*;

    fn label(id: u64) -> Label {
        Label::Synthetic(id)
    }

    fn build_successor_map(edges: &[(u64, u64)]) -> AdjacencyList {
        let mut graph: AdjacencyList = IndexMap::new();
        for &(from, to) in edges {
            graph.entry(label(from)).or_default().insert(label(to));
        }
        // Ensure all nodes are in the graph, even if they have no outgoing edges.
        for &(_, to) in edges {
            graph.entry(label(to)).or_default();
        }
        graph
    }

    fn build_predecessor_map(edges: &[(u64, u64)]) -> AdjacencyList {
        flip_edges(build_successor_map(edges))
    }

    #[test]
    fn test_dominators_linear_chain() {
        // Linear CFG: 1 -> 2 -> 3 -> 4
        let predecessors = build_predecessor_map(&[(1, 2), (2, 3), (3, 4)]);
        let entry = label(1);
        let doms = compute_dominators(&entry, &predecessors);

        assert_eq!(doms.get(&label(1)), Some(&indexset! { label(1) }));
        assert_eq!(doms.get(&label(2)), Some(&indexset! { label(1), label(2) }));
        assert_eq!(
            doms.get(&label(3)),
            Some(&indexset! { label(1), label(2), label(3) })
        );
        assert_eq!(
            doms.get(&label(4)),
            Some(&indexset! { label(1), label(2), label(3), label(4) })
        );
    }

    #[test]
    fn test_dominators_diamond() {
        // Diamond CFG:
        //       1
        //      / \
        //     2   3
        //      \ /
        //       4
        let predecessors = build_predecessor_map(&[(1, 2), (1, 3), (2, 4), (3, 4)]);
        let entry = label(1);
        let doms = compute_dominators(&entry, &predecessors);

        assert_eq!(doms.get(&label(1)), Some(&indexset! { label(1) }));
        assert_eq!(doms.get(&label(2)), Some(&indexset! { label(1), label(2) }));
        assert_eq!(doms.get(&label(3)), Some(&indexset! { label(1), label(3) }));
        assert_eq!(doms.get(&label(4)), Some(&indexset! { label(1), label(4) }));
    }

    #[test]
    fn test_dominators_loop() {
        // CFG with a loop:
        //     1 -> 2 -> 3
        //          ^    |
        //          +----+
        let predecessors = build_predecessor_map(&[(1, 2), (2, 3), (3, 2)]);
        let entry = label(1);
        let doms = compute_dominators(&entry, &predecessors);

        assert_eq!(doms.get(&label(1)), Some(&indexset! { label(1) }));
        assert_eq!(doms.get(&label(2)), Some(&indexset! { label(1), label(2) }));
        assert_eq!(
            doms.get(&label(3)),
            Some(&indexset! { label(1), label(2), label(3) })
        );
    }

    #[test]
    fn test_dominators_complex() {
        // More complex CFG:
        //       1
        //      / \
        //     2   3
        //     |   |
        //     4   5
        //      \ /
        //       6
        let predecessors = build_predecessor_map(&[(1, 2), (1, 3), (2, 4), (3, 5), (4, 6), (5, 6)]);
        let entry = label(1);
        let doms = compute_dominators(&entry, &predecessors);

        assert_eq!(doms.get(&label(1)), Some(&indexset! { label(1) }));
        assert_eq!(doms.get(&label(2)), Some(&indexset! { label(1), label(2) }));
        assert_eq!(doms.get(&label(3)), Some(&indexset! { label(1), label(3) }));
        assert_eq!(
            doms.get(&label(4)),
            Some(&indexset! { label(1), label(2), label(4) })
        );
        assert_eq!(
            doms.get(&label(5)),
            Some(&indexset! { label(1), label(3), label(5) })
        );
        assert_eq!(doms.get(&label(6)), Some(&indexset! { label(1), label(6) }));
    }

    #[test]
    fn test_dominators_single_node() {
        let predecessors: AdjacencyList = IndexMap::new();
        let entry = label(1);
        let doms = compute_dominators(&entry, &predecessors);

        assert_eq!(doms.get(&label(1)), Some(&indexset! { label(1) }));
    }

    #[test]
    fn test_dominators_self_loop() {
        // Node with self-loop: 1 -> 2 -> 2
        let predecessors = build_predecessor_map(&[(1, 2), (2, 2)]);
        let entry = label(1);
        let doms = compute_dominators(&entry, &predecessors);

        assert_eq!(doms.get(&label(1)), Some(&indexset! { label(1) }));
        assert_eq!(doms.get(&label(2)), Some(&indexset! { label(1), label(2) }));
    }

    #[test]
    fn test_dominators_loop_with_branch() {
        // CFG with loop and branches:
        //     1 -> 2, 3
        //     2 -> 3, 4
        //     3 -> 1, 5  (back edge to 1)
        //     4 -> 5
        //     5 -> END
        let predecessors =
            build_predecessor_map(&[(1, 2), (1, 3), (2, 3), (2, 4), (3, 1), (3, 5), (4, 5)]);
        let entry = label(1);
        let doms = compute_dominators(&entry, &predecessors);

        assert_eq!(doms.get(&label(1)), Some(&indexset! { label(1) }));
        assert_eq!(doms.get(&label(2)), Some(&indexset! { label(1), label(2) }));
        assert_eq!(doms.get(&label(3)), Some(&indexset! { label(1), label(3) }));
        assert_eq!(
            doms.get(&label(4)),
            Some(&indexset! { label(1), label(2), label(4) })
        );
        assert_eq!(doms.get(&label(5)), Some(&indexset! { label(1), label(5) }));
    }
}
