//! This modules handles converting a a control-flow graph `Cfg` into `Vec<Structure>`, optionally
//! simplifying the latter.

use super::*;

/// Convert the CFG into a sequence of structures
pub fn reloop(
    cfg: Cfg<Label, StmtOrDecl>,  // the control flow graph to reloop
    mut store: DeclStmtStore,     // store of what to do with declarations
    simplify_structures: bool,    // simplify the output structure
    use_c_loop_info: bool,        // use the loop information in the CFG (slower, but better)
    use_c_multiple_info: bool,    // use the multiple information in the CFG (slower, but better)
    live_in: IndexSet<CDeclId>,    // declarations we assume are live going into this graph
) -> (Vec<Stmt>, Vec<Structure<StmtOrComment>>) {

    let entries = cfg.entries;
    let blocks = cfg.nodes
        .into_iter()
        .map(|(lbl, bb)| {
            let terminator = bb.terminator.map_labels(|l| StructureLabel::GoTo(*l));
            (lbl, BasicBlock { body: bb.body, terminator, defined: bb.defined, live: bb.live })
        })
        .collect();

    let mut relooped_with_decls: Vec<Structure<StmtOrDecl>> = vec![];
    let loop_info = if use_c_loop_info { Some(cfg.loops) } else { None };
    let multiple_info = if use_c_multiple_info { Some(cfg.multiples) } else { None };
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
    let mut relooped: Vec<Structure<StmtOrComment>> = relooped_with_decls
        .into_iter()
        .map(|s| s.place_decls(&lift_me, &mut store))
        .collect();

    if simplify_structures {
        relooped = simplify_structure(relooped)
    }

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

impl RelooperState {

    /// Recursive helper for `reloop`.
    ///
    /// TODO: perhaps manually perform TCO?
    fn relooper(
        &mut self,
        entries: IndexSet<Label>, // current entry points into the CFG
        mut blocks: IndexMap<Label, BasicBlock<StructureLabel<StmtOrDecl>, StmtOrDecl>>, // the blocks in the sub-CFG considered
        result: &mut Vec<Structure<StmtOrDecl>>, // the generated structures are appended to this
        disable_heuristics: bool,
    ) {
        // Find nodes outside the graph pointed to from nodes inside the graph. Note that `ExitTo`
        // is not considered here - only `GoTo`.
        fn out_edges<T>(blocks: &IndexMap<Label, BasicBlock<StructureLabel<StmtOrDecl>, T>>) -> IndexSet<Label> {
            blocks
                .iter()
                .flat_map(|(_, bb)| bb.successors())
                .filter(|lbl| !blocks.contains_key(lbl))
                .collect()
        }

        // Transforms `{1: {'a', 'b'}, 2: {'b'}}` into `{'a': {1}, 'b': {1,2}}`.
        fn flip_edges(map: IndexMap<Label, IndexSet<Label>>) -> IndexMap<Label, IndexSet<Label>> {
            let mut flipped_map: IndexMap<Label, IndexSet<Label>> = IndexMap::new();
            for (lbl, vals) in map {
                for val in vals {
                    flipped_map.entry(val).or_insert(IndexSet::new()).insert(lbl);
                }
            }
            flipped_map
        }

        type StructuredBlocks = IndexMap<Label, BasicBlock<StructureLabel<StmtOrDecl>, StmtOrDecl>>;

        // Find all labels reachable via a `GoTo` from the current set of blocks
        let reachable_labels: IndexSet<Label> = blocks
            .iter()
            .flat_map(|(_, bb)| bb.successors())
            .collect();

        // Split the entry labels into those that some basic block may goto versus those that none can
        // goto.
        let (some_branch_to, none_branch_to): (IndexSet<Label>, IndexSet<Label>) = entries
            .iter()
            .cloned()
            .partition(|entry| reachable_labels.contains(entry));


        // --------------------------------------
        // Base case
        if none_branch_to.is_empty() && some_branch_to.is_empty() {
            return;
        }

        // --------------------------------------
        // Simple blocks
        if none_branch_to.len() == 1 && some_branch_to.is_empty() {
            let entry = *none_branch_to.iter().next().expect("Should find exactly one entry");

            if let Some(bb) = blocks.remove(&entry) {
                let new_entries = bb.successors();
                let BasicBlock { body, terminator, live, defined } = bb;

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

                result.push(Structure::Simple { entries, body, terminator });

                self.relooper(new_entries, blocks, result, false);
            } else {
                let body = vec![];
                let terminator = Jump(StructureLabel::GoTo(entry));

                result.push(Structure::Simple { entries, body, terminator });
            };

            return;
        }

        // --------------------------------------
        // Skipping to blocks placed later

        // Split the entry labels into those that are in the current blocks, and those that aren't
        let (present, absent): (IndexSet<Label>, IndexSet<Label>) = entries
            .iter()
            .cloned()
            .partition(|entry| blocks.contains_key(entry));

        if !absent.is_empty() {
            if !present.is_empty() {
                let branches = absent.into_iter().map(|lbl| (lbl, vec![])).collect();

                let mut then = vec![];
                self.relooper(present, blocks, &mut then, false);

                result.push(Structure::Multiple { entries, branches, then })
            };

            return;
        }


        // --------------------------------------
        // Loops


        // DFS transitive closure
        fn transitive_closure<V: Copy + Hash + Eq>(adjacency_list: &IndexMap<V, IndexSet<V>>) -> IndexMap<V, IndexSet<V>> {
            let mut edges: IndexSet<(V, V)> = IndexSet::new();
            let mut to_visit: Vec<(V, V)> = adjacency_list.keys().map(|v| (*v,*v)).collect();

            while let Some((s,v)) = to_visit.pop() {
                for i in adjacency_list.get(&v).unwrap_or(&IndexSet::new()) {
                    if edges.insert((s,*i)) {
                        to_visit.push((s, *i));
                    }
                }
            }

            let mut closure: IndexMap<V, IndexSet<V>> = IndexMap::new();
            for (f,t) in edges {
                closure.entry(f).or_insert(IndexSet::new()).insert(t);
            }

            closure
        }

        // This information is necessary for both the `Loop` and `Multiple` cases
        let (predecessor_map, strict_reachable_from) = {
            let mut successor_map: IndexMap<Label, IndexSet<Label>> = blocks
                .iter()
                .map(|(lbl, bb)| (*lbl, bb.successors()))
                .collect();

            let strict_reachable_from = flip_edges(transitive_closure(&successor_map));
            let predecessor_map = flip_edges(successor_map);

            (predecessor_map, strict_reachable_from)
        };


        // Try to match an existing branch point (from the intial C). See `MultipleInfo` for more
        // information on this.
        let mut recognized_c_multiple = false;
        if let Some(ref multiple_info) = self.multiple_info {

            let entries_key = entries.iter().cloned().collect();
            if let Some(&(join, ref arms)) = multiple_info.get_multiple(&entries_key) {
                recognized_c_multiple = true;

                for (entry, content) in arms {
                    let mut to_visit: Vec<Label> = vec![*entry];
                    let mut visited: IndexSet<Label> = IndexSet::new();

                    while let Some(lbl) = to_visit.pop() {
                        // Stop at things you've already seen or the join block
                        if !visited.insert(lbl) || lbl == join {
                            continue;
                        }

                        if let Some(bb) = blocks.get(&lbl) {

                            // If this isn't something we are supposed to encounter, break and fail.
                            if !content.contains(&lbl) {
                                recognized_c_multiple = false;
                                break;
                            }

                            to_visit.extend(bb.successors())
                        }
                    }

                    // Check we've actually visited all of the expected content
                    visited.remove(&join);
                    if let Some(_) = visited.difference(content).next() {
                        recognized_c_multiple = false;
                    }
                }
            }
        }
        recognized_c_multiple = recognized_c_multiple && !disable_heuristics;


        if none_branch_to.is_empty() && !recognized_c_multiple {
            let new_returns: IndexSet<Label> = strict_reachable_from
                .iter()
                .filter(|&(lbl, _)| blocks.contains_key(lbl) && entries.contains(lbl))
                .flat_map(|(_, ref reachable)| reachable.iter())
                .cloned()
                .collect();

            // Partition blocks into those belonging in or after the loop
            let (mut body_blocks, mut follow_blocks): (StructuredBlocks, StructuredBlocks) = blocks
                .into_iter()
                .partition(|&(ref lbl, _)| new_returns.contains(lbl) || entries.contains(lbl));
            let mut follow_entries = out_edges(&body_blocks);


            // Try to match an existing loop (from the initial C)
            let mut matched_existing_loop = false;
            if let Some(ref loop_info) = self.loop_info {
                let must_be_in_loop = entries.iter().chain(new_returns.iter()).cloned();
                if let Some(loop_id) = loop_info.tightest_common_loop(must_be_in_loop) {

                    // Construct the target group of labels
                    let mut desired_body: IndexSet<Label> = loop_info.get_loop_contents(loop_id).clone();
                    desired_body.retain(|l| !entries.contains(l));
                    desired_body.retain(|l| !new_returns.contains(l));

                    // Make copies that we can trash
                    let mut body_blocks_copy = body_blocks.clone();
                    let mut follow_blocks_copy = follow_blocks.clone();
                    let mut follow_entries_copy = follow_entries.clone();

                    if loops::match_loop_body(
                        desired_body,
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

            // If matching an existing loop didn't work, fall back on a heuristic
            if !matched_existing_loop {
                loops::heuristic_loop_body(
                    &predecessor_map,
                    &mut body_blocks,
                    &mut follow_blocks,
                    &mut follow_entries,
                );
            }

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

            let mut body = vec![];
            self.open_scope();
            self.relooper(entries.clone(), body_blocks, &mut body, false);
            self.close_scope();

            result.push(Structure::Loop { entries, body });
            self.relooper(follow_entries, follow_blocks, result, false);

            return;
        }


        // --------------------------------------
        // Multiple

        // Like `strict_reachable_from`, but entries also reach themselves
        let mut reachable_from: IndexMap<Label, IndexSet<Label>> = strict_reachable_from;
        for entry in &entries {
            reachable_from.entry(*entry).or_insert(IndexSet::new()).insert(*entry);
        }

        // Blocks that are reached by only one label
        let singly_reached: IndexMap<Label, IndexSet<Label>> = flip_edges(reachable_from
            .into_iter()
            .map(|(lbl, reachable)| (lbl, &reachable & &entries.clone()))
            .filter(|&(_, ref reachable)| reachable.len() == 1)
            .collect()
        );

        let handled_entries: IndexMap<Label, StructuredBlocks> = singly_reached
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

        let unhandled_entries: IndexSet<Label> = entries
            .iter()
            .filter(|e| !handled_entries.contains_key(*e))
            .cloned()
            .collect();

        let mut handled_blocks: StructuredBlocks = IndexMap::new();
        for (_, map) in &handled_entries {
            for (k, v) in map {
                handled_blocks.entry(*k).or_insert(v.clone());
            }
        }
        let handled_blocks = handled_blocks;

        let follow_blocks: StructuredBlocks = blocks
            .into_iter()
            .filter(|&(lbl, _)| !handled_blocks.contains_key(&lbl))
            .collect();

        let follow_entries: IndexSet<Label> = &unhandled_entries | &out_edges(&handled_blocks);

        let mut all_handlers: IndexMap<Label, Vec<Structure<StmtOrDecl>>> = handled_entries
            .into_iter()
            .map(|(lbl, blocks)| {
                let entries = indexset![lbl];

                let mut structs: Vec<Structure<StmtOrDecl>> = vec![];
                self.open_scope();
                self.relooper(entries, blocks, &mut structs, false);
                self.close_scope();

                (lbl, structs)
            })
            .collect();

        let handler_keys: IndexSet<Label> = all_handlers.keys().cloned().collect();
        let (then, branches) = if handler_keys == entries {
            let a_key = *all_handlers.keys().next().expect("no handlers found");
            let last_handler = all_handlers.remove(&a_key).expect("just got this key");
            (last_handler, all_handlers)
        } else {
            (vec![], all_handlers)
        };

        let disable_heuristics = follow_entries == entries;
        result.push(Structure::Multiple { entries, branches, then });
        self.relooper(follow_entries, follow_blocks, result, disable_heuristics);

        return;
    }
}

/// Nested precondition: `structures` will contain no `StructureLabel::Nested` terminators.
fn simplify_structure<Stmt: Clone>(structures: Vec<Structure<Stmt>>) -> Vec<Structure<Stmt>> {

    // Recursive calls come first
    let structures: Vec<Structure<Stmt>> = structures
        .into_iter()
        .map(|structure: Structure<Stmt>| -> Structure<Stmt> {
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

    let mut acc_structures: Vec<Structure<Stmt>> = vec![];

    for structure in structures.iter().rev() {
        match structure {
            &Structure::Simple { ref entries, ref body, ref terminator } => {

                // Here, we ensure that all labels in a terminator are mentioned only once in the
                // terminator.
                let terminator: GenTerminator<StructureLabel<Stmt>> = if let &Switch { ref expr, ref cases } = terminator {

                    // Here, we group patterns by the label they go to.
                    let mut merged_goto: IndexMap<Label, Vec<P<Pat>>> = IndexMap::new();
                    let mut merged_exit: IndexMap<Label, Vec<P<Pat>>> = IndexMap::new();

                    for &(ref pats, ref lbl) in cases {
                        match lbl {
                            &StructureLabel::GoTo(lbl) => merged_goto.entry(lbl).or_insert(vec![]).extend(pats.clone()),
                            &StructureLabel::ExitTo(lbl) => merged_exit.entry(lbl).or_insert(vec![]).extend(pats.clone()),
                            _ => panic!("simplify_structure: Nested precondition violated")
                        }
                    }

                    // When converting these patterns back into a vector, we have to be careful to
                    // preserve their initial order (so that the default pattern doesn't end up on
                    // top).
                    let mut cases_new: Vec<_> = vec![];
                    for &(_, ref lbl) in cases.iter().rev() {
                        match lbl {
                            &StructureLabel::GoTo(lbl) =>
                                match merged_goto.remove(&lbl) {
                                    None => { },
                                    Some(pats) => cases_new.push((pats, StructureLabel::GoTo(lbl))),
                                }
                            &StructureLabel::ExitTo(lbl) =>
                                match merged_exit.remove(&lbl) {
                                    None => { },
                                    Some(pats) => cases_new.push((pats, StructureLabel::ExitTo(lbl))),
                                }
                            _ => panic!("simplify_structure: Nested precondition violated")
                        };
                    }
                    cases_new.reverse();

                    Switch { expr: expr.clone(), cases: cases_new }
                } else {
                    terminator.clone()
                };

                match acc_structures.pop() {
                    Some(Structure::Multiple { entries: _, ref branches, ref then }) => {
                        let rewrite = |t: &StructureLabel<Stmt>| {
                            match t {
                                &StructureLabel::GoTo(ref to) => {
                                    let entries: IndexSet<_> = vec![*to].into_iter().collect();
                                    let body: Vec<Stmt> = vec![];
                                    let terminator = Jump(StructureLabel::GoTo(*to));
                                    let first_structure = Structure::Simple { entries, body, terminator };

                                    let mut nested: Vec<Structure<Stmt>> = vec![first_structure];
                                    nested.extend(branches.get(to).cloned().unwrap_or(then.clone()));

                                    StructureLabel::Nested(nested)
                                }
                                &StructureLabel::ExitTo(ref to) => StructureLabel::ExitTo(*to),
                                _ => panic!("simplify_structure: Nested precondition violated")
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
