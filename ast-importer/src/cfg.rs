
use syntax;
use syntax::ast::*;
use syntax::ptr::P;
use idiomize::ast_manip::make_ast::*;
use std::collections::{HashSet, HashMap};
use c_ast::CLabelId;
use std::ops::Index;
use syntax::print::pprust;
use std::io;
use std::fs::File;
use std::io::Write;
use std::ops::Deref;


use translator::*;
use c_ast::*;

/// These labels identify basic blocks in the CFG.
#[derive(Copy,Clone,PartialEq,Eq,PartialOrd,Ord,Debug,Hash)]
pub enum Label {
    /// Some labels come directly from the C side (namely those created from labels, cases, and
    /// defaults). For those, we just re-use the `CLabelId` of the C AST node.
    FromC(CLabelId),

    /// Most labels are synthetically created while unwrapping control-flow constructs (like loops)
    /// into basic blocks.
    Synthetic(u64),
}

impl Label {
    fn pretty_print(&self) -> String {
        match self {
            &Label::FromC(CStmtId(label_id)) => format!("c_{}", label_id),
            &Label::Synthetic(syn_id) => format!("s_{}", syn_id),
        }
    }
}

#[derive(Clone,Debug)]
pub enum StructureLabel {
    GoTo(Label),
    ExitTo(Label),
    Nested(Vec<Structure>),
}

/// These are the things that the relooper algorithm produces.
#[derive(Clone,Debug)]
pub enum Structure {
    /// Series of statements and what to do after
    Simple {
        entries: HashSet<Label>,
        body: Vec<Stmt>,
        terminator: GenTerminator<StructureLabel>,
    },
    /// Looping constructs
    Loop {
        entries: HashSet<Label>,
        body: Vec<Structure>,
    },
    /// Branching constructs??
    Multiple {
        entries: HashSet<Label>,
        branches: HashMap<Label, Vec<Structure>>,
        then: Vec<Structure>,
    }
}

impl Structure {
    fn get_entries(&self) -> &HashSet<Label> {
        match self {
            &Structure::Simple { ref entries, .. } => entries,
            &Structure::Loop { ref entries, .. } => entries,
            &Structure::Multiple { ref entries, .. } => entries,
        }
    }
}

#[derive(Clone, Debug)]
struct BasicBlock<L> {
    /// Jump-free code
    body: Vec<Stmt>,

    /// How to find the next (if any) basic block to go to
    terminator: GenTerminator<L>,
}

impl<L> BasicBlock<L> {
    fn new(terminator: GenTerminator<L>) -> Self {
        BasicBlock {
            body: vec![],
            terminator,
        }
    }

    fn new_jump(target: L) -> Self {
        BasicBlock::new(Jump(target))
    }

    // FIXME: change the type of the 'body' field of 'BasicBlock' to something that makes this efficient
    fn prepend_stmts(self, stmts: Vec<Stmt>) -> Self {
        let new_body = stmts
            .into_iter()
            .chain(self.body)
            .collect();
        BasicBlock { body: new_body, terminator: self.terminator }
    }
}

impl BasicBlock<StructureLabel> {

    /// Get all of the `GoTo` targets of a structure basic block
    fn successors(&self) -> HashSet<Label> {
        self.terminator
            .get_labels()
            .iter()
            .filter_map(|&slbl|
                match slbl {
                    &StructureLabel::GoTo(tgt) => Some(tgt),
                    _ => None,
                }
            )
            .collect()
    }
}

type Terminator = GenTerminator<Label>;
type StructuredTerminator = GenTerminator<StructureLabel>;

#[derive(Clone, Debug)]
pub enum GenTerminator<L> {
    /// End of control-flow. For example: the last statement in a function, or a return
    End,

    /// Unconditional branch to another block
    Jump(L),

    /// Conditional branch to another block. The expression is expected to be a boolean Rust
    /// expression
    Branch(P<Expr>, L, L),

    /// Multi-way branch.
    ///
    /// FIXME: specify more invariants on `expr`/`cases`
    Switch {
        expr: P<Expr>,
        cases: Vec<(P<Expr>, L)>,
        default: L
    }
}

use self::GenTerminator::*;

impl<L> GenTerminator<L> {
    fn map_labels<F: Fn(&L) -> N, N>(&self, func: F) -> GenTerminator<N> {
        match self {
            &End => End,
            &Jump(ref l) => Jump(func(l)),
            &Branch(ref e, ref l1, ref l2) => Branch(e.clone(), func(l1), func(l2)),
            &Switch { ref expr, ref cases, ref default } => Switch {
                expr: expr.clone(),
                cases: cases.iter().map(|&(ref e, ref l)| (e.clone(), func(l))).collect(),
                default: func(default),
            }
        }
    }

    fn get_labels(&self) -> Vec<&L> {
        match self {
            &End => vec![],
            &Jump(ref l) => vec![l],
            &Branch(_, ref l1, ref l2) => vec![l1,l2],
            &Switch { ref cases, ref default, .. } =>
                cases.iter().map(|&(_, ref l)| l).chain(vec![default]).collect(),
        }
    }

    fn get_labels_mut(&mut self) -> Vec<&mut L> {
        match self {
            &mut End => vec![],
            &mut Jump(ref mut l) => vec![l],
            &mut Branch(_, ref mut l1, ref mut l2) => vec![l1,l2],
            &mut Switch { ref mut cases, ref mut default, .. } =>
                cases.iter_mut().map(|&mut (_, ref mut l)| l).chain(vec![default]).collect(),
        }
    }
}

/// The sole purpose of this structure is to accumulate information about what cases/default have
/// been seen which translating the body of the switch.
#[derive(Clone, Debug, Default)]
pub struct SwitchCases {
    cases: Vec<(P<Expr>,Label)>,
    default: Option<Label>,
}

/// A CFG graph.
#[derive(Clone, Debug)]
pub struct Cfg {
    /// Entry point in the graph
    entry: Label,

    /// Nodes in the graph
    nodes: HashMap<Label, BasicBlock<Label>>,
}


fn mk_goto(to: Label) -> Vec<Stmt> {
    unimplemented!()
}

fn mk_if(cond: P<Expr>, then: Vec<Stmt>, els: Vec<Stmt>) -> Vec<Stmt> {
    let e = if els.is_empty() {
        mk().ifte_expr(cond, mk().block(then), None as Option<P<Expr>>)
    } else if then.is_empty() {
        mk().ifte_expr(mk().unary_expr(syntax::ast::UnOp::Not, cond), mk().block(els), None as Option<P<Expr>>)
    } else {
        mk().ifte_expr(cond, mk().block(then), Some(mk().block_expr(mk().block(els))))
    };

    vec![mk().expr_stmt(e)]
}

fn mk_match(cases: Vec<(Label, Vec<Stmt>)>, then: Vec<Stmt>) -> Vec<Stmt> {
    unimplemented!()
}

fn mk_loop(lbl: Option<Label>, body: Vec<Stmt>) -> Vec<Stmt> {
    let e = mk().loop_expr(mk().block(body), lbl.map(|l| l.pretty_print()));
    vec![mk().expr_stmt(e)]
}

fn mk_exit(immediate: bool, exit_style: ExitStyle, label: Label) -> Vec<Stmt> {
    let lbl = if immediate { None } else { Some(label.pretty_print()) };
    let e = match exit_style {
        ExitStyle::Break => mk().break_expr(lbl),
        ExitStyle::Continue => mk().continue_expr(lbl),
    };
    vec![mk().semi_stmt(e)]
}

#[derive(Copy, Clone, Debug)]
enum ExitStyle {
    Continue,
    Break,
}

/// A complete control-flow graph
impl Cfg {

    /// Completely process a statement into a control flow graph.
    pub fn from_stmt_backward(translator: &Translation, stmt_id: CStmtId) -> Cfg {
        let mut cfg_builder = CfgBuilder::new();
        let entry = cfg_builder.graph.entry;

        let end = BasicBlock {
            body: vec![],
            terminator: End,
        };

        let bb = translator.with_scope(|| cfg_builder.convert_stmt_cfg(translator, stmt_id, end));
        cfg_builder.add_block(entry, bb);

        let graph = cfg_builder.graph;

        graph.prune_empty_blocks()
    }

    /// Completely process a statement into a control flow graph.
    pub fn from_stmt_forward(translator: &Translation, stmt_id: CStmtId) -> Cfg {
        let mut cfg_builder = CfgBuilder::new();
        let entry = cfg_builder.graph.entry;

        let body_stuff = translator.with_scope(|| cfg_builder.convert_stmt_cfg_new(translator, stmt_id, (entry, vec![])));
        if let Some((body_lbl, body_stmts)) = body_stuff {
            let body_bb = BasicBlock {
                body: body_stmts,
                terminator: End,
            };
            cfg_builder.add_block(body_lbl, body_bb);
        }

        let graph = cfg_builder.graph;

        graph.prune_empty_blocks()
    }

    /// Removes empty blocks whose terminator is just a `Jump` by merging them with the block they
    /// are jumping to.
    pub fn prune_empty_blocks(&self) -> Self {

        /// Given an empty `BasicBlock` that ends in a `Jump`, return the target label. In all other
        /// cases, return `None`.
        fn empty_bb(bb: &BasicBlock<Label>) -> Option<Label> {
            match bb.terminator {
                Jump(lbl) if bb.body.is_empty() => Some(lbl),
                _ => None,
            }
        }

        // Keys are labels corresponding to empty basic blocks with a jump terminator, values are
        // the labels they jump to (and can hopefully be replaced by).
        let mut proposed_rewrites: HashMap<Label, Label> = self.nodes
            .iter()
            .filter_map(|(lbl, bb)| empty_bb(bb).map(|tgt| (*lbl, tgt)))
            .collect();

        // Rewrites to actually apply. Keys are labels to basic blocks that were remapped into the
        // basic block corresponding to the value.
        let mut actual_rewrites: HashMap<Label, Label> = HashMap::new();

        while let Some((from, to)) = proposed_rewrites.iter().map(|(f,t)| (*f,*t)).next() {
            proposed_rewrites.remove(&from);

            // Try to apply more rewrites from `proposed_rewrites`
            let mut to_intermediate: Label = to;
            while let Some(to_new) = proposed_rewrites.remove(&to_intermediate) {
                to_intermediate = to_new;
            }

            // Check if there were already some rewrites applied
            let to_final = *actual_rewrites.get(&to_intermediate).unwrap_or(&to_intermediate);

            actual_rewrites.insert(from, to_final);
        }

        // Apply the remaps
        let entry = *actual_rewrites.get(&self.entry).unwrap_or(&self.entry);
        let nodes = self
            .nodes
            .iter()
            .filter_map(|(label, bb)| -> Option<(Label, BasicBlock<Label>)> {
                match actual_rewrites.get(label) {
                    // We keep only the basic blocks that weren't remapped to anything. Before
                    // returning them though, we have to remap any labels in their terminator.
                    None => {
                        let new_terminator = bb.terminator.map_labels(|label: &Label| -> Label {
                            match actual_rewrites.get(label) {
                                None => *label,
                                Some(new_label) => *new_label,
                            }
                        });
                        let new_bb = BasicBlock {
                            body: bb.body.clone(),
                            terminator: new_terminator,
                        };
                        Some((*label, new_bb))
                    }
                    Some(_) => None
                }
            })
            .collect();

        Cfg { entry, nodes }
    }

    pub fn into_stmts(self) -> Vec<Stmt> {
        let structures = self.reloop();
        Self::structured_cfg(&structures)
    }

    pub fn reloop(self) -> Vec<Structure> {

        let entries = vec![self.entry].into_iter().collect();
        let blocks = self.nodes
            .into_iter()
            .map(|(lbl, bb)| {
                let terminator = bb.terminator.map_labels(|l| StructureLabel::GoTo(*l));
                (lbl, BasicBlock { body: bb.body, terminator })
            })
            .collect();

        Self::relooper(entries, blocks)
    }

    fn relooper(entries: HashSet<Label>, mut blocks: HashMap<Label, BasicBlock<StructureLabel>>) -> Vec<Structure> {

        // Edges in the graph pointing out of the graph
        fn out_edges(blocks: &HashMap<Label, BasicBlock<StructureLabel>>) -> HashSet<Label> {
            blocks
                .iter()
                .flat_map(|(_, bb)| bb.successors())
                .filter(|lbl| !blocks.contains_key(lbl))
                .collect()
        }

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
                    result.extend(Self::relooper(new_entries, blocks));
                    result
                } else {
                    println!("Nope");
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
                    let then = Self::relooper(present, blocks);
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

                let children = Self::relooper(entries.clone(), body_blocks);

                let mut result = vec![Structure::Loop { entries, body: children }];
                result.extend(Self::relooper(follow_entries, follow_blocks));
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
                    .filter(|&(lbl, _)| handled_blocks.contains_key(&lbl))
                    .collect();

                let follow_entries: HashSet<Label> = &unhandled_entries | &out_edges(&handled_blocks);

                let mut all_handlers: HashMap<Label, Vec<Structure>> = handled_entries
                    .into_iter()
                    .map(|(lbl, blocks)| {
                        let entries: HashSet<Label> = vec![lbl].into_iter().collect();
                        (lbl, Self::relooper(entries, blocks))
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
                result.extend(Self::relooper(follow_entries, follow_blocks));
                result
            }
        }
    }

    pub fn structured_cfg(root: &Vec<Structure>) -> Vec<Stmt> {
        Self::structured_cfg_help(vec![], &HashSet::new(), root, &mut HashSet::new())
    }

    fn structured_cfg_help(
        exits: Vec<(Label, HashMap<Label, (HashSet<Label>, ExitStyle)>)>,
        next: &HashSet<Label>,
        root: &Vec<Structure>,
        used_loop_labels: &mut HashSet<Label>,
    ) -> Vec<Stmt> {

        let mut next: &HashSet<Label> = next;
        let mut rest: Vec<Stmt> = vec![];

        for structure in root.iter().rev() {
            let mut new_rest: Vec<Stmt> = vec![];

            match structure {
                &Structure::Simple { ref body, ref terminator, .. } => {
                    new_rest.extend(body.clone());

                    let insert_goto = |to: Label, target: &HashSet<Label>, stmts: Vec<Stmt>| -> Vec<Stmt> {
                        if target.len() == 1 {
                            stmts
                        } else {
                            let mut result = mk_goto(to);
                            result.extend(stmts);
                            result
                        }
                    };

                    let mut branch = |slbl: &StructureLabel| -> Vec<Stmt> {
                        match slbl {
                            &StructureLabel::Nested(ref nested) => Cfg::structured_cfg_help(exits.clone(), next, nested, used_loop_labels),
                            &StructureLabel::GoTo(to) | &StructureLabel::ExitTo(to) if next.contains(&to) => insert_goto(to, &next, vec![]),
                            &StructureLabel::ExitTo(to) => {

                                let mut immediate = true;
                                for &(label, ref local) in &exits {
                                    if let Some(&(ref follow, exit_style)) = local.get(&to) {
                                        return insert_goto(to, follow, mk_exit(immediate, exit_style, label))
                                    }
                                    immediate = false;
                                }

                                panic!("Not a valid exit - nothing to exit to")
                            }
                            _ => panic!("Not a valid exit"),
                        }
                    };

                    new_rest.extend(match terminator {
                        &End => vec![],
                        &Jump(ref to) => branch(to),
                        &Branch(ref c, ref t, ref f) => mk_if(c.clone(), branch(t), branch(f)),
                        &Switch { ref expr, ref cases, ref default } => unimplemented!(),
                    });
                }

                &Structure::Multiple { ref branches, ref then, .. } => {
                    let cases: Vec<(Label, Vec<Stmt>)> = branches
                        .iter()
                        .map(|(lbl, body)| (*lbl, Self::structured_cfg_help(exits.clone(), next, body, used_loop_labels)))
                        .collect();
                    let then: Vec<Stmt> = Self::structured_cfg_help(exits.clone(), next, then, used_loop_labels);

                    new_rest.extend(mk_match(cases, then));
                }

                &Structure::Loop { ref body, ref entries } => {
                    let label = entries.iter().next().expect("there were no labels");

                    let mut these_exits = HashMap::new();
                    these_exits.extend(entries
                        .iter()
                        .map(|e| (*e, (entries.clone(), ExitStyle::Continue)))
                    );
                    these_exits.extend(next
                        .iter()
                        .map(|e| (*e, (entries.clone(), ExitStyle::Break)))
                    );

                    let mut exits_new = vec![(*label, these_exits)];
                    exits_new.extend(exits.clone());

                    let body = Self::structured_cfg_help(exits_new, entries, body, used_loop_labels);
                    let loop_lbl = if used_loop_labels.contains(label) { Some(*label) } else { None };
                    new_rest.extend(mk_loop(loop_lbl, body));
                }
            }

            new_rest.extend(rest);

            rest = new_rest;
            next = structure.get_entries();
        }

        rest
    }

    fn has_multiple(root: &Vec<Structure>) -> bool {
        root.iter().any(|structure| {
            match structure {
                &Structure::Simple { ref terminator, .. } => terminator
                    .get_labels()
                    .into_iter()
                    .any(|structure_label|
                        match structure_label {
                            &StructureLabel::Nested(ref nested) => Self::has_multiple(nested),
                            _ => false,
                        }
                    ),
                &Structure::Multiple { .. } => return true,
                &Structure::Loop { ref body, .. } => Self::has_multiple(body),
            }
        })
    }
}

/// This stores all of the state required to construct a control-flow graph from C statements. Once
/// the graph is constructed, we only really care about the 'graph' field.
#[derive(Clone, Debug)]
struct CfgBuilder {

    /// Identifies the 'BasicBlock' to start with in 'graph'
    graph: Cfg,

    /// Source for generating fresh synthetic labels
    prev_label: u64,

    /// Stack of labels identifying what a 'break' should jump to. We push onto this stack when
    /// entering a construct that can break and pop when exiting that construct.
    break_labels: Vec<Label>,
    /// Like 'break_labels', but for 'continue'.
    continue_labels: Vec<Label>,
    /// Accumulates information for the 'case'/'default' encountered so far while translating the
    /// body of a 'switch'.
    switch_expr_cases: Vec<SwitchCases>,
}

/// This impl block deals with creating control flow graphs
impl CfgBuilder {
    /// Add a basic block to the control flow graph, specifying under which label to insert it.
    fn add_block(&mut self, lbl: Label, bb: BasicBlock<Label>) -> () {
        match self.graph.nodes.insert(lbl, bb) {
            None => { },
            Some(_) => panic!("Label {:?} cannot identify two basic blocks", lbl),
        }
    }

    /// Add a basic block to the control flow graph, creating and returning a fresh label under
    /// which to insert it.
    fn add_fresh_block(&mut self, bb: BasicBlock<Label>) -> Label {
        let lbl = self.fresh_label();
        self.add_block(lbl, bb);
        lbl
    }

    /// Generate a fresh (synthetic) label.
    fn fresh_label(&mut self) -> Label {
        self.prev_label += 1;
        Label::Synthetic(self.prev_label)
    }

    /// Create a new, empty `CfgBuilder`.
    fn new() -> CfgBuilder {
        let entry = Label::Synthetic(0);

        CfgBuilder {
            graph: Cfg {
                entry,
                nodes: HashMap::new(),
            },

            prev_label: 0,

            break_labels: vec![],
            continue_labels: vec![],
            switch_expr_cases: vec![],
        }
    }

    /// Translate and tack a C statement onto a continuation basic block, producing a new basic
    /// block. If necessary, intermediate basic blocks can be outputed to the control flow graph.
    ///
    /// This is the workhorse for generating control flow graphs. By passing in a continuation
    /// basic block, we can avoid making a lot of small blocks.
    fn convert_stmt_cfg(
        &mut self,
        translator: &Translation,
        stmt_id: CStmtId,
        continuation: BasicBlock<Label>
    ) -> BasicBlock<Label> {
        match translator.ast_context.index(stmt_id).kind {
            CStmtKind::Empty => continuation,

            CStmtKind::Decls(ref decls) => continuation.prepend_stmts(
                decls
                    .iter()
                    .flat_map(|decl| translator.convert_decl_stmt(*decl))
                    .collect()
            ),

            CStmtKind::Return(expr) => {

                let val = expr.map(|i| translator.convert_expr(ExprUse::RValue, i));

                let WithStmts { mut stmts, val } = with_stmts_opt(val);
                stmts.push(mk().expr_stmt(mk().return_expr(val)));

                BasicBlock {
                    body: stmts,
                    terminator: End,
                }
            }

            CStmtKind::If { scrutinee, true_variant, false_variant } => {

                let cont_label = self.add_fresh_block(continuation);

                let then_entry = {
                    let then_bb = self.convert_stmt_cfg(
                        translator,
                        true_variant,
                        BasicBlock::new_jump(cont_label),
                    );
                    self.add_fresh_block(then_bb)
                };

                let else_entry = match false_variant {
                    None => cont_label,
                    Some(false_var) => {
                        let else_bb = self.convert_stmt_cfg(
                            translator,
                            false_var,
                            BasicBlock::new_jump(cont_label),
                        );
                        self.add_fresh_block(else_bb)
                    }
                };

                let WithStmts { stmts, val: cond } = translator.convert_condition(true, scrutinee);
                BasicBlock {
                    body: stmts,
                    terminator: Branch(cond, then_entry, else_entry),
                }
            }

            CStmtKind::While { condition, body } => {

                let cond_entry = self.fresh_label();
                let body_entry = self.fresh_label();
                let cont_label = self.add_fresh_block(continuation);

                // Generate the 'cond' block
                let WithStmts { stmts, val: cond } = translator.convert_condition(true, condition);
                self.add_block(cond_entry, BasicBlock {
                    body: stmts,
                    terminator: Branch(cond, body_entry, cont_label),
                });

                // Generate the 'body' block
                self.break_labels.push(cont_label);
                self.continue_labels.push(cond_entry);

                let body_bb = self.convert_stmt_cfg(
                    translator,
                    body,
                    BasicBlock::new_jump(cond_entry),
                );
                self.add_block(body_entry, body_bb);

                self.break_labels.pop();
                self.continue_labels.pop();

                BasicBlock::new_jump(cond_entry)
            }


            CStmtKind::DoWhile { body, condition } => {

                let body_entry = self.fresh_label();
                let cond_entry = self.fresh_label();
                let cont_label = self.add_fresh_block(continuation);

                // Generate the 'body' block
                self.break_labels.push(cont_label);
                self.continue_labels.push(cond_entry);

                let body_bb = self.convert_stmt_cfg(
                    translator,
                    body,
                    BasicBlock::new_jump(cond_entry),
                );
                self.add_block(body_entry, body_bb);

                self.break_labels.pop();
                self.continue_labels.pop();

                // Generate the 'cond' block
                let WithStmts { stmts, val: cond } = translator.convert_condition(true, condition);
                self.add_block(cond_entry, BasicBlock {
                    body: stmts,
                    terminator: Branch(cond, body_entry, cont_label),
                });

                BasicBlock::new_jump(body_entry)
            }

            CStmtKind::ForLoop { init, condition, increment, body } => translator.with_scope(|| {

                let cond_entry = self.fresh_label();
                let body_entry = self.fresh_label();
                let cont_label = self.add_fresh_block(continuation);

                // Generate the 'cond' block
                match condition {
                    Some(cond) => {
                        let WithStmts { stmts, val } = translator.convert_condition(true, cond);
                        self.add_block(cond_entry, BasicBlock {
                            body: stmts,
                            terminator: Branch(val, body_entry, cont_label),
                        });
                    }
                    None => self.add_block(cond_entry, BasicBlock::new_jump(body_entry)),
                }

                // Generate the 'body'/'increment' block
                let increment_bb = BasicBlock {
                    terminator: Jump(cond_entry),
                    body: match increment {
                        None => vec![],
                        Some(inc) => translator.convert_expr(ExprUse::Unused, inc).stmts,
                    }
                };

                self.break_labels.push(cont_label);
                self.continue_labels.push(cond_entry);

                let body_bb = self.convert_stmt_cfg(translator, body, increment_bb);
                self.add_block(body_entry, body_bb);

                self.break_labels.pop();
                self.continue_labels.pop();

                // Return a next block containing the 'init'
                match init {
                    None => BasicBlock::new_jump(cond_entry),
                    Some(init) => self.convert_stmt_cfg(
                        translator,
                        init,
                        BasicBlock::new_jump(cond_entry),
                    ),
                }
            }),

            CStmtKind::Label(sub_stmt) => {
                let this_label = Label::FromC(stmt_id);
                let this_block = self.convert_stmt_cfg(translator, sub_stmt, continuation);

                self.add_block(this_label, this_block);

                BasicBlock::new_jump(this_label)
            }

            CStmtKind::Goto(label_id) => BasicBlock::new_jump(Label::FromC(label_id)),

            CStmtKind::Compound(ref stmts) => translator.with_scope(|| {
                stmts
                    .iter()
                    .rev()
                    .fold(continuation, |bb, &stmt| self.convert_stmt_cfg(translator, stmt, bb))
            }),

            CStmtKind::Expr(expr) => continuation.prepend_stmts(
                translator.convert_expr(ExprUse::Unused, expr).stmts
            ),

            CStmtKind::Break => BasicBlock::new_jump(
                *self.break_labels.last().expect("Nothing to 'break' to")
            ),

            CStmtKind::Continue => BasicBlock::new_jump(
                *self.continue_labels.last().expect("Nothing to 'continue' to")
            ),

            CStmtKind::Case(case_expr, sub_stmt) => {
                let this_label = Label::FromC(stmt_id);
                let this_block = self.convert_stmt_cfg(translator, sub_stmt, continuation);

                self.add_block(this_label, this_block);

                let branch = translator.convert_expr(ExprUse::RValue, case_expr).to_expr();
                self.switch_expr_cases
                    .last_mut()
                    .expect("'case' outside of 'switch'")
                    .cases
                    .push((branch, this_label));

                BasicBlock::new_jump(this_label)
            }

            CStmtKind::Default(sub_stmt) => {
                let this_label = Label::FromC(stmt_id);
                let this_block = self.convert_stmt_cfg(translator, sub_stmt, continuation);

                self.add_block(this_label, this_block);

                self.switch_expr_cases
                    .last_mut()
                    .expect("'default' outside of 'switch'")
                    .default
                    .get_or_insert(this_label);

                BasicBlock::new_jump(this_label)
            }

            CStmtKind::Switch { scrutinee, body } => {

                let cont_label = self.add_fresh_block(continuation);

                self.break_labels.push(cont_label);
                self.switch_expr_cases.push(SwitchCases::default());

                // We don't care about the BasicBlock this returns because it is impossible to just
                // enter the switch body statement - you have to jump into it via a 'case' or a
                // 'default'.
                self.convert_stmt_cfg(translator, body, BasicBlock::new_jump(cont_label));

                self.break_labels.pop();
                let switch_case = self.switch_expr_cases.pop().expect("No 'SwitchCases' to pop");

                let WithStmts { stmts, val } = translator.convert_expr(ExprUse::RValue, scrutinee);
                BasicBlock {
                    body: stmts,
                    terminator: Switch {
                        expr: val,
                        cases: switch_case.cases,
                        default: switch_case.default.unwrap_or(cont_label),
                    }
                }
            }
        }
    }

    /// TODO doc
    fn convert_stmt_cfg_new(
        &mut self,
        translator: &Translation,
        stmt_id: CStmtId,
        (lbl, mut stmts): (Label, Vec<Stmt>),
    ) -> Option<(Label, Vec<Stmt>)> {

        match translator.ast_context.index(stmt_id).kind {
            CStmtKind::Empty => Some((lbl, stmts)),

            CStmtKind::Decls(ref decls) => {
                stmts.extend(decls
                    .iter()
                    .flat_map(|decl| translator.convert_decl_stmt(*decl)));

                Some((lbl, stmts))
            }

            CStmtKind::Return(expr) => {

                let val = expr.map(|i| translator.convert_expr(ExprUse::RValue, i));

                let WithStmts { stmts: ret_stmts, val: ret_val } = with_stmts_opt(val);
                stmts.extend(ret_stmts);
                stmts.push(mk().expr_stmt(mk().return_expr(ret_val)));

                let bb = BasicBlock {
                    body: stmts,
                    terminator: End,
                };

                self.add_block(lbl, bb);

                None
            }

            CStmtKind::If { scrutinee, true_variant, false_variant } => {

                let next_entry = self.fresh_label();
                let then_entry = self.fresh_label();
                let else_entry = if false_variant.is_none() { next_entry } else { self.fresh_label() };

                // Condition
                let WithStmts { stmts: cond_stmts, val: cond_val } = translator.convert_condition(true, scrutinee);
                stmts.extend(cond_stmts);

                let cond_bb = BasicBlock {
                    body: stmts,
                    terminator: Branch(cond_val, then_entry, else_entry),
                };
                self.add_block(lbl, cond_bb);

                // Then case
                let then_stuff = self.convert_stmt_cfg_new(
                    translator,
                    true_variant,
                    (then_entry, vec![]),
                );
                if let Some((new_then_entry, then_stmts)) = then_stuff {
                    let then_bb = BasicBlock {
                        body: then_stmts,
                        terminator: Jump(next_entry),
                    };
                    self.add_block(new_then_entry, then_bb);
                }

                // Else case
                if let Some(false_var) = false_variant {
                    let else_stuff = self.convert_stmt_cfg_new(
                        translator,
                        false_var,
                        (else_entry, vec![]),
                    );
                    if let Some((new_else_entry, else_stmts)) = else_stuff {
                        let else_bb = BasicBlock {
                            body: else_stmts,
                            terminator: Jump(next_entry),
                        };
                        self.add_block(new_else_entry, else_bb);
                    }
                };

                // Return
                Some((next_entry, vec![]))
            }

            CStmtKind::While { condition, body } => {

                let cond_entry = self.fresh_label();
                let body_entry = self.fresh_label();
                let next_entry = self.fresh_label();

                let prev_bb = BasicBlock {
                    body: stmts,
                    terminator: Jump(cond_entry),
                };
                self.add_block(lbl, prev_bb);

                // Condition
                let WithStmts { stmts: cond_stmts, val: cond_val } = translator.convert_condition(true, condition);
                let cond_bb = BasicBlock {
                    body: cond_stmts,
                    terminator: Branch(cond_val, body_entry, next_entry),
                };
                self.add_block(cond_entry, cond_bb);

                // Body
                self.break_labels.push(next_entry);
                self.continue_labels.push(cond_entry);

                let body_stuff = self.convert_stmt_cfg_new(
                    translator,
                    body,
                    (body_entry, vec![]),
                );
                if let Some((body_new_entry, body_stmts)) = body_stuff {
                    let body_bb = BasicBlock {
                        body: body_stmts,
                        terminator: Jump(cond_entry),
                    };
                    self.add_block(body_new_entry, body_bb);
                }

                self.break_labels.pop();
                self.continue_labels.pop();

                //Return
                Some((next_entry, vec![]))
            }

            CStmtKind::DoWhile { body, condition } => {

                let body_entry = self.fresh_label();
                let cond_entry = self.fresh_label();
                let next_entry = self.fresh_label();

                let prev_bb = BasicBlock {
                    body: stmts,
                    terminator: Jump(body_entry),
                };
                self.add_block(lbl, prev_bb);

                // Body
                self.break_labels.push(next_entry);
                self.continue_labels.push(cond_entry);

                let body_stuff = self.convert_stmt_cfg_new(
                    translator,
                    body,
                    (body_entry, vec![]),
                );
                if let Some((body_new_entry, body_stmts)) = body_stuff {
                    let body_bb = BasicBlock {
                        body: body_stmts,
                        terminator: Jump(cond_entry),
                    };
                    self.add_block(body_new_entry, body_bb);
                }

                self.break_labels.pop();
                self.continue_labels.pop();

                // Condition
                let WithStmts { stmts: cond_stmts, val: cond_val } = translator.convert_condition(true, condition);
                let cond_bb = BasicBlock {
                    body: cond_stmts,
                    terminator: Branch(cond_val, body_entry, next_entry),
                };
                self.add_block(cond_entry, cond_bb);

                //Return
                Some((next_entry, vec![]))
            }

            CStmtKind::ForLoop { init, condition, increment, body } => translator.with_scope(|| {

                let cond_entry = self.fresh_label();
                let body_entry = self.fresh_label();
                let next_label = self.fresh_label();

                // Init
                let init_stuff = match init {
                    None => Some((lbl, stmts)),
                    Some(init) => self.convert_stmt_cfg_new(
                        translator,
                        init,
                        (lbl, stmts),
                    ),
                };
                if let Some((init_lbl, init_stmts)) = init_stuff {
                    let init_bb = BasicBlock {
                        body: init_stmts,
                        terminator: Jump(cond_entry),
                    };
                    self.add_block(init_lbl, init_bb);
                }

                // Condition
                match condition {
                    Some(cond) => {
                        let WithStmts { stmts, val } = translator.convert_condition(true, cond);
                        self.add_block(cond_entry, BasicBlock {
                            body: stmts,
                            terminator: Branch(val, body_entry, next_label),
                        });
                    }
                    None => self.add_block(cond_entry, BasicBlock::new_jump(body_entry)),
                }

                // Body and increment
                self.break_labels.push(next_label);
                self.continue_labels.push(cond_entry);

                let body_stuff = self.convert_stmt_cfg_new(translator, body, (body_entry, vec![]));

                self.break_labels.pop();
                self.continue_labels.pop();

                if let Some((body_new_lbl, mut body_stmts)) = body_stuff {
                    let inc_stmts = match increment {
                        None => vec![],
                        Some(inc) => translator.convert_expr(ExprUse::Unused, inc).stmts,
                    };

                    body_stmts.extend(inc_stmts);

                    let body_inc_bb = BasicBlock {
                        terminator: Jump(cond_entry),
                        body: body_stmts
                    };
                    self.add_block(body_new_lbl, body_inc_bb);
                }

                // Return
                Some((next_label, vec![]))
            }),

            CStmtKind::Label(sub_stmt) => {

                let this_label = Label::FromC(stmt_id);

                let prev_bb = BasicBlock {
                    body: stmts,
                    terminator: Jump(this_label),
                };
                self.add_block(lbl, prev_bb);

                // Sub stmt
                self.convert_stmt_cfg_new(translator, sub_stmt, (this_label, vec![]))
            }

            CStmtKind::Goto(label_id) => {

                let tgt_label = Label::FromC(label_id);
                let prev_bb = BasicBlock {
                    body: stmts,
                    terminator: Jump(tgt_label),
                };
                self.add_block(lbl, prev_bb);

                None
            }

            CStmtKind::Compound(ref comp_stmts) => translator.with_scope(|| {

                let mut lbl_stmts = Some((lbl, stmts));

                for stmt in comp_stmts {
                    let (lbl,stmts) = lbl_stmts.unwrap_or((self.fresh_label(), vec![]));
                    lbl_stmts = self.convert_stmt_cfg_new(translator, *stmt, (lbl, stmts));
                }

                lbl_stmts
            }),

            CStmtKind::Expr(expr) => {
                stmts.extend(translator.convert_expr(ExprUse::Unused, expr).stmts);

                Some((lbl, stmts))
            }

            CStmtKind::Break => {
                let tgt_label = *self.break_labels.last().expect("Nothing to 'break' to");
                let prev_bb = BasicBlock {
                    body: stmts,
                    terminator: Jump(tgt_label),
                };
                self.add_block(lbl, prev_bb);

                None
            }

            CStmtKind::Continue => {
                let tgt_label = *self.continue_labels.last().expect("Nothing to 'continue' to");
                let prev_bb = BasicBlock {
                    body: stmts,
                    terminator: Jump(tgt_label),
                };
                self.add_block(lbl, prev_bb);

                None
            }

            CStmtKind::Case(case_expr, sub_stmt) => {
                let this_label = Label::FromC(stmt_id);

                let prev_bb = BasicBlock {
                    body: stmts,
                    terminator: Jump(this_label),
                };
                self.add_block(lbl, prev_bb);

                // Case
                let branch = translator.convert_expr(ExprUse::RValue, case_expr).to_expr();
                self.switch_expr_cases
                    .last_mut()
                    .expect("'case' outside of 'switch'")
                    .cases
                    .push((branch, this_label));

                // Sub stmt
                self.convert_stmt_cfg_new(translator, sub_stmt, (this_label, vec![]))
            }

            CStmtKind::Default(sub_stmt) => {
                let this_label = Label::FromC(stmt_id);

                let prev_bb = BasicBlock {
                    body: stmts,
                    terminator: Jump(this_label),
                };
                self.add_block(lbl, prev_bb);

                // Default case
                self.switch_expr_cases
                    .last_mut()
                    .expect("'default' outside of 'switch'")
                    .default
                    .get_or_insert(this_label);

                // Sub stmt
                self.convert_stmt_cfg_new(translator, sub_stmt, (this_label, vec![]))
            }

            CStmtKind::Switch { scrutinee, body } => {

                let next_label = self.fresh_label();
                let body_label = self.fresh_label();

                // Convert the condition
                let WithStmts { stmts: cond_stmts, val: cond_val } = translator.convert_expr(ExprUse::RValue, scrutinee);
                stmts.extend(cond_stmts);

                // Body
                self.break_labels.push(next_label);
                self.switch_expr_cases.push(SwitchCases::default());

                // We don't care about the label or statements this returns because it is impossible
                // to just enter the switch body statement - you have to jump into it via a 'case'
                // or a 'default'.
                self.convert_stmt_cfg_new(translator, body, (body_label, vec![]));

                self.break_labels.pop();
                let switch_case = self.switch_expr_cases.pop().expect("No 'SwitchCases' to pop");

                // Add the condition basic block (we need the information built up during the
                // conversion of the body to make the right terminator)
                let cond_bb = BasicBlock {
                    body: stmts,
                    terminator: Switch {
                        expr: cond_val,
                        cases: switch_case.cases,
                        default: switch_case.default.unwrap_or(next_label),
                    }
                };
                self.add_block(lbl, cond_bb);

                // Return
                Some((next_label, vec![]))
            }
        }
    }
}


/// This impl block deals with pretty-printing control flow graphs into a format that `dot` can
/// consume. Compiling these files into images means running something like:
///
/// ```
/// dot -Tpng cfg_func.dot > cfg_func.png
/// ```
impl Cfg {

    pub fn dump_dot_graph(&self, file_path: String) -> io::Result<()> {

        // Utility function for sanitizing strings
        fn sanitize_label(lbl: String) -> String {
            format!("{}\\l", lbl.replace("\n", "\\l").replace("\t", "  "))
        }

        let mut file = File::create(file_path)?;
        file.write_all(b"digraph cfg {\n")?;
        file.write_all(b"  node [shape=box,fontname=Courier];\n")?;
        file.write_all(b"  edge [fontname=Courier,fontsize=10.0];\n")?;

        // Entry
        file.write_all(b"  entry [shape=plaintext];\n")?;
        file.write_fmt(format_args!("  entry -> {};\n", self.entry.pretty_print()))?;

        // Rest of graph
        for (lbl, &BasicBlock { ref body, ref terminator }) in self.nodes.iter() {

            let pretty_terminator = match terminator {
                &End | &Jump(_) => String::from(""),
                &Branch(ref cond, _, _) => format!("\n{}",pprust::expr_to_string(cond.deref())),
                &Switch { ref expr, .. } => format!("\n{}",pprust::expr_to_string(expr.deref())),
            };

            // A node
            file.write_fmt(format_args!(
                "  {} [label=\"{}:\\l{}-----{}\"];\n",
                lbl.pretty_print(),
                lbl.pretty_print(),
                format!("-----\\l{}", if body.is_empty() {
                    String::from("")
                } else {
                    sanitize_label(body
                        .iter()
                        .map(|stmt| pprust::stmt_to_string(stmt))
                        .collect::<Vec<String>>()
                        .join("\n")
                    )
                }),
                sanitize_label(pretty_terminator),
            ))?;

            // All the edges starting from this node
            let edges: Vec<(String, Label)> = match terminator {
                &End => vec![],
                &Jump(tgt) => vec![(String::from(""),tgt)],
                &Branch(_, tru, fal) => vec![
                    (String::from("true"),tru),
                    (String::from("false"),fal)
                ],
                &Switch { ref cases, default, .. } => {
                    let mut cases: Vec<(String, Label)> = cases
                        .iter()
                        .map(|&(ref expr, tgt)| (pprust::expr_to_string(expr.deref()), tgt))
                        .collect();
                    cases.push((String::from("_"), default));
                    cases
                },
            };

            for (desc,tgt) in edges {
                file.write_fmt(format_args!(
                    "  {} -> {} [label=\"{}\"];\n",
                    lbl.pretty_print(),
                    tgt.pretty_print(),
                    sanitize_label(desc),
                ))?;
            }
        }

        file.write_all(b"}\n")?;

        Ok(())
    }
}
