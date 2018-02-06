//! # Control Flow Graph analysis
//!
//! Through `switch`/`case`/`default` and labels/`goto`, the C language supports jumping directly
//! from one position in the code to another. Rust supports on structured control flow constructs.
//! This means that during translation, we need to somehow eliminate the unstructured control-flow
//! constructs C has. This module is where that happens.
//!
//! In a nutshell, here are the steps:
//!
//!   - given an entry point C statement, translate it into a CFG consisting of `BasicBlock<Label>`
//!   - simplify this CFG (by eliminating empty blocks that jump unconditionally to the next block)
//!   - use the _Relooper algorithm_ to convert this CFG into a sequence of `Structure`s
//!   - simplify that sequence of `Structure`s into another such sequence
//!   - convert the `Vec<Structure>` back into a `Vec<Stmt>`
//!

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
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::hash::Hash;

use translator::*;
use c_ast::*;

pub mod relooper;
pub mod structures;

/// These labels identify basic blocks in a regular CFG.
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
            &Label::FromC(CStmtId(label_id)) => format!("'c_{}", label_id),
            &Label::Synthetic(syn_id) => format!("'s_{}", syn_id),
        }
    }

    fn debug_print(&self) -> String {
        String::from(self.pretty_print().trim_left_matches('\''))
    }

    fn to_num_expr(&self) -> P<Expr> {
        let mut s = DefaultHasher::new();
        self.hash(&mut s);
        let as_num = s.finish();

        mk().lit_expr(mk().int_lit(as_num as u128, ""))
    }
}

/// These labels identify _structure_ basic blocks in a structure CFG.
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

/// Generalized basic block.
#[derive(Clone, Debug)]
struct BasicBlock<L> {
    /// Jump-free code
    body: Vec<Stmt>,

    /// How to find the next (if any) basic block to go to
    terminator: GenTerminator<L>,
}

impl<L> BasicBlock<L> {
    fn new(terminator: GenTerminator<L>) -> Self {
        BasicBlock { body: vec![], terminator }
    }

    fn new_jump(target: L) -> Self {
        BasicBlock::new(Jump(target))
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

/// Represents the control flow choices one can make when at the end of a `BasicBlock`.
#[derive(Clone, Debug)]
pub enum GenTerminator<Lbl> {
    /// End of control-flow. For example: the last statement in a function, or a return
    End,

    /// Unconditional branch to another block
    Jump(Lbl),

    /// Conditional branch to another block. The expression is expected to be a boolean Rust
    /// expression
    Branch(P<Expr>, Lbl, Lbl),

    /// Multi-way branch.
    ///
    /// FIXME: specify more invariants on `expr`/`cases`
    Switch {
        expr: P<Expr>,
        cases: Vec<(Vec<P<Pat>>, Lbl)>, // TODO: support ranges of expressions
    }
}

// We use this a lot, so import its constructors
use self::GenTerminator::*;

impl<L> GenTerminator<L> {

    /// Produce a new terminator by transforming all of the labels in that terminator.
    fn map_labels<F: Fn(&L) -> N, N>(&self, func: F) -> GenTerminator<N> {
        match self {
            &End => End,
            &Jump(ref l) => Jump(func(l)),
            &Branch(ref e, ref l1, ref l2) => Branch(e.clone(), func(l1), func(l2)),
            &Switch { ref expr, ref cases } => Switch {
                expr: expr.clone(),
                cases: cases.iter().map(|&(ref e, ref l)| (e.clone(), func(l))).collect(),
            }
        }
    }

    /// Extract references to all of the labels in the terminator
    fn get_labels(&self) -> Vec<&L> {
        match self {
            &End => vec![],
            &Jump(ref l) => vec![l],
            &Branch(_, ref l1, ref l2) => vec![l1,l2],
            &Switch { ref cases, .. } =>
                cases.iter().map(|&(_, ref l)| l).collect(),
        }
    }

    /// Extract mutable references to all of the labels in the terminator
    fn get_labels_mut(&mut self) -> Vec<&mut L> {
        match self {
            &mut End => vec![],
            &mut Jump(ref mut l) => vec![l],
            &mut Branch(_, ref mut l1, ref mut l2) => vec![l1,l2],
            &mut Switch { ref mut cases, .. } =>
                cases.iter_mut().map(|&mut (_, ref mut l)| l).collect(),
        }
    }
}

/// The sole purpose of this structure is to accumulate information about what cases/default have
/// been seen which translating the body of the switch.
#[derive(Clone, Debug, Default)]
pub struct SwitchCases {
    cases: Vec<(P<Pat>,Label)>,
    default: Option<Label>,
}

/// A CFG graph of regular basic blocks.
///
/// TODO: consider parametrizing this:
/// `struct Cfg<Lbl> { entry: HashSet<Label>, nodes: HashMap<Label, BasicBlock<Lbl>> }`
#[derive(Clone, Debug)]
pub struct Cfg<Lbl> {
    /// Entry point in the graph
    entries: HashSet<Label>,

    /// Nodes in the graph
    nodes: HashMap<Label, BasicBlock<Lbl>>,
}

#[derive(Copy, Clone, Debug)]
enum ExitStyle {
    Continue,
    Break,
}

/// A complete control-flow graph
impl Cfg<Label> {

    /// Completely process a statement into a control flow graph.
    pub fn from_stmt(translator: &Translation, stmt_id: CStmtId) -> Self {
        let mut cfg_builder = CfgBuilder::new();
        let entry = *cfg_builder.graph.entries.iter().next().expect("from_stmt: expected an entry");

        let body_stuff = translator.with_scope(|| cfg_builder.convert_stmt_help(translator, stmt_id, (entry, vec![])));
        if let Some((body_lbl, body_stmts)) = body_stuff {
            let body_bb = BasicBlock {
                body: body_stmts,
                terminator: End,
            };
            cfg_builder.add_block(body_lbl, body_bb);
        }

        let graph = cfg_builder.graph;

        graph.prune_empty_blocks().prune_unreachable_blocks()
       // graph
    }

    /// Removes blocks that cannot be reached
    pub fn prune_unreachable_blocks(&self) -> Self {
        let mut new_nodes: HashMap<Label, BasicBlock<Label>> = HashMap::new();
        let mut to_visit: Vec<&Label> = self.entries.iter().collect();

        while let Some(lbl) = to_visit.pop() {
            let blk = self.nodes.get(lbl).expect("prune_unreachable_blocks: could not find block");
            new_nodes.insert(*lbl,blk.clone());

            for lbl in &blk.terminator.get_labels() {
                if !new_nodes.contains_key(lbl) {
                    to_visit.push(lbl);
                }
            }
        }

        Cfg {
           entries: self.entries.clone(),
           nodes: new_nodes,
        }
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
            let mut from_any: HashSet<Label> = vec![from].into_iter().collect();

            // Try to apply more rewrites from `proposed_rewrites`
            let mut to_intermediate: Label = to;
            while let Some(to_new) = proposed_rewrites.remove(&to_intermediate) {
                from_any.insert(to_intermediate);
                to_intermediate = to_new;
            }

            // Check if there were already some rewrites applied
            let to_final = match actual_rewrites.get(&to_intermediate) {
                None => to_intermediate,
                Some(&to_final) => {
                    from_any.insert(to_intermediate);
                    to_final
                }
            };

            // It makes no sense to remap something to itself
            for from in from_any {
                if from != to_final {
                    actual_rewrites.insert(from, to_final);
                }
            }
        }

        // Apply the remaps
        let entries = self.entries
            .iter()
            .map(|entry| *actual_rewrites.get(entry).unwrap_or(entry))
            .collect();
        let nodes = self.nodes
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

        Cfg { entries, nodes }
    }
}

/// This stores all of the state required to construct a control-flow graph from C statements. Once
/// the graph is constructed, we only really care about the 'graph' field.
#[derive(Clone, Debug)]
struct CfgBuilder {

    /// Identifies the 'BasicBlock' to start with in 'graph'
    graph: Cfg<Label>,

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

    /// Generate a fresh (synthetic) label.
    fn fresh_label(&mut self) -> Label {
        self.prev_label += 1;
        Label::Synthetic(self.prev_label)
    }

    /// Create a new `CfgBuilder` with a single entry label.
    fn new() -> CfgBuilder {
        let entries = vec![Label::Synthetic(0)].into_iter().collect();

        CfgBuilder {
            graph: Cfg {
                entries,
                nodes: HashMap::new(),
            },

            prev_label: 0,

            break_labels: vec![],
            continue_labels: vec![],
            switch_expr_cases: vec![],
        }
    }


    /// Translate a C statement and tack it onto the end of the WIP `Vec<Stmt>` passed in. If
    /// necessary, intermediate basic blocks can be outputted to the control flow graph.
    ///
    /// If the input C statement naturally passes control to the statement that follows it, the
    /// return should be the new WIP `Vec<Stmt>` (and the label by which this WIP block is referred
    /// to).
    ///
    /// This is the workhorse for generating control flow graphs. By passing threading through a
    /// vector of statements, we can avoid making a lot of small blocks.
    fn convert_stmt_help(
        &mut self,
        translator: &Translation,
        stmt_id: CStmtId,                     // C statement to translate
        (lbl, mut stmts): (Label, Vec<Stmt>), // WIP statements and the label referring to them
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

                let val =
                    match expr.map(|i| translator.convert_expr(ExprUse::RValue, i)) {
                        Some(r) => Some(r.unwrap()),
                        None => None,
                    };

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
                let WithStmts { stmts: cond_stmts, val: cond_val } =
                    translator.convert_condition(true, scrutinee).unwrap();
                stmts.extend(cond_stmts);

                let cond_bb = BasicBlock {
                    body: stmts,
                    terminator: Branch(cond_val, then_entry, else_entry),
                };
                self.add_block(lbl, cond_bb);

                // Then case
                let then_stuff = self.convert_stmt_help(
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
                    let else_stuff = self.convert_stmt_help(
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
                let WithStmts { stmts: cond_stmts, val: cond_val } =
                    translator.convert_condition(true, condition).unwrap();
                let cond_bb = BasicBlock {
                    body: cond_stmts,
                    terminator: Branch(cond_val, body_entry, next_entry),
                };
                self.add_block(cond_entry, cond_bb);

                // Body
                self.break_labels.push(next_entry);
                self.continue_labels.push(cond_entry);

                let body_stuff = self.convert_stmt_help(
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

                let body_stuff = self.convert_stmt_help(
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
                let WithStmts { stmts: cond_stmts, val: cond_val } =
                    translator.convert_condition(true, condition).unwrap();
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
                    Some(init) => self.convert_stmt_help(
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
                        let WithStmts { stmts, val } =
                            translator.convert_condition(true, cond).unwrap();
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

                let body_stuff = self.convert_stmt_help(translator, body, (body_entry, vec![]));

                self.break_labels.pop();
                self.continue_labels.pop();

                if let Some((body_new_lbl, mut body_stmts)) = body_stuff {
                    let inc_stmts = match increment {
                        None => vec![],
                        Some(inc) =>
                            translator
                                .convert_expr(ExprUse::Unused, inc)
                                .unwrap().stmts,
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
                self.convert_stmt_help(translator, sub_stmt, (this_label, vec![]))
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
                    lbl_stmts = self.convert_stmt_help(translator, *stmt, (lbl, stmts));
                }

                lbl_stmts
            }),

            CStmtKind::Expr(expr) => {
                stmts.extend(translator.convert_expr(ExprUse::Unused, expr).unwrap().stmts);

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

            CStmtKind::Case(_case_expr, sub_stmt, cie) => {
                let this_label = Label::FromC(stmt_id);

                let prev_bb = BasicBlock {
                    body: stmts,
                    terminator: Jump(this_label),
                };
                self.add_block(lbl, prev_bb);

                // Case
                let branch = match cie {
                    ConstIntExpr::U(n) => mk().lit_expr(mk().int_lit(n as u128, LitIntType::Unsuffixed)),
                    ConstIntExpr::I(n) if n < 0 => mk().unary_expr(syntax::ast::UnOp::Neg, mk().lit_expr(mk().int_lit((-n) as u128, LitIntType::Unsuffixed))),
                    ConstIntExpr::I(n) => mk().lit_expr(mk().int_lit(n as u128, LitIntType::Unsuffixed)),
                };
                self.switch_expr_cases
                    .last_mut()
                    .expect("'case' outside of 'switch'")
                    .cases
                    .push((mk().lit_pat(branch), this_label));

                // Sub stmt
                self.convert_stmt_help(translator, sub_stmt, (this_label, vec![]))
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
                self.convert_stmt_help(translator, sub_stmt, (this_label, vec![]))
            }

            CStmtKind::Switch { scrutinee, body } => {

                let next_label = self.fresh_label();
                let body_label = self.fresh_label();

                // Convert the condition
                let WithStmts { stmts: cond_stmts, val: cond_val } =
                    translator.convert_expr(ExprUse::RValue, scrutinee).unwrap();
                stmts.extend(cond_stmts);

                // Body
                self.break_labels.push(next_label);
                self.switch_expr_cases.push(SwitchCases::default());

                let body_stuff = self.convert_stmt_help(translator, body, (body_label, vec![]));
                if let Some((body_new_lbl, body_stmts)) = body_stuff {
                    self.add_block(body_new_lbl, BasicBlock {
                        body: body_stmts,
                        terminator: Jump(next_label),
                    });
                }

                self.break_labels.pop();
                let switch_case = self.switch_expr_cases.pop().expect("No 'SwitchCases' to pop");

                let mut cases: Vec<_> = switch_case.cases
                    .into_iter()
                    .map(|(p,lbl)| (vec![p],lbl))
                    .collect();
                cases.push((vec![mk().wild_pat()], switch_case.default.unwrap_or(next_label)));


                // Add the condition basic block (we need the information built up during the
                // conversion of the body to make the right terminator)
                let cond_bb = BasicBlock {
                    body: stmts,
                    terminator: Switch { expr: cond_val, cases },
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
impl Cfg<Label> {

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
        for (i, entry) in self.entries.iter().enumerate() {
            file.write_fmt(format_args!("  entry{} [shape=plaintext];\n", i))?;
            file.write_fmt(format_args!("  entry{} -> {};\n", i, entry.debug_print()))?;
        }

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
                lbl.debug_print(),
                lbl.debug_print(),
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
                &Switch { ref cases, .. } => {
                    let mut cases: Vec<(String, Label)> = cases
                        .iter()
                        .map(|&(ref pats, tgt)| -> (String, Label) {
                            let pats: Vec<String> = pats
                                .iter()
                                .map(|p| pprust::pat_to_string(p.deref()))
                                .collect();

                            (pats.join(" | "), tgt)
                        })
                        .collect();
                    cases
                },
            };

            for (desc,tgt) in edges {
                file.write_fmt(format_args!(
                    "  {} -> {} [label=\"{}\"];\n",
                    lbl.debug_print(),
                    tgt.debug_print(),
                    sanitize_label(desc),
                ))?;
            }
        }

        file.write_all(b"}\n")?;

        Ok(())
    }
}
