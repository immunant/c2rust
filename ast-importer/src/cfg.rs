
use syntax::ast::*;
use syntax::ptr::P;
use idiomize::ast_manip::make_ast::*;
use std::collections::HashMap;
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
enum Label {
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

#[derive(Clone, Debug)]
struct BasicBlock {
    /// Jump-free code
    body: Vec<Stmt>,

    /// How to find the next (if any) basic block to go to
    terminator: Terminator,
}

impl BasicBlock {
    fn new(terminator: Terminator) -> Self {
        BasicBlock {
            body: vec![],
            terminator,
        }
    }

    fn new_jump(target: Label) -> Self {
        BasicBlock::new(Terminator::Jump(target))
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

#[derive(Clone, Debug)]
enum Terminator {
    /// End of control-flow. For example: the last statement in a function, or a return
    End,

    /// Unconditional branch to another block
    Jump(Label),

    /// Conditional branch to another block. The expression is expected to be a boolean Rust
    /// expression
    Branch(P<Expr>, Label, Label),

    /// Multi-way branch.
    ///
    /// FIXME: specify more invariants on `expr`/`cases`
    Switch {
        expr: P<Expr>,
        cases: Vec<(P<Expr>, Label)>,
        default: Label
    }
}

impl Terminator {
    fn map_labels<F: Fn(Label) -> Label>(&self, func: F) -> Self {
        match self {
            &Terminator::End => Terminator::End,
            &Terminator::Jump(l) => Terminator::Jump(func(l)),
            &Terminator::Branch(ref e, ref l1, ref l2) => Terminator::Branch(e.clone(), func(*l1), func(*l2)),
            &Terminator::Switch { ref expr, ref cases, ref default } => Terminator::Switch {
                expr: expr.clone(),
                cases: cases.iter().map(|&(ref e, ref l)| (e.clone(), func(*l))).collect(),
                default: func(*default),
            }
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
    nodes: HashMap<Label, BasicBlock>,
}

/// A complete control-flow graph
impl Cfg {

    /// Completely process a statement into a control flow graph.
    pub fn from_stmt(translator: &Translation, stmt_id: CStmtId) -> Cfg {
        let mut cfg_builder = CfgBuilder::new(translator);
        let entry = cfg_builder.graph.entry;

        let end = BasicBlock {
            body: vec![],
            terminator: Terminator::End,
        };

        let bb = translator.with_scope(|| cfg_builder.convert_stmt_cfg(translator, stmt_id, end));
        cfg_builder.add_block(entry, bb);

        let graph = cfg_builder.graph;

        graph.prune_empty_blocks()
    }

    /// Removes empty blocks whose terminator is just a `Jump` by merging them with the block they
    /// are jumping to.
    pub fn prune_empty_blocks(&self) -> Self {

        /// Given an empty `BasicBlock` that ends in a `Jump`, return the target label. In all other
        /// cases, return `None`.
        fn empty_bb(bb: &BasicBlock) -> Option<Label> {
            match bb.terminator {
                Terminator::Jump(lbl) if bb.body.is_empty() => Some(lbl),
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
            .filter_map(|(label, bb)| -> Option<(Label, BasicBlock)> {
                match actual_rewrites.get(label) {
                    // We keep only the basic blocks that weren't remapped to anything. Before
                    // returning them though, we have to remap any labels in their terminator.
                    None => {
                        let new_terminator = bb.terminator.map_labels(|label: Label| -> Label {
                            match actual_rewrites.get(&label) {
                                None => label,
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
    fn add_block(&mut self, lbl: Label, bb: BasicBlock) -> () {
        match self.graph.nodes.insert(lbl, bb) {
            None => { },
            Some(_) => panic!("Label {:?} cannot identify two basic blocks", lbl),
        }
    }

    /// Add a basic block to the control flow graph, creating and returning a fresh label under
    /// which to insert it.
    fn add_fresh_block(&mut self, bb: BasicBlock) -> Label {
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
    fn new(translator: &Translation) -> CfgBuilder {
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
        continuation: BasicBlock
    ) -> BasicBlock {
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
                    terminator: Terminator::End,
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
                    terminator: Terminator::Branch(cond, then_entry, else_entry),
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
                    terminator: Terminator::Branch(cond, body_entry, cont_label),
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
                    terminator: Terminator::Branch(cond, body_entry, cont_label),
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
                            terminator: Terminator::Branch(val, body_entry, cont_label),
                        });
                    }
                    None => self.add_block(cond_entry, BasicBlock::new_jump(body_entry)),
                }

                // Generate the 'body'/'increment' block
                let increment_bb = BasicBlock {
                    terminator: Terminator::Jump(cond_entry),
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
                    terminator: Terminator::Switch {
                        expr: val,
                        cases: switch_case.cases,
                        default: switch_case.default.unwrap_or(cont_label),
                    }
                }
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
                &Terminator::End | &Terminator::Jump(_) => String::from(""),
                &Terminator::Branch(ref cond, _, _) => format!("\n{}",pprust::expr_to_string(cond.deref())),
                &Terminator::Switch { ref expr, .. } => format!("\n{}",pprust::expr_to_string(expr.deref())),
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
                &Terminator::End => vec![],
                &Terminator::Jump(tgt) => vec![(String::from(""),tgt)],
                &Terminator::Branch(_, tru, fal) => vec![
                    (String::from("true"),tru),
                    (String::from("false"),fal)
                ],
                &Terminator::Switch { ref cases, default, .. } => {
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
