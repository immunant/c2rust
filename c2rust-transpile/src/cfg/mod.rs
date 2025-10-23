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
//!   - use the _Relooper algorithm_ to convert this CFG into a sequence of `Structure<StmtOrDecl>`s
//!   - place the declarations in the right place and produce a sequence of `Structure<Stmt>`s
//!   - simplify that sequence of `Structure<Stmt>`s into another such sequence
//!   - convert the `Vec<Structure<Stmt>>` back into a `Vec<Stmt>`
//!
//! See the [`relooper`] module for more details about the Relooper algorithm.

use crate::c_ast::iterators::{DFExpr, SomeId};
use crate::c_ast::CLabelId;
use crate::diagnostics::TranslationResult;
use crate::rust_ast::SpanExt;
use c2rust_ast_printer::pprust;
use proc_macro2::Span;
use std::collections::hash_map::DefaultHasher;
use std::collections::BTreeSet;
use std::fs::File;
use std::hash::Hash;
use std::hash::Hasher;
use std::io;
use std::io::Write;
use std::ops::Deref;
use std::ops::Index;
use syn::Lit;
use syn::{spanned::Spanned, Arm, Expr, Pat, Stmt};

use failure::format_err;
use indexmap::indexset;
use indexmap::{IndexMap, IndexSet};

use serde::ser::{
    Serialize, SerializeStruct, SerializeStructVariant, SerializeTupleVariant, Serializer,
};
use serde_json;

use crate::c_ast::*;
use crate::translator::*;
use crate::with_stmts::WithStmts;
use c2rust_ast_builder::mk;

mod inc_cleanup;
pub mod loops;
pub mod multiples;
pub mod relooper;
pub mod structures;

use crate::cfg::inc_cleanup::IncCleanup;
use crate::cfg::loops::*;
use crate::cfg::multiples::*;

/// These labels identify basic blocks in a regular CFG.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Label {
    /// Some labels come directly from the C side (namely those created from labels, cases, and
    /// defaults). For those, we just re-use the `CLabelId` of the C AST node.
    FromC(CLabelId, Option<Rc<str>>),

    /// Most labels are synthetically created while unwrapping control-flow constructs (like loops)
    /// into basic blocks.
    Synthetic(u64),
}

impl Label {
    pub fn pretty_print(&self) -> String {
        match self {
            Label::FromC(_, Some(s)) => format!("_{}", s.as_ref()),
            Label::FromC(CStmtId(label_id), None) => format!("c_{}", label_id),
            Label::Synthetic(syn_id) => format!("s_{}", syn_id),
        }
    }

    fn debug_print(&self) -> String {
        String::from(self.pretty_print().trim_start_matches('\''))
    }

    fn to_num_expr(&self) -> Box<Expr> {
        let mut s = DefaultHasher::new();
        self.hash(&mut s);
        let as_num = s.finish();

        mk().lit_expr(as_num as u128)
    }

    fn to_string_expr(&self) -> Box<Expr> {
        mk().lit_expr(self.debug_print())
    }

    fn to_int_lit(&self) -> Lit {
        let mut s = DefaultHasher::new();
        self.hash(&mut s);
        let as_num = s.finish();
        mk().int_lit(as_num as u128, "")
    }

    fn to_string_lit(&self) -> Lit {
        mk().str_lit(&self.debug_print())
    }
}

impl Serialize for Label {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.debug_print())
    }
}

/// These labels identify _structure_ basic blocks in a structure CFG.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum StructureLabel<S> {
    GoTo(Label),
    ExitTo(Label),
    Nested(Vec<Structure<S>>),
}

impl StructureLabel<StmtOrDecl> {
    /// Produce a new `StructureLabel` from the existing one by replacing all `StmtOrDecl::Decl`
    /// variants with either a declaration with initializer or only an initializer.
    fn place_decls(
        self,
        lift_me: &IndexSet<CDeclId>,
        store: &mut DeclStmtStore,
    ) -> StructureLabel<Stmt> {
        match self {
            StructureLabel::GoTo(l) => StructureLabel::GoTo(l),
            StructureLabel::ExitTo(l) => StructureLabel::ExitTo(l),
            StructureLabel::Nested(vs) => {
                let vs = vs
                    .into_iter()
                    .map(|s| s.place_decls(lift_me, store))
                    .collect();
                StructureLabel::Nested(vs)
            }
        }
    }
}

/// These are the things that the relooper algorithm produces.
#[derive(Clone, Debug)]
pub enum Structure<Stmt> {
    /// Series of statements and what to do after
    Simple {
        entries: IndexSet<Label>,
        body: Vec<Stmt>,
        span: Span,
        terminator: GenTerminator<StructureLabel<Stmt>>,
    },
    /// Looping constructs
    Loop {
        entries: IndexSet<Label>,
        body: Vec<Structure<Stmt>>,
    },
    /// Branching constructs
    Multiple {
        entries: IndexSet<Label>,
        branches: IndexMap<Label, Vec<Structure<Stmt>>>,
        then: Vec<Structure<Stmt>>,
    },
}

impl<S> Structure<S> {
    fn get_entries(&self) -> &IndexSet<Label> {
        use Structure::*;
        match self {
            Simple { entries, .. } => entries,
            Loop { entries, .. } => entries,
            Multiple { entries, .. } => entries,
        }
    }
}

impl Structure<StmtOrDecl> {
    /// Produce a new `Structure` from the existing one by replacing all `StmtOrDecl::Decl`
    /// variants with either a declaration with initializer or only an initializer.
    fn place_decls(
        self,
        lift_me: &IndexSet<CDeclId>,
        store: &mut DeclStmtStore,
    ) -> Structure<Stmt> {
        match self {
            Structure::Simple {
                entries,
                body,
                span,
                terminator,
            } => {
                let body = body
                    .into_iter()
                    .flat_map(|s: StmtOrDecl| -> Vec<Stmt> { s.place_decls(lift_me, store) })
                    .collect();
                let terminator = terminator.place_decls(lift_me, store);
                Structure::Simple {
                    entries,
                    body,
                    span,
                    terminator,
                }
            }
            Structure::Loop { entries, body } => {
                let body = body
                    .into_iter()
                    .map(|s| s.place_decls(lift_me, store))
                    .collect();
                Structure::Loop { entries, body }
            }
            Structure::Multiple {
                entries,
                branches,
                then,
            } => {
                let branches = branches
                    .into_iter()
                    .map(|(lbl, vs)| {
                        (
                            lbl,
                            vs.into_iter()
                                .map(|s| s.place_decls(lift_me, store))
                                .collect(),
                        )
                    })
                    .collect();
                let then = then
                    .into_iter()
                    .map(|s| s.place_decls(lift_me, store))
                    .collect();
                Structure::Multiple {
                    entries,
                    branches,
                    then,
                }
            }
        }
    }
}

/// Generalized basic block.
#[derive(Clone, Debug)]
pub struct BasicBlock<L, S> {
    /// Jump-free code
    body: Vec<S>,

    /// How to find the next (if any) basic block to go to
    terminator: GenTerminator<L>,

    /// Variables live at the beginning of this block
    live: IndexSet<CDeclId>,

    /// Variables defined in this block
    defined: IndexSet<CDeclId>,

    /// Span of this block
    span: Span,
}

impl<L: Clone, S1> BasicBlock<L, S1> {
    /// Produce a copy of the current basic block, but transform all of the statements using the
    /// function provided.
    fn map_stmts<S2, F: Fn(&S1) -> S2>(&self, f: F) -> BasicBlock<L, S2> {
        BasicBlock {
            body: self.body.iter().map(f).collect(),
            terminator: self.terminator.clone(),
            live: self.live.clone(),
            defined: self.defined.clone(),
            span: self.span,
        }
    }
}

impl<L: Serialize, St: Serialize> Serialize for BasicBlock<L, St> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut st = serializer.serialize_struct("BasicBlock", 2)?;
        st.serialize_field("body", &self.body)?;
        st.serialize_field("terminator", &self.terminator)?;
        st.end()
    }
}

impl<L, S> BasicBlock<L, S> {
    fn new(terminator: GenTerminator<L>) -> Self {
        BasicBlock {
            body: vec![],
            terminator,
            live: IndexSet::new(),
            defined: IndexSet::new(),
            span: Span::call_site(),
        }
    }

    fn new_jump(target: L) -> Self {
        BasicBlock::new(Jump(target))
    }
}

impl<S1, S2> BasicBlock<StructureLabel<S1>, S2> {
    /// Get all of the `GoTo` targets of a structure basic block
    fn successors(&self) -> IndexSet<Label> {
        self.terminator
            .get_labels()
            .iter()
            .filter_map(|&slbl| match slbl {
                StructureLabel::GoTo(tgt) => Some(tgt.clone()),
                _ => None,
            })
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
    Branch(Box<Expr>, Lbl, Lbl),

    /// Multi-way branch. The patterns are expected to match the type of the expression.
    Switch {
        expr: Box<Expr>,
        cases: Vec<(Pat, Lbl)>, // TODO: support ranges of expressions
    },
}

impl<L: Serialize> Serialize for GenTerminator<L> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match *self {
            GenTerminator::End => serializer.serialize_unit_variant("Terminator", 0, "End"),
            GenTerminator::Jump(ref l) => {
                let mut tv = serializer.serialize_tuple_variant("Terminator", 1, "Jump", 1)?;
                tv.serialize_field(l)?;
                tv.end()
            }
            GenTerminator::Branch(ref e, ref l1, ref l2) => {
                let mut tv = serializer.serialize_struct_variant("Terminator", 2, "Branch", 3)?;
                tv.serialize_field("condition", &pprust::expr_to_string(e))?;
                tv.serialize_field("then", l1)?;
                tv.serialize_field("else", l2)?;
                tv.end()
            }
            GenTerminator::Switch {
                ref expr,
                ref cases,
            } => {
                let mut cases_sane: Vec<(String, &L)> = vec![];
                for (p, l) in cases {
                    let pat: String = pprust::pat_to_string(p);
                    cases_sane.push((pat, l));
                }

                let mut tv = serializer.serialize_struct_variant("Terminator", 3, "Switch", 2)?;
                tv.serialize_field("expression", &pprust::expr_to_string(expr))?;
                tv.serialize_field("cases", &cases_sane)?;
                tv.end()
            }
        }
    }
}

// We use this a lot, so import its constructors
use self::GenTerminator::*;

impl<L> GenTerminator<L> {
    /// Produce a new terminator by transforming all of the labels in that terminator.
    fn map_labels<F: Fn(&L) -> N, N>(&self, func: F) -> GenTerminator<N> {
        match self {
            End => End,
            Jump(l) => Jump(func(l)),
            Branch(e, l1, l2) => Branch(e.clone(), func(l1), func(l2)),
            Switch { expr, cases } => Switch {
                expr: expr.clone(),
                cases: cases.iter().map(|(e, l)| (e.clone(), func(l))).collect(),
            },
        }
    }

    /// Extract references to all of the labels in the terminator
    fn get_labels(&self) -> Vec<&L> {
        match self {
            End => vec![],
            Jump(l) => vec![l],
            Branch(_, l1, l2) => vec![l1, l2],
            Switch { cases, .. } => cases.iter().map(|(_, l)| l).collect(),
        }
    }

    /// Extract mutable references to all of the labels in the terminator
    fn get_labels_mut(&mut self) -> Vec<&mut L> {
        match self {
            End => vec![],
            Jump(l) => vec![l],
            Branch(_, l1, l2) => vec![l1, l2],
            Switch { cases, .. } => cases.iter_mut().map(|(_, l)| l).collect(),
        }
    }
}

impl GenTerminator<StructureLabel<StmtOrDecl>> {
    /// Produce a new `GenTerminator` from the existing one by replacing all `StmtOrDecl::Decl`
    /// variants with either a declaration with initializer or only an initializer.
    fn place_decls(
        self,
        lift_me: &IndexSet<CDeclId>,
        store: &mut DeclStmtStore,
    ) -> GenTerminator<StructureLabel<Stmt>> {
        match self {
            End => End,
            Jump(l) => {
                let l = l.place_decls(lift_me, store);
                Jump(l)
            }
            Branch(e, l1, l2) => {
                let l1 = l1.place_decls(lift_me, store);
                let l2 = l2.place_decls(lift_me, store);
                Branch(e, l1, l2)
            }
            Switch { expr, cases } => {
                let cases = cases
                    .into_iter()
                    .map(|(e, l)| (e, l.place_decls(lift_me, store)))
                    .collect();
                Switch { expr, cases }
            }
        }
    }
}

/// The sole purpose of this structure is to accumulate information about what cases/default have
/// been seen which translating the body of the switch.
#[derive(Clone, Debug, Default)]
pub struct SwitchCases {
    cases: Vec<(Pat, Label)>,
    default: Option<Label>,
}

/// A Rust statement, or a C declaration, or a comment
///
/// `#[allow(clippy::large_enum_variant)]`ed instead of [`Box`]ing the large variant,
/// [`Stmt`] (472 bytes), as doing so would cause a lot of code churn and refactoring.
#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug)]
pub enum StmtOrDecl {
    /// Rust statement that was translated from a non-compound and non-declaration C statement.
    // TODO(kkysen): See if this should be boxed.
    Stmt(Stmt), // 472 bytes, not [`Box`]ed yet as it would cause a lot of code churn

    /// C declaration
    Decl(CDeclId), // 8 bytes
}

impl StmtOrDecl {
    pub fn to_string(&self, store: &DeclStmtStore) -> Vec<String> {
        match *self {
            StmtOrDecl::Stmt(ref s) => vec![pprust::stmt_to_string(s)],
            StmtOrDecl::Decl(ref d) => {
                let ss = store.peek_decl_and_assign(*d).unwrap();
                ss.iter().map(pprust::stmt_to_string).collect()
            }
        }
    }
}

impl StmtOrDecl {
    /// Produce a `Stmt` by replacing `StmtOrDecl::Decl`  variants with either a declaration with
    /// initializer or only an initializer.
    fn place_decls(self, lift_me: &IndexSet<CDeclId>, store: &mut DeclStmtStore) -> Vec<Stmt> {
        match self {
            StmtOrDecl::Stmt(s) => vec![s],
            StmtOrDecl::Decl(d) if lift_me.contains(&d) => {
                store.extract_assign(d).unwrap().into_iter().collect()
            }
            StmtOrDecl::Decl(d) => store
                .extract_decl_and_assign(d)
                .unwrap()
                .into_iter()
                .collect(),
        }
    }
}

/// A CFG graph of regular basic blocks.
#[derive(Clone, Debug)]
pub struct Cfg<Lbl: Ord + Hash, Stmt> {
    /// Entry point in the graph
    entries: Lbl,

    /// Nodes in the graph
    nodes: IndexMap<Lbl, BasicBlock<Lbl, Stmt>>,

    /// Loops in the graph
    loops: LoopInfo<Lbl>,

    /// Branching in the graph
    multiples: MultipleInfo<Lbl>,
}

impl<L: Clone + Ord + Hash, S1> Cfg<L, S1> {
    /// Produce a copy of the current CFG, but transform all of the statements using the
    /// function provided.
    pub fn map_stmts<S2, F: Fn(&S1) -> S2>(&self, f: F) -> Cfg<L, S2> {
        let entries = self.entries.clone();
        let nodes = self
            .nodes
            .iter()
            .map(|(l, bb)| (l.clone(), bb.map_stmts(&f)))
            .collect();
        let loops = self.loops.clone();
        let multiples = self.multiples.clone();

        Cfg {
            entries,
            nodes,
            loops,
            multiples,
        }
    }
}

impl<L: Serialize + Ord + Hash, St: Serialize> Serialize for Cfg<L, St> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut st = serializer.serialize_struct("ControlFlowGraph", 2)?;
        st.serialize_field("entries", &self.entries)?;
        st.serialize_field("nodes", &self.nodes)?;
        st.end()
    }
}

/// Reaching the end of a body without encountering a `return` means different things depending on
/// the function we are in.
#[derive(Clone, Debug)]
pub enum ImplicitReturnType {
    /// The `main` function implicitly returns `0`
    Main,

    /// `void` functions implicitly `return;` at the end of their bodies
    Void,

    /// We require that a non-`main` function not returning `void` have an explicit return. C99 is
    /// annoyingly more permissive. From 6.9.1 paragraph 12,
    ///
    /// > If the `}` that terminates a function is reached, and the value of the function call is
    /// > used by the caller, the behavior is undefined."
    NoImplicitReturnType,

    /// This is for handling GNU C statement expressions
    /// <https://gcc.gnu.org/onlinedocs/gcc-3.2.3/gcc/Statement-Exprs.html>
    ///
    /// TODO: document
    StmtExpr(ExprContext, CExprId, Label),
}

/// A complete control-flow graph
impl Cfg<Label, StmtOrDecl> {
    /// Completely process a statement into a control flow graph.
    pub fn from_stmts(
        translator: &Translation,
        ctx: ExprContext,
        stmt_ids: &[CStmtId],
        ret: ImplicitReturnType,
        ret_ty: Option<CQualTypeId>,
    ) -> TranslationResult<(Self, DeclStmtStore)> {
        let mut c_label_to_goto: IndexMap<CLabelId, IndexSet<CStmtId>> = IndexMap::new();
        for (target, x) in stmt_ids
            .iter()
            .flat_map(|&stmt_id| DFExpr::new(&translator.ast_context, stmt_id.into()))
            .flat_map(SomeId::stmt)
            .flat_map(|x| match translator.ast_context[x].kind {
                CStmtKind::Goto(target) => Some((target, x)),
                _ => None,
            })
        {
            c_label_to_goto.entry(target).or_default().insert(x);
        }

        let mut cfg_builder = CfgBuilder::new(c_label_to_goto);
        let entry = cfg_builder.entry.clone();
        cfg_builder.per_stmt_stack.push(PerStmt::new(
            stmt_ids.first().cloned(),
            entry.clone(),
            IndexSet::new(),
        ));

        translator.with_scope(|| -> TranslationResult<()> {
            let body_exit = cfg_builder.convert_stmts_help(
                translator,
                ctx,
                stmt_ids,
                Some(ret.clone()),
                entry,
                ret_ty,
            )?;

            if let Some(body_exit) = body_exit {
                let mut wip = cfg_builder.new_wip_block(body_exit);

                // Add in what to do after control-flow exits the statement
                match ret {
                    ImplicitReturnType::Main => {
                        let ret_expr: Option<Box<Expr>> = Some(mk().lit_expr(mk().int_lit(0, "")));
                        wip.body
                            .push(StmtOrDecl::Stmt(mk().semi_stmt(mk().return_expr(ret_expr))));
                    }
                    ImplicitReturnType::Void => {
                        wip.body
                            .push(StmtOrDecl::Stmt(mk().semi_stmt(mk().return_expr(None))));
                    }
                    ImplicitReturnType::NoImplicitReturnType => {
                        // NOTE: emitting `ret_expr` is not necessarily an error. For instance,
                        // this statement exit may be dominated by one or more return statements.
                        let ret_expr: Box<Expr> =
                            translator.panic("Reached end of non-void function without returning");
                        wip.body.push(StmtOrDecl::Stmt(mk().semi_stmt(ret_expr)));
                    }
                    ImplicitReturnType::StmtExpr(ctx, expr_id, brk_label) => {
                        let (stmts, val) = translator
                            .convert_expr(ctx, expr_id, None)?
                            .discard_unsafe();

                        wip.body.extend(stmts.into_iter().map(StmtOrDecl::Stmt));
                        wip.body.push(StmtOrDecl::Stmt(mk().semi_stmt(
                            mk().break_expr_value(Some(brk_label.pretty_print()), Some(val)),
                        )));
                    }
                };

                cfg_builder.add_wip_block(wip, End);
            }

            Ok(())
        })?;

        let last_per_stmt = cfg_builder.per_stmt_stack.pop().unwrap();

        //        {
        //            let
        //            // Check the graph doesn't reference any labels it doesn't contain
        //            let bad_labels: Vec<&CLabelId> = last_per_stmt.c_labels_used.keys().cloned().collect::<IndexSet<CLabelId>>()
        //                .difference(&last_per_stmt.c_labels_defined)
        //                .collect();
        //            if !bad_labels.is_empty() {
        //                Err(format!(
        //                    "Control flow graph for statements {:?} references undefined label(s): {:?}",
        //                    stmt_ids,
        //                    bad_labels,
        //                ))?
        //            }
        //        }

        // Make a CFG from the PerStmt.

        let (graph, decls_seen, live_in) = last_per_stmt.into_cfg();
        assert!(live_in.is_empty(), "non-empty live_in");

        Ok((graph, decls_seen))
    }
}

use std::fmt::Debug;
use std::rc::Rc;

/// The polymorphism here is only to make it clear exactly how little these functions need to know
/// about the actual contents of the CFG - we only actual call these on one monomorphic CFG type.
impl<Lbl: Clone + Ord + Hash + Debug, Stmt> Cfg<Lbl, Stmt> {
    /// Removes blocks that cannot be reached from the CFG
    pub fn prune_unreachable_blocks_mut(&mut self) {
        let visited: IndexSet<Lbl> = {
            let mut visited: IndexSet<Lbl> = IndexSet::new();
            let mut to_visit: Vec<Lbl> = vec![self.entries.clone()];

            while let Some(lbl) = to_visit.pop() {
                if visited.contains(&lbl) {
                    continue;
                }

                let blk = self.nodes.get(&lbl).unwrap_or_else(|| {
                    panic!(
                        "prune_unreachable_blocks: block not found\n{:?}\n{:?}",
                        lbl,
                        self.nodes.keys().cloned().collect::<Vec<Lbl>>()
                    )
                });
                visited.insert(lbl);

                for lbl in blk.terminator.get_labels() {
                    if !visited.contains(lbl) {
                        to_visit.push(lbl.clone());
                    }
                }
            }

            visited
        };

        self.nodes.retain(|lbl, _| visited.contains(lbl));
        self.loops.filter_unreachable(&visited);
        // TODO multiple info
    }

    /// Removes empty blocks whose terminator is just a `Jump` by merging them with the block they
    /// are jumping to.
    pub fn prune_empty_blocks_mut(&mut self) {
        // Keys are labels corresponding to empty basic blocks with a jump terminator, values are
        // the labels they jump to (and can hopefully be replaced by).
        let mut proposed_rewrites: IndexMap<Lbl, Lbl> = self
            .nodes
            .iter()
            .filter_map(|(lbl, bb)| Cfg::empty_bb(bb).map(|tgt| (lbl.clone(), tgt)))
            .collect();

        // Rewrites to actually apply. Keys are labels to basic blocks that were remapped into the
        // basic block corresponding to the value.
        let mut actual_rewrites: IndexMap<Lbl, Lbl> = IndexMap::new();

        while let Some((from, to)) = proposed_rewrites
            .iter()
            .map(|(f, t)| (f.clone(), t.clone()))
            .next()
        {
            proposed_rewrites.swap_remove(&from);
            let mut from_any: IndexSet<Lbl> = indexset![from];

            // Try to apply more rewrites from `proposed_rewrites`
            let mut to_intermediate: Lbl = to;
            while let Some(to_new) = proposed_rewrites.swap_remove(&to_intermediate) {
                from_any.insert(to_intermediate);
                to_intermediate = to_new;
            }

            // Check if there were already some rewrites applied
            let to_final = match actual_rewrites.get(&to_intermediate) {
                None => to_intermediate,
                Some(to_final) => {
                    from_any.insert(to_intermediate);
                    to_final.clone()
                }
            };

            // It makes no sense to remap something to itself
            for from in from_any {
                if from != to_final {
                    let span = self.nodes[&from].span;
                    let tgt_span = &mut self.nodes[&to_final].span;
                    if tgt_span.is_dummy() {
                        *tgt_span = span;
                    } else if !span.is_dummy() {
                        // If we can't transfer this basic block's span to the
                        // target, don't delete it
                        continue;
                    }
                    actual_rewrites.insert(from, to_final.clone());
                }
            }
        }

        // Apply the remaps to the entries
        self.entries = actual_rewrites
            .get(&self.entries)
            .unwrap_or(&self.entries)
            .clone();

        // We keep only the basic blocks that weren't remapped to anything.
        self.nodes
            .retain(|lbl, _| actual_rewrites.get(lbl).is_none());

        // However, those block we do keep, we remap the labels in their terminators.
        for bb in self.nodes.values_mut() {
            for lbl in bb.terminator.get_labels_mut() {
                if let Some(new_lbl) = actual_rewrites.get(lbl) {
                    *lbl = new_lbl.clone();
                }
            }
        }

        self.loops.rewrite_blocks(&actual_rewrites);
        self.multiples.rewrite_blocks(&actual_rewrites);
    }

    /// Given an empty `BasicBlock` that ends in a `Jump`, return the target label. In all other
    /// cases, return `None`.
    fn empty_bb(bb: &BasicBlock<Lbl, Stmt>) -> Option<Lbl> {
        match &bb.terminator {
            Jump(lbl) if bb.body.is_empty() => Some(lbl.clone()),
            _ => None,
        }
    }
}

/// This stores all of the state required to construct a control-flow graph from C statements. Once
/// the graph is constructed, we only really care about the 'graph' field.
#[derive(Clone, Debug)]
struct CfgBuilder {
    /// Identifies the 'BasicBlock' to start with in the graph
    entry: Label,

    per_stmt_stack: Vec<PerStmt>,

    /// Variables in scope right before the current statement. The wrapping `Vec` witnesses the
    /// notion of scope: later elements in the vector are always supersets of earlier elements.
    currently_live: Vec<IndexSet<CDeclId>>,

    // Book-keeping information for translating switch statements
    /// Stack of labels identifying what a 'break' should jump to. We push onto this stack when
    /// entering a construct that can break and pop when exiting that construct.
    break_labels: Vec<Label>,
    /// Like 'break_labels', but for 'continue'.
    continue_labels: Vec<Label>,
    /// Accumulates information for the 'case'/'default' encountered so far while translating the
    /// body of a 'switch'.
    switch_expr_cases: Vec<SwitchCases>,

    // Fresh ID sources
    /// Source for generating fresh synthetic labels
    prev_label: u64,
    /// Source for generating fresh loop IDs
    prev_loop_id: u64,

    /// Global (immutable) mapping of `CLabelId` -> ID of pointing gotos (basically, reverse the dir
    /// of the goto)
    c_label_to_goto: IndexMap<CLabelId, IndexSet<CStmtId>>,

    // Book-keeping information to build up the `loops` and `multiples` fields in `graph`.
    /// Loops we are currently in. Every time we enter a loop, we push a new vector onto this field.
    /// When we exit that loop, we pop the vector, add all the labels to the next entry in the
    /// `Vec`, and also add the loop to the CFG.
    loops: Vec<(LoopId, Vec<Label>)>,
    /// Multiple branching we are currently in. Every time we enter another arm of a branching
    /// construct, we add it into here. When we finish processing the branch, we remove it.
    ///
    /// NOTE: we technically don't need the `Label` here - it is just for debugging.
    multiples: Vec<(Label, Vec<Label>)>,
}

/// We keep a stack of these in `CfgBuilder`. We push a new one on every time we start a statement
/// and pop it off when the statement ends.
#[derive(Debug, Clone)]
struct PerStmt {
    /// Statement id of statement we are processing and nodes in the graph
    stmt_id: Option<CStmtId>, // for debugging only
    entry: Label,
    nodes: IndexMap<Label, BasicBlock<Label, StmtOrDecl>>,

    // Nodes in the statements graph, along with loop info and multiple info and decls
    loop_info: LoopInfo<Label>,
    multiple_info: MultipleInfo<Label>,
    decls_seen: DeclStmtStore,

    /// Encountered a `break`/`continue`/`case`/`default` whose loop/switch has not yet been closed
    saw_unmatched_break: bool,
    saw_unmatched_continue: bool,
    saw_unmatched_default: bool,
    saw_unmatched_case: bool,

    /// All of the C labels we have seen defined
    c_labels_defined: IndexSet<CLabelId>,
    /// All of the C labels we have seen used and the gotos point to it
    c_labels_used: IndexMap<CLabelId, IndexSet<CStmtId>>,

    /// What declarations were live going into this statement
    live_in: IndexSet<CDeclId>,
}

impl PerStmt {
    /// Create a fresh `PerStmt`
    pub fn new(stmt_id: Option<CStmtId>, entry: Label, live_in: IndexSet<CDeclId>) -> PerStmt {
        PerStmt {
            stmt_id,
            entry,
            nodes: IndexMap::new(),

            loop_info: LoopInfo::new(),
            multiple_info: MultipleInfo::new(),
            decls_seen: DeclStmtStore::new(),

            saw_unmatched_break: false,
            saw_unmatched_continue: false,
            saw_unmatched_default: false,
            saw_unmatched_case: false,

            c_labels_defined: IndexSet::new(),
            c_labels_used: IndexMap::new(),

            live_in,
        }
    }

    /// Merge into this `PerStmt` another `PerStmt`
    pub fn absorb(&mut self, other: PerStmt) {
        self.nodes.extend(other.nodes);

        self.loop_info.absorb(other.loop_info);
        self.multiple_info.absorb(other.multiple_info);
        self.decls_seen.absorb(other.decls_seen);

        self.saw_unmatched_break |= other.saw_unmatched_break;
        self.saw_unmatched_continue |= other.saw_unmatched_continue;
        self.saw_unmatched_default |= other.saw_unmatched_default;
        self.saw_unmatched_case |= other.saw_unmatched_case;

        self.c_labels_defined.extend(other.c_labels_defined);
        self.c_labels_used.extend(other.c_labels_used);
    }

    /// Check if a current `PerStmt` is self contained
    pub fn is_contained(
        &self,
        c_label_to_goto: &IndexMap<CLabelId, IndexSet<CStmtId>>, // exhaustive pre-computed list
        currently_live: &IndexSet<CDeclId>,
    ) -> bool {
        // Have we seen a `break`/`continue`/`case`/`default` whose loop/switch has not yet been
        // closed?
        if self.saw_unmatched_break
            || self.saw_unmatched_continue
            || self.saw_unmatched_case
            || self.saw_unmatched_default
        {
            return false;
        }

        // Check the subgraph doesn't reference any labels it doesn't contain
        if self
            .c_labels_used
            .keys()
            .cloned()
            .collect::<IndexSet<CLabelId>>()
            != self.c_labels_defined
        {
            return false;
        }

        // Check that the subgraph doesn't have any gotos pointing into it
        if self
            .c_labels_used
            .iter()
            .any(|(lbl, gotos)| c_label_to_goto.get(lbl) != Some(gotos))
        {
            return false;
        }

        // Did this statement define any new declarations that are still in scope? If so, we can't
        // say this is contained (since perhaps this declaration is going to be jumped over, in
        // which case we'd have to lift the decl).
        if &self.live_in != currently_live {
            return false;
        }

        true
    }

    /// Flatten the current `PerStmt` into
    pub fn into_cfg(self) -> (Cfg<Label, StmtOrDecl>, DeclStmtStore, IndexSet<CDeclId>) {
        // Synthesize a CFG from the current `PerStmt`
        let mut graph = Cfg {
            entries: self.entry,
            nodes: self.nodes,
            loops: self.loop_info,
            multiples: self.multiple_info,
        };

        graph.prune_empty_blocks_mut();
        graph.prune_unreachable_blocks_mut();

        (graph, self.decls_seen, self.live_in)
    }
}

/// Stores information about translating C declarations to Rust statements. When seeing a C
/// declaration, we often don't know if it is already in the right place. The fix is to punt: we
/// put into a `DeclStmtStore` information about what to do in all possible cases and we delay
/// choosing what to do until later.
#[derive(Clone, Debug, Default)]
pub struct DeclStmtStore {
    store: IndexMap<CDeclId, DeclStmtInfo>,
}

/// This contains the information one needs to convert a C declaration in all the possible ways:
///
///  1. declare and initialize
///  2. declare only (and incidentally zero-initialize)
///  3. initialize only (if the declaration has already been emitted)
///
#[derive(Clone, Debug)]
pub struct DeclStmtInfo {
    /// Just the declaration
    pub decl: Option<Vec<Stmt>>,

    /// Just the assignment
    pub assign: Option<Vec<Stmt>>,

    /// Both the declaration and the assignment
    pub decl_and_assign: Option<Vec<Stmt>>,
}

impl DeclStmtInfo {
    pub fn new(decl: Vec<Stmt>, assign: Vec<Stmt>, decl_and_assign: Vec<Stmt>) -> Self {
        Self {
            decl: Some(decl),
            assign: Some(assign),
            decl_and_assign: Some(decl_and_assign),
        }
    }

    pub fn empty() -> Self {
        Self {
            decl: Some(Vec::new()),
            assign: Some(Vec::new()),
            decl_and_assign: Some(Vec::new()),
        }
    }
}

impl DeclStmtStore {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn absorb(&mut self, other: DeclStmtStore) {
        self.store.extend(other.store);
    }

    /// Extract _just_ the Rust statements for a declaration (without initialization). Used when you
    /// want to move just a declaration to a larger scope.
    pub fn extract_decl(&mut self, decl_id: CDeclId) -> TranslationResult<Vec<Stmt>> {
        let DeclStmtInfo { decl, assign, .. } = self
            .store
            .swap_remove(&decl_id)
            .ok_or_else(|| format_err!("Cannot find information on declaration 1 {:?}", decl_id))?;

        let decl: Vec<Stmt> = decl.ok_or_else(|| {
            format_err!("Declaration for {:?} has already been extracted", decl_id)
        })?;

        let pruned = DeclStmtInfo {
            decl: None,
            assign,
            decl_and_assign: None,
        };
        self.store.insert(decl_id, pruned);

        Ok(decl)
    }

    /// Extract _just_ the Rust statements for an initializer (without the declaration it was
    /// initially attached to). Used when you've moved a declaration but now you need to also run the
    /// initializer.
    pub fn extract_assign(&mut self, decl_id: CDeclId) -> TranslationResult<Vec<Stmt>> {
        let DeclStmtInfo { decl, assign, .. } =
            self.store.swap_remove(&decl_id).ok_or_else(|| {
                format_err!("Cannot find information on declaration 2 {:?}", decl_id,)
            })?;

        let assign: Vec<Stmt> = assign.ok_or_else(|| {
            format_err!("Assignment for {:?} has already been extracted", decl_id)
        })?;

        let pruned = DeclStmtInfo {
            decl,
            assign: None,
            decl_and_assign: None,
        };
        self.store.insert(decl_id, pruned);

        Ok(assign)
    }

    /// Extract the Rust statements for the full declaration and initializers. Used for when you
    /// didn't need to move a declaration at all.
    pub fn extract_decl_and_assign(&mut self, decl_id: CDeclId) -> TranslationResult<Vec<Stmt>> {
        let DeclStmtInfo {
            decl_and_assign, ..
        } = self
            .store
            .swap_remove(&decl_id)
            .ok_or_else(|| format_err!("Cannot find information on declaration 3 {:?}", decl_id))?;

        let decl_and_assign: Vec<Stmt> = decl_and_assign.ok_or_else(|| {
            format_err!(
                "Declaration with assignment for {:?} has already been extracted",
                decl_id
            )
        })?;

        let pruned = DeclStmtInfo {
            decl: None,
            assign: None,
            decl_and_assign: None,
        };
        self.store.insert(decl_id, pruned);

        Ok(decl_and_assign)
    }

    /// Extract the Rust statements for the full declaration and initializers. DEBUGGING ONLY.
    pub fn peek_decl_and_assign(&self, decl_id: CDeclId) -> TranslationResult<Vec<Stmt>> {
        let DeclStmtInfo {
            decl_and_assign, ..
        } = self
            .store
            .get(&decl_id)
            .ok_or_else(|| format_err!("Cannot find information on declaration 4 {:?}", decl_id))?;

        let decl_and_assign: Vec<Stmt> = decl_and_assign.clone().ok_or_else(|| {
            format_err!(
                "Declaration with assignment for {:?} has already been extracted",
                decl_id
            )
        })?;

        Ok(decl_and_assign)
    }
}

/// Represents a `BasicBlock` under construction where the bit under construction is the end. Extra
/// statements may be added in the `body` and extra declarations may be added to `defined`. However,
/// the `label` and `live` should not change.
#[derive(Debug)]
struct WipBlock {
    /// Label of WIP.
    label: Label,

    /// Statements so far in the WIP.
    body: Vec<StmtOrDecl>,

    /// Variables defined so far in this WIP.
    defined: IndexSet<CDeclId>,

    /// Variables live in this WIP.
    live: IndexSet<CDeclId>,

    /// Span of this block
    span: Span,
}

impl Extend<Stmt> for WipBlock {
    fn extend<T: IntoIterator<Item = Stmt>>(&mut self, iter: T) {
        for stmt in iter.into_iter() {
            self.body.push(StmtOrDecl::Stmt(stmt))
        }
    }
}

impl WipBlock {
    pub fn push_stmt(&mut self, stmt: Stmt) {
        self.body.push(StmtOrDecl::Stmt(stmt))
    }

    pub fn push_decl(&mut self, decl: CDeclId) {
        self.body.push(StmtOrDecl::Decl(decl))
    }
}

/// This impl block deals with creating control flow graphs
impl CfgBuilder {
    fn last_per_stmt_mut(&mut self) -> &mut PerStmt {
        self.per_stmt_stack
            .last_mut()
            .expect("'per_stmt_stack' is empty")
    }

    /// Add a basic block to the control flow graph, specifying under which label to insert it.
    fn add_block(&mut self, lbl: Label, bb: BasicBlock<Label, StmtOrDecl>) {
        let currently_live = self
            .currently_live
            .last_mut()
            .expect("Found no live currently live scope");

        for decl in &bb.defined {
            currently_live.insert(*decl);
        }

        match self
            .per_stmt_stack
            .last_mut()
            .expect("'per_stmt_stack' is empty")
            .nodes
            .insert(lbl.clone(), bb)
        {
            None => {}
            Some(_) => panic!("Label {:?} cannot identify two basic blocks", lbl),
        }

        if let Some((_, loop_vec)) = self.loops.last_mut() {
            loop_vec.push(lbl.clone());
        }
        if let Some((_, arm_vec)) = self.multiples.last_mut() {
            arm_vec.push(lbl);
        }
    }

    /// Create a basic block from a WIP block by tacking on the right terminator. Once this is done,
    /// add the block into the graph.
    fn add_wip_block(&mut self, wip: WipBlock, terminator: GenTerminator<Label>) {
        let WipBlock {
            label,
            body,
            defined,
            live,
            span,
        } = wip;
        self.add_block(
            label,
            BasicBlock {
                body,
                terminator,
                defined,
                live,
                span,
            },
        );
    }

    /// Update the terminator of an existing block. This is for the special cases where you don't
    /// know the terminators of a block by visiting it.
    fn update_terminator(&mut self, lbl: Label, new_term: GenTerminator<Label>) {
        match self.last_per_stmt_mut().nodes.get_mut(&lbl) {
            None => panic!("Cannot find label {:?} to update", lbl),
            Some(bb) => bb.terminator = new_term,
        }
    }

    /// Open a loop
    fn open_loop(&mut self) {
        let loop_id: LoopId = self.fresh_loop_id();
        self.loops.push((loop_id, vec![]));
    }

    /// Close a loop
    fn close_loop(&mut self) {
        let (loop_id, loop_contents) = self.loops.pop().expect("No loop to close.");
        let outer_loop_id: Option<LoopId> = self.loops.last().map(|&(i, _)| i);

        // Add the loop contents to the outer loop (if there is one)
        if let Some((_, outer_loop)) = self.loops.last_mut() {
            outer_loop.extend(loop_contents.iter().cloned());
        }

        self.last_per_stmt_mut().loop_info.add_loop(
            loop_id,
            loop_contents.into_iter().collect(),
            outer_loop_id,
        );
    }

    /// Open an arm
    fn open_arm(&mut self, arm_start: Label) {
        self.multiples.push((arm_start, vec![]));
    }

    /// Close an arm
    fn close_arm(&mut self) -> (Label, IndexSet<Label>) {
        let (arm_start, arm_contents) = self.multiples.pop().expect("No arm to close.");

        // Add the arm contents to the outer arm (if there is one)
        if let Some((_, outer_arm)) = self.multiples.last_mut() {
            outer_arm.extend(arm_contents.iter().cloned());
        }

        (arm_start, arm_contents.into_iter().collect())
    }

    /// REMARK: make sure that basic blocks are constructed either entirely inside or entirely
    ///         outside `with_scope`. Otherwise, the scope of the block is going to be confused.
    fn with_scope<B, F: FnOnce(&mut Self) -> B>(
        &mut self,
        _translator: &Translation,
        cont: F,
    ) -> B {
        // Open a new scope
        let new_vars = self.current_variables();
        self.currently_live.push(new_vars);

        let b = cont(self);

        // Close the scope
        self.currently_live
            .pop()
            .expect("Found no live currently live scope to close");

        b
    }

    fn current_variables(&self) -> IndexSet<CDeclId> {
        self.currently_live
            .last()
            .expect("Found no live currently live scope")
            .clone()
    }

    // Start a new basic block WIP.
    fn new_wip_block(&mut self, new_label: Label) -> WipBlock {
        WipBlock {
            label: new_label,
            body: vec![],
            defined: IndexSet::new(),
            live: self.current_variables(),
            span: Span::call_site(),
        }
    }

    /// Generate a fresh (synthetic) label.
    fn fresh_label(&mut self) -> Label {
        self.prev_label += 1;
        Label::Synthetic(self.prev_label)
    }

    /// Generate a fresh (synthetic) label.
    fn fresh_loop_id(&mut self) -> LoopId {
        self.prev_loop_id += 1;
        LoopId::new(self.prev_loop_id)
    }

    /// Create a new `CfgBuilder` with a single entry label.
    fn new(c_label_to_goto: IndexMap<CLabelId, IndexSet<CStmtId>>) -> CfgBuilder {
        let entry = Label::Synthetic(0);

        CfgBuilder {
            entry,

            per_stmt_stack: vec![],

            prev_label: 0,
            prev_loop_id: 0,

            c_label_to_goto,

            break_labels: vec![],
            continue_labels: vec![],
            switch_expr_cases: vec![],

            currently_live: vec![IndexSet::new()],

            loops: vec![],
            multiples: vec![],
        }
    }

    /// Same as `convert_stmt_help`, but operates over a sequence of statements
    fn convert_stmts_help(
        &mut self,
        translator: &Translation,
        ctx: ExprContext,
        stmt_ids: &[CStmtId],                // C statements to translate
        in_tail: Option<ImplicitReturnType>, // Are we in tail position (is there anything to fallthrough to)?
        entry: Label,                        // Current WIP block
        ret_ty: Option<CQualTypeId>,
    ) -> TranslationResult<Option<Label>> {
        self.with_scope(translator, |slf| -> TranslationResult<Option<Label>> {
            let mut lbl = Some(entry);
            let last = stmt_ids.last();

            // We feed the optional output label into the entry label of the next block
            for stmt in stmt_ids {
                let new_label: Label = lbl.unwrap_or_else(|| slf.fresh_label());
                let sub_in_tail = in_tail.clone().filter(|_| Some(stmt) == last);
                lbl =
                    slf.convert_stmt_help(translator, ctx, *stmt, sub_in_tail, new_label, ret_ty)?;
            }

            Ok(lbl)
        })
    }

    /// Translate a C statement, inserting it into the CFG under the label key passed in.
    ///
    /// If the input C statement naturally passes control to the statement that follows it, the
    /// return should be the new label to give to the fallthrough block.
    ///
    /// NOTE: It is important that we finish adding a block to the graph before we start creating
    ///       the next one. Every time a new block is started with `new_wip_block`, we take a
    ///       snapshot of the live variables from `currently_live`.
    fn convert_stmt_help(
        &mut self,
        translator: &Translation,
        ctx: ExprContext,

        // C statement to translate
        stmt_id: CStmtId,

        // Are we in tail position (does the function end with this statement)?
        in_tail: Option<ImplicitReturnType>,

        // Entry label
        entry: Label,

        // The type this statement should return. May differ from the type the C AST is returning,
        // in which case we must cast.
        ret_ty: Option<CQualTypeId>,
    ) -> TranslationResult<Option<Label>> {
        // Add to the per_stmt_stack
        let live_in: IndexSet<CDeclId> = self.currently_live.last().unwrap().clone();
        self.per_stmt_stack
            .push(PerStmt::new(Some(stmt_id), entry.clone(), live_in));

        let mut wip = self.new_wip_block(entry.clone());

        wip.span = translator
            .get_span(SomeId::Stmt(stmt_id))
            .unwrap_or_else(Span::call_site);

        let out_wip: TranslationResult<Option<WipBlock>> = match translator
            .ast_context
            .index(stmt_id)
            .kind
        {
            CStmtKind::Empty => Ok(Some(wip)),

            CStmtKind::Decls(ref decls) => {
                for decl in decls {
                    let info = translator.convert_decl_stmt_info(ctx, *decl)?;
                    self.last_per_stmt_mut()
                        .decls_seen
                        .store
                        .insert(*decl, info);

                    wip.push_decl(*decl);
                    wip.defined.insert(*decl);
                }
                Ok(Some(wip))
            }

            CStmtKind::Return(expr) => {
                let val = match expr.map(|i| translator.convert_expr(ctx.used(), i, ret_ty)) {
                    Some(r) => Some(r?),
                    None => None,
                };

                let (stmts, ret_val) = WithStmts::with_stmts_opt(val).discard_unsafe();
                wip.extend(stmts);
                wip.push_stmt(mk().expr_stmt(mk().return_expr(ret_val)));

                self.add_wip_block(wip, End);

                Ok(None)
            }

            CStmtKind::If {
                scrutinee,
                true_variant,
                false_variant,
            } => {
                let next_entry = self.fresh_label();
                let then_entry = self.fresh_label();
                let else_entry = if false_variant.is_none() {
                    next_entry.clone()
                } else {
                    self.fresh_label()
                };

                // Condition
                let (stmts, val) = translator
                    .convert_condition(ctx, true, scrutinee)?
                    .discard_unsafe();
                wip.extend(stmts);

                let cond_val = translator.ast_context[scrutinee].kind.get_bool();
                self.add_wip_block(
                    wip,
                    match cond_val {
                        Some(true) => Jump(then_entry.clone()),
                        Some(false) => Jump(else_entry.clone()),
                        None => Branch(val, then_entry.clone(), else_entry.clone()),
                    },
                );

                // Then case
                self.open_arm(then_entry.clone());
                let then_stuff = self.convert_stmt_help(
                    translator,
                    ctx,
                    true_variant,
                    in_tail.clone(),
                    then_entry,
                    ret_ty,
                )?;
                if let Some(then_end) = then_stuff {
                    let wip_then = self.new_wip_block(then_end);
                    self.add_wip_block(wip_then, Jump(next_entry.clone()));
                }
                let then_arm = self.close_arm();

                // Else case
                self.open_arm(else_entry.clone());
                if let Some(false_var) = false_variant {
                    let else_stuff = self.convert_stmt_help(
                        translator,
                        ctx,
                        false_var,
                        in_tail.clone(),
                        else_entry,
                        ret_ty,
                    )?;
                    if let Some(else_end) = else_stuff {
                        let wip_else = self.new_wip_block(else_end);
                        self.add_wip_block(wip_else, Jump(next_entry.clone()));
                    }
                };
                let else_arm = self.close_arm();

                self.last_per_stmt_mut()
                    .multiple_info
                    .add_multiple(next_entry.clone(), vec![then_arm, else_arm]);

                // Return
                Ok(Some(self.new_wip_block(next_entry)))
            }

            CStmtKind::While {
                condition,
                body: body_stmt,
            } => {
                let cond_entry = self.fresh_label();
                let body_entry = self.fresh_label();
                let next_entry = self.fresh_label();

                self.add_wip_block(wip, Jump(cond_entry.clone()));
                self.open_loop();

                // Condition
                let (stmts, val) = translator
                    .convert_condition(ctx, true, condition)?
                    .discard_unsafe();
                let cond_val = translator.ast_context[condition].kind.get_bool();
                let mut cond_wip = self.new_wip_block(cond_entry.clone());
                cond_wip.extend(stmts);

                self.add_wip_block(
                    cond_wip,
                    match cond_val {
                        Some(true) => Jump(body_entry.clone()),
                        Some(false) => Jump(next_entry.clone()),
                        None => Branch(val, body_entry.clone(), next_entry.clone()),
                    },
                );

                // Body
                let saw_unmatched_break = self.last_per_stmt_mut().saw_unmatched_break;
                let saw_unmatched_continue = self.last_per_stmt_mut().saw_unmatched_continue;
                self.break_labels.push(next_entry.clone());
                self.continue_labels.push(cond_entry.clone());

                let body_stuff =
                    self.convert_stmt_help(translator, ctx, body_stmt, None, body_entry, ret_ty)?;
                if let Some(body_end) = body_stuff {
                    let wip_body = self.new_wip_block(body_end);
                    self.add_wip_block(wip_body, Jump(cond_entry));
                }

                self.last_per_stmt_mut().saw_unmatched_break = saw_unmatched_break;
                self.last_per_stmt_mut().saw_unmatched_continue = saw_unmatched_continue;
                self.break_labels.pop();
                self.continue_labels.pop();
                self.close_loop();

                //Return
                Ok(Some(self.new_wip_block(next_entry)))
            }

            CStmtKind::DoWhile {
                body: body_stmt,
                condition,
            } => {
                let body_entry = self.fresh_label();
                let cond_entry = self.fresh_label();
                let next_entry = self.fresh_label();

                self.add_wip_block(wip, Jump(body_entry.clone()));
                self.open_loop();

                // Body
                let saw_unmatched_break = self.last_per_stmt_mut().saw_unmatched_break;
                let saw_unmatched_continue = self.last_per_stmt_mut().saw_unmatched_continue;
                self.break_labels.push(next_entry.clone());
                self.continue_labels.push(cond_entry.clone());

                let body_stuff = self.convert_stmt_help(
                    translator,
                    ctx,
                    body_stmt,
                    None,
                    body_entry.clone(),
                    ret_ty,
                )?;
                if let Some(body_end) = body_stuff {
                    let wip_body = self.new_wip_block(body_end);
                    self.add_wip_block(wip_body, Jump(cond_entry.clone()));
                }

                self.last_per_stmt_mut().saw_unmatched_break = saw_unmatched_break;
                self.last_per_stmt_mut().saw_unmatched_continue = saw_unmatched_continue;
                self.break_labels.pop();
                self.continue_labels.pop();

                // Condition
                let (stmts, val) = translator
                    .convert_condition(ctx, true, condition)?
                    .discard_unsafe();
                let cond_val = translator.ast_context[condition].kind.get_bool();
                let mut cond_wip = self.new_wip_block(cond_entry);
                cond_wip.extend(stmts);
                self.add_wip_block(
                    cond_wip,
                    match cond_val {
                        Some(true) => Jump(body_entry),
                        Some(false) => Jump(next_entry.clone()),
                        None => Branch(val, body_entry, next_entry.clone()),
                    },
                );

                self.close_loop();

                //Return
                Ok(Some(self.new_wip_block(next_entry)))
            }

            CStmtKind::ForLoop {
                init,
                condition,
                increment,
                body,
            } => {
                let init_entry = self.fresh_label();
                let cond_entry = self.fresh_label();
                let body_entry = self.fresh_label();
                let incr_entry = self.fresh_label();
                let next_label = self.fresh_label();

                self.with_scope(translator, |slf| -> TranslationResult<()> {
                    // Init
                    slf.add_wip_block(wip, Jump(init_entry.clone()));
                    let init_stuff: Option<Label> = match init {
                        None => Some(init_entry),
                        Some(init) => {
                            slf.convert_stmt_help(translator, ctx, init, None, init_entry, ret_ty)?
                        }
                    };
                    if let Some(init_end) = init_stuff {
                        let wip_init = slf.new_wip_block(init_end);
                        slf.add_wip_block(wip_init, Jump(cond_entry.clone()));
                    }

                    slf.open_loop();

                    // Condition
                    if let Some(cond) = condition {
                        let (stmts, val) = translator
                            .convert_condition(ctx, true, cond)?
                            .discard_unsafe();
                        let cond_val = translator.ast_context[cond].kind.get_bool();
                        let mut cond_wip = slf.new_wip_block(cond_entry.clone());
                        cond_wip.extend(stmts);
                        slf.add_wip_block(
                            cond_wip,
                            match cond_val {
                                Some(true) => Jump(body_entry.clone()),
                                Some(false) => Jump(next_label.clone()),
                                None => Branch(val, body_entry.clone(), next_label.clone()),
                            },
                        );
                    } else {
                        slf.add_block(cond_entry.clone(), BasicBlock::new_jump(body_entry.clone()));
                    }

                    // Body
                    let saw_unmatched_break = slf.last_per_stmt_mut().saw_unmatched_break;
                    let saw_unmatched_continue = slf.last_per_stmt_mut().saw_unmatched_continue;
                    slf.break_labels.push(next_label.clone());
                    slf.continue_labels.push(incr_entry.clone());

                    let body_stuff =
                        slf.convert_stmt_help(translator, ctx, body, None, body_entry, ret_ty)?;

                    if let Some(body_end) = body_stuff {
                        let wip_body = slf.new_wip_block(body_end);
                        slf.add_wip_block(wip_body, Jump(incr_entry.clone()));
                    }

                    slf.last_per_stmt_mut().saw_unmatched_break = saw_unmatched_break;
                    slf.last_per_stmt_mut().saw_unmatched_continue = saw_unmatched_continue;
                    slf.break_labels.pop();
                    slf.continue_labels.pop();

                    // Increment
                    match increment {
                        None => slf.add_block(incr_entry, BasicBlock::new_jump(cond_entry)),
                        Some(incr) => {
                            let incr_stmts = translator
                                .convert_expr(ctx.unused(), incr, None)?
                                .into_stmts();
                            let mut incr_wip = slf.new_wip_block(incr_entry);
                            incr_wip.extend(incr_stmts);
                            slf.add_wip_block(incr_wip, Jump(cond_entry));
                        }
                    }

                    slf.close_loop();

                    Ok(())
                })?;

                // Return (it is important this happen _outside_ the `with_scope` call)
                Ok(Some(self.new_wip_block(next_label)))
            }

            CStmtKind::Label(sub_stmt) => {
                let label_name = translator
                    .ast_context
                    .label_names
                    .get(&stmt_id)
                    .cloned()
                    .expect("missing name for a label defined in C source");
                let this_label = Label::FromC(stmt_id, Some(label_name));
                self.add_wip_block(wip, Jump(this_label.clone()));
                self.last_per_stmt_mut().c_labels_defined.insert(stmt_id);

                // Sub stmt
                let sub_stmt_next = self.convert_stmt_help(
                    translator,
                    ctx,
                    sub_stmt,
                    in_tail.clone(),
                    this_label,
                    ret_ty,
                )?;
                Ok(sub_stmt_next.map(|l| self.new_wip_block(l)))
            }

            CStmtKind::Attributed { substatement, .. } => {
                // Note: we only support the fallthrough attribute for which no action is
                // required.
                match translator.ast_context.index(substatement).kind {
                    CStmtKind::Empty => Ok(Some(wip)),
                    _ => panic!("Expected empty attributed statement"),
                }
            }

            CStmtKind::Goto(label_id) => {
                let label_name = translator
                    .ast_context
                    .label_names
                    .get(&label_id)
                    .cloned()
                    .expect("missing label name for a Goto label");
                let tgt_label = Label::FromC(label_id, Some(label_name));
                self.add_wip_block(wip, Jump(tgt_label));
                self.last_per_stmt_mut()
                    .c_labels_used
                    .entry(label_id)
                    .or_default()
                    .insert(stmt_id);

                Ok(None)
            }

            CStmtKind::Compound(ref comp_stmts) => {
                let comp_entry = self.fresh_label();
                self.add_wip_block(wip, Jump(comp_entry.clone()));
                let next_lbl = self.convert_stmts_help(
                    translator,
                    ctx,
                    comp_stmts.as_slice(),
                    in_tail.clone(),
                    comp_entry,
                    ret_ty,
                )?;

                Ok(next_lbl.map(|l| self.new_wip_block(l)))
            }

            CStmtKind::Expr(expr) => {
                // This case typically happens in macros from system headers.
                // We simply inline the common statement at this point rather
                // than to try and create new control-flow blocks.
                let blk_or_wip = if let CExprKind::Unary(_, UnOp::Extension, sube, _) =
                    translator.ast_context[expr].kind
                {
                    if let CExprKind::Statements(_, stmtid) = translator.ast_context[sube].kind {
                        let comp_entry = self.fresh_label();
                        self.add_wip_block(wip, Jump(comp_entry.clone()));
                        let next_lbl = self.convert_stmt_help(
                            translator,
                            ctx,
                            stmtid,
                            in_tail.clone(),
                            comp_entry,
                            ret_ty,
                        )?;

                        Ok(next_lbl.map(|l| self.new_wip_block(l)))
                    } else {
                        Err(wip)
                    }
                } else {
                    Err(wip)
                };

                match blk_or_wip {
                    Ok(blk) => Ok(blk),
                    Err(mut wip) => {
                        wip.extend(
                            translator
                                .convert_expr(ctx.unused(), expr, None)?
                                .into_stmts(),
                        );

                        // If we can tell the expression is going to diverge, there is no falling through to
                        // the next block.
                        let next = if translator.ast_context.expr_diverges(expr) {
                            self.add_wip_block(wip, End);
                            None
                        } else {
                            Some(wip)
                        };

                        Ok(next)
                    }
                }
            }

            CStmtKind::Break => {
                self.last_per_stmt_mut().saw_unmatched_break = true;
                let tgt_label = self
                    .break_labels
                    .last()
                    .ok_or_else(|| {
                        format_err!(
                            "Cannot find what to break from in this ({:?}) 'break' statement",
                            stmt_id,
                        )
                    })?
                    .clone();
                self.add_wip_block(wip, Jump(tgt_label));

                Ok(None)
            }

            CStmtKind::Continue => {
                self.last_per_stmt_mut().saw_unmatched_continue = true;
                let tgt_label = self
                    .continue_labels
                    .last()
                    .ok_or_else(|| {
                        format_err!(
                            "Cannot find what to continue from in this ({:?}) 'continue' statement",
                            stmt_id,
                        )
                    })?
                    .clone();
                self.add_wip_block(wip, Jump(tgt_label));

                Ok(None)
            }

            CStmtKind::Case(case_expr, sub_stmt, cie) => {
                self.last_per_stmt_mut().saw_unmatched_case = true;
                let this_label = Label::FromC(stmt_id, None);
                self.add_wip_block(wip, Jump(this_label.clone()));

                // Case
                let resolved = translator.ast_context.resolve_expr(case_expr);
                let branch = match resolved.1 {
                    CExprKind::Literal(..) | CExprKind::ConstantExpr(_, _, Some(_)) => {
                        match translator
                            .convert_expr(ctx.used(), resolved.0, None)?
                            .to_pure_expr()
                        {
                            Some(expr) => match *expr {
                                Expr::Lit(lit) => Some(mk().lit_pat(lit.lit)),
                                Expr::Path(path) => Some(mk().path_pat(path.path, path.qself)),
                                _ => None,
                            },
                            _ => None,
                        }
                    }
                    _ => None,
                };

                let pat = match branch {
                    Some(pat) => pat,
                    None => match cie {
                        ConstIntExpr::U(n) => mk().lit_pat(mk().int_unsuffixed_lit(n)),
                        ConstIntExpr::I(n) => mk().lit_pat(mk().int_unsuffixed_lit(n)),
                    },
                };

                self.switch_expr_cases
                    .last_mut()
                    .ok_or_else(|| {
                        format_err!(
                            "Cannot find the 'switch' wrapping this ({:?}) 'case' statement",
                            stmt_id,
                        )
                    })?
                    .cases
                    .push((pat, this_label.clone()));

                // Sub stmt
                let sub_stmt_next = self.convert_stmt_help(
                    translator,
                    ctx,
                    sub_stmt,
                    in_tail.clone(),
                    this_label,
                    ret_ty,
                )?;
                Ok(sub_stmt_next.map(|l| self.new_wip_block(l)))
            }

            CStmtKind::Default(sub_stmt) => {
                self.last_per_stmt_mut().saw_unmatched_default = true;
                let this_label = Label::FromC(stmt_id, None);
                self.add_wip_block(wip, Jump(this_label.clone()));

                // Default case
                self.switch_expr_cases
                    .last_mut()
                    .expect("'default' outside of 'switch'")
                    .default
                    .get_or_insert(this_label.clone());

                // Sub stmt
                let sub_stmt_next = self.convert_stmt_help(
                    translator,
                    ctx,
                    sub_stmt,
                    in_tail.clone(),
                    this_label,
                    ret_ty,
                )?;
                Ok(sub_stmt_next.map(|l| self.new_wip_block(l)))
            }

            CStmtKind::Switch {
                scrutinee,
                body: switch_body,
            } => {
                let next_label = self.fresh_label();
                let body_label = self.fresh_label();

                // Convert the condition
                let (stmts, val) = translator
                    .convert_expr(ctx.used(), scrutinee, None)?
                    .discard_unsafe();
                wip.extend(stmts);

                let wip_label = wip.label.clone();
                self.add_wip_block(wip, End); // NOTE: the `End` here is temporary and gets updated

                // Body
                let saw_unmatched_break = self.last_per_stmt_mut().saw_unmatched_break;
                let saw_unmatched_case = self.last_per_stmt_mut().saw_unmatched_case;
                let saw_unmatched_default = self.last_per_stmt_mut().saw_unmatched_default;
                self.break_labels.push(next_label.clone());
                self.switch_expr_cases.push(SwitchCases::default());

                let body_stuff = self.convert_stmt_help(
                    translator,
                    ctx,
                    switch_body,
                    in_tail.clone(),
                    body_label,
                    ret_ty,
                )?;
                if let Some(body_end) = body_stuff {
                    let body_wip = self.new_wip_block(body_end);
                    self.add_wip_block(body_wip, Jump(next_label.clone()));
                }

                self.last_per_stmt_mut().saw_unmatched_break = saw_unmatched_break;
                self.last_per_stmt_mut().saw_unmatched_case = saw_unmatched_case;
                self.last_per_stmt_mut().saw_unmatched_default = saw_unmatched_default;
                self.break_labels.pop();
                let switch_case = self
                    .switch_expr_cases
                    .pop()
                    .expect("No 'SwitchCases' to pop");

                let mut cases: Vec<_> = switch_case.cases.clone();
                cases.push((
                    mk().wild_pat(),
                    switch_case.default.unwrap_or_else(|| next_label.clone()),
                ));

                // Add the condition basic block terminator (we need the information built up during
                // the conversion of the body to make the right terminator)
                self.update_terminator(wip_label, Switch { expr: val, cases });

                // Return
                Ok(Some(self.new_wip_block(next_label)))
            }

            CStmtKind::Asm {
                is_volatile,
                ref asm,
                ref inputs,
                ref outputs,
                ref clobbers,
            } => {
                wip.extend(translator.convert_asm(
                    ctx,
                    Span::call_site(),
                    is_volatile,
                    asm,
                    inputs,
                    outputs,
                    clobbers,
                )?);
                Ok(Some(wip))
            }
        };
        let out_wip: Option<WipBlock> = out_wip?; // This statement exists to help type inference...

        let out_end = self.fresh_label();
        let out_wip: Option<WipBlock> = out_wip.map(|w| {
            self.add_wip_block(w, GenTerminator::Jump(out_end.clone()));
            self.new_wip_block(out_end)
        });

        // Is the CFG for this statement self contained so can we reloop it immediately?
        if translator.tcfg.incremental_relooper
            && self
                .per_stmt_stack
                .last()
                .unwrap()
                .is_contained(&self.c_label_to_goto, self.currently_live.last().unwrap())
        {
            self.incrementally_reloop_subgraph(translator, in_tail, entry, out_wip)
        } else {
            let last_per_stmt = self.per_stmt_stack.pop().unwrap();
            self.per_stmt_stack
                .last_mut()
                .unwrap()
                .absorb(last_per_stmt);

            Ok(out_wip.map(|w| {
                let next_lbl = self.fresh_label();
                self.add_wip_block(w, GenTerminator::Jump(next_lbl.clone()));
                next_lbl
            }))
        }
    }

    /// While we are building a control-flow graph, there are times when we can easily tell that the
    /// set of blocks we've just added form a closed subgraph (closed in the sense that there are no
    /// edges point into or out of the subgraph, save for the initial entry point). In these cases,
    /// we can run the subgraph through relooper and replace the subgraph with one basic block
    /// containing the Rust statements relooper came up with.
    ///
    /// This incremental approach is a win for a couple reasons:
    ///
    ///   * we end up running relooper more, but we run it on small CFGs where it performs much better
    ///   * the Rust output is less likely to change drastically if we tweak the C input
    ///
    fn incrementally_reloop_subgraph(
        &mut self,
        translator: &Translation,

        // Are we in tail position (does the function end with this statement)?
        in_tail: Option<ImplicitReturnType>,

        // Entry label
        entry: Label,

        // Exit WIP
        out_wip: Option<WipBlock>,
    ) -> TranslationResult<Option<Label>> {
        // Close off the `wip` using a `break` terminator
        let brk_lbl: Label = self.fresh_label();

        let (tail_expr, use_brk_lbl) = match in_tail {
            Some(ImplicitReturnType::Main) => (
                mk().return_expr(Some(mk().lit_expr(mk().int_lit(0, "")))),
                false,
            ),

            Some(ImplicitReturnType::Void) => (mk().return_expr(None), false),

            _ => (
                mk().break_expr_value(Some(brk_lbl.pretty_print()), None),
                true,
            ),
        };

        let fallthrough_id: Option<Label> = out_wip.map(|mut w| {
            w.push_stmt(mk().semi_stmt(tail_expr.clone()));
            let id = w.label.clone();
            self.add_wip_block(w, GenTerminator::End);
            id
        });

        let last_per_stmt = self.per_stmt_stack.pop().unwrap();
        let stmt_id = last_per_stmt.stmt_id.unwrap_or(CStmtId(0));

        // Make a CFG from the PerStmt.
        let (graph, store, live_in) = last_per_stmt.into_cfg();
        let has_fallthrough: bool = if let Some(fid) = fallthrough_id {
            graph.nodes.contains_key(&fid)
        } else {
            false
        };
        let next_lbl = if has_fallthrough {
            Some(self.fresh_label())
        } else {
            None
        };

        // Run relooper
        let mut stmts = translator.convert_cfg(
            &format!("<substmt_{:?}>", stmt_id),
            graph,
            store,
            live_in,
            false,
        )?;

        let inner_span = stmts.first().map(|stmt| stmt.span());

        // Remove unnecessary break statements. We only need a break statement if we failed to
        // remove the tail expr.
        let need_block = stmts.is_empty()
            || !IncCleanup::new(in_tail, brk_lbl.clone()).remove_tail_expr(&mut stmts);

        if has_fallthrough && need_block && use_brk_lbl {
            translator.use_feature("label_break_value");
            let block_body = mk().block(stmts);
            let block: Box<Expr> = mk().labelled_block_expr(block_body, brk_lbl.pretty_print());
            stmts = vec![mk().expr_stmt(block)]
        }

        let mut flattened_wip = self.new_wip_block(entry);
        // Copy span from removed statement if there was only one.
        if stmts.is_empty() {
            if let Some(span) = inner_span {
                // We move any comments on the high end of the span to the low,
                // because those comments should go before the next node, not after.
                flattened_wip.span = translator
                    .comment_store
                    .borrow_mut()
                    .move_comments_to_begin(span);
            }
        }
        flattened_wip.extend(stmts);
        let term = if let Some(l) = next_lbl.clone() {
            GenTerminator::Jump(l)
        } else {
            GenTerminator::End
        };
        self.add_wip_block(flattened_wip, term);

        Ok(next_lbl)
    }
}

/// This impl block deals with pretty-printing control flow graphs into a format that `dot` can
/// consume. Compiling these files into images means running something like:
///
/// ```sh
/// dot -Tpng cfg_func.dot > cfg_func.png
/// ```
impl Cfg<Label, StmtOrDecl> {
    /// Write out a JSON representation of the control flow graph
    pub fn dump_json_graph(&self, store: &DeclStmtStore, file_path: String) -> io::Result<()> {
        let cfg_mapped = self.map_stmts(|sd: &StmtOrDecl| -> Vec<String> { sd.to_string(store) });

        let file = File::create(file_path)?;
        serde_json::to_writer(file, &cfg_mapped)?;

        Ok(())
    }

    /// Write out a `.dot` representation of the control flow graph
    pub fn dump_dot_graph(
        &self,
        ctx: &TypedAstContext,
        store: &DeclStmtStore,
        show_liveness: bool,
        show_loops: bool,
        file_path: String,
    ) -> io::Result<()> {
        // Utility function for sanitizing strings
        fn sanitize_label(lbl: String) -> String {
            format!(
                "{}\\l",
                lbl.replace('\t', "  ")
                    .replace('\\', "\\\\")
                    .replace('\"', "\\\"")
                    .replace('\n', "\\l")
            )
        }

        let mut file = File::create(file_path)?;
        file.write_all(b"digraph cfg {\n")?;
        file.write_all(b"  node [shape=box,fontname=Courier];\n")?;
        file.write_all(b"  edge [fontname=Courier,fontsize=10.0];\n")?;

        // Entry
        file.write_all(b"  entry [shape=plaintext];\n")?;
        file.write_fmt(format_args!("  entry -> {};\n", self.entries.debug_print()))?;

        // Rest of graph
        for (lbl, bb) in self.nodes.iter() {
            let pretty_terminator = match bb.terminator {
                End | Jump(_) => String::from(""),
                Branch(ref cond, _, _) => format!("\n{}", pprust::expr_to_string(cond.deref())),
                Switch { ref expr, .. } => format!("\n{}", pprust::expr_to_string(expr.deref())),
            };

            let defined = if bb.defined.is_empty() {
                "".into()
            } else {
                format!(
                    "\\ldefined: {{{}}}",
                    bb.defined
                        .iter()
                        .filter_map(|decl| ctx.index(*decl).kind.get_name())
                        .cloned()
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            };

            let live = if bb.live.is_empty() {
                "".into()
            } else {
                format!(
                    "\\llive in: {{{}}}",
                    bb.live
                        .iter()
                        .filter_map(|decl| ctx.index(*decl).kind.get_name())
                        .cloned()
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            };

            //  Scope the node with the loops it is part of
            let mut closing_braces = 0;
            if show_loops {
                file.write_all(b"  ")?;

                let loop_ids: Vec<LoopId> = self.loops.enclosing_loops(lbl);

                closing_braces = loop_ids.len();
                for loop_id in loop_ids.iter().rev() {
                    file.write_fmt(format_args!(
                        "subgraph cluster_{} {{ label = \"{}\"; graph[style=dotted];",
                        loop_id.pretty_print(),
                        loop_id.pretty_print(),
                    ))?;
                }
            }

            // A node
            writeln!(
                file,
                "  {} [label=\"{}:\\l-----{}{}\\l{}-----{}\"];",
                lbl.debug_print(),
                lbl.debug_print(),
                if show_liveness { live } else { String::new() },
                if show_liveness {
                    defined
                } else {
                    String::new()
                },
                format_args!(
                    "-----\\l{}",
                    if bb.body.is_empty() {
                        String::from("")
                    } else {
                        sanitize_label(
                            bb.body
                                .iter()
                                .flat_map(|sd: &StmtOrDecl| -> Vec<String> { sd.to_string(store) })
                                .collect::<Vec<String>>()
                                .join("\n"),
                        )
                    }
                ),
                sanitize_label(pretty_terminator),
            )?;

            //  Close the loops the node is part of
            for _ in 0..closing_braces {
                file.write_all(b"  }")?;
            }
            if closing_braces > 0 {
                file.write_all(b"\n")?;
            }

            // All the edges starting from this node
            let edges: Vec<(String, Label)> = match bb.terminator.clone() {
                End => vec![],
                Jump(tgt) => vec![(String::from(""), tgt)],
                Branch(_, tru, fal) => {
                    vec![(String::from("true"), tru), (String::from("false"), fal)]
                }
                Switch { ref cases, .. } => {
                    let cases: Vec<(String, Label)> = cases
                        .iter()
                        .map(|(pat, tgt)| -> (String, Label) {
                            let pat: String = pprust::pat_to_string(pat);

                            (pat, tgt.clone())
                        })
                        .collect();
                    cases
                }
            };

            for (desc, tgt) in edges {
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
