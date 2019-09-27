//! Command management and overall refactoring state.

use rustc::hir;
use rustc::hir::def_id::LOCAL_CRATE;
use rustc::session::{self, DiagnosticOutput, Session};
use rustc_data_structures::sync::Lrc;
use rustc_interface::interface;
use rustc_interface::util;
use rustc_lint;
use rustc_metadata::cstore::CStore;
use std::cell::{self, Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::iter;
use std::io::Write;
use std::mem;
use std::ops::Deref;
use std::process;
use std::sync::Arc;
use syntax::ast::{Crate, NodeId, CRATE_NODE_ID};
use syntax::ast::{Expr, Item, Pat, Stmt, Ty};
use syntax::ext::base::NamedSyntaxExtension;
use syntax::feature_gate::AttributeType;
use syntax::parse::lexer::comments::{gather_comments, Comment};
use syntax::ptr::P;
use syntax::source_map::SourceMap;
use syntax::symbol::Symbol;
use syntax::visit::Visitor;

use crate::ast_manip::map_ast_into;
use crate::ast_manip::number_nodes::{
    number_nodes, number_nodes_with, reset_node_ids, NodeIdCounter,
};
use crate::ast_manip::{remove_paren, ListNodeIds, MutVisit, Visit};
use crate::ast_manip::{collect_comments, CommentMap};
use crate::collapse::CollapseInfo;
use crate::driver::{self, Phase};
use crate::file_io::FileIO;
use crate::node_map::NodeMap;
use crate::rewrite;
use crate::rewrite::files;
use crate::span_fix;
use crate::RefactorCtxt;
use c2rust_ast_builder::IntoSymbol;

/// Extra nodes that were parsed from strings while running a transformation pass.  During
/// rewriting, we'd like to reuse the original strings for these, rather than pretty-printing them.
///
/// For node_map and rewriting purposes, these nodes are treated as if they were part of the
/// original input AST.  We add identity entries to the node_map upon parsing them, just like we do
/// for the initial crate, and we place them in the "old nodes" table during rewriting.
#[derive(Clone, Default, Debug)]
struct ParsedNodes {
    exprs: Vec<P<Expr>>,
    pats: Vec<P<Pat>>,
    tys: Vec<P<Ty>>,
    stmts: Vec<Stmt>,
    items: Vec<P<Item>>,
}

impl Visit for ParsedNodes {
    fn visit<'ast, V: Visitor<'ast>>(&'ast self, v: &mut V) {
        self.exprs.iter().for_each(|x| (&**x).visit(v));
        self.pats.iter().for_each(|x| (&**x).visit(v));
        self.tys.iter().for_each(|x| (&**x).visit(v));
        self.stmts.iter().for_each(|x| x.visit(v));
        self.items.iter().for_each(|x| (&**x).visit(v));
    }
}

#[derive(Default)]
struct PluginInfo {
    _syntax_exts: Vec<NamedSyntaxExtension>,
    _attributes: Vec<(String, AttributeType)>,
}

/// Stores the overall state of the refactoring process, which can be read and updated by
/// `Command`s.
pub struct RefactorState {
    config: interface::Config,
    compiler: interface::Compiler,
    cmd_reg: Registry,
    file_io: Arc<dyn FileIO + Sync + Send>,

    /// The original crate AST.  This is used as the "old" AST when rewriting.  This is always a
    /// real unexpanded AST, as it was loaded from disk, with full user-provided source text.
    orig_krate: Crate,

    /// Original comments from the parsed crate
    comment_map: CommentMap,

    /// Mapping from `krate` IDs to `disk_krate` IDs
    node_map: NodeMap,

    /// Commands run so far
    commands: Vec<String>,

    /// Mutable state available to a driver command
    cs: CommandState,
}

#[cfg_attr(feature = "profile", flame)]
fn parse_crate(compiler: &interface::Compiler) -> Crate {
    let mut krate = compiler.parse().unwrap().take();
    remove_paren(&mut krate);
    number_nodes(&mut krate);
    krate
}

#[cfg_attr(feature = "profile", flame)]
fn parse_extras(compiler: &interface::Compiler) -> Vec<Comment> {
    let mut comments = vec![];
    for file in compiler.source_map().files().iter() {
        if let Some(src) = &file.src {
            let mut new_comments = gather_comments(
                &compiler.session().parse_sess,
                file.name.clone(),
                src.deref().clone(),
            );

            // gather_comments_and_literals starts positions at 0 each time, so
            // we need to adjust by the file offset
            for c in &mut new_comments {
                c.pos = c.pos + file.start_pos;
            }
            comments.append(&mut new_comments);
        }
    }

    comments
}

pub const FRESH_NODE_ID_START: u32 = 0x8000_0000;

impl RefactorState {
    #[cfg_attr(feature = "profile", flame)]
    pub fn new(
        config: interface::Config,
        cmd_reg: Registry,
        file_io: Arc<dyn FileIO + Sync + Send>,
        marks: HashSet<(NodeId, Symbol)>,
    ) -> RefactorState {
        let compiler = driver::make_compiler(&config, file_io.clone());
        let krate = parse_crate(&compiler);
        let comments = parse_extras(&compiler);
        let comment_map = collect_comments(&krate, &comments);
        let orig_krate = krate.clone();
        let (node_map, cs) = Self::init(krate, Some(marks));
        RefactorState {
            config,
            compiler,
            cmd_reg,
            file_io,

            orig_krate,
            comment_map,

            node_map,

            commands: vec![],

            cs,
        }
    }

    /// Initialization shared between new() and load_crate()
    #[cfg_attr(feature = "profile", flame)]
    fn init(krate: Crate, marks: Option<HashSet<(NodeId, Symbol)>>) -> (NodeMap, CommandState) {
        // (Re)initialize `node_map` and `marks`.
        let mut node_map = NodeMap::new();
        node_map.init(krate.list_node_ids().into_iter());
        // Special case: CRATE_NODE_ID doesn't actually appear anywhere in the AST.
        node_map.init(iter::once(CRATE_NODE_ID));
        let marks = marks.unwrap_or_else(HashSet::new);

        // The newly loaded `krate` and reinitialized `node_map` reference none of the old
        // `parsed_nodes`.  That means we can reset the ID counter without risk of ID collisions.
        let parsed_nodes = ParsedNodes::default();
        let node_id_counter = NodeIdCounter::new(FRESH_NODE_ID_START);

        let cs = CommandState::new(krate, Phase::Phase1, marks, parsed_nodes, node_id_counter);

        (node_map, cs)
    }

    pub fn session(&self) -> &Session {
        self.compiler.session()
    }

    pub fn source_map(&self) -> &SourceMap {
        self.compiler.source_map()
    }

    pub fn drain_commands(&mut self) -> Vec<String> {
        mem::replace(&mut self.commands, vec![])
    }

    /// Load the crate from disk.  This also resets a bunch of internal state, since we won't be
    /// rewriting with the previous `orig_crate` any more.
    #[cfg_attr(feature = "profile", flame)]
    pub fn load_crate(&mut self) {
        self.compiler = driver::make_compiler(&self.config, self.file_io.clone());
        let krate = parse_crate(&self.compiler);
        let comments = parse_extras(&self.compiler);
        let comment_map = collect_comments(&krate, &comments);
        self.orig_krate = krate.clone();
        self.comment_map = comment_map;
        let (node_map, cs) = Self::init(krate, None);
        self.node_map = node_map;
        self.cs = cs;
    }

    /// Save the crate to disk, by writing out the new source text produced by rewriting.
    ///
    /// Note that we allow multiple calls to `save_crate` with no intervening `load_crate`.  The
    /// later `save_crate`s will simply keep using the original source text (even if it no longer
    /// matches the text on disk) as the basis for rewriting.
    #[cfg_attr(feature = "profile", flame)]
    pub fn save_crate(&mut self) {
        let old = &self.orig_krate;
        let new = &self.cs.krate();
        let node_id_map = self.node_map.clone().into_inner();

        self.file_io
            .save_marks(
                new,
                self.session().source_map(),
                &node_id_map,
                &self.cs.marks(),
            )
            .unwrap();

        let parsed_nodes = self.cs.parsed_nodes.borrow();
        let rw = rewrite::rewrite(self.session(), old, new, &self.comment_map, node_id_map, |map| {
            map_ast_into(&*parsed_nodes, map);
        });
        // Note that `rewrite_files_with` does not read any files from disk - it uses the
        // `SourceMap` to get files' original source text.
        files::rewrite_files_with(self.source_map(), &rw, &*self.file_io).unwrap();
    }

    #[cfg_attr(feature = "profile", flame)]
    pub fn transform_crate<F, R>(&mut self, phase: Phase, f: F) -> interface::Result<R>
    where
        F: FnOnce(&CommandState, &RefactorCtxt) -> R,
    {
        // let mut krate = mem::replace(&mut self.krate, dummy_crate());
        // let marks = mem::replace(&mut self.marks, HashSet::new());

        let unexpanded = self.cs.krate().clone();

        self.cs.reset();

        self.rebuild_session();

        // Immediately fix up the attr spans, since during expansion, any
        // `derive` attrs will be removed.
        span_fix::fix_attr_spans(self.cs.krate.get_mut());

        // Replace current parse query results
        profile_start!("Replace compiler crate");
        let parse = self.compiler.parse()?;
        let _ = parse.take();
        parse.give(self.cs.krate().clone());
        profile_end!("Replace compiler crate");

        match phase {
            Phase::Phase1 => {}

            Phase::Phase2 | Phase::Phase3 => {
                profile_start!("Expand crate");
                self.cs
                    .krate
                    .replace(self.compiler.expansion()?.peek().0.clone());
                profile_end!("Expand crate");
                remove_paren(self.cs.krate.get_mut());
            }
        }

        self.cs.phase = phase;

        span_fix::fix_format(self.cs.krate.get_mut());
        let expanded = self.cs.krate().clone();
        let collapse_info = match phase {
            Phase::Phase1 => None,
            Phase::Phase2 | Phase::Phase3 => {
                Some(CollapseInfo::collect(&unexpanded, &expanded, &mut self.node_map, &self.cs))
            }
        };

        // Run the transform
        let r = match phase {
            Phase::Phase1 => {
                let cx =
                    RefactorCtxt::new_phase_1(&self.compiler.session(), &self.compiler.cstore());

                f(&self.cs, &cx)
            }

            Phase::Phase2 => {
                profile_start!("Lower to HIR");
                let hir = self.compiler.lower_to_hir()?.take();
                let (ref hir_forest, ref expansion) = hir;
                let hir_forest = hir_forest.borrow();
                let defs = expansion.defs.borrow();
                let map = hir::map::map_crate(
                    self.compiler.session(),
                    &*self.compiler.cstore().clone(),
                    &hir_forest,
                    &defs,
                );
                profile_end!("Lower to HIR");

                let cx = RefactorCtxt::new_phase_2(
                    self.compiler.session(),
                    self.compiler.cstore(),
                    &map,
                );

                f(&self.cs, &cx)
            }

            Phase::Phase3 => {
                profile_start!("Compiler Phase 3");
                let r = self.compiler.global_ctxt()?.take().enter(|tcx| {
                    let _result = tcx.analysis(LOCAL_CRATE);
                    let cx = RefactorCtxt::new_phase_3(
                        self.compiler.session(),
                        self.compiler.cstore(),
                        tcx.hir(),
                        tcx,
                    );
                    profile_end!("Compiler Phase 3");

                    f(&self.cs, &cx)
                });

                // Ensure that we've dropped any copies of the session Lrc
                let _ = self.compiler.lower_to_hir()?.take();

                r
            }
        };

        self.node_map
            .init(self.cs.new_parsed_node_ids.get_mut().drain(..));

        if let Some(collapse_info) = collapse_info {
            collapse_info.collapse(&mut self.node_map, &self.cs);
        }

        for (node, comment) in self.cs.new_comments.get_mut().drain(..) {
            if let Some(node) = self.node_map.get(&node) {
                self.comment_map.insert(*node, comment);
            } else {
                warn!("Could not create comment: {:?}", comment.lines);
            }
        }

        Ok(r)
    }

    #[cfg_attr(feature = "profile", flame)]
    fn rebuild_session(&mut self) {
        // Ensure we've take the expansion result if we're in phase 2 or 3 since
        // we need later queries to rebuild it.
        match self.cs.phase {
            Phase::Phase1 => {}
            Phase::Phase2 | Phase::Phase3 => {
                let _ = self.compiler.expansion().unwrap().take();
            }
        }

        let compiler: &mut driver::Compiler = unsafe { mem::transmute(&mut self.compiler) };
        let old_session = &compiler.sess;

        let descriptions = util::diagnostics_registry();
        let mut new_sess = session::build_session_with_source_map(
            old_session.opts.clone(),
            old_session.local_crate_source_file.clone(),
            descriptions,
            self.compiler.source_map().clone(),
            DiagnosticOutput::Default,
            Default::default(),
        );
        let new_codegen_backend = util::get_codegen_backend(&new_sess);
        let new_cstore = CStore::new(new_codegen_backend.metadata_loader());

        rustc_lint::register_builtins(&mut new_sess.lint_store.borrow_mut(), Some(&new_sess));
        if new_sess.unstable_options() {
            rustc_lint::register_internals(&mut new_sess.lint_store.borrow_mut(), Some(&new_sess));
        }

        new_sess.parse_sess.config = old_session.parse_sess.config.clone();

        *Lrc::get_mut(&mut compiler.sess).unwrap() = new_sess;
        *Lrc::get_mut(&mut compiler.codegen_backend).unwrap() = new_codegen_backend;
        *Lrc::get_mut(&mut compiler.cstore).unwrap() = new_cstore;
    }

    #[cfg_attr(feature = "profile", flame)]
    pub fn run_typeck_loop<F>(&mut self, mut func: F) -> Result<(), &'static str>
    where
        F: FnMut(&mut Crate, &CommandState, &RefactorCtxt) -> TypeckLoopResult,
    {
        let func = &mut func;

        let mut result = None;
        while result.is_none() {
            self.transform_crate(Phase::Phase3, |st, cx| {
                match func(&mut st.krate_mut(), st, cx) {
                    TypeckLoopResult::Iterate => {}
                    TypeckLoopResult::Err(e) => {
                        result = Some(Err(e));
                    }
                    TypeckLoopResult::Finished => {
                        result = Some(Ok(()));
                    }
                }
            })
            .expect("Failed to run compiler");
        }
        result.unwrap()
    }

    pub fn clear_marks(&mut self) {
        self.cs.marks.get_mut().clear()
    }

    /// Invoke a registered command with the given command name and arguments.
    #[cfg_attr(feature = "profile", flame)]
    pub fn run<S: AsRef<str>>(&mut self, cmd_name: &str, args: &[S]) -> Result<(), String> {
        let args = args
            .iter()
            .map(|s| s.as_ref().to_owned())
            .collect::<Vec<_>>();
        info!("running command: {} {:?}", cmd_name, args);
        self.commands.push(args.iter().fold(cmd_name.to_string(), |mut s, arg| {
            s.push_str(arg);
            s
        }));

        let mut cmd = self.cmd_reg.get_command(cmd_name, &args)?;
        profile_start!(format!("Command {}", cmd_name));
        cmd.run(self);
        profile_end!(format!("Command {}", cmd_name));
        Ok(())
    }

    pub fn marks(&self) -> cell::Ref<HashSet<(NodeId, Symbol)>> {
        self.cs.marks.borrow()
    }

    pub fn marks_mut(&mut self) -> cell::RefMut<HashSet<(NodeId, Symbol)>> {
        self.cs.marks.borrow_mut()
    }
}

pub enum TypeckLoopResult {
    Iterate,
    Err(&'static str),
    Finished,
}

/// Mutable state that can be modified by a "driver" command.  This is normally paired with a
/// `RefactorCtxt`, which contains immutable analysis results from the original input `Crate`.
pub struct CommandState {
    parsed_nodes: RefCell<ParsedNodes>,
    /// Counter for assigning fresh `NodeId`s to newly parsed nodes (among others).
    ///
    /// It's important that this counter is preserved across `transform_crate` calls.  Parsed
    /// nodes' IDs stick around after the originating `transform_crate` ends: they remain in
    /// `parsed_nodes`, and they can be referenced by `node_map` as "old" IDs.  Preserving this
    /// counter ensures that every parsed node has a distinct `NodeId`.
    node_id_counter: NodeIdCounter,

    /// The current crate AST.  This is used as the "new" AST when rewriting.
    /// This is always starts "unexpanded" - meaning either actually unexpanded,
    /// or expanded and then subsequently macro-collapsed.
    krate: RefCell<Crate>,

    /// The current compiler phase of the crate.
    phase: Phase,

    /// Current marks.  The `NodeId`s here refer to nodes in `krate`.
    marks: RefCell<HashSet<(NodeId, Symbol)>>,

    // krate: RefCell<Crate>,
    // marks: RefCell<HashSet<(NodeId, Symbol)>>,
    // parsed_nodes: RefCell<ParsedNodes>,
    new_parsed_node_ids: RefCell<Vec<NodeId>>,

    new_comments: RefCell<Vec<(NodeId, Comment)>>,

    krate_changed: Cell<bool>,
    marks_changed: Cell<bool>,
}

impl CommandState {
    fn new(
        krate: Crate,
        phase: Phase,
        marks: HashSet<(NodeId, Symbol)>,
        parsed_nodes: ParsedNodes,
        node_id_counter: NodeIdCounter,
    ) -> CommandState {
        CommandState {
            krate: RefCell::new(krate),
            phase,
            marks: RefCell::new(marks),
            parsed_nodes: RefCell::new(parsed_nodes),
            new_parsed_node_ids: RefCell::new(Vec::new()),
            new_comments: RefCell::new(Vec::new()),

            krate_changed: Cell::new(false),
            marks_changed: Cell::new(false),

            node_id_counter,
        }
    }

    /// Reset the command state in preparation for a new transform iteration
    fn reset(&mut self) {
        reset_node_ids(self.krate.get_mut());

        self.new_parsed_node_ids.get_mut().clear();
        self.new_comments.get_mut().clear();
        self.krate_changed.set(false);
        self.marks_changed.set(false);
    }

    pub fn krate(&self) -> cell::Ref<Crate> {
        self.krate.borrow()
    }

    pub fn krate_mut(&self) -> cell::RefMut<Crate> {
        self.krate_changed.set(true);
        self.krate.borrow_mut()
    }

    pub fn map_krate<R, F: FnOnce(&mut Crate) -> R>(&self, func: F) -> R {
        func(&mut self.krate_mut())
    }

    pub fn krate_changed(&self) -> bool {
        self.krate_changed.get()
    }

    pub fn add_comment(&self, node: NodeId, comment: Comment) {
        self.new_comments.borrow_mut().push((node, comment));
    }

    pub fn marks(&self) -> cell::Ref<HashSet<(NodeId, Symbol)>> {
        self.marks.borrow()
    }

    pub fn marks_mut(&self) -> cell::RefMut<HashSet<(NodeId, Symbol)>> {
        self.marks_changed.set(true);
        self.marks.borrow_mut()
    }

    pub fn marked<S: IntoSymbol>(&self, id: NodeId, label: S) -> bool {
        self.marks().contains(&(id, label.into_symbol()))
    }

    pub fn add_mark<S: IntoSymbol>(&self, id: NodeId, label: S) {
        self.marks_mut().insert((id, label.into_symbol()));
    }

    pub fn remove_mark<S: IntoSymbol>(&self, id: NodeId, label: S) {
        self.marks_mut().remove(&(id, label.into_symbol()));
    }

    pub fn marks_changed(&self) -> bool {
        self.marks_changed.get()
    }

    pub fn node_id_counter(&self) -> &NodeIdCounter {
        &self.node_id_counter
    }

    /// Generate a fresh NodeId.
    pub fn next_node_id(&self) -> NodeId {
        self.node_id_counter.next()
    }

    /// Transfer marks on `old` to a fresh NodeId, and return that fresh NodeId.
    pub fn transfer_marks(&self, old: NodeId) -> NodeId {
        let new = self.next_node_id();

        let mut marks = self.marks_mut();
        let labels = marks
            .iter()
            .filter(|x| x.0 == old)
            .map(|x| x.1)
            .collect::<Vec<_>>();
        for label in labels {
            marks.remove(&(old, label));
            marks.insert((new, label));
        }

        new
    }

    fn process_parsed<T>(&self, x: &mut T)
    where
        T: MutVisit + ListNodeIds,
    {
        number_nodes_with(x, &self.node_id_counter);
        self.new_parsed_node_ids
            .borrow_mut()
            .extend(x.list_node_ids());
    }

    /// Parse an `Expr`, keeping the original `src` around for use during rewriting.
    pub fn parse_expr(&self, cx: &RefactorCtxt, src: &str) -> P<Expr> {
        let mut e = driver::parse_expr(cx.session(), src);
        self.process_parsed(&mut e);
        self.parsed_nodes.borrow_mut().exprs.push(e.clone());
        e
    }

    pub fn parse_items(&self, cx: &RefactorCtxt, src: &str) -> Vec<P<Item>> {
        let mut is = driver::parse_items(cx.session(), src);
        for i in &mut is {
            self.process_parsed(i);
            self.parsed_nodes.borrow_mut().items.push(i.clone());
        }
        is
    }

    // TODO: similar methods for other node types
    // TODO: check that parsed_node reuse works for expr and other non-seqitems

    pub fn into_inner(self) -> (Crate, HashSet<(NodeId, Symbol)>) {
        (self.krate.into_inner(), self.marks.into_inner())
    }
}

/// Implementation of a refactoring command.
pub trait Command {
    fn run(&mut self, state: &mut RefactorState);
}

/// A command builder is a function that takes some string arguments and produces a `Command`.
pub type Builder = dyn FnMut(&[String]) -> Box<dyn Command> + Send;

/// Tracks known refactoring command builders, and allows invoking them by name.
pub struct Registry {
    commands: HashMap<String, Box<Builder>>,
}

impl Registry {
    pub fn new() -> Registry {
        Registry {
            commands: HashMap::new(),
        }
    }

    pub fn register<B>(&mut self, name: &str, builder: B)
    where
        B: FnMut(&[String]) -> Box<dyn Command> + 'static + Send,
    {
        self.commands.insert(name.to_owned(), Box::new(builder));
    }

    pub fn get_command(&mut self, name: &str, args: &[String]) -> Result<Box<dyn Command>, String> {
        let builder = match self.commands.get_mut(name) {
            Some(command) => command,
            None => return Err(format!("Invalid command: {:#?}", name)),
        };
        Ok(builder(args))
    }
}

/// Wraps a `FnMut` to produce a `Command`.
pub struct FuncCommand<F>(pub F);

impl<F> Command for FuncCommand<F>
where
    F: FnMut(&mut RefactorState),
{
    fn run(&mut self, state: &mut RefactorState) {
        (self.0)(state);
    }
}

/// Wrap a `FnMut` to produce a command that invokes the `rustc` driver and operates over the
/// results.
pub struct DriverCommand<F>
where
    F: FnMut(&CommandState, &RefactorCtxt),
{
    func: F,
    phase: Phase,
}

impl<F> DriverCommand<F>
where
    F: FnMut(&CommandState, &RefactorCtxt),
{
    pub fn new(phase: Phase, func: F) -> DriverCommand<F> {
        DriverCommand { func, phase }
    }
}

impl<F> Command for DriverCommand<F>
where
    F: FnMut(&CommandState, &RefactorCtxt),
{
    fn run(&mut self, state: &mut RefactorState) {
        state
            .transform_crate(self.phase, |st, cx| (self.func)(st, cx))
            .expect("Failed to run compiler");
    }
}

/// # `commit` Command
///
/// Usage: `commit`
///
/// Write the current crate to disk (by rewriting the original source files), then
/// read it back in, clearing all mark.  This can be useful as a "checkpoint"
/// between two sets of transformations, if applying both sets of changes at once
/// proves to be too much for the rewriter.
///
/// This is only useful when the rewrite mode is `inplace`.  Otherwise the "write"
/// part of the operation won't actually change the original source files, and the
/// "read" part will revert the crate to its original form.
fn register_commit(reg: &mut Registry) {
    reg.register("commit", |args| {
        let git_commit = match args.get(0) {
            Some(arg) if arg == "git" => true,
            _ => false,
        };
        Box::new(FuncCommand(move |rs: &mut RefactorState| {
            let clean = if git_commit {
                let result = process::Command::new("git")
                    .arg("status")
                    .arg("--porcelain")
                    .arg("--ignore-submodules=dirty")
                    .output()
                    .expect("Could not get git status");
                result.stdout.is_empty() && result.stderr.is_empty()
            } else {
                false
            };

            rs.save_crate();

            let mut commands = rs.drain_commands();
            let _ = commands.pop(); // remove commit command
            if git_commit && !commands.is_empty() {
                let commit_msg = format!(
                    "refactor {} {}",
                    rs.config.input_path.as_ref().map_or(String::new(), |s| s.display().to_string()),
                    commands.join("\n"),
                );
                if !clean {
                    warn!("Working tree is dirty, not committing");
                } else {
                    let mut git = process::Command::new("git")
                        .arg("commit")
                        .arg("--all")
                        .arg("--file=-") // read commit message from stdin
                        .stdin(process::Stdio::piped())
                        .spawn()
                        .expect("Could not execute git commit");
                    git
                        .stdin
                        .as_mut()
                        .expect("failed to open stdin")
                        .write_all(commit_msg.as_bytes())
                        .expect("could not write commit message");
                    let status = git.wait().expect("Git did not terminate successfully");
                    if !status.success() {
                        warn!("Git commit was unsuccessful");
                    }
                }
            }

            rs.load_crate();
            rs.clear_marks();
        }))
    });

    reg.register("write", |_args| {
        Box::new(FuncCommand(|rs: &mut RefactorState| {
            rs.save_crate();
        }))
    });

    reg.register("dump_crate", |_args| {
        Box::new(FuncCommand(|rs: &mut RefactorState| {
            eprintln!("{:#?}", rs.cs.krate());
        }))
    });
}

pub fn register_commands(reg: &mut Registry) {
    register_commit(reg);
}
