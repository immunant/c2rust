//! Command management and overall refactoring state.

use std::cell::{self, Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::iter;
use std::mem;
use std::sync::Arc;
use rustc::session::Session;
use syntax::ast::{NodeId, Crate, CRATE_NODE_ID, Mod};
use syntax::ast::{Expr, Pat, Ty, Stmt, Item};
use syntax::source_map::DUMMY_SP;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::visit::Visitor;

use crate::ast_manip::{ListNodeIds, remove_paren, Visit, MutVisit};
use crate::ast_manip::ast_map::map_ast_into;
use crate::ast_manip::number_nodes::{number_nodes, number_nodes_with, NodeIdCounter, reset_node_ids};
use crate::collapse;
use crate::driver::{self, Phase, Phase1Bits};
use crate::file_io::{FileIO, ArcFileIO};
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


/// Stores the overall state of the refactoring process, which can be read and updated by
/// `Command`s.
pub struct RefactorState {
    file_io: Arc<FileIO+Sync+Send>,
    cmd_reg: Registry,
    session: Session,

    parsed_nodes: ParsedNodes,
    /// Counter for assigning fresh `NodeId`s to newly parsed nodes (among others).
    ///
    /// It's important that this counter is preserved across `transform_crate` calls.  Parsed
    /// nodes' IDs stick around after the originating `transform_crate` ends: they remain in
    /// `parsed_nodes`, and they can be referenced by `node_map` as "old" IDs.  Preserving this
    /// counter ensures that every parsed node has a distinct `NodeId`.
    node_id_counter: NodeIdCounter,

    /// The current crate AST.  This is used as the "new" AST when rewriting.  This is always
    /// "unexpanded" - meaning either actually unexpanded, or expanded and then subsequently
    /// macro-collapsed.
    krate: Crate,

    /// The original crate AST.  This is used as the "old" AST when rewriting.  This is always a
    /// real unexpanded AST, as it was loaded from disk, with full user-provided source text.
    orig_krate: Crate,

    /// Mapping from `krate` IDs to `disk_krate` IDs
    node_map: NodeMap,

    /// Current marks.  The `NodeId`s here refer to nodes in `krate`.
    marks: HashSet<(NodeId, Symbol)>,
}

fn dummy_crate() -> Crate {
    Crate {
        module: Mod {
            inner: DUMMY_SP,
            items: vec![],
            inline: false,
        },
        attrs: vec![],
        span: DUMMY_SP,
    }
}

impl RefactorState {
    pub fn new(session: Session,
               cmd_reg: Registry,
               file_io: Arc<FileIO+Sync+Send>,
               marks: HashSet<(NodeId, Symbol)>) -> RefactorState {
        let mut session = session;
        // Force disable incremental compilation.  It causes panics with multiple typechecking.
        session.opts.incremental = None;

        RefactorState {
            file_io,
            cmd_reg,
            session,

            parsed_nodes: ParsedNodes::default(),
            node_id_counter: NodeIdCounter::new(0x8000_0000),

            krate: dummy_crate(),
            orig_krate: dummy_crate(),

            node_map: NodeMap::new(),

            marks: marks,
        }
    }

    pub fn from_rustc_args(rustc_args: &[String],
                           cmd_reg: Registry,
                           file_io: Arc<FileIO+Sync+Send>,
                           marks: HashSet<(NodeId, Symbol)>) -> RefactorState {
        let session = driver::build_session_from_args(
            rustc_args, Some(Box::new(ArcFileIO(file_io.clone()))));
        Self::new(session, cmd_reg, file_io, marks)
    }


    pub fn session(&self) -> &Session {
        &self.session
    }


    fn load_crate_inner(&self) -> Crate {
        let bits = Phase1Bits::from_session_reparse(&self.session);
        bits.into_crate()
    }

    /// Load the crate from disk.  This also resets a bunch of internal state, since we won't be
    /// rewriting with the previous `orig_crate` any more.
    pub fn load_crate(&mut self) {
        // Discard any existing krate, overwriting it with one loaded from disk.
        let mut krate = self.load_crate_inner();
        remove_paren(&mut krate);
        number_nodes(&mut krate);
        self.orig_krate = krate.clone();
        self.krate = krate;

        // Re-initialize `node_map` and `marks`.
        self.node_map = NodeMap::new();
        self.node_map.init(self.krate.list_node_ids().into_iter());
        // Special case: CRATE_NODE_ID doesn't actually appear anywhere in the AST.
        self.node_map.init(iter::once(CRATE_NODE_ID));
        self.marks = HashSet::new();

        // The newly loaded `krate` and reinitialized `node_map` reference none of the old
        // `parsed_nodes`.  That means we can reset the ID counter without risk of ID collisions.
        self.parsed_nodes = ParsedNodes::default();
        self.node_id_counter = NodeIdCounter::new(0x8000_0000);
    }

    /// Save the crate to disk, by writing out the new source text produced by rewriting.
    ///
    /// Note that we allow multiple calls to `save_crate` with no intervening `load_crate`.  The
    /// later `save_crate`s will simply keep using the original source text (even if it no longer
    /// matches the text on disk) as the basis for rewriting.
    pub fn save_crate(&mut self) {
        let old = &self.orig_krate;
        let new = &self.krate;
        let node_id_map = self.node_map.clone().into_inner();

        self.file_io.save_marks(
            new, self.session.source_map(), &node_id_map, &self.marks).unwrap();

        let rw = rewrite::rewrite(&self.session, old, new, node_id_map, |map| {
            map_ast_into(&self.parsed_nodes, map);
        });
        // Note that `rewrite_files_with` does not read any files from disk - it uses the
        // `SourceMap` to get files' original source text.
        files::rewrite_files_with(self.session.source_map(), &rw, &*self.file_io).unwrap();
    }

    pub fn transform_crate<F, R>(&mut self, phase: Phase, f: F) -> R
            where F: FnOnce(&CommandState, &RefactorCtxt) -> R {
        let mut krate = mem::replace(&mut self.krate, dummy_crate());
        let marks = mem::replace(&mut self.marks, HashSet::new());

        let unexpanded = krate.clone();
        reset_node_ids(&mut krate);
        let bits = Phase1Bits::from_session_and_crate(&self.session, krate);
        driver::run_compiler_from_phase1(bits, phase, |mut krate, cx| {
            span_fix::fix_format(&mut krate);
            let expanded = krate.clone();

            // Collect info + update node_map, then transfer and commit
            let (mac_table, matched_ids) =
                collapse::collect_macro_invocations(&unexpanded, &expanded);
            self.node_map.add_edges(&matched_ids);
            self.node_map.add_edges(&[(CRATE_NODE_ID, CRATE_NODE_ID)]);
            let cfg_attr_info = collapse::collect_cfg_attrs(&unexpanded);
            let deleted_info = collapse::collect_deleted_nodes(
                &unexpanded, &self.node_map, &mac_table);
            collapse::match_nonterminal_ids(&mut self.node_map, &mac_table);

            let marks = self.node_map.transfer_marks(&marks);
            let cfg_attr_info = self.node_map.transfer_map(cfg_attr_info);
            self.node_map.commit();

            // Run the transform
            let r: R;
            let new_krate: Crate;
            let new_marks: HashSet<(NodeId, Symbol)>;
            {
                let parsed_nodes = mem::replace(&mut self.parsed_nodes,
                                                ParsedNodes::default());
                let node_id_counter = mem::replace(&mut self.node_id_counter,
                                                   NodeIdCounter::new(0));
                let cmd_state = CommandState::new(krate, marks, parsed_nodes, node_id_counter);
                r = f(&cmd_state, &cx);

                new_krate = cmd_state.krate.into_inner();
                new_marks = cmd_state.marks.into_inner();
                self.parsed_nodes = cmd_state.parsed_nodes.into_inner();
                self.node_id_counter = cmd_state.counter;

                self.node_map.init(cmd_state.new_parsed_node_ids.into_inner().into_iter());
            }

            // Collapse macros + update node_map.  The cfg_attr step requires the updated node_map
            // TODO: we should be able to skip some of these steps if `!cmd_state.krate_changed()`
            let mut new_krate = collapse::collapse_injected(new_krate);
            let matched_ids = collapse::collapse_macros(&mut new_krate, &mac_table);
            self.node_map.add_edges(&matched_ids);
            self.node_map.add_edges(&[(CRATE_NODE_ID, CRATE_NODE_ID)]);

            let cfg_attr_info = self.node_map.transfer_map(cfg_attr_info);
            collapse::restore_cfg_attrs(&mut new_krate, cfg_attr_info);

            collapse::restore_deleted_nodes(
                &mut new_krate, &mut self.node_map, &mut self.node_id_counter, deleted_info);

            let new_marks = self.node_map.transfer_marks(&new_marks);
            self.node_map.commit();

            // Write back new crate and marks
            self.krate = new_krate;
            self.marks = new_marks;

            r
        })
    }

    pub fn run_typeck_loop<F>(&mut self, mut func: F) -> Result<(), &'static str>
            where F: FnMut(&mut Crate, &CommandState, &RefactorCtxt) -> TypeckLoopResult {
        let func = &mut func;

        let mut result = None;
        while result.is_none() {
            self.transform_crate(Phase::Phase3, |st, cx| {
                st.map_krate(|krate| {
                    match func(krate, st, cx) {
                        TypeckLoopResult::Iterate => {}
                        TypeckLoopResult::Err(e) => {
                            result = Some(Err(e));
                        }
                        TypeckLoopResult::Finished => {
                            result = Some(Ok(()));
                        }
                    }
                });
            });
        }
        result.unwrap()
    }

    pub fn clear_marks(&mut self) {
        self.marks.clear()
    }


    /// Invoke a registered command with the given command name and arguments.
    pub fn run<S: AsRef<str>>(&mut self, cmd: &str, args: &[S]) -> Result<(), String> {
        let args = args.iter().map(|s| s.as_ref().to_owned()).collect::<Vec<_>>();
        info!("running command: {} {:?}", cmd, args);

        let mut cmd = self.cmd_reg.get_command(cmd, &args)?;
        cmd.run(self);
        Ok(())
    }


    pub fn marks(&self) -> &HashSet<(NodeId, Symbol)> {
        &self.marks
    }

    pub fn marks_mut(&mut self) -> &mut HashSet<(NodeId, Symbol)> {
        &mut self.marks
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
    krate: RefCell<Crate>,
    marks: RefCell<HashSet<(NodeId, Symbol)>>,
    parsed_nodes: RefCell<ParsedNodes>,
    new_parsed_node_ids: RefCell<Vec<NodeId>>,

    krate_changed: Cell<bool>,
    marks_changed: Cell<bool>,

    counter: NodeIdCounter,
}

impl CommandState {
    fn new(krate: Crate,
           marks: HashSet<(NodeId, Symbol)>,
           parsed_nodes: ParsedNodes,
           counter: NodeIdCounter) -> CommandState {
        CommandState {
            krate: RefCell::new(krate),
            marks: RefCell::new(marks),
            parsed_nodes: RefCell::new(parsed_nodes),
            new_parsed_node_ids: RefCell::new(Vec::new()),

            krate_changed: Cell::new(false),
            marks_changed: Cell::new(false),

            counter
        }
    }


    pub fn krate(&self) -> cell::Ref<Crate> {
        self.krate.borrow()
    }

    pub fn krate_mut(&self) -> cell::RefMut<Crate> {
        self.krate_changed.set(true);
        self.krate.borrow_mut()
    }

    pub fn map_krate<F: FnOnce(&mut Crate)>(&self, func: F) {
        func(&mut self.krate_mut());
    }

    pub fn krate_changed(&self) -> bool {
        self.krate_changed.get()
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


    /// Generate a fresh NodeId.
    pub fn next_node_id(&self) -> NodeId {
        self.counter.next()
    }

    /// Transfer marks on `old` to a fresh NodeId, and return that fresh NodeId.
    pub fn transfer_marks(&self, old: NodeId) -> NodeId {
        let new = self.next_node_id();

        let mut marks = self.marks_mut();
        let labels = marks.iter().filter(|x| x.0 == old).map(|x| x.1).collect::<Vec<_>>();
        for label in labels {
            marks.remove(&(old, label));
            marks.insert((new, label));
        }

        new
    }


    fn process_parsed<T>(&self, x: &mut T)
            where T: MutVisit + ListNodeIds {
        number_nodes_with(x, &self.counter);
        self.new_parsed_node_ids.borrow_mut()
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
        (self.krate.into_inner(),
         self.marks.into_inner())
    }
}


/// Implementation of a refactoring command.
pub trait Command {
    fn run(&mut self, state: &mut RefactorState);
}

/// A command builder is a function that takes some string arguments and produces a `Command`.
pub type Builder = FnMut(&[String]) -> Box<Command>;

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
            where B: FnMut(&[String]) -> Box<Command> + 'static {
        self.commands.insert(name.to_owned(), Box::new(builder));
    }

    pub fn get_command(&mut self, name: &str, args: &[String]) -> Result<Box<Command>, String> {
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
        where F: FnMut(&mut RefactorState) {
    fn run(&mut self, state: &mut RefactorState) {
        (self.0)(state);
    }
}


/// Wrap a `FnMut` to produce a command that invokes the `rustc` driver and operates over the
/// results.
pub struct DriverCommand<F>
        where F: FnMut(&CommandState, &RefactorCtxt) {
    func: F,
    phase: Phase,
}

impl<F> DriverCommand<F>
        where F: FnMut(&CommandState, &RefactorCtxt) {
    pub fn new(phase: Phase, func: F) -> DriverCommand<F> {
        DriverCommand { func, phase }
    }
}

impl<F> Command for DriverCommand<F>
        where F: FnMut(&CommandState, &RefactorCtxt) {
    fn run(&mut self, state: &mut RefactorState) {
        state.transform_crate(self.phase, |st, cx| (self.func)(st, cx));
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
    reg.register("commit", |_args| Box::new(FuncCommand(|rs: &mut RefactorState| {
        rs.save_crate();
        rs.load_crate();
        rs.clear_marks();
    })));

    reg.register("write", |_args| Box::new(FuncCommand(|rs: &mut RefactorState| {
        rs.save_crate();
    })));
}

pub fn register_commands(reg: &mut Registry) {
    register_commit(reg);
}
