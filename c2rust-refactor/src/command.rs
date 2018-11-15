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

use ast_manip::{ListNodeIds, remove_paren, Visit, Fold};
use ast_manip::ast_map::map_ast_into;
use ast_manip::number_nodes::{number_nodes, number_nodes_with, NodeIdCounter, reset_node_ids};
use collapse;
use driver::{self, Phase, Phase1Bits};
use file_io::{FileIO, ArcFileIO};
use node_map::NodeMap;
use rewrite;
use rewrite::files;
use span_fix;
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
        self.exprs.iter().for_each(|x| x.visit(v));
        self.pats.iter().for_each(|x| x.visit(v));
        self.tys.iter().for_each(|x| x.visit(v));
        self.stmts.iter().for_each(|x| x.visit(v));
        self.items.iter().for_each(|x| x.visit(v));
    }
}


/// Stores the overall state of the refactoring process, which can be read and updated by
/// `Command`s.
pub struct RefactorState {
    file_io: Arc<FileIO+Sync+Send>,
    cmd_reg: Registry,
    session: Session,

    parsed_nodes: ParsedNodes,
    node_id_counter: NodeIdCounter,

    /// The current crate AST.  This is used as the "new" AST when rewriting.  This is always
    /// "unexpanded" - meaning either actually unexpanded, or expanded and then subsequently
    /// macro-collapsed.
    krate: Option<Crate>,

    /// The original crate AST.  This is used as the "old" AST when rewriting.  This is always an
    /// unexpanded AST.
    orig_krate: Option<Crate>,

    /// Mapping from `krate` IDs to `disk_krate` IDs
    node_map: NodeMap,

    /// Current marks.  The `NodeId`s here refer to nodes in `krate`.
    marks: HashSet<(NodeId, Symbol)>,
}

impl RefactorState {
    pub fn new(session: Session,
               cmd_reg: Registry,
               file_io: Arc<FileIO+Sync+Send>,
               marks: HashSet<(NodeId, Symbol)>) -> RefactorState {
        RefactorState {
            file_io,
            cmd_reg,
            session,

            parsed_nodes: ParsedNodes::default(),
            node_id_counter: NodeIdCounter::new(0x8000_0000),

            krate: None,
            orig_krate: None,

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

    /// Load the crate from disk.  Transitions to `Loaded`, regardless of current mode.
    pub fn load_crate(&mut self) {
        // Discard any existing krate, and proceed to `Loaded` regardless of current mode.
        let krate = self.load_crate_inner();
        let krate = remove_paren(krate);
        let krate = number_nodes(krate);
        self.orig_krate = Some(krate.clone());
        self.krate = Some(krate);

        self.node_map = NodeMap::new();
        self.node_map.init(self.krate.list_node_ids().into_iter());
        // Special case: CRATE_NODE_ID doesn't actually appear anywhere in the AST.
        self.node_map.init(iter::once(CRATE_NODE_ID));
        self.marks = HashSet::new();
    }

    /// Save the crate to disk by applying any pending rewrites.  Transitions to `Unloaded`, mainly
    /// so we don't have to deal with questions of how to keep `orig_krate` in sync with disk while
    /// also being usable for rewriting from `krate`.
    pub fn save_crate(&mut self) {
        {
            let old = self.orig_krate.take().unwrap();
            let new = self.krate.take().unwrap();
            let node_map = mem::replace(&mut self.node_map, NodeMap::new());
            let node_id_map = node_map.into_inner();

            let rws = rewrite::rewrite(&self.session, &old, &new, node_id_map, |map| {
                map_ast_into(&self.parsed_nodes, map);
            });
            if rws.len() == 0 {
                info!("(no files to rewrite)");
            } else {
                files::rewrite_files_with(
                    self.session.source_map(), &rws, &*self.file_io).unwrap();
            }
        }

        // We already cleared most of the fields in the block above.
        self.marks = HashSet::new();
    }

    pub fn transform_crate<F, R>(&mut self, phase: Phase, f: F) -> R
            where F: FnOnce(&CommandState, &driver::Ctxt) -> R {
        let krate = self.krate.take().unwrap();
        let marks = mem::replace(&mut self.marks, HashSet::new());

        let unexpanded = krate.clone();
        let krate = reset_node_ids(krate);
        let bits = Phase1Bits::from_session_and_crate(&self.session, krate);
        driver::run_compiler_from_phase1(bits, phase, |krate, cx| {
            let krate = span_fix::fix_format(krate);
            let expanded = krate.clone();

            // Collect info + update node_map, then transfer and commit
            let (mac_table, matched_ids) =
                collapse::collect_macro_invocations(&unexpanded, &expanded);
            self.node_map.add_edges(&matched_ids);
            self.node_map.add_edges(&[(CRATE_NODE_ID, CRATE_NODE_ID)]);
            let cfg_attr_info = collapse::collect_cfg_attrs(&unexpanded);
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
            let new_krate = collapse::collapse_injected(new_krate);
            let (new_krate, matched_ids) = collapse::collapse_macros(new_krate, &mac_table);
            self.node_map.add_edges(&matched_ids);
            self.node_map.add_edges(&[(CRATE_NODE_ID, CRATE_NODE_ID)]);

            let cfg_attr_info = self.node_map.transfer_map(cfg_attr_info);
            let new_krate = collapse::restore_cfg_attrs(new_krate, cfg_attr_info);

            let new_marks = self.node_map.transfer_marks(&new_marks);
            self.node_map.commit();

            // Write back new crate and marks
            self.krate = Some(new_krate);
            self.marks = new_marks;

            r
        })
    }

    pub fn run_typeck_loop<F>(&mut self, mut func: F) -> Result<(), &'static str>
            where F: FnMut(Crate, &CommandState, &driver::Ctxt) -> TypeckLoopResult {
        let func = &mut func;

        let mut result = None;
        while result.is_none() {
            self.transform_crate(Phase::Phase3, |st, cx| {
                st.map_krate(|krate| {
                    match func(krate, st, cx) {
                        TypeckLoopResult::Iterate(krate) => krate,
                        TypeckLoopResult::Err(e, krate) => {
                            result = Some(Err(e));
                            krate
                        },
                        TypeckLoopResult::Finished(krate) => {
                            result = Some(Ok(()));
                            krate
                        },
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
    Iterate(Crate),
    Err(&'static str, Crate),
    Finished(Crate),
}


/// Mutable state that can be modified by a "driver" command.  This is normally paired with a
/// `driver::Ctxt`, which contains immutable analysis results from the original input `Crate`.
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

    pub fn map_krate<F: FnOnce(Crate) -> Crate>(&self, func: F) {
        let dummy_crate = Crate {
            module: Mod {
                inner: DUMMY_SP,
                items: Vec::new(),
                inline: true,
            },
            attrs: Vec::new(),
            span: DUMMY_SP,
        };
        let old_krate = mem::replace(&mut *self.krate_mut(), dummy_crate);
        let new_krate = func(old_krate);
        *self.krate_mut() = new_krate;
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


    fn process_parsed<T>(&self, x: T) -> <T as Fold>::Result
            where T: Fold, <T as Fold>::Result: ListNodeIds {
        let x = number_nodes_with(x, &self.counter);
        self.new_parsed_node_ids.borrow_mut()
            .extend(x.list_node_ids());
        x
    }

    /// Parse an `Expr`, keeping the original `src` around for use during rewriting.
    pub fn parse_expr(&self, cx: &driver::Ctxt, src: &str) -> P<Expr> {
        let e = driver::parse_expr(cx.session(), src);
        let e = self.process_parsed(e);
        self.parsed_nodes.borrow_mut().exprs.push(e.clone());
        e
    }

    pub fn parse_items(&self, cx: &driver::Ctxt, src: &str) -> Vec<P<Item>> {
        let is = driver::parse_items(cx.session(), src);
        let is: Vec<P<Item>> = is.into_iter()
            .flat_map(|i| self.process_parsed(i)).collect();
        for i in &is {
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
        where F: FnMut(&CommandState, &driver::Ctxt) {
    func: F,
    phase: Phase,
}

impl<F> DriverCommand<F>
        where F: FnMut(&CommandState, &driver::Ctxt) {
    pub fn new(phase: Phase, func: F) -> DriverCommand<F> {
        DriverCommand { func, phase }
    }
}

impl<F> Command for DriverCommand<F>
        where F: FnMut(&CommandState, &driver::Ctxt) {
    fn run(&mut self, state: &mut RefactorState) {
        state.transform_crate(self.phase, |st, cx| (self.func)(st, cx));
    }
}


pub fn register_commands(reg: &mut Registry) {
    reg.register("commit", |_args| Box::new(FuncCommand(|rs: &mut RefactorState| {
        rs.save_crate();
        rs.load_crate();
        rs.clear_marks();
    })));
}
