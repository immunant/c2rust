//! Command management and overall refactoring state.

use std::cell::{self, Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::mem;
use rustc::session::Session;
use syntax::ast::{NodeId, Crate, Mod, DUMMY_NODE_ID};
use syntax::codemap::DUMMY_SP;
use syntax::codemap::FileLoader;
use syntax::codemap::FileMap;
use syntax::ptr::P;
use syntax::symbol::Symbol;

use ast_manip::{ListNodeIds, number_nodes};
use collapse;
use driver::{self, Phase, Phase1Bits};
use node_map::NodeMap;
use recheck::{self, PrepareRecheckInfo};
use rewrite;
use rewrite::files;
use span_fix;
use util::IntoSymbol;


/// Stores the overall state of the refactoring process, which can be read and updated by
/// `Command`s.
pub struct RefactorState {
    rewrite_handler: Option<Box<FnMut(Rc<FileMap>, &str)>>,
    cmd_reg: Registry,
    session: Session,

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
               rewrite_handler: Option<Box<FnMut(Rc<FileMap>, &str)>>,
               marks: HashSet<(NodeId, Symbol)>) -> RefactorState {
        RefactorState {
            rewrite_handler,
            cmd_reg,
            session,

            krate: None,
            orig_krate: None,

            node_map: NodeMap::new(),

            marks: marks,
        }
    }

    pub fn from_rustc_args(rustc_args: &[String],
                           cmd_reg: Registry,
                           rewrite_handler: Option<Box<FnMut(Rc<FileMap>, &str)>>,
                           file_loader: Option<Box<FileLoader+Sync+Send>>,
                           marks: HashSet<(NodeId, Symbol)>) -> RefactorState {
        let session = driver::build_session_from_args(rustc_args, file_loader);
        Self::new(session, cmd_reg, rewrite_handler, marks)
    }


    fn load_crate_inner(&self) -> Crate {
        let bits = Phase1Bits::from_session_reparse(&self.session);
        bits.into_crate()
    }

    /// Load the crate from disk.  Transitions to `Loaded`, regardless of current mode.
    pub fn load_crate(&mut self) {
        // Discard any existing krate, and proceed to `Loaded` regardless of current mode.
        let krate = self.load_crate_inner();
        let krate = number_nodes(krate);
        self.orig_krate = Some(krate.clone());
        self.krate = Some(krate);

        self.node_map = NodeMap::new();
        self.node_map.init(self.krate.list_node_ids().into_iter());
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

            let rws = rewrite::rewrite(&self.session, &old, &new, node_id_map);
            if rws.len() == 0 {
                info!("(no files to rewrite)");
            } else {
                if let Some(ref mut handler) = self.rewrite_handler {
                    files::rewrite_files_with(self.session.codemap(),
                                              &rws,
                                              |fm, s| handler(fm, s));
                }
            }
        }

        // We already cleared most of the fields in the block above.
        self.marks = HashSet::new();
    }

    pub fn transform_crate<F, R>(&mut self, phase: Phase, f: F) -> R
            where F: FnOnce(&CommandState, &driver::Ctxt) -> R {
        let mut krate = self.krate.take().unwrap();
        let mut marks = mem::replace(&mut self.marks, HashSet::new());

        let unexpanded = krate.clone();
        let krate = recheck::reset_node_ids(krate);
        let bits = Phase1Bits::from_session_and_crate(&self.session, krate);
        driver::run_compiler_from_phase1(bits, phase, |krate, cx| {
            let krate = span_fix::fix_format(cx.session(), krate);
            //let krate = span_fix::fix_derived_impls(sess, krate);
            //let krate = span_fix::fix_attr_spans(sess, krate);
            let expanded = krate.clone();

            info!("old ast **********");
            ::print_spans::print_spans(&unexpanded, self.session.codemap());
            info!("new ast **********");
            ::print_spans::print_spans(&expanded, self.session.codemap());
            info!("end of asts **********");
            info!("old marks: {:?}", marks);
            let (mac_table, matched_ids) =
                collapse::collect_macro_invocations(&unexpanded, &expanded);
            self.node_map.add_edges(&matched_ids);
            collapse::match_nonterminal_ids(&mut self.node_map, &mac_table);
            let marks = self.node_map.transfer_marks(&marks);
            info!("expanded marks: {:?}", marks);
            self.node_map.commit();

            // Run the transform
            let cmd_state = CommandState::new(krate, marks);
            let r = f(&cmd_state, &cx);

            // Update internal state
            let changed = cmd_state.krate_changed();
            let (new_krate, new_marks) = cmd_state.into_inner();
            info!("transformed ast **********");
            ::print_spans::print_spans(&new_krate, self.session.codemap());
            info!("new marks: {:?}", new_marks);

            let (new_krate, matched_ids) = collapse::collapse_macros(new_krate, &mac_table);
            self.node_map.add_edges(&matched_ids);
            info!("collapse_macros - matched_ids: {:?}", matched_ids);
            let new_krate = collapse::collapse_injected(new_krate);
            let new_marks = self.node_map.transfer_marks(&new_marks);
            info!("collapsed marks: {:?}", new_marks);
            info!("collapsed ast **********");
            ::print_spans::print_spans(&new_krate, self.session.codemap());
            info!("end of transformed asts **********");
            self.node_map.commit();
            self.krate = Some(new_krate);

            self.marks = new_marks;

            r
        })
    }

    pub fn clear_marks(&mut self) {
        self.marks.clear()
    }


    /// Invoke a registered command with the given command name and arguments.
    pub fn run<S: AsRef<str>>(&mut self, cmd: &str, args: &[S]) {
        let args = args.iter().map(|s| s.as_ref().to_owned()).collect::<Vec<_>>();
        info!("running command: {} {:?}", cmd, args);

        let mut cmd = self.cmd_reg.get_command(cmd, &args);
        cmd.run(self);
    }


    pub fn marks(&self) -> &HashSet<(NodeId, Symbol)> {
        &self.marks
    }

    pub fn marks_mut(&mut self) -> &mut HashSet<(NodeId, Symbol)> {
        &mut self.marks
    }
}


/// Mutable state that can be modified by a "driver" command.  This is normally paired with a
/// `driver::Ctxt`, which contains immutable analysis results from the original input `Crate`.
pub struct CommandState {
    krate: RefCell<Crate>,
    marks: RefCell<HashSet<(NodeId, Symbol)>>,

    krate_changed: Cell<bool>,
    marks_changed: Cell<bool>,

    next_node_id: Cell<u32>,
}

impl CommandState {
    pub fn new(krate: Crate,
               marks: HashSet<(NodeId, Symbol)>) -> CommandState {
        CommandState {
            krate: RefCell::new(krate),
            marks: RefCell::new(marks),

            krate_changed: Cell::new(false),
            marks_changed: Cell::new(false),

            // Start at an unreasonably large value which no real AST will ever collide with.  It's
            // okay to generate the same IDs in different commands because we renumber nodes at the
            // start of every command.
            next_node_id: Cell::new(0x8000_0000),
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
        let id = NodeId::from_u32(self.next_node_id.get());
        self.next_node_id.set(self.next_node_id.get() + 1);
        id
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

    pub fn get_command(&mut self, name: &str, args: &[String]) -> Box<Command> {
        let builder = self.commands.get_mut(name)
            .unwrap_or_else(|| panic!("no such command: {}", name));
        builder(args)
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
