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

use ast_manip::ListNodeIds;
use driver::{self, Phase, Phase1Bits};
use recheck::{self, PrepareRecheckInfo};
use rewrite;
use rewrite::files;
use span_fix;
use util::IntoSymbol;


#[derive(Clone, Debug)]
enum CrateState {
    None,
    /// The crate has not yet been macro-expanded.
    Unexpanded(Crate),
    /// The crate has been macro-expanded.  Note that crates prepared for rechecking are still
    /// considered expanded - `prepare_recheck` can't actually reverse macro expansion.
    Expanded(Crate),
}

impl CrateState {
    pub fn is_none(&self) -> bool {
        match *self {
            CrateState::None => true,
            _ => false,
        }
    }

    pub fn is_unexpanded(&self) -> bool {
        match *self {
            CrateState::Unexpanded(..) => true,
            _ => false,
        }
    }

    pub fn is_expanded(&self) -> bool {
        match *self {
            CrateState::Expanded(..) => true,
            _ => false,
        }
    }
}


/// A possible state of the refactoring engine.  (I'd call this enum "State", as in "state
/// machine", but that word is already heavily overloaded.)
///
/// For the most part, any state can transition to any other state by one means or another.  The
/// exception is that there are no transitions between `Unexpanded` and `Expanded` in either
/// direction, because we can't unexpand a crate, and we also don't know how to do rewriting when a
/// crate is transformed before expansion.  (Note that `Unexpanded` means the crate is unexpanded
/// *and* a transformation has been applied, as opposed to `Loaded` where it's unexpanded but has
/// never been transformed.)
///
/// See individual `RefactorState` method docs for the possible state transitions.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Mode {
    /// The crate has not been loaded from disk yet.  `krate` and `orig_krate` are both `None`.
    Unloaded,
    /// The crate has been loaded, but nothing has been done with it yet.  `krate` is `Some` and
    /// contains an unexpanded AST.
    Loaded,
    /// The crate has been transformed in unexpanded (Phase1) mode.  `krate` contains the current
    /// AST, and `orig_crate` contains the original unexpanded AST that was loaded from disk.
    Unexpanded,
    /// The crate has been transformed in expanded (Phase2+) mode.  `krate` contains the current
    /// AST, and `orig_crate` contains the expanded version of the original crate as it was loaded
    /// from disk.
    Expanded,
}


/// Stores the overall state of the refactoring process, which can be read and updated by
/// `Command`s.
pub struct RefactorState {
    rewrite_handler: Option<Box<FnMut(Rc<FileMap>, &str)>>,
    cmd_reg: Registry,
    session: Session,

    mode: Mode,

    /// The current crate AST.  This is used as the "new" AST when rewriting.
    krate: Option<Crate>,

    /// Info collected during the last compiler run, which allows us to prepare `krate` for
    /// rechecking.
    recheck_info: Option<PrepareRecheckInfo>,

    /// The original crate AST.  This is used as the "old" AST when rewriting.
    orig_krate: Option<Crate>,

    /// Mapping from `krate` NodeIds to `disk_krate` NodeIds
    node_id_map: HashMap<NodeId, NodeId>,

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

            mode: Mode::Unloaded,

            krate: None,
            recheck_info: None,
            orig_krate: None,

            node_id_map: HashMap::new(),

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
        self.mode = Mode::Loaded;
        self.krate = Some(self.load_crate_inner());
        self.recheck_info = None;
        self.orig_krate = None;

        self.node_id_map = HashMap::new();
        self.marks = HashSet::new();
    }

    /// Save the crate to disk by applying any pending rewrites.  Transitions to `Unloaded`, mainly
    /// so we don't have to deal with questions of how to keep `orig_krate` in sync with disk while
    /// also being usable for rewriting from `krate`.
    pub fn save_crate(&mut self) {
        if self.mode == Mode::Unloaded || self.mode == Mode::Loaded {
            // No rewrites need to be made.
            self.mode = Mode::Unloaded;
            self.krate = None;
            return;
        }

        {
            let old = self.orig_krate.take().unwrap();
            let new = self.krate.take().unwrap();
            let node_id_map = mem::replace(&mut self.node_id_map, HashMap::new());
            let recheck_info = self.recheck_info.take().unwrap();

            // Resolve `$crate`/`{{root}}` in paths.  Printing and reparsing these paths doesn't
            // work - `$crate` prints as `::std`, which reparses as `{{root}}::std`, which isn't
            // the same length as the original, and rewriting gets confused.  Resolving these to
            // absolute paths avoids this problem, and should only affects code inside macro
            // expansions, which we are hopefully replacing with recycled text anyway.
            let new = recheck::resolve_crate_with_info(&recheck_info, new);

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
        self.mode = Mode::Unloaded;
        self.marks = HashSet::new();
    }

    pub fn transform_crate<F, R>(&mut self, phase: Phase, f: F) -> R
            where F: FnOnce(&CommandState, &driver::Ctxt) -> R {
        if self.mode == Mode::Unloaded {
            self.load_crate();
        }

        let target_mode = match self.mode {
            Mode::Unloaded => unreachable!(),
            Mode::Loaded => {
                if phase == Phase::Phase1 {
                    Mode::Unexpanded
                } else {
                    Mode::Expanded
                }
            },
            Mode::Unexpanded => {
                assert!(phase == Phase::Phase1,
                        "can't expand a transformed crate - \
                         run `commit` between phase1 and phase2+ commands");
                Mode::Unexpanded
            },
            Mode::Expanded => {
                if phase == Phase::Phase1 {
                    warn!("running a phase1 command on an expanded crate");
                }
                Mode::Expanded
            },
        };

        match target_mode {
            Mode::Unexpanded => self.transform_crate_unexpanded(phase, f),
            Mode::Expanded => self.transform_crate_expanded(phase, f),
            _ => unreachable!(),
        }
    }

    pub fn transform_crate_expanded<F, R>(&mut self, phase: Phase, f: F) -> R
            where F: FnOnce(&CommandState, &driver::Ctxt) -> R {
        let first_transform = self.mode == Mode::Loaded;
        self.mode = Mode::Expanded;

        let mut krate = self.krate.take().unwrap();
        // Used only when `first_transform` is set.
        let mut old_ids = Vec::new();

        if !first_transform {
            // We're re-analyzing an already-expanded crate.  Run `prepare`.
            let info = self.recheck_info.as_ref().unwrap();
            old_ids = krate.list_node_ids();
            krate = recheck::prepare_recheck_with_info(info, krate);
        }

        let mut marks = mem::replace(&mut self.marks, HashSet::new());

        let bits = Phase1Bits::from_session_and_crate(&self.session, krate);
        driver::run_compiler_from_phase1(bits, phase, |krate, cx| {
            let krate = span_fix::fix_spans(&self.session, krate);

            if first_transform {
                // Set `orig_krate` to the newly-expanded `krate`.
                self.orig_krate = Some(krate.clone());
            }

            let new_ids = krate.list_node_ids();

            if first_transform {
                // Map each new node to itself.  `rewrite` interprets absence from the map as
                // "newly inserted", not as "kept same NodeId".
                self.node_id_map = new_ids.iter()
                    .filter(|&&x| x != DUMMY_NODE_ID)
                    .map(|&x| (x, x))
                    .collect();
            } else {
                // Expansion renumbers all NodeIds.  Transfer information from the old numbering to
                // the new one.

                // Convert mark NodeIds.  Note this needs to handle the case of a marked node being
                // copied to 2+ different locations in the new AST.
                let mut mark_map = HashMap::new();
                for &(id, label) in &marks {
                    mark_map.entry(id).or_insert_with(Vec::new).push(label);
                }
                let mut new_marks = HashSet::with_capacity(marks.len());
                for (&old, &new) in old_ids.iter().zip(new_ids.iter()) {
                    if let Some(labels) = mark_map.get(&old) {
                        for &label in labels {
                            new_marks.insert((new, label));
                        }
                    }
                }
                marks = new_marks;

                // Compose `self.node_id_map` with the mapping from each entry in `old_ids` to
                // the corresponding entry in `new_ids`.  (We don't actually construct the
                // second mapping explicitly.)
                let mut updated_id_map = HashMap::new();
                for (&old, &new) in old_ids.iter().zip(new_ids.iter()) {
                    if new == DUMMY_NODE_ID || old == DUMMY_NODE_ID {
                        continue;
                    }
                    // [new -> old] + [old -> older] = [new -> older]
                    if let Some(&older) = self.node_id_map.get(&old) {
                        updated_id_map.insert(new, older);
                    }
                }
                self.node_id_map = updated_id_map;
            }

            // Run the transform
            let cmd_state = CommandState::new(krate, marks);
            let r = f(&cmd_state, &cx);

            // Update internal state
            let changed = cmd_state.krate_changed();
            if cmd_state.krate_changed() || first_transform {
                let info = recheck::collect_prepare_recheck_info(&cx, &cmd_state.krate());
                self.recheck_info = Some(info);
            }
            let (new_krate, new_marks) = cmd_state.into_inner();
            self.krate = Some(new_krate);
            self.marks = new_marks;

            r
        })
    }

    pub fn transform_crate_unexpanded<F, R>(&mut self, phase: Phase, f: F) -> R
            where F: FnOnce(&CommandState, &driver::Ctxt) -> R {
        warn!("transform_crate(phase1) is unfinished and probably doesn't work (see comments)");
        let first_transform = self.mode == Mode::Loaded;
        self.mode = Mode::Unexpanded;

        let krate = self.krate.take().unwrap();
        // Used only when `first_transform` is set.
        let old_ids = krate.list_node_ids();

        let mut marks = mem::replace(&mut self.marks, HashSet::new());

        let bits = Phase1Bits::from_session_and_crate(&self.session, krate);
        driver::run_compiler_from_phase1(bits, phase, |krate, cx| {
            // TODO: Need to assign NodeIds at this point.  Otherwise rewrite will get confused
            // because they are all DUMMY_NODE_ID.

            if first_transform {
                // Set `orig_krate` only after renumbering, so that `rewrite` will have non-DUMMY
                // NodeIds to work with.
                self.orig_krate = Some(krate.clone());
            }

            let new_ids = krate.list_node_ids();

            if first_transform {
                self.node_id_map = new_ids.iter()
                    .filter(|&&x| x != DUMMY_NODE_ID)
                    .map(|&x| (x, x))
                    .collect();
            } else {
                // We just renumbered all NodeIds.  Transfer information from the old numbering to
                // the new one.

                // Convert mark NodeIds.  Note this needs to handle the case of a marked node being
                // copied to 2+ different locations in the new AST.
                let mut mark_map = HashMap::new();
                for &(id, label) in &marks {
                    mark_map.entry(id).or_insert_with(Vec::new).push(label);
                }
                let mut new_marks = HashSet::with_capacity(marks.len());
                for (&old, &new) in old_ids.iter().zip(new_ids.iter()) {
                    if let Some(labels) = mark_map.get(&old) {
                        for &label in labels {
                            new_marks.insert((new, label));
                        }
                    }
                }
                marks = new_marks;

                // Compose `self.node_id_map` with the mapping from each entry in `old_ids` to
                // the corresponding entry in `new_ids`.  (We don't actually construct the
                // second mapping explicitly.)
                let mut updated_id_map = HashMap::new();
                for (&old, &new) in old_ids.iter().zip(new_ids.iter()) {
                    if new == DUMMY_NODE_ID || old == DUMMY_NODE_ID {
                        continue;
                    }
                    // [new -> old] + [old -> older] = [new -> older]
                    if let Some(&older) = self.node_id_map.get(&old) {
                        updated_id_map.insert(new, older);
                    }
                }
                self.node_id_map = updated_id_map;
            }

            // Run the transform
            let cmd_state = CommandState::new(krate, marks);
            let r = f(&cmd_state, &cx);

            // Update internal state
            let (new_krate, new_marks) = cmd_state.into_inner();
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
