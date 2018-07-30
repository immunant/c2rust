//! Command management and overall refactoring state.

use std::cell::{self, Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::mem;
use syntax::ast::{NodeId, Crate, Mod};
use syntax::codemap::DUMMY_SP;
use syntax::codemap::FileLoader;
use syntax::codemap::FileMap;
use syntax::symbol::Symbol;

use driver::{self, Phase};
use rewrite;
use rewrite::files;
use span_fix;
use util::IntoSymbol;


/// Stores the overall state of the refactoring process, which can be read and updated by
/// `Command`s.
///
/// TODO: The current design has a few issues.  Specifically:
///
///  1. The `Crate` is regenerated from the filesystem contents for each command, so running
///     multiple transform commands will fail if the rewriting mode is not (effectively) "inplace".
///  2. Running a transform command requires discarding all the marks, because parsing the
///     transformed code will assign NodeIds differently.
///  3. There's no actual tracking of the relationship between the marks and the `Crate` (which,
///     again, depends on the on-disk source code state).
///
/// I think these could be addressed by keeping the expanded `Crate` as part of the refactor state.
/// Run `Phase1` once and keep the results, rerunning only `Phase2` and `Phase3` when analysis
/// results are needed.  Explicitly run renumbering when the `Crate` is changed (so that NodeIds
/// will remain unique), and update the marks to compensate for the changes.  And if the `Crate` is
/// kept in-memory, then we can delay rewriting until the very end, producing good results for
/// multi-command invocations even under non-"inplace" rewrite modes.
pub struct RefactorState {
    rustc_args: Vec<String>,
    make_file_loader: Option<Box<Fn() -> Box<FileLoader+Sync+Send>>>,
    rewrite_handler: Option<Box<FnMut(Rc<FileMap>, &str)>>,
    cmd_reg: Registry,
    marks: HashSet<(NodeId, Symbol)>,
}

impl RefactorState {
    pub fn new(rustc_args: Vec<String>,
               cmd_reg: Registry,
               marks: HashSet<(NodeId, Symbol)>) -> RefactorState {
        RefactorState {
            rustc_args: rustc_args,
            make_file_loader: None,
            rewrite_handler: None,
            cmd_reg: cmd_reg,
            marks: marks,
        }
    }

    /// Provide a function that builds a custom file loader for `rustc` driver invocations.
    pub fn make_file_loader<F: Fn() -> Box<FileLoader+Sync+Send> + 'static>(&mut self, f: F) {
        self.make_file_loader = Some(Box::new(f));
    }

    /// Provide a callback for handling rewritten file contents.
    pub fn rewrite_handler<F: FnMut(Rc<FileMap>, &str) + 'static>(&mut self, f: F) {
        self.rewrite_handler = Some(Box::new(f));
    }

    /// Run the compiler driver to the given phase, and pass the command and driver state to the
    /// provided callback.
    pub fn with_context_at_phase<F, R>(&mut self, phase: Phase, f: F) -> R
            where F: FnOnce(&CommandState, &driver::Ctxt) -> R{
        let marks = &mut self.marks;
        let mut rewrite_handler = self.rewrite_handler.as_mut();

        let file_loader = self.make_file_loader.as_ref().map(|f| f());
        driver::run_compiler(&self.rustc_args, file_loader, phase, |krate, cx| {
            let krate = span_fix::fix_spans(cx.session(), krate);

            let cmd_state = CommandState::new(krate.clone(),
                                              marks.clone());

            let r = f(&cmd_state, &cx);

            if cmd_state.marks_changed() {
                *marks = cmd_state.marks().clone();
            }

            if cmd_state.krate_changed() {
                let rws = rewrite::rewrite(cx.session(), &krate, &cmd_state.krate());
                if rws.len() == 0 {
                    info!("(no files to rewrite)");
                } else {
                    if let Some(ref mut handler) = rewrite_handler {
                        files::rewrite_files_with(cx.session().codemap(),
                                                  &rws,
                                                  |fm, s| handler(fm, s));
                    }
                }

                *marks = HashSet::new();
            }

            r
        })
    }

    /// Run the compiler driver to `Phase3`, and pass the command and driver state to the provided
    /// callback.
    pub fn with_context<F, R>(&mut self, f: F) -> R
            where F: FnOnce(&CommandState, &driver::Ctxt) -> R {
        self.with_context_at_phase(Phase::Phase3, f)
    }

    /// Invoke a registered command with the given command name and arguments.
    pub fn run<S: AsRef<str>>(&mut self, cmd: &str, args: &[S]) {
        let args = args.iter().map(|s| s.as_ref().to_owned()).collect::<Vec<_>>();

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
}

impl CommandState {
    pub fn new(krate: Crate,
               marks: HashSet<(NodeId, Symbol)>) -> CommandState {
        CommandState {
            krate: RefCell::new(krate),
            marks: RefCell::new(marks),

            krate_changed: Cell::new(false),
            marks_changed: Cell::new(false),
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
        state.with_context_at_phase(self.phase, |st, cx| (self.func)(st, cx));
    }
}
