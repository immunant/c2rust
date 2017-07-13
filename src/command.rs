use std::cell::{self, Cell, RefCell};
use std::collections::HashSet;
use std::collections::hash_map::{HashMap, Entry};
use std::ffi::{CString, CStr};
use std::mem;
use libc::{dlopen, dlsym, RTLD_LAZY};
use syntax::ast::{NodeId, Crate, Mod};
use syntax::codemap::DUMMY_SP;
use syntax::symbol::Symbol;

use driver::{self, Phase};
use rewrite::TextRewrite;
use util::IntoSymbol;


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


pub trait Command {
    fn run(&mut self,
           st: &CommandState,
           cx: &driver::Ctxt);

    fn min_phase(&self) -> Phase {
        // Most operations should run on expanded code.
        Phase::Phase2
    }
}


/// A command builder is a function that takes some string arguments and produces a `Command`.
pub type Builder = FnMut(&[String]) -> Box<Command>;

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


pub struct FuncCommand<F>
        where F: FnMut(&CommandState, &driver::Ctxt) {
    func: F,
    phase: Phase,
}

impl<F> FuncCommand<F>
        where F: FnMut(&CommandState, &driver::Ctxt) {
    pub fn new(phase: Phase, func: F) -> FuncCommand<F> {
        FuncCommand { func, phase }
    }
}

impl<F> Command for FuncCommand<F>
        where F: FnMut(&CommandState, &driver::Ctxt) {
    fn run(&mut self, st: &CommandState, cx: &driver::Ctxt) {
        (self.func)(st, cx);
    }

    fn min_phase(&self) -> Phase {
        self.phase
    }
}


pub fn register_misc_commands(reg: &mut Registry) {
    use pick_node;
    use mark_adjust;

    reg.register("pick_node", |args| {
        let args = args.to_owned();
        Box::new(FuncCommand::new(Phase::Phase2, move |st, cx| {
            pick_node::pick_node_command(&st.krate(), &cx, &args);
        }))
    });

    reg.register("print_marks", |args| {
        Box::new(FuncCommand::new(Phase::Phase2, move |st, cx| {
            let mut marks = st.marks().iter().map(|&x| x).collect::<Vec<_>>();
            marks.sort();

            for (id, label) in marks {
                info!("{}:{}", id.as_usize(), label.as_str());
            }
        }))
    });

    reg.register("mark_uses", |args| {
        let arg = args[0].clone();
        Box::new(FuncCommand::new(Phase::Phase2, move |st, cx| {
            mark_adjust::find_mark_uses_command(st, cx, &arg);
        }))
    });

    reg.register("rename_marks", |args| {
        let old = (&args[0]).into_symbol();
        let new = (&args[1]).into_symbol();
        Box::new(FuncCommand::new(Phase::Phase2, move |st, cx| {
            mark_adjust::rename_marks(st, old, new);
        }))
    });
}
