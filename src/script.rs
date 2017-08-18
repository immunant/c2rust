use std::collections::HashSet;
use std::borrow::Borrow;
use std::rc::Rc;
use syntax::ast::NodeId;
use syntax::codemap::FileLoader;
use syntax::codemap::FileMap;
use syntax::symbol::Symbol;

use command::{Command, CommandState, Registry};
use driver::{self, Phase};
use file_rewrite::{self, RewriteMode};
use rewrite;
use span_fix;

pub struct RefactorState {
    rustc_args: Vec<String>,
    make_file_loader: Option<Box<Fn() -> Box<FileLoader>>>,
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

    pub fn make_file_loader<F: Fn() -> Box<FileLoader> + 'static>(&mut self, f: F) {
        self.make_file_loader = Some(Box::new(f));
    }

    pub fn rewrite_handler<F: FnMut(Rc<FileMap>, &str) + 'static>(&mut self, f: F) {
        self.rewrite_handler = Some(Box::new(f));
    }

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
                        file_rewrite::rewrite_files_with(cx.session().codemap(),
                                                         &rws,
                                                         |fm, s| handler(fm, s));
                    }
                }

                *marks = HashSet::new();
            }

            r
        })
    }

    pub fn with_context<F, R>(&mut self, f: F) -> R
            where F: FnOnce(&CommandState, &driver::Ctxt) -> R {
        self.with_context_at_phase(Phase::Phase3, f)
    }

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
