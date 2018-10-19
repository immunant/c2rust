//! Transformation passes used for testing parts of the system.

use std::collections::{HashSet, HashMap};
use std::str::FromStr;
use syntax::ast::{Crate, Ty};
use syntax::ptr::P;

use api::*;
use command::{RefactorState, CommandState, Command, Registry, TypeckLoopResult};
use driver;
use transform::Transform;


/// `2 -> 1 + 1`.  Useful for testing the rewriter's handling of operator precedence.  The `1 + 1`
/// may or may not need enclosing parens, depending on the context.
pub struct OnePlusOne;

impl Transform for OnePlusOne {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let krate = replace_expr(st, cx, krate, "2", "1 + 1");
        krate
    }
}


/// `f(x) -> x + 1`.  Useful for testing the rewriter's handling of operator precedence.  The `x`
/// may or may not need enclosing parens, depending on what type of expression it is.
pub struct FPlusOne;

impl Transform for FPlusOne {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let krate = replace_expr(st, cx, krate, "f(__x)", "__x + 1");
        krate
    }
}


pub struct ReplaceStmts(pub String, pub String);

impl Transform for ReplaceStmts {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let krate = replace_stmts(st, cx, krate, &self.0, &self.1);
        krate
    }
}


pub struct InsertRemoveArgs {
    insert_idxs: HashMap<usize, usize>,
    remove_idxs: HashSet<usize>,
}

impl Transform for InsertRemoveArgs {
    fn transform(&self, krate: Crate, st: &CommandState, _cx: &driver::Ctxt) -> Crate {
        let krate = fold_fns(krate, |mut fl| {
            if !st.marked(fl.id, "target") {
                return fl;
            }

            let mut counter = 0;
            let mut mk_arg = || {
                let arg = mk().arg(mk().tuple_ty::<P<Ty>>(vec![]),
                                   mk().ident_pat(&format!("new_arg{}", counter)));
                counter += 1;
                arg
            };

            fl.decl = fl.decl.clone().map(|mut decl| {
                let mut new_args = Vec::new();
                let old_arg_count = decl.inputs.len();
                for (i, arg) in decl.inputs.into_iter().enumerate() {
                    for _ in 0 .. self.insert_idxs.get(&i).cloned().unwrap_or(0) {
                        new_args.push(mk_arg());
                    }

                    if !self.remove_idxs.contains(&i) {
                        new_args.push(arg);
                    }
                }

                for _ in 0 .. self.insert_idxs.get(&old_arg_count).cloned().unwrap_or(0) {
                    new_args.push(mk_arg());
                }

                decl.inputs = new_args;
                decl
            });

            fl
        });

        krate
    }
}


/// Command for testing basic `run_typeck_loop` functionality.
pub struct TestTypeckLoop;

impl Command for TestTypeckLoop {
    fn run(&mut self, state: &mut RefactorState) {
        let mut i = 3;
        state.run_typeck_loop(|krate, _st, _cx| {
            i -= 1;
            info!("ran typeck loop iteration {}", i);
            if i == 0 {
                TypeckLoopResult::Finished(krate)
            } else {
                TypeckLoopResult::Iterate(krate)
            }
        }).unwrap();
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("test_one_plus_one", |_args| mk(OnePlusOne));
    reg.register("test_f_plus_one", |_args| mk(FPlusOne));
    reg.register("test_replace_stmts", |args| mk(
            ReplaceStmts(args[0].clone(), args[1].clone())));

    reg.register("test_insert_remove_args", |args| {
        let mut insert_idxs = HashMap::new();
        let mut remove_idxs = HashSet::new();

        for part in args[0].split(",") {
            if part == "" {
                continue;
            }
            let idx = usize::from_str(part).unwrap();
            *insert_idxs.entry(idx).or_insert(0) += 1;
        }

        for part in args[1].split(",") {
            if part == "" {
                continue;
            }
            let idx = usize::from_str(part).unwrap();
            remove_idxs.insert(idx);
        }

        mk(InsertRemoveArgs { insert_idxs, remove_idxs })
    });

    reg.register("test_typeck_loop", |_| Box::new(TestTypeckLoop));
}
