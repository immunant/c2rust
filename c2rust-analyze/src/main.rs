#![feature(rustc_private)]
#![feature(iter_order_by)]
extern crate either;
extern crate rustc_arena;
extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_type_ir;

mod analyze;
mod borrowck;
mod c_void_casts;
mod context;
mod dataflow;
mod equiv;
mod known_fn;
mod labeled_ty;
mod log;
mod panic_detail;
mod pointee_type;
mod pointer_id;
mod rewrite;
mod trivial;
mod type_desc;
mod util;

use crate::log::init_logger;
use analyze::AnalysisCallbacks;
use rustc_driver::RunCompiler;
use std::env;

fn main() -> rustc_interface::interface::Result<()> {
    init_logger();

    let dont_catch = env::var_os("C2RUST_ANALYZE_TEST_DONT_CATCH_PANIC").is_some();
    if !dont_catch {
        panic_detail::set_hook();
    }

    let args = env::args().collect::<Vec<_>>();

    RunCompiler::new(&args, &mut AnalysisCallbacks).run()
}
