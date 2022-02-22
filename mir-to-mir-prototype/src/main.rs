#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

use std::env;
use std::mem;
use rustc_ast::ast::{Item, ItemKind, Visibility, VisibilityKind};
use rustc_ast::node_id::NodeId;
use rustc_ast::ptr::P;
use rustc_driver::Compilation;
use rustc_interface::Queries;
use rustc_interface::interface::Compiler;
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, START_BLOCK, Terminator, TerminatorKind, SourceInfo, Local,
    LocalDecl, Mutability, Rvalue, AggregateKind, Place, Operand, Statement, StatementKind,
    BorrowKind, Constant, ConstantKind,
};
use rustc_middle::mir::interpret::{Allocation, ConstValue};
use rustc_middle::ty::{RegionKind, WithOptConstParam};
use rustc_middle::ty::query::{Providers, ExternProviders};
use rustc_session::Session;
use rustc_span::DUMMY_SP;
use rustc_span::def_id::{DefId, LocalDefId, CRATE_DEF_INDEX};
use rustc_span::symbol::Ident;
use rustc_target::abi::Align;


struct MirTransformCallbacks {
}

impl rustc_driver::Callbacks for MirTransformCallbacks {
    fn config(&mut self, config: &mut rustc_interface::Config) {
        config.override_queries = Some(override_queries);
    }

    fn after_parsing<'tcx>(
        &mut self,
        compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        let parse = queries.parse().unwrap();
        let mut parse = parse.peek_mut();
        parse.items.push(P(Item {
            attrs: Vec::new(),
            id: NodeId::from_u32(0),
            span: DUMMY_SP,
            vis: Visibility {
                kind: VisibilityKind::Inherited,
                span: DUMMY_SP,
                tokens: None,
            },
            ident: Ident::from_str("instrument_support"),
            kind: ItemKind::ExternCrate(None),
            tokens: None,
        }));
        Compilation::Continue
    }
}

fn override_queries(
    sess: &Session,
    providers: &mut Providers,
    extern_providers: &mut ExternProviders,
) {
    providers.mir_built = |tcx, def: WithOptConstParam<LocalDefId>| {
        let mut providers = Providers::default();
        rustc_mir_build::provide(&mut providers);

        let steal_mir = (providers.mir_built)(tcx, def);
        let mut mir = steal_mir.steal();


        // Get the name of the function we're compiling.
        let name = tcx.item_name(def.did.to_def_id());
        let name = name.as_str();


        // Find the instrumentation function we want to call.
        let support_crate = tcx.crates(()).iter().cloned()
            .find(|&krate| tcx.crate_name(krate).as_str() == "instrument_support")
            .unwrap();
        eprintln!("found support crate at {:?}", support_crate);

        let support_crate_def_id = DefId {
            krate: support_crate,
            index: CRATE_DEF_INDEX,
        };
        let func_def_id = tcx.module_children(support_crate_def_id).iter()
            .find(|child| child.ident.name.as_str() == "handle_call")
            .unwrap().res.def_id();
        eprintln!("found handle_call at {:?}", func_def_id);
        eprintln!("type: {:?}", tcx.type_of(func_def_id));


        // Move the start block to the end so we can build a new start block.
        let start_data = mem::replace(&mut mir.basic_blocks_mut()[START_BLOCK], BasicBlockData {
            statements: Vec::new(),
            terminator: None,
            is_cleanup: false,
        });
        let new_start_block = mir.basic_blocks_mut().push(start_data);
        for blk in mir.basic_blocks_mut() {
            let term = match blk.terminator {
                Some(ref mut x) => x,
                None => continue,
            };
            for succ in term.successors_mut() {
                if *succ == START_BLOCK {
                    *succ = new_start_block;
                }
            }
        }


        // Build the new start block.  It will call the instrumentation function and then jump to
        // the old start block.
        let mut stmts = Vec::new();

        // Build a tuple of `(&arg0, &arg1, ...)`.
        let mut args_tuple_elems = Vec::new();
        for arg_local in mir.args_iter() {
            // Declare a new variable and store a reference to `arg_local` in it.
            let addr = Rvalue::Ref(
                tcx.mk_region(RegionKind::ReErased),
                BorrowKind::Shared,
                Place::from(arg_local),
            );
            let addr_local = mir.local_decls.push(LocalDecl::new(addr.ty(&mir, tcx), DUMMY_SP));
            stmts.push(Statement {
                source_info: SourceInfo::outermost(DUMMY_SP),
                kind: StatementKind::Assign(Box::new((Place::from(addr_local), addr))),
            });

            // Push the variable containing the reference onto `args_tuple_elems`.
            args_tuple_elems.push(Operand::Move(Place::from(addr_local)));
        }

        let args_tuple = Rvalue::Aggregate(Box::new(AggregateKind::Tuple), args_tuple_elems);
        let args_local = mir.local_decls.push(LocalDecl::new(args_tuple.ty(&mir, tcx), DUMMY_SP));
        stmts.push(Statement {
            source_info: SourceInfo::outermost(DUMMY_SP),
            kind: StatementKind::Assign(Box::new((Place::from(args_local), args_tuple))),
        });
        let args_operand = Operand::Move(Place::from(args_local));

        // Allocate a string literal.
        let name_alloc = Allocation::from_bytes(name.as_bytes(), Align::ONE, Mutability::Not);
        let name_alloc = tcx.intern_const_alloc(name_alloc);
        let name_const = Constant {
            span: DUMMY_SP,
            user_ty: None,
            literal: ConstantKind::Val(
                ConstValue::Slice {
                    data: name_alloc,
                    start: 0,
                    end: name.len(),
                },
                tcx.mk_static_str(),
            ),
        };
        let name_operand = Operand::Constant(Box::new(name_const));


        let unit_local = mir.local_decls.push(LocalDecl::new(tcx.mk_unit(), DUMMY_SP));
        // `handle_call` is generic, so we must provide concrete type arguments ("substitutions").
        let func_substs = tcx.mk_substs([args_operand.ty(&mir, tcx).into()].iter());
        let call = TerminatorKind::Call {
            func: Operand::function_handle(tcx, func_def_id, func_substs, DUMMY_SP),
            args: vec![name_operand, args_operand],
            destination: Some((Place::from(unit_local), new_start_block)),
            cleanup: None,
            from_hir_call: true,
            fn_span: DUMMY_SP,
        };
        let term = Terminator {
            source_info: SourceInfo::outermost(DUMMY_SP),
            kind: call,
        };

        {
            let blk = &mut mir.basic_blocks_mut()[START_BLOCK];
            blk.statements = stmts;
            blk.terminator = Some(term);
        }

        tcx.alloc_steal_mir(mir)
    };
}

fn main() -> rustc_interface::interface::Result<()> {
    let mut args = env::args().collect::<Vec<_>>();
    args.push("--extern".into());
    args.push("instrument_support=./libinstrument_support.rlib".into());
    let mut callbacks = MirTransformCallbacks {
    };
    rustc_driver::RunCompiler::new(&args, &mut callbacks).run()
}
