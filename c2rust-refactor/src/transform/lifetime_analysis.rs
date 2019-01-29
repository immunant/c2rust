use std::collections::HashMap;
use std::fs::File;

use syntax::{ast, entry};
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::symbol::Ident;
use syntax::source_map::{Span, FileName};

use indexmap::IndexSet;
use failure::{Error, ResultExt};

use c2rust_analysis_rt::{SourceSpan, BytePos};

use crate::api::*;
use crate::command::{CommandState, Registry};
use crate::driver::{self, Phase};
use crate::transform::Transform;
use crate::ast_manip::visit_nodes;
use c2rust_ast_builder::Make;


struct LifetimeAnalysis {
    span_file_path: String,
    main_path: String,
}

impl Transform for LifetimeAnalysis {
    fn transform(&self, krate: ast::Crate, _st: &CommandState, cx: &driver::Ctxt) -> ast::Crate {
        let mut folder = LifetimeInstrumentation::new(cx, &self.span_file_path, &self.main_path);
        let folded = folder.fold_crate(krate);
        folder.finalize().expect("Error instrumenting lifetimes");
        folded
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

/// List of functions we want hooked for the lifetime analyis runtime (see
/// ../../runtime/src/lib.rs for the implementations of these hooks)
const HOOK_FUNCTIONS: &[&'static str] = c2rust_analysis_rt::HOOK_FUNCTIONS;

struct LifetimeInstrumentation<'a, 'tcx: 'a> {
    cx: &'a driver::Ctxt<'a, 'tcx>,
    span_file_path: &'a str,
    main_path: ast::Path,
    hooked_functions: HashMap<Ident, P<ast::FnDecl>>,

    spans: IndexSet<SourceSpan>,
    depth: usize,
}

impl<'a, 'tcx> LifetimeInstrumentation<'a, 'tcx> {
    fn new(cx: &'a driver::Ctxt<'a, 'tcx>, span_file_path: &'a str, main_path: &'a str) -> Self {
        let main_path = {
            if let ast::TyKind::Path(_, ref path) = parse_ty(cx.session(), main_path).node {
                let mut segments = path.segments.clone();
                if let Some(seg) = path.make_root() {
                    segments.insert(0, seg);
                }
                mk().path(segments)
            } else {
                panic!("Could not parse lifetime_analysis main path argument: {:?}", main_path);
            }
        };
        Self {
            cx,
            span_file_path,
            hooked_functions: HashMap::new(),
            spans: IndexSet::new(),
            depth: 0,
            main_path,
        }
    }

    fn finalize(self) -> Result<(), Error> {
        eprintln!("Writing spans to {:?}", self.span_file_path);
        let span_file = File::create(self.span_file_path)
            .context("Could not open span file")?;
        let spans: Vec<SourceSpan> = self.spans.into_iter().collect();
        bincode::serialize_into(span_file, &spans)
            .context("Span serialization failed")?;
        Ok(())
    }

    /// Check if the callee expr is a function we've hooked. Returns the name of
    /// the function and its declaration if found.
    fn hooked_fn(&self, callee: &ast::Expr) -> Option<(Ident, &ast::FnDecl)> {
        match &callee.node {
            ast::ExprKind::Path(None, path)
                if path.segments.len() == 1 =>
            {
                self.hooked_functions
                    .get(&path.segments[0].ident)
                    .and_then(|decl| Some((path.segments[0].ident, &**decl)))
            }
            _ => None,
        }
    }

    /// If ty is a Ptr type, return a new expr that is a cast of expr to usize,
    /// otherwise just return a clone of expr.
    fn add_ptr_cast(&self, expr: &P<ast::Expr>, ty: &ast::Ty) -> P<ast::Expr> {
        match ty.node {
            ast::TyKind::Ptr(_) => mk().cast_expr(expr, mk().ident_ty("usize")),
            _ => expr.clone(),
        }
    }

    fn get_source_location_idx(&mut self, span: Span) -> usize {
        let lo = self.cx.session().source_map().lookup_byte_offset(span.lo());
        let hi = self.cx.session().source_map().lookup_byte_offset(span.hi());

        if lo.sf.start_pos != hi.sf.start_pos {
            self.cx.session().span_err(span, "Location crosses source files");
        }
        let file_path = match &lo.sf.name {
            FileName::Real(path) => path.to_owned(),
            _ => {
                self.cx.session().span_err(span, "Location does not refer to a source file");
                unreachable!()
            }
        };

        let source_span = SourceSpan::new(file_path, BytePos(lo.pos.0), BytePos(hi.pos.0));

        let (idx, _) = self.spans.insert_full(source_span);
        idx
    }

    fn instrument_entry_block(&self, block: P<ast::Block>) -> P<ast::Block> {
        let init_stmt = mk().semi_stmt(
            mk().call_expr(
                mk().path_expr(vec!["c2rust_analysis_rt", "init"]),
                vec![mk().lit_expr(mk().str_lit(self.span_file_path))],
            )
        );
        block.map(|mut block| {
            block.stmts.insert(0, init_stmt);
            block
        })
    }

    fn instrument_main_block(&self, block: P<ast::Block>) -> P<ast::Block> {
        let init_stmt = mk().local_stmt(
            P(mk().local::<_, P<ast::Ty>, _>(
                mk().ident_pat("c2rust_analysis_ctx"), None, Some(
                    mk().call_expr(
                        mk().path_expr(vec!["c2rust_analysis_rt", "context"]),
                        vec![] as Vec<P<ast::Expr>>,
                    )
                )
            ))
        );
        block.map(|mut block| {
            block.stmts.insert(0, init_stmt);
            block
        })
    }

    fn instrument_expr(&self, expr: P<ast::Expr>, stmts: &[ast::Stmt]) -> P<ast::Expr> {
        let local = P(mk().local::<_, P<ast::Ty>, _>(
            mk().ident_pat("ret"), None, Some(expr)
        ));

        let mut block_stmts = vec![mk().local_stmt(local)];
        block_stmts.extend(stmts.into_iter().cloned());
        block_stmts.push(mk().expr_stmt(mk().path_expr(vec!["ret"])));

        // Build the instrumentation block
        return mk().block_expr(mk().block(block_stmts));
    }

    fn instrument_expr_call_rt<I>(
        &mut self,
        expr: P<ast::Expr>,
        span: Span,
        fn_name: I,
        args: &[P<ast::Expr>]
    ) -> P<ast::Expr>
    where I: Make<Ident>
    {
        let source_loc_idx = self.get_source_location_idx(span);
        let mut hook_args = vec![mk().lit_expr(mk().int_lit(source_loc_idx as u128, "usize"))];
        hook_args.extend(args.into_iter().cloned());
        let call = mk().call_expr(
            mk().path_expr(vec![
                mk().ident("c2rust_analysis_rt"),
                mk().ident(fn_name),
            ]),
            hook_args,
        );
        self.instrument_expr(expr, &[mk().semi_stmt(call)])
    }

    fn is_constant(&self, expr: &ast::Expr) -> bool {
        let mut constant = true;
        visit_nodes(expr, |e: &ast::Expr| {
            match e.node {
                // These expressions are const if composed of const expressions
                ast::ExprKind::Box(..)
                | ast::ExprKind::ObsoleteInPlace(..)
                | ast::ExprKind::Array(..)
                | ast::ExprKind::Tup(..)
                | ast::ExprKind::Binary(..)
                | ast::ExprKind::Unary(..)
                | ast::ExprKind::Lit(..)
                | ast::ExprKind::Cast(..)
                | ast::ExprKind::Type(..)
                | ast::ExprKind::AddrOf(..)
                | ast::ExprKind::Struct(..)
                | ast::ExprKind::Repeat(..)
                | ast::ExprKind::Paren(..) => (),

                // Assume everything else is non-const. This may be an
                // overapproximation, but that's alright.
                _ => {
                    constant = false;
                }
            }
        });
        constant
    }
}

impl<'a, 'tcx> Folder for LifetimeInstrumentation<'a, 'tcx> {
    fn fold_foreign_item_simple(&mut self, item: ast::ForeignItem) -> ast::ForeignItem {
        if let ast::ForeignItemKind::Fn(decl, _) = &item.node {
            if HOOK_FUNCTIONS.contains(&&*item.ident.name.as_str()) {
                self.hooked_functions.insert(item.ident, decl.clone());
            }
        }
        self.depth += 1;
        let folded = fold::noop_fold_foreign_item_simple(item, self);
        self.depth -= 1;
        folded
    }

    fn fold_expr(&mut self, expr: P<ast::Expr>) -> P<ast::Expr> {
        // Post-order traversal so we instrument any arguments before processing
        // the expr.
        let expr = expr.map(|expr| fold::noop_fold_expr(expr, self));

        match &expr.node {
            ast::ExprKind::Call(callee, args) => {
                if let Some((fn_name, decl)) = self.hooked_fn(callee) {
                    // Add all original arguments, casting pointers to usize
                    let mut args: Vec<P<ast::Expr>> = args
                        .iter()
                        .zip(decl.inputs.iter())
                        .map(|(arg, arg_decl)| self.add_ptr_cast(arg, &arg_decl.ty))
                        .collect();
                    // Add the return value of the hooked call.
                    args.push({
                        let ret_expr = mk().path_expr(vec!["ret"]);
                        match &decl.output {
                            ast::FunctionRetTy::Ty(ty) => self.add_ptr_cast(&ret_expr, ty),
                            _ => ret_expr,
                        }
                    });

                    return self.instrument_expr_call_rt(expr.clone(), expr.span, fn_name, &args);
                }

                let mut instrumented = false;
                let args = args.into_iter().map(|arg| {
                    if self.cx.node_type(arg.id).is_unsafe_ptr() && !self.is_constant(arg) {
                        instrumented = true;
                        println!("Instrumenting arg: {:?}", arg);
                        self.instrument_expr_call_rt(
                            arg.clone(),
                            arg.span,
                            "ptr_arg",
                            &[mk().cast_expr(
                                mk().path_expr(vec!["ret"]), mk().ident_ty("usize"),
                            )],
                        )
                    } else {
                        arg.clone()
                    }
                }).collect();
                if instrumented {
                    return mk().call_expr(callee, args);
                }
            }
            ast::ExprKind::Unary(ast::UnOp::Deref, ptr_expr) =>
                if self.cx.node_type(ptr_expr.id).is_unsafe_ptr()
            {
                return mk().unary_expr(
                    "*",
                    self.instrument_expr_call_rt(
                        ptr_expr.clone(),
                        expr.span,
                        "ptr_deref",
                        &[mk().cast_expr(mk().path_expr(vec!["ret"]), mk().ident_ty("usize"))],
                    ),
                );
            }
            ast::ExprKind::Assign(lhs, rhs) =>
                if self.cx.node_type(lhs.id).is_unsafe_ptr()
            {
                return mk().assign_expr(
                    lhs,
                    self.instrument_expr_call_rt(
                        rhs.clone(),
                        expr.span,
                        "ptr_assign",
                        &[mk().cast_expr(mk().path_expr(vec!["ret"]), mk().ident_ty("usize"))],
                    ),
                );
            }
            _ => (),
        }

        expr
    }

    fn fold_item_simple(&mut self, item: ast::Item) -> ast::Item {
        self.depth += 1;
        let item = fold::noop_fold_item_simple(item, self);
        self.depth -= 1;

        // Instrument entry point if found
        match entry::entry_point_type(&item, self.depth) {
            entry::EntryPointType::MainNamed |
            entry::EntryPointType::MainAttr |
            entry::EntryPointType::Start => {
                return ast::Item {
                    node: {
                        if let ast::ItemKind::Fn(decl, header, generics, block) = item.node {
                            ast::ItemKind::Fn(decl, header, generics, {
                                self.instrument_entry_block(block)
                            })
                        } else {
                            panic!("Expected a function item");
                        }
                    },
                    ..item
                };
            }
            _ => (),
        }

        // Instrument the real main function
        if let ast::ItemKind::Fn(decl, header, generics, block) = item.node.clone() {
            if self.cx.def_path(self.cx.node_def_id(item.id)).ast_equiv(&self.main_path) {
                return ast::Item {
                    node: ast::ItemKind::Fn(decl, header, generics, {
                        self.instrument_main_block(block)
                    }),
                    ..item
                };
            }
        }

        item
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("lifetime_analysis", |args| mk(LifetimeAnalysis {
        span_file_path: args[0].clone(),
        main_path: args[1].clone(),
    }));
}
