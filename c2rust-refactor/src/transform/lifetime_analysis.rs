use log::{debug, warn};
use std::collections::{BTreeMap, HashMap};
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::fs::File;
use std::iter::FromIterator;
use std::ops::{Bound, Deref, DerefMut};
use std::path::Path;
use std::u32;

use rustc_hir::{self, HirId, DUMMY_HIR_ID};
use rustc_errors::Level;
use smallvec::SmallVec;
use rustc_ast::{ast, entry};
use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_span::symbol::{kw, Ident, Symbol};
use rustc_middle::ty;
use rustc_span::{Span, FileName, BytePos, Pos, DUMMY_SP};
use rustc_span::hygiene::SyntaxContext;

use indexmap::IndexSet;
use failure::{Error, ResultExt};
use petgraph::dot::Dot;
use petgraph::graph::{Graph, NodeIndex};
use petgraph::visit::{IntoNodeIdentifiers};

use c2rust_analysis_rt::{SourceSpan, SourcePos, SpanId};
use c2rust_analysis_rt::events::{Pointer, Event, EventKind};
use crate::ast_builder::{mk, Make};

use crate::ast_manip::{lr_expr, map_ast_unified, visit_nodes, AstEquiv, AstNodeRef, UnifiedAstMap};
use crate::ast_manip::fn_edit::{visit_fns, FnLike};
use crate::context::RefactorCtxt;
use crate::command::{Command, CommandState, RefactorState, Registry};
use crate::driver::{parse_ty, Phase};
use crate::expect;
use crate::reflect;
use crate::transform::Transform;
use crate::util::Lone;


struct InstrumentCmd {
    span_file_path: String,
    main_path: String,
}

impl Transform for InstrumentCmd {
    fn transform(&self, krate: &mut ast::Crate, _st: &CommandState, cx: &RefactorCtxt) {
        let mut folder = LifetimeInstrumenter::new(cx, &self.span_file_path, &self.main_path, krate);
        let folded = folder.visit_crate(krate);
        folder.finalize().expect("Error instrumenting lifetimes");
        folded
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

/// List of functions we want hooked for the lifetime analysis runtime (see
/// ../../runtime/src/lib.rs for the implementations of these hooks)
const HOOK_FUNCTIONS: &[&'static str] = c2rust_analysis_rt::HOOK_FUNCTIONS;

trait GetPointerArg {
    /// Get the input pointer expression out of `ast_node`
    fn get_ptr_expr<'a>(&self, ast_node: AstNodeRef<'a>) -> Option<&'a ast::Expr>;
}

impl GetPointerArg for EventKind {
    fn get_ptr_expr<'a>(&self, ast_node: AstNodeRef<'a>) -> Option<&'a ast::Expr> {
        match self {
            EventKind::Alloc{..} => None,
            EventKind::Free{..} | EventKind::Realloc{..} => {
                let expr: &ast::Expr = ast_node.try_into().unwrap();
                let args = expect!([&expr.kind] ast::ExprKind::Call(_, args) => args);
                Some(&args[0])
            }
            EventKind::Arg{..} | EventKind::Assign{..} | EventKind::Deref{..} | EventKind::Ret{..} => {
                let expr: &ast::Expr = ast_node.try_into().unwrap();
                Some(get_ptr_expr(expr))
            }
            EventKind::Done => None,
        }
    }
}

/// Get the final value of a block
fn get_block_value(block: &ast::Block) -> &ast::Expr {
    let last_stmt = block
        .stmts
        .last()
        .expect("Instrumented block expression must have a non-trivial value");
    if let ast::StmtKind::Expr(last_expr) = &last_stmt.kind {
        last_expr
    } else {
        panic!("Instrumented block expression must have a non-trivial value");
    }
}

fn get_block_value_mut(block: &mut ast::Block) -> &mut P<ast::Expr> {
    let last_stmt = block
        .stmts
        .last_mut()
        .expect("Instrumented block expression must have a non-trivial value");
    if let ast::StmtKind::Expr(last_expr) = &mut last_stmt.kind {
        last_expr
    } else {
        panic!("Instrumented block expression must have a non-trivial value");
    }
}

fn get_ptr_expr(expr: &ast::Expr) -> &ast::Expr {
    match &expr.kind {
        ast::ExprKind::Cast(e, _) | ast::ExprKind::Type(e, _) | ast::ExprKind::Paren(e) => {
            get_ptr_expr(&e)
        }

        ast::ExprKind::Loop(block, _) | ast::ExprKind::Block(block, _)
        | ast::ExprKind::TryBlock(block) => {
            get_block_value(&block)
        }

        ast::ExprKind::Unary(ast::UnOp::Deref, e) => e,

        _ => expr,
    }
}

fn instrumented_inner_value(expr: &ast::Expr) -> &ast::Expr {
    if let ast::ExprKind::Block(block, _) = &expr.kind {
        if let ast::StmtKind::Local(local) = &block.stmts[0].kind {
            if let Some(init) = &local.init {
                return init;
            }
        }
    }

    expr
}

struct LifetimeInstrumenter<'a, 'tcx: 'a> {
    cx: &'a RefactorCtxt<'a, 'tcx>,
    span_file_path: &'a str,
    main_path: ast::Path,
    hooked_functions: HashMap<Symbol, P<ast::FnDecl>>,

    spans: IndexSet<SourceSpan>,
    depth: usize,
}

impl<'a, 'tcx> LifetimeInstrumenter<'a, 'tcx> {
    fn new(cx: &'a RefactorCtxt<'a, 'tcx>, span_file_path: &'a str, main_path: &'a str, krate: &ast::Crate) -> Self {
        let main_path = {
            if let ast::TyKind::Path(_, mut path) = parse_ty(cx.session(), main_path)
                .into_inner()
                .kind
            {
                if !path.segments[0].ident.is_path_segment_keyword() {
                    path.segments.insert(0, mk().path_segment(kw::Crate));
                }
                path
            } else {
                panic!("Could not parse lifetime_analysis main path argument: {:?}", main_path);
            }
        };
        let mut hooked_functions = HashMap::new();
        visit_fns(krate, |function: FnLike| {
            if HOOK_FUNCTIONS.contains(&&*function.ident.name.as_str()) {
                hooked_functions.insert(function.ident.name, function.decl.clone());
            }
        });
        Self {
            cx,
            span_file_path,
            hooked_functions,
            spans: IndexSet::new(),
            depth: 0,
            main_path,
        }
    }

    fn finalize(self) -> Result<(), Error> {
        debug!("Writing spans to {:?}", self.span_file_path);
        let span_file = File::create(self.span_file_path)
            .context("Could not open span file")?;
        let spans: Vec<SourceSpan> = self.spans.into_iter().collect();
        bincode::serialize_into(span_file, &spans)
            .context("Span serialization failed")?;
        Ok(())
    }

    /// Check if the callee expr is a function we've hooked. Returns the name of
    /// the function and its declaration if found.
    fn hooked_fn(&self, fn_hir_id: HirId) -> Option<(Ident, &ast::FnDecl)> {
        match self.cx.hir_map().find_by_hir_id(fn_hir_id) {
            Some(hir::Node::ForeignItem(item)) => {
                self.hooked_functions
                    .get(&item.ident.name)
                    .and_then(|decl| Some((item.ident, &**decl)))
            }
            Some(hir::Node::Item(item)) => {
                self.hooked_functions
                    .get(&item.ident.name)
                    .and_then(|decl| Some((item.ident, &**decl)))
            }
            _ => None
        }
    }

    /// If ty is a Ptr type, return a new expr that is a cast of expr to usize,
    /// otherwise just return a clone of expr.
    fn add_ptr_cast(&self, expr: &P<ast::Expr>, ty: &ast::Ty) -> P<ast::Expr> {
        match ty.kind {
            ast::TyKind::Ptr(_) => mk().cast_expr(expr, mk().ident_ty("usize")),
            _ => expr.clone(),
        }
    }

    fn get_source_location_idx(&mut self, span: Span) -> SpanId {
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

        let source_span = SourceSpan::new(file_path, SourcePos(lo.pos.0), SourcePos(hi.pos.0));

        let (idx, _) = self.spans.insert_full(source_span);
        u32::try_from(idx).unwrap()
    }

    fn instrument_entry_block(&self, block: &mut P<ast::Block>) {
        let init_stmt = mk().semi_stmt(
            mk().call_expr(
                mk().path_expr(vec!["c2rust_analysis_rt", "init"]),
                vec![mk().lit_expr(self.span_file_path)],
            )
        );
        block.stmts.insert(0, init_stmt);
    }

    fn instrument_main_block(&self, block: &mut P<ast::Block>) {
        let init_stmt = mk().local_stmt(
            P(mk().local::<_, P<ast::Ty>, _>(
                mk().ident_pat("c2rust_analysis_ctx"), None, Some(
                    mk().call_expr(
                        mk().path_expr(vec!["c2rust_analysis_rt", "context"]),
                        vec![],
                    )
                )
            ))
        );
        block.stmts.insert(0, init_stmt);
    }

    fn instrument_expr_block(&self, expr: &mut P<ast::Expr>, stmts: &[ast::Stmt]) {
        let local = P(mk().local::<_, P<ast::Ty>, _>(
            mk().ident_pat("ret"), None, Some(expr.clone())
        ));

        let mut block_stmts = vec![mk().local_stmt(local)];
        block_stmts.extend(stmts.into_iter().cloned());
        block_stmts.push(mk().expr_stmt(mk().path_expr(vec!["ret"])));

        // Build the instrumentation block
        *expr = mk().block_expr(mk().block(block_stmts));
    }

    fn instrument_expr_call<I>(
        &mut self,
        span: Span,
        fn_name: I,
        args: &[P<ast::Expr>]
    ) -> P<ast::Expr>
    where I: Make<Ident>
    {
        let source_loc_idx = self.get_source_location_idx(span);
        let mut hook_args = vec![mk().lit_expr(mk().int_lit(source_loc_idx as u128, "u32"))];
        hook_args.extend(args.into_iter().cloned());
        mk().call_expr(
            mk().path_expr(vec![
                mk().ident("c2rust_analysis_rt"),
                mk().ident(fn_name),
            ]),
            hook_args,
        )
    }

    fn instrument_expr_use<I>(&mut self, expr: &mut P<ast::Expr>, fn_name: I)
        where I: Make<Ident> + Copy
    {
        match &mut expr.kind {
            ast::ExprKind::If(_, true_block, else_block) => {
                self.instrument_expr_use(get_block_value_mut(true_block), fn_name);
                if let Some(else_block) = else_block {
                    self.instrument_expr_use(else_block, fn_name);
                }
                return;
            }

            ast::ExprKind::While(..)
            | ast::ExprKind::ForLoop(..) => {
                panic!("Unexpected loop expression without value: {:?}", expr);
            }

            ast::ExprKind::Match(_, arms) => {
                for arm in arms.iter_mut() {
                    self.instrument_expr_use(&mut arm.body, fn_name);
                }
                return;
            }

            _ => {}
        }

        let inner = instrumented_inner_value(expr);

        // Insert the instrumentation block
        debug!("Instrumenting expression: {:?}", inner);
        let span = inner.span;

        // `ret` is the local created inside the instrumentation block
        // by `instrument_expr_block`
        let mut value = mk().path_expr(vec!["ret"]);
        if let Some(ty::TyKind::Ref(_, ty, _)) = self.cx
            .opt_node_type(inner.id)
            .map(|x| &x.kind)
        {
            // Cast the reference to a raw pointer
            value = mk().cast_expr(
                value,
                mk().ptr_ty(reflect::reflect_tcx_ty(self.cx.ty_ctxt(), ty)),
            );
        }
        let call = self.instrument_expr_call(
            span,
            fn_name,
            &[mk().cast_expr(value, mk().ident_ty("usize"))],
        );
        if inner.id != expr.id {
            if let ast::ExprKind::Block(block, _) = &mut expr.kind {
                // Insert the new instrumentation call right before the
                // return value
                let index = block.stmts.len()-1;
                block.stmts.insert(index, mk().semi_stmt(call));
            } else {
                panic!("Expected a block for already instrumented expression: {:?}", expr);
            }
        } else {
            // Make a new instrumentation block
            self.instrument_expr_block(expr, &[mk().semi_stmt(call)]);
        }
    }

    fn is_constant(&self, expr: &ast::Expr) -> bool {
        let mut constant = true;
        visit_nodes(expr, |e: &ast::Expr| {
            match e.kind {
                // These expressions are const if composed of const expressions
                ast::ExprKind::Box(..)
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

impl<'a, 'tcx> MutVisitor for LifetimeInstrumenter<'a, 'tcx> {
    // fn flat_map_foreign_item(&mut self, item: P<ast::ForeignItem>) -> SmallVec<[P<ast::ForeignItem>; 1]> {
    //     if let ast::ForeignItemKind::Fn(box Fn { sig, .. }) = &item.node {
    //         if HOOK_FUNCTIONS.contains(&&*item.ident.name.as_str()) {
    //             self.hooked_functions.insert(item.ident.name, sig.decl.clone());
    //         }
    //     }
    //     self.depth += 1;
    //     let folded = mut_visit::noop_flat_map_foreign_item(item, self);
    //     self.depth -= 1;
    //     folded
    // }

    fn visit_expr(&mut self, expr: &mut P<ast::Expr>) {
        // Post-order traversal so we instrument any arguments before processing
        // the expr.
        mut_visit::noop_visit_expr(expr, self);

        // Don't re-instrument a value
        if expr.span == DUMMY_SP {
            return;
        }

        if let ast::ExprKind::Call(callee, args) = &expr.kind {
            if let Some(def) = self.cx.try_resolve_expr_to_hid(callee) {
                if let Some((fn_name, decl)) = self.hooked_fn(def) {
                    // Add all original arguments, casting pointers to usize
                    let mut args: Vec<P<ast::Expr>> = args
                        .iter()
                        .zip(decl.inputs.iter())
                    // .filter(|(_, arg_decl)| {
                    //     // We don't want to pass ADT types to the handlers,
                    //     // since they can't define a correct argument type for
                    //     // ADTs in the instrumented program.
                    //     if let Some(ty) = self.cx.opt_node_type(arg_decl.ty.id) {
                    //         if let ty::Adt(..) = ty.sty {
                    //             return false;
                    //         }
                    //     }
                    //     true
                    // })
                        .map(|(arg, arg_decl)| self.add_ptr_cast(arg, &arg_decl.ty))
                        .collect();
                    // Add the return value of the hooked call.
                    args.push({
                        let ret_expr = mk().path_expr(vec!["ret"]);
                        match &decl.output {
                            ast::FnRetTy::Ty(ty) => self.add_ptr_cast(&ret_expr, ty),
                            _ => ret_expr,
                        }
                    });

                    let call = self.instrument_expr_call(expr.span, fn_name, &args);
                    self.instrument_expr_block(expr, &[mk().semi_stmt(call)]);
                    return;
                }
            }
        }

        match &mut expr.kind {
            ast::ExprKind::Call(_callee, args) => {
                for arg in args.iter_mut() {
                    if self.cx.node_type(arg.id).is_unsafe_ptr() && !self.is_constant(arg) {
                        debug!("Instrumenting arg: {:?}", arg);
                        self.instrument_expr_use(arg, "ptr_arg");
                    }
                }
            }
            ast::ExprKind::Unary(ast::UnOp::Deref, ptr_expr) => {
                let inner = instrumented_inner_value(ptr_expr);
                if self.cx.node_type(inner.id).is_unsafe_ptr() {
                    self.instrument_expr_use(ptr_expr, "ptr_deref");
                }
            }
            ast::ExprKind::Assign(lhs, rhs) => {
                // We assume that we never instrument the left-hand side of an
                // assignment here. That should hold true but if it ever does
                // not, we should grab its inner value.
                assert_ne!(lhs.span, DUMMY_SP);

                if self.cx.node_type(lhs.id).is_unsafe_ptr() {
                    self.instrument_expr_use(rhs, "ptr_assign");
                }
            }
            ast::ExprKind::Ret(Some(ret)) => {
                let inner = instrumented_inner_value(ret);
                if self.cx.node_type(inner.id).is_unsafe_ptr() {
                    self.instrument_expr_use(ret, "ptr_ret");
                }
            }
            _ => {}
        }
    }

    fn flat_map_stmt(&mut self, mut stmt: ast::Stmt) -> SmallVec<[ast::Stmt; 1]> {
        match &mut stmt.kind {
            ast::StmtKind::Local(local) => {
                // TODO: We don't handle @ subpattern patterns or let binding
                // decomposition yet.
                if let ast::PatKind::Ident(binding, _local_ident, None) = local.pat.kind {
                    let is_unsafe_ptr = self.cx.opt_node_type(local.id)
                        .map_or(false, |ty| ty.is_unsafe_ptr());
                    if let Some(init) = &mut local.init {
                        if is_unsafe_ptr {
                            if let ast::BindingMode::ByRef(_) = binding {
                                // We can only handle taking a reference to a
                                // directly dereferenced value for now. As this
                                // is the only kind of reference the translator
                                // creates, that's fine.
                                if let ast::ExprKind::Unary(ast::UnOp::Deref, ptr_value) = &mut init.kind {
                                    self.instrument_expr_use(ptr_value, "ptr_assign");
                                }
                            } else {
                                self.instrument_expr_use(init, "ptr_assign");
                            }
                        }
                    }
                }
            }
            _ => {}
        }

        // TODO: Instrument implicit function returns (block with Expr at end)

        // We do pre-order traversal because we want let ref mut x = *foo to be
        // a pointer assignment rather than a deref.
        mut_visit::noop_flat_map_stmt(stmt, self)
    }

    fn flat_map_item(&mut self, item: P<ast::Item>) -> SmallVec<[P<ast::Item>; 1]> {
        if let Some(hir_id) = self.cx.hir_map().opt_node_to_hir_id(item.id) {
            // We shouldn't instrument inside any functions that we are hooking to
            // avoid double instrumenting malloc and wrapper function calls.
            if self.hooked_fn(hir_id).is_some() {
                return smallvec![item];
            }
        }

        self.depth += 1;
        let mut item: P<ast::Item> = mut_visit::noop_flat_map_item(item, self).lone();
        self.depth -= 1;

        // Instrument entry point if found
        match entry::entry_point_type(&*item, self.depth) {
            entry::EntryPointType::MainNamed |
            entry::EntryPointType::MainAttr |
            entry::EntryPointType::Start => {
                if let ast::ItemKind::Fn(_sig, _generics, block) = &mut item.kind {
                    self.instrument_entry_block(block);
                } else {
                    panic!("Expected a function item");
                };
            }
            _ => {}
        }

        let item_id = item.id;
        // Instrument the real main function
        if let ast::ItemKind::Fn(_sig, _generics, block) = &mut item.kind {
            if self.cx.def_path(self.cx.node_def_id(item_id)).ast_equiv(&self.main_path) {
                self.instrument_main_block(block);
            }
        }

        smallvec![item]
    }
}


struct AnalysisCmd {
    span_filename: String,
    log_filename: String,
}

impl Command for AnalysisCmd {
    fn run(&mut self, state: &mut RefactorState) {
        state.transform_crate(Phase::Phase3, |st, cx| {
            // Initialize the analysis runtime so we get debug pretty printing for
            // spans
            c2rust_analysis_rt::span::set_file(&self.span_filename);

            // let arena = DroplessArena::default();
            // let ownership_analysis = ownership::analyze(&st, &cx, &arena);

            let mut analyzer = LifetimeAnalyzer::new(
                cx,
                &self.span_filename,
                &self.log_filename,
                // ownership_analysis,
            );
            analyzer.run(&mut *st.krate_mut());
            analyzer.visit_crate(&mut *st.krate_mut());
        })
            .expect("Failed to run lifetime analysis");
    }
}

#[derive(Clone, Debug)]
struct EventNode {
    kind: EventKind,
    // AST node where this event occurred
    node: ast::NodeId,
}

#[derive(Clone)]
struct AllocInfo {
    base: Pointer,
    size: usize,
    events: Vec<EventNode>,
}

impl AllocInfo {
    fn new(base: Pointer, size: usize) -> Self {
        Self {
            base,
            size,
            events: vec![],
        }
    }

    fn contains(&self, ptr: Pointer) -> bool {
        ptr >= self.base && ptr - self.size < self.base
    }

    fn add_event(&mut self, kind: EventKind, node: ast::NodeId) {
        self.events.push(EventNode { kind, node });
    }
}

impl fmt::Debug for AllocInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:p}[{}]", self.base as *const u8, self.size)
    }
}

struct MemMap ( BTreeMap<Pointer, AllocInfo> );

impl MemMap {
    fn new() -> Self {
        Self ( BTreeMap::new() )
    }

    fn is_allocated(&self, ptr: Pointer) -> bool {
        self.get(&ptr).is_some()
    }

    fn get(&self, ptr: &Pointer) -> Option<&AllocInfo> {
        self.0.range((Bound::Unbounded, Bound::Included(ptr)))
            .next_back()
            .and_then(|a| {
                if a.1.contains(*ptr) {
                    Some(a.1)
                } else {
                    None
                }
            })
    }

    fn get_mut(&mut self, ptr: &Pointer) -> Option<&mut AllocInfo> {
        self.0.range_mut((Bound::Unbounded, Bound::Included(ptr)))
            .next_back()
            .and_then(|a| {
                if a.1.contains(*ptr) {
                    Some(a.1)
                } else {
                    None
                }
            })
    }

    fn alloc(&mut self, size: usize, ptr: Pointer) -> &mut AllocInfo {
        if self.is_allocated(ptr) {
            panic!("Inserting an allocation overlapping an existing allocation??");
        }

        if let Some(old) = self.0.insert(ptr, AllocInfo::new(ptr, size)) {
            panic!("Inserting an allocation at the same address as {:?}", old);
        }

        self.0.get_mut(&ptr).unwrap()
    }

    fn free(&mut self, ptr: Pointer) -> Option<AllocInfo> {
        self.0.remove(&ptr)
    }

    fn realloc(&mut self, old_ptr: Pointer, size: usize, new_ptr: Pointer) -> &mut AllocInfo {
        if old_ptr == new_ptr {
            let alloc = self.0.get_mut(&old_ptr)
                .unwrap_or_else(|| panic!("Could not realloc from {:?}", old_ptr));
            alloc.size = size;
            alloc
        } else {
            self.free(old_ptr);
            self.alloc(new_ptr, size)
        }
    }
}

impl Deref for MemMap {
    type Target = BTreeMap<Pointer, AllocInfo>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}


impl DerefMut for MemMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl fmt::Debug for MemMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list().entries(self.values()).finish()
    }
}

struct LifetimeAnalyzer<'a, 'tcx: 'a> {
    cx: &'a RefactorCtxt<'a, 'tcx>,
    log_path: &'a Path,

    spans: Vec<Span>,
    span_ids: HashMap<Span, SpanId>,

    // TODO: replace with a Vec, SpanIds are dense
    span_to_node_id: HashMap<SpanId, ast::NodeId>,

    context_map: HashMap<ast::NodeId, lr_expr::Context>,

    data_flow: DataFlowGraph,

    // _ownership_analysis: ownership::AnalysisResult<'lty, 'tcx>,

    mem_map: MemMap,
}
    
impl<'a, 'tcx> LifetimeAnalyzer<'a, 'tcx> {
    fn new(
        cx: &'a RefactorCtxt<'a, 'tcx>,
        span_file: &'a str,
        log_file: &'a str,
        // ownership_analysis: ownership::AnalysisResult<'lty, 'tcx>,
    ) -> Self {
        let file = File::open(span_file)
            .expect(&format!("Could not open span file: {:?}", span_file));
        let spans: Vec<SourceSpan> = bincode::deserialize_from(file)
            .expect("Error deserializing span file");

        let spans: Vec<Span> = spans.into_iter().map(|s: SourceSpan| {
            let filename = FileName::from(s.source.clone());
            let source_file = cx.session().source_map().get_source_file(&filename)
                .unwrap_or_else(|| panic!("Could not find source file: {:?}", filename));
            Span::new(
                source_file.start_pos + BytePos::from_u32(s.lo.to_u32()),
                source_file.start_pos + BytePos::from_u32(s.hi.to_u32()),
                SyntaxContext::root(),
                None,
            )
        }).collect();

        if spans.len() > u32::MAX as usize {
            panic!("Too many spans");
        }
        let span_ids = HashMap::from_iter(
            spans.iter().enumerate().map(|(i, v)| (*v, i as SpanId))
        );
        
        Self {
            cx: cx,
            log_path: Path::new(log_file),
            spans,
            span_ids,
            span_to_node_id: HashMap::new(),
            context_map: HashMap::new(),
            data_flow: DataFlowGraph::new(),
            // _ownership_analysis: ownership_analysis,
            mem_map: MemMap::new(),
        }
    }

    fn run(&mut self, krate: &mut ast::Crate) {
        // Construct a mapping from expressions to spans. It is important that
        // we visit statements before expressions, as we want the expression for
        // most statements.
        visit_nodes(krate, |e: &ast::Stmt| {
            if let ast::StmtKind::Local(..) = e.kind {
                if let Some(id) = self.span_ids.get(&e.span) {
                    if self.span_to_node_id.insert(*id, e.id).is_some() {
                        warn!("Duplicate node for span {:?}", e.span);
                    }
                }
            }
        });
        visit_nodes(krate, |e: &ast::Expr| {
            if let Some(id) = self.span_ids.get(&e.span) {
                if self.span_to_node_id.insert(*id, e.id).is_some() {
                    warn!("Duplicate node for span {:?}", e.span);
                }
            }
        });

        lr_expr::fold_exprs_with_context(krate, |expr, ctx| {
            self.context_map.insert(expr.id, ctx);
        });

        let ast_map = map_ast_unified(krate);

        let mut log_file = File::open(&self.log_path)
            .expect("Could not open instrumentation log file");
        while let Ok(event) = bincode::deserialize_from(&mut log_file) as bincode::Result<Event> {
            let span = self.spans[event.span as usize];
            debug!("{:?} {:?}", span, event.kind);

            let node_id = *match self.span_to_node_id.get(&event.span) {
                Some(id) => id,
                _ => {
                    let span = self.spans[event.span as usize];
                    panic!("Could not find expression corresponding to span: {:?}\nMaybe the source code is out of sync with the event log?", span);
                }
            };

            match event.kind {
                EventKind::Alloc { size, ptr } => {
                    self.mem_map.alloc(size, ptr)
                        .add_event(event.kind, node_id);
                }
                EventKind::Free { ptr } => {
                    if let Some(mut info) = self.mem_map.free(ptr) {
                        info.add_event(event.kind, node_id);
                        self.process_chain(&info.events, &ast_map);
                    }
                }
                EventKind::Realloc { old_ptr, size, new_ptr } => {
                    self.mem_map.realloc(old_ptr, size, new_ptr)
                        .add_event(event.kind, node_id);
                }
                EventKind::Assign(ptr) |
                EventKind::Arg(ptr) |
                EventKind::Ret(ptr) |
                EventKind::Deref(ptr) => {
                    match self.mem_map.get_mut(&ptr) {
                        Some(info) => info.add_event(event.kind, node_id),
                        None => {
                            warn!("Warning: Could not find allocation for {:?}", event);
                        }
                    }
                }
                EventKind::Done => continue,
            };

            debug!("{:?}", self.mem_map);
        }

        // Process memory leaks
        for (_addr, mut info) in self.mem_map.clone().into_iter() {
            info.add_event(EventKind::Done, ast::DUMMY_NODE_ID);
            self.process_chain(&info.events, &ast_map);
        }

        debug!("{:?}", self.data_flow.dot());
        let owned_paths = self.data_flow.find_owned_paths();
        for path in &owned_paths {
            let path_str = path.iter()
                .map(|node| {
                    let node_id = self.cx.hir_map().hir_to_node_id(node.id);
                    if let Some(ast_node) = ast_map.get(&node_id) {
                        format!("{:?}", ast_node)
                    } else {
                        "??".to_string()
                    }
                })
                .collect::<Vec<_>>()
                .as_slice()
                .join(" -> ");
            debug!("{}", path_str);

            let node_id = self.cx.hir_map().hir_to_node_id(path[0].id);
            let alloc_span = ast_map.get_ast::<ast::Expr>(&node_id).unwrap().span;
            let mut diagnostic = self.cx.make_diagnostic(
                Level::Note,
                "Found candidate for Boxing",
            );
            diagnostic.set_span(alloc_span);
            diagnostic.emit();
        }
    }

    fn canonical_def(&self, expr: &ast::Expr) -> hir::HirId {
        match &expr.kind {
            ast::ExprKind::Cast(sub, _) => self.canonical_def(sub),
            
            _ => {
                if let Some(hir_id) = self.cx.try_resolve_expr_to_hid(expr) {
                    return hir_id;
                }

                self.cx.hir_map().node_to_hir_id(expr.id)
            }
        }
    }

    /// Look up the canonical definition of `expr` in `sources`.
    fn get_source(&self, sources: &HashMap<HirId, NodeIndex>, expr: &ast::Expr) -> Option<NodeIndex> {
        if let ast::ExprKind::Call(callee, _) = &expr.kind {
            debug!("    Callee node id: {:?}", self.cx.hir_map().hir_to_node_id(self.canonical_def(callee)));
            if let Some(source) = sources.get(&self.canonical_def(callee)) {
                return Some(*source);
            }
        }
        sources.get(&self.canonical_def(expr)).copied()
    }

    /// Process the sequence of events in `events`, adding nodes and edges to
    /// `self.data_flow`. All events must refer to the same allocation.
    fn process_chain(&mut self, events: &[EventNode], ast_map: &UnifiedAstMap) {
        debug!("Processing chain of memory events:");
        let mut allocation = None;

        let mut sources: HashMap<HirId, NodeIndex> = HashMap::new();

        for event in events {
            debug!(
                "  Event: {:?} @ {:?}",
                event.kind,
                ast_map.get(&event.node),
            );
            let input_ptr = if let EventKind::Done = event.kind {
                None
            } else {
                event.kind.get_ptr_expr(*ast_map.get(&event.node).unwrap())
            };
            match event.kind {
                EventKind::Alloc{..} => {
                    let id = self.cx.hir_map().node_to_hir_id(event.node);
                    let alloc_node = self.data_flow.get_node(id, DataFlowNodeKind::Alloc);
                    allocation = Some(alloc_node);
                    sources.insert(id, alloc_node);
                }
                EventKind::Realloc{..} => {
                    let id = self.cx.hir_map().node_to_hir_id(event.node);
                    let realloc_node = self.data_flow.get_node(id, DataFlowNodeKind::Realloc);
                    sources.insert(id, realloc_node);
                    allocation = Some(realloc_node);

                    let input_ptr = input_ptr.expect("Could not identify input pointer to realloc");
                    if let Some(source) = self.get_source(&sources, input_ptr) {
                        self.data_flow.add_flow(source, realloc_node, DataFlowEdge::Use);
                    }
                }
                EventKind::Free{..} | EventKind::Deref(..) => {
                    let input_ptr = input_ptr.expect("Could not identify input pointer to free or deref");
                    if let Some(source) = self.get_source(&sources, input_ptr) {
                        let id = self.cx.hir_map().node_to_hir_id(event.node);
                        let node_kind = match event.kind {
                            EventKind::Free{..} => DataFlowNodeKind::Free,
                            EventKind::Deref{..} => DataFlowNodeKind::Deref,
                            _ => panic!("Unexpected event kind"),
                        };
                        let new_node = self.data_flow.get_node(id, node_kind);
                        self.data_flow.add_flow(source, new_node, DataFlowEdge::Use);
                    } else {
                        warn!("    Did not find def {:?} for in sources", input_ptr);
                    }
                }
                EventKind::Ret{..} => {
                    let input_ptr = input_ptr.expect("Could not identify input pointer to ret");
                    if let Some(source) = self.get_source(&sources, input_ptr) {
                        let id = self.cx.hir_map().node_to_hir_id(event.node);
                        let fn_id = self.cx.hir_map().get_parent_item(id);
                        let dest = self.data_flow.get_node(fn_id, DataFlowNodeKind::Ret);
                        sources.insert(fn_id, dest);
                        self.data_flow.add_flow(source, dest, DataFlowEdge::Use);
                    } else {
                        warn!("    Did not find def {:?} for in sources", input_ptr);
                    }
                }                    
                EventKind::Done => {
                    let leak_node = self.data_flow.get_node(DUMMY_HIR_ID, DataFlowNodeKind::Leak);
                    self.data_flow.add_flow(allocation.unwrap(), leak_node, DataFlowEdge::Use);
                }
                EventKind::Assign(..) => {
                    let input_ptr = input_ptr.expect("Could not identify input pointer to assignment");
                    let source = self.get_source(&sources, input_ptr);
                    let mut hir_id = self.cx.hir_map().node_to_hir_id(event.node);
                    // Walk up from the assigned value to the assignment node
                    loop {
                        let node_id = self.cx.hir_map().hir_to_node_id(hir_id);
                        if let Some(expr) = ast_map.get_ast::<ast::Expr>(&node_id) {
                            if let ast::ExprKind::Assign(..) = &expr.kind {
                                break;
                            }
                        } else if let Some(_stmt) = ast_map.get_ast::<ast::Stmt>(&node_id) {
                            break;
                        }
                        hir_id = self.cx.hir_map().get_parent_node(hir_id);
                    }
                    let node_id = self.cx.hir_map().hir_to_node_id(hir_id);
                    if let Some(expr) = ast_map.get_ast::<ast::Expr>(&node_id) {
                        let lhs = expect!([expr.kind] ast::ExprKind::Assign(ref lhs, _) => lhs);
                        let dest_id = self.canonical_def(lhs);
                        let dest = self.data_flow.get_node(
                            dest_id,
                            DataFlowNodeKind::from_lvalue(&lhs.kind),
                        );
                        if let Some(source) = source {
                            self.data_flow.add_flow(source, dest, DataFlowEdge::Assign);
                        }
                        sources.insert(dest_id, dest);
                    } else if let Some(stmt) = ast_map.get_ast::<ast::Stmt>(&node_id) {
                        let local = expect!([&stmt.kind] ast::StmtKind::Local(local) => local);
                        let id = self.cx.hir_map().node_to_hir_id(local.pat.id);
                        let dest = self.data_flow.get_node(id, DataFlowNodeKind::Local);
                        if let Some(source) = source {
                            self.data_flow.add_flow(source, dest, DataFlowEdge::Assign);
                        }
                        sources.insert(id, dest);
                    } else {
                        panic!("Unexpected AST node kind for event {:?}", event);
                    }
                },
                EventKind::Arg(..) => {
                    let input_ptr = input_ptr.expect("Could not identify input pointer to argument");
                    let mut hir_id = self.cx.hir_map().node_to_hir_id(event.node);
                    hir_id = self.cx.hir_map().get_parent_node(hir_id);
                    // Walk up to the call
                    let (callee, call_args) = loop {
                        let node_id = self.cx.hir_map().hir_to_node_id(hir_id);
                        if let Some(expr) = ast_map.get_ast::<ast::Expr>(&node_id) {
                            if let ast::ExprKind::Call(callee, call_args) = &expr.kind {
                                break (callee, call_args);
                            }
                        }
                        hir_id = self.cx.hir_map().get_parent_node(hir_id);
                    };
                    let arg_index = call_args.iter().position(|arg| arg.id == event.node)
                        .expect("Could not find event node id in call argument list");
                    // let call = ast_map.get_ast::<ast::Expr>(&node_id)
                    //     .expect("Could not find call expr for argument");
                    if let Some(source) = self.get_source(&sources, input_ptr) {
                        // TODO: construct an argument node for the callee argument
                        let callee_hir_id = self.canonical_def(callee);
                        if let Some(body_id) = self.cx.hir_map().maybe_body_owned_by(callee_hir_id) {
                            let body = self.cx.hir_map().body(body_id);
                            let arg = &body.params[arg_index];
                            let arg_id = arg.pat.hir_id;
                            let dest = self.data_flow.get_node(arg_id, DataFlowNodeKind::Arg);
                            self.data_flow.add_flow(source, dest, DataFlowEdge::Use);
                            sources.insert(arg_id, dest);
                        }
                    }
                },
            };
        }
    }
}

impl<'a, 'tcx> MutVisitor for LifetimeAnalyzer<'a, 'tcx> {
}


#[derive(Copy, Clone, Debug, PartialEq)]
enum DataFlowNodeKind {
    Alloc,
    Realloc,
    Local,
    Arg,
    Deref,
    Pointer,
    Ret,
    Free,
    Leak,
}

impl DataFlowNodeKind {
    fn from_lvalue(kind: &ast::ExprKind) -> Self {
        match kind {
            ast::ExprKind::Path(..) => DataFlowNodeKind::Local,
            ast::ExprKind::Index(base, _) => DataFlowNodeKind::from_lvalue(&base.kind),
            ast::ExprKind::Field(base, _) => DataFlowNodeKind::from_lvalue(&base.kind),
            ast::ExprKind::Unary(ast::UnOp::Deref, _) => DataFlowNodeKind::Pointer,
            _ => panic!("Unexpected lvalue expression kind"),
        }
    }
}

#[derive(Clone, Debug)]
struct DataFlowNode {
    kind: DataFlowNodeKind,
    id: HirId,
}

#[derive(Copy, Clone, Debug)]
enum DataFlowEdge {
    Assign,
    Call,
    Use,
}

struct DataFlowGraph {
    graph: Graph<DataFlowNode, DataFlowEdge>,
    nodes: HashMap<HirId, Vec<NodeIndex>>,
}

impl DataFlowGraph {
    fn new() -> Self {
        Self {
            graph: Graph::new(),
            nodes: HashMap::new(),
        }
    }

    fn get_node(&mut self, id: HirId, kind: DataFlowNodeKind) -> NodeIndex {
        if let Some(indices) = self.nodes.get(&id) {
            for index in indices {
                if self.graph[*index].kind == kind {
                    return *index;
                }
            }
        }
        let new_index = self.graph.add_node(DataFlowNode { kind, id });
        self.nodes.entry(id).or_default().push(new_index);
        new_index
    }

    fn add_flow(&mut self, start: NodeIndex, end: NodeIndex, kind: DataFlowEdge) {
        self.graph.update_edge(start, end, kind);
    }

    fn dot<'a>(&'a self) -> Dot<&'a Graph<DataFlowNode, DataFlowEdge>> {
        Dot::new(&self.graph)
    }

    fn find_owned_paths(&self) -> Vec<Vec<DataFlowNode>> {
        let frees = self.graph.node_identifiers().filter(|node_id| {
            let node_kind = self.graph[*node_id].kind;
            node_kind == DataFlowNodeKind::Free || node_kind == DataFlowNodeKind::Leak
        });

        // Paths from allocation to free, where the allocation can be owned the
        // entire time.
        let mut allocation_paths = HashMap::new();

        // Allocation nodes with multiple paths to a free, so there is no clear
        // ownership path.
        let mut multiple_allocation_paths = vec![];

        for free in frees {
            let mut path = vec![self.graph[free].clone()];
            let mut cur = free;
            while self.graph[cur].kind != DataFlowNodeKind::Alloc {
                cur = self.graph
                    .neighbors_directed(cur, petgraph::Direction::Incoming)
                    .next()
                    .unwrap_or_else(|| panic!("Could not find incoming edge to {:?}", cur));

                // We don't care about derefs along the ownership path
                if self.graph[cur].kind != DataFlowNodeKind::Deref {
                    path.push(self.graph[cur].clone());
                }
            }

            // We want a path from allocation to free
            path.reverse();

            let existing = allocation_paths.insert(cur, path);
            if existing.is_some() {
                multiple_allocation_paths.push(cur);
            }
        }

        for node_id in &multiple_allocation_paths {
            allocation_paths.remove(&node_id);
        }

        allocation_paths.into_iter().map(|(_, v)| v).collect()
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("lifetime_analysis_instrument", |args| mk(InstrumentCmd {
        span_file_path: args[0].clone(),
        main_path: args[1].clone(),
    }));

    reg.register("lifetime_analysis", |args| Box::new(AnalysisCmd {
        span_filename: args[0].clone(),
        log_filename: args[1].clone(),
    }));
}
