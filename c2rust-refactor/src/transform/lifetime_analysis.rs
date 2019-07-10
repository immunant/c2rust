use std::collections::{BTreeMap, HashMap};
use std::convert::TryFrom;
use std::fmt;
use std::fs::File;
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};
use std::path::Path;
use std::u32;

use arena::SyncDroplessArena;
use smallvec::SmallVec;
use syntax::{ast, entry};
use syntax::mut_visit::{self, MutVisitor};
use syntax::ptr::P;
use syntax::symbol::{keywords, Ident};
use syntax_pos::{Span, FileName, BytePos, Pos, NO_EXPANSION};

use indexmap::IndexSet;
use failure::{Error, ResultExt};

use c2rust_analysis_rt::{SourceSpan, SourcePos, SpanId};
use c2rust_analysis_rt::events::{Pointer, Event, EventKind};
use c2rust_ast_builder::{mk, Make};

use crate::analysis::ownership;
use crate::ast_manip::{visit_nodes, AstEquiv};
use crate::context::RefactorCtxt;
use crate::command::{CommandState, Registry};
use crate::driver::{parse_ty, Phase};
use crate::transform::Transform;
use crate::util::Lone;


struct InstrumentCmd {
    span_file_path: String,
    main_path: String,
}

impl Transform for InstrumentCmd {
    fn transform(&self, krate: &mut ast::Crate, _st: &CommandState, cx: &RefactorCtxt) {
        let mut folder = LifetimeInstrumenter::new(cx, &self.span_file_path, &self.main_path);
        let folded = folder.visit_crate(krate);
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

struct LifetimeInstrumenter<'a, 'tcx: 'a> {
    cx: &'a RefactorCtxt<'a, 'tcx>,
    span_file_path: &'a str,
    main_path: ast::Path,
    hooked_functions: HashMap<Ident, P<ast::FnDecl>>,

    spans: IndexSet<SourceSpan>,
    depth: usize,
}

impl<'a, 'tcx> LifetimeInstrumenter<'a, 'tcx> {
    fn new(cx: &'a RefactorCtxt<'a, 'tcx>, span_file_path: &'a str, main_path: &'a str) -> Self {
        let main_path = {
            if let ast::TyKind::Path(_, mut path) = parse_ty(cx.session(), main_path)
                .into_inner()
                .node
            {
                if !path.segments[0].ident.is_path_segment_keyword() {
                    path.segments.insert(0, ast::PathSegment::from_ident(keywords::Crate.ident()));
                }
                path
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
                vec![mk().lit_expr(mk().str_lit(self.span_file_path))],
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
                        vec![] as Vec<P<ast::Expr>>,
                    )
                )
            ))
        );
        block.stmts.insert(0, init_stmt);
    }

    fn instrument_expr(&self, expr: &mut P<ast::Expr>, stmts: &[ast::Stmt]) {
        let local = P(mk().local::<_, P<ast::Ty>, _>(
            mk().ident_pat("ret"), None, Some(expr.clone())
        ));

        let mut block_stmts = vec![mk().local_stmt(local)];
        block_stmts.extend(stmts.into_iter().cloned());
        block_stmts.push(mk().expr_stmt(mk().path_expr(vec!["ret"])));

        // Build the instrumentation block
        *expr = mk().block_expr(mk().block(block_stmts));
    }

    fn instrument_expr_call_rt<I>(
        &mut self,
        expr: &mut P<ast::Expr>,
        span: Span,
        fn_name: I,
        args: &[P<ast::Expr>]
    )
    where I: Make<Ident>
    {
        let source_loc_idx = self.get_source_location_idx(span);
        let mut hook_args = vec![mk().lit_expr(mk().int_lit(source_loc_idx as u128, "u32"))];
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

impl<'a, 'tcx> MutVisitor for LifetimeInstrumenter<'a, 'tcx> {
    fn flat_map_foreign_item(&mut self, item: ast::ForeignItem) -> SmallVec<[ast::ForeignItem; 1]> {
        if let ast::ForeignItemKind::Fn(decl, _) = &item.node {
            if HOOK_FUNCTIONS.contains(&&*item.ident.name.as_str()) {
                self.hooked_functions.insert(item.ident, decl.clone());
            }
        }
        self.depth += 1;
        let folded = mut_visit::noop_flat_map_foreign_item(item, self);
        self.depth -= 1;
        folded
    }

    fn visit_expr(&mut self, expr: &mut P<ast::Expr>) {
        // Post-order traversal so we instrument any arguments before processing
        // the expr.
        mut_visit::noop_visit_expr(expr, self);

        if let ast::ExprKind::Call(callee, args) = &expr.node {
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

                self.instrument_expr_call_rt(expr, expr.span, fn_name, &args);
                return;
            }
        }

        let expr_span = expr.span.clone();
        match &mut expr.node {
            ast::ExprKind::Call(_callee, args) => {
                for arg in args.iter_mut() {
                    if self.cx.node_type(arg.id).is_unsafe_ptr() && !self.is_constant(arg) {
                        debug!("Instrumenting arg: {:?}", arg);
                        self.instrument_expr_call_rt(
                            arg,
                            arg.span,
                            "ptr_arg",
                            &[mk().cast_expr(
                                mk().path_expr(vec!["ret"]), mk().ident_ty("usize"),
                            )],
                        );
                    }
                }
            }
            ast::ExprKind::Unary(ast::UnOp::Deref, ptr_expr) =>
                if self.cx.node_type(ptr_expr.id).is_unsafe_ptr()
            {
                self.instrument_expr_call_rt(
                    ptr_expr,
                    expr_span,
                    "ptr_deref",
                    &[mk().cast_expr(mk().path_expr(vec!["ret"]), mk().ident_ty("usize"))],
                );
            }
            ast::ExprKind::Assign(lhs, rhs) =>
                if self.cx.node_type(lhs.id).is_unsafe_ptr()
            {
                self.instrument_expr_call_rt(
                    rhs,
                    expr_span,
                    "ptr_assign",
                    &[mk().cast_expr(mk().path_expr(vec!["ret"]), mk().ident_ty("usize"))],
                )
            }
            _ => (),
        }
    }

    fn flat_map_item(&mut self, item: P<ast::Item>) -> SmallVec<[P<ast::Item>; 1]> {
        self.depth += 1;
        let mut item: P<ast::Item> = mut_visit::noop_flat_map_item(item, self).lone();
        self.depth -= 1;

        // Instrument entry point if found
        match entry::entry_point_type(&*item, self.depth) {
            entry::EntryPointType::MainNamed |
            entry::EntryPointType::MainAttr |
            entry::EntryPointType::Start => {
                if let ast::ItemKind::Fn(_decl, _header, _generics, block) = &mut item.node {
                    self.instrument_entry_block(block);
                } else {
                    panic!("Expected a function item");
                };
            }
            _ => {}
        }

        let item_id = item.id;
        // Instrument the real main function
        if let ast::ItemKind::Fn(_decl, _header, _generics, block) = &mut item.node {
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

impl Transform for AnalysisCmd {
    fn transform(&self, krate: &mut ast::Crate, st: &CommandState, cx: &RefactorCtxt) {
        // Initialize the analysis runtime so we get debug pretty printing for
        // spans
        c2rust_analysis_rt::span::set_file(&self.span_filename);

        let arena = SyncDroplessArena::default();
        let ownership_analysis = ownership::analyze(&st, &cx, &arena);

        let mut analyzer = LifetimeAnalyzer::new(
            cx,
            &self.span_filename,
            &self.log_filename,
            ownership_analysis,
        );
        analyzer.run(krate);
        analyzer.visit_crate(krate)
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

#[derive(Debug)]
struct EventPlace {
    kind: EventKind,
    // AST node where this event occurred
    node: ast::NodeId,
}

struct AllocInfo {
    base: Pointer,
    size: usize,
    events: Vec<EventPlace>,
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
        self.events.push(EventPlace { kind, node });
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
        let alloc = match self.range(..ptr).next_back() {
            Some(a) => a.1,
            None => return false,
        };

        alloc.contains(ptr)
    }

    fn alloc(&mut self, size: usize, ptr: Pointer) -> &mut AllocInfo {
        if self.is_allocated(ptr) {
            panic!("Inserting an allocation overlapping an existing allocation??");
        }

        if let Some(old) = self.insert(ptr, AllocInfo::new(ptr, size)) {
            panic!("Inserting an allocation at the same address as {:?}", old);
        }

        self.get_mut(&ptr).unwrap()
    }

    fn free(&mut self, ptr: Pointer) -> AllocInfo {
        self.remove(&ptr)
            .unwrap_or_else(|| panic!("Could not free allocation at {:?}", ptr))
    }

    fn realloc(&mut self, old_ptr: Pointer, size: usize, new_ptr: Pointer) -> &mut AllocInfo {
        if old_ptr == new_ptr {
            let alloc = self.get_mut(&old_ptr)
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

struct LifetimeAnalyzer<'lty, 'a: 'lty, 'tcx: 'a> {
    _cx: &'a RefactorCtxt<'a, 'tcx>,
    log_path: &'a Path,

    spans: Vec<Span>,
    span_ids: HashMap<Span, SpanId>,

    // TODO: replace with a Vec, SpanIds are dense
    span_to_expr: HashMap<SpanId, ast::NodeId>,

    _ownership_analysis: ownership::AnalysisResult<'lty, 'tcx>,

    mem_map: MemMap,
}
    
impl<'lty, 'a, 'tcx> LifetimeAnalyzer<'lty, 'a, 'tcx> {
    fn new(
        cx: &'a RefactorCtxt<'a, 'tcx>,
        span_file: &'a str,
        log_file: &'a str,
        ownership_analysis: ownership::AnalysisResult<'lty, 'tcx>,
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
                NO_EXPANSION,
            )
        }).collect();

        if spans.len() > u32::MAX as usize {
            panic!("Too many spans");
        }
        let span_ids = HashMap::from_iter(
            spans.iter().enumerate().map(|(i, v)| (*v, i as SpanId))
        );
        
        Self {
            _cx: cx,
            log_path: Path::new(log_file),
            spans,
            span_ids,
            span_to_expr: HashMap::new(),
            _ownership_analysis: ownership_analysis,
            mem_map: MemMap::new(),
        }
    }

    fn run(&mut self, krate: &ast::Crate) {
        // Construct a mapping from expressions to spans
        visit_nodes(krate, |e: &ast::Expr| {
            if let Some(id) = self.span_ids.get(&e.span) {
                self.span_to_expr.insert(*id, e.id);
            }
        });

        let mut log_file = File::open(&self.log_path)
            .expect("Could not open instrumentation log file");
        while let Ok(event) = bincode::deserialize_from(&mut log_file) as bincode::Result<Event> {
            let span = self.spans[event.span as usize];
            debug!("{:?} {:?}", span, event.kind);

            let node_id = *match self.span_to_expr.get(&event.span) {
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
                    let info = self.mem_map.free(ptr);
                    self.process_chain(&info.events);
                }
                EventKind::Realloc { old_ptr, size, new_ptr } => {
                    self.mem_map.realloc(old_ptr, size, new_ptr)
                        .add_event(event.kind, node_id);
                }
                EventKind::Assign(ptr) |
                EventKind::Arg(ptr) |
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
    }

    fn process_chain(&self, events: &[EventPlace]) {
        debug!("Processing chain of memory events: {:#?}", events);

        // TODO: create dataflow graph from this chain

    }
}

impl<'lty, 'a, 'tcx> MutVisitor for LifetimeAnalyzer<'lty, 'a, 'tcx> {
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("lifetime_analysis_instrument", |args| mk(InstrumentCmd {
        span_file_path: args[0].clone(),
        main_path: args[1].clone(),
    }));

    reg.register("lifetime_analysis", |args| mk(AnalysisCmd {
        span_filename: args[0].clone(),
        log_filename: args[1].clone(),
    }));
}
