#![feature(plugin_registrar, rustc_private, quote)]

extern crate rustc_plugin;
extern crate rustc_data_structures;
extern crate syntax;

extern crate bincode;
#[macro_use]
extern crate error_chain;
extern crate indexmap;

use std::collections::HashMap;
use std::fs::File;
use std::path::PathBuf;

use rustc_plugin::Registry;
use syntax::{ast, entry};
use syntax::ext::base::{SyntaxExtension, ExtCtxt, Annotatable, MultiItemModifier};
use syntax::ext::build::AstBuilder;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::symbol::{Symbol, Ident};
use syntax::source_map::{Span, DUMMY_SP, FileName};

use indexmap::IndexSet;

use c2rust_analysis_rt::{SourceSpan, BytePos};

pub mod errors {
    error_chain! {
        foreign_links {
            Io(std::io::Error);
            Bincode(Box<bincode::ErrorKind>);
        }
    }
}

use self::errors::*;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    let plugin = LifetimeAnalysis::new(reg.args());
    reg.register_syntax_extension(
        Symbol::intern("lifetime_analysis"),
        SyntaxExtension::MultiModifier(Box::new(plugin)));
}

struct LifetimeAnalysis {
}

impl LifetimeAnalysis {
    fn new(_args: &[ast::NestedMetaItem]) -> Self {
        Self { }
    }
}

impl MultiItemModifier for LifetimeAnalysis {
    fn expand(
        &self,
        cx: &mut ExtCtxt,
        _sp: Span,
        _mi: &ast::MetaItem,
        item: Annotatable
    ) -> Vec<Annotatable> {
        match item {
            Annotatable::Item(i) => {
                match &i.node {
                    ast::ItemKind::Mod(_) => {
                        let mut folder = LifetimeInstrumentation::new(cx);
                        let folded = folder.fold_item(i);
                        folder.finalize().expect("Error instrumenting lifetimes");
                        folded
                    }
                    _ => panic!("unexpected item: {:#?}", i),
                }.into_iter().map(|i| Annotatable::Item(i)).collect()
            }
            // TODO: handle TraitItem
            // TODO: handle ImplItem
            _ => panic!("unexpected item: {:?}", item),
        }
    }
}

/// List of functions we want hooked for the lifetime analyis runtime (see
/// ../../runtime/src/lib.rs for the implementations of these hooks)
const HOOKED_FUNCTIONS: &[&'static str] = &[
    "malloc",
    "free",
    "calloc",
    "realloc",
    "reallocarray",
];

const SPAN_FILENAME: &str = "lifetime_analysis_spans.bincode";


struct LifetimeInstrumentation<'a, 'cx: 'a> {
    cx: &'a mut ExtCtxt<'cx>,
    hooked_functions: HashMap<Ident, P<ast::FnDecl>>,

    spans: IndexSet<SourceSpan>,
    depth: usize,
    span_file_path: PathBuf,
}

impl<'a, 'cx> LifetimeInstrumentation<'a, 'cx> {
    fn new(cx: &'a mut ExtCtxt<'cx>) -> Self {
        let mut path = cx.root_path.canonicalize().unwrap();
        path.push(SPAN_FILENAME);
        Self {
            cx,
            hooked_functions: HashMap::new(),
            spans: IndexSet::new(),
            depth: 0,
            span_file_path: path,
        }
    }

    fn finalize(self) -> Result<()> {
        eprintln!("Writing spans to {:?}", self.span_file_path);
        let span_file = File::create(self.span_file_path)
            .chain_err(|| "Could not open span file")?;
        let spans: Vec<SourceSpan> = self.spans.into_iter().collect();
        bincode::serialize_into(span_file, &spans)
            .chain_err(|| "Span serialization failed")
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
            ast::TyKind::Ptr(_) => quote_expr!(self.cx, $expr as usize),
            _ => expr.clone(),
        }
    }

    fn get_source_location_idx(&mut self, span: Span) -> usize {
        let lo = self.cx.source_map().lookup_byte_offset(span.lo());
        let hi = self.cx.source_map().lookup_byte_offset(span.hi());

        if lo.sf.start_pos != hi.sf.start_pos {
            self.cx.span_err(span, "Location crosses source files");
        }
        let file_path = match &lo.sf.name {
            FileName::Real(path) => path
                .strip_prefix(&self.cx.root_path)
                .expect("Could not strip root path from source filename")
                .to_owned(),
            _ => {
                self.cx.span_err(span, "Location does not refer to a source file");
                unreachable!()
            }
        };

        let source_span = SourceSpan::new(file_path, BytePos(lo.pos.0), BytePos(hi.pos.0));

        let (idx, _) = self.spans.insert_full(source_span);
        idx
    }

    fn instrument_main_block(&self, block: P<ast::Block>) -> P<ast::Block> {
        let span_file = self.span_file_path.to_str().unwrap();
        block.map(|mut block| {
            block.stmts.insert(0, quote_stmt!(
                self.cx, c2rust_analysis_rt::set_span_file($span_file);
            ).unwrap());
            block
        })
    }
}

impl<'a, 'cx> Folder for LifetimeInstrumentation<'a, 'cx> {
    fn fold_foreign_item_simple(&mut self, item: ast::ForeignItem) -> ast::ForeignItem {
        if let ast::ForeignItemKind::Fn(decl, _) = &item.node {
            if HOOKED_FUNCTIONS.contains(&&*item.ident.name.as_str()) {
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
                if self.hooked_fn(callee).is_some() {
                    let source_loc_idx = self.get_source_location_idx(expr.span);
                    let mut hook_args: Vec<P<ast::Expr>> = vec![
                        quote_expr!(self.cx, $source_loc_idx)
                    ];

                    let (fn_name, decl) = self.hooked_fn(callee).unwrap();

                    // Add all original arguments, casting pointers to usize
                    hook_args.extend(
                        args
                            .iter()
                            .zip(decl.inputs.iter())
                            .map(|(arg, arg_decl)| self.add_ptr_cast(arg, &arg_decl.ty))
                    );
                    // Add the return value of the hooked call.
                    hook_args.push({
                        let ret_expr = quote_expr!(self.cx, ret);
                        match &decl.output {
                            ast::FunctionRetTy::Ty(ty) => self.add_ptr_cast(&ret_expr, ty),
                            _ => ret_expr,
                        }
                    });

                    // Build the hook call (we can't do this with just quoting
                    // because I couldn't figure out how to get quote_expr! to
                    // play nice with multiple arguments in a variable).
                    let hook_call = self.cx.expr_call(
                        DUMMY_SP,
                        quote_expr!(self.cx, c2rust_analysis_rt::$fn_name),
                        hook_args,
                    );

                    // Build the instrumentation block
                    quote_expr!(self.cx, {
                        let ret = $expr;
                        $hook_call;
                        ret
                    })
                } else {
                    expr
                }
            }
            _ => expr,
        }
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
                ast::Item {
                    node: {
                        if let ast::ItemKind::Fn(decl, header, generics, block) = item.node {
                            ast::ItemKind::Fn(decl, header, generics, {
                                self.instrument_main_block(block)
                            })
                        } else {
                            panic!("Expected a function item");
                        }
                    },
                    ..item
                }
            }
            _ => item,
        }
    }
}
