#![feature(plugin_registrar, rustc_private, quote)]

extern crate rustc_plugin;
extern crate syntax;

use std::collections::HashMap;

use rustc_plugin::Registry;
use syntax::ast;
use syntax::ext::base::{SyntaxExtension, ExtCtxt, Annotatable, MultiItemModifier};
use syntax::ext::build::AstBuilder;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::symbol::{Symbol, Ident};
use syntax::source_map::{Span, DUMMY_SP};

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
                        folder.fold_item(i)
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

const HOOKED_FUNCTIONS: &[&'static str] = &[
    "malloc",
    "free",
    "calloc",
    "realloc",
    "reallocarray",
];


struct LifetimeInstrumentation<'a, 'cx: 'a> {
    cx: &'a mut ExtCtxt<'cx>,
    hooked_functions: HashMap<Ident, P<ast::FnDecl>>,
}

impl<'a, 'cx> LifetimeInstrumentation<'a, 'cx> {
    fn new(cx: &'a mut ExtCtxt<'cx>) -> Self {
        Self {
            cx,
            hooked_functions: HashMap::new(),
        }
    }

    /// Check if the callee expr is a function we've hooked. Returns the name of
    /// the function and its declaration if found.
    fn hooked_fn(&self, callee: &ast::Expr) -> Option<(Ident, &ast::FnDecl)> {
        match &callee.node {
            ast::ExprKind::Path(None, path) => {
                if path.segments.len() == 1 {
                    if let Some(decl) = self.hooked_functions.get(&path.segments[0].ident) {
                        return Some((path.segments[0].ident, decl));
                    }
                }
            }
            _ => (),
        }

        None
    }

    /// If ty is a Ptr type, return a new expr that is a cast of expr to usize,
    /// otherwise just return a clone of expr.
    fn add_ptr_cast(&self, expr: &P<ast::Expr>, ty: &ast::Ty) -> P<ast::Expr> {
        match ty.node {
            ast::TyKind::Ptr(_) => quote_expr!(self.cx, $expr as usize),
            _ => expr.clone(),
        }
    }
}

impl<'a, 'cx> Folder for LifetimeInstrumentation<'a, 'cx> {
    // Will be needed for other crates?
    // fn fold_mod(&mut self, m: ast::Mod) -> ast::Mod {
    //     let mut items = vec![
    //         quote_item!(self.cx, extern crate c2rust_analysis_rt;).unwrap(),
    //         quote_item!(self.cx, use c2rust_analysis_rt::*;).unwrap(),
    //     ];
    //     items.extend(m.items.move_flat_map(|x| self.fold_item(x)));
    //     ast::Mod {
    //         items,
    //         ..m
    //     }
    // }

    fn fold_foreign_item_simple(&mut self, item: ast::ForeignItem) -> ast::ForeignItem {
        if let ast::ForeignItemKind::Fn(decl, _) = &item.node {
            if HOOKED_FUNCTIONS.contains(&&*item.ident.name.as_str()) {
                self.hooked_functions.insert(item.ident, decl.clone());
            }
        }
        fold::noop_fold_foreign_item_simple(item, self)
    }

    fn fold_expr(&mut self, expr: P<ast::Expr>) -> P<ast::Expr> {
        // Post-order traversal so we instrument any arguments before processing
        // the expr.
        let expr = expr.map(|expr| fold::noop_fold_expr(expr, self));

        match &expr.node {
            ast::ExprKind::Call(callee, args) => {
                if let Some((fn_name, decl)) = self.hooked_fn(callee) {
                    // Cast all original arguments to usize
                    let mut hook_args: Vec<P<ast::Expr>> = args
                        .iter()
                        .zip(decl.inputs.iter())
                        .map(|(arg, arg_decl)| self.add_ptr_cast(arg, &arg_decl.ty))
                        .collect();
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
}
