#![feature(plugin_registrar, quote, rustc_private, try_from)]

extern crate rustc_plugin;
extern crate syntax;

use rustc_plugin::Registry;
use syntax::ast;
use syntax::fold;

use std::convert::TryInto;

use syntax::ext::base::{SyntaxExtension, ExtCtxt, Annotatable};
use syntax::codemap::Span;
use syntax::fold::Folder;
use syntax::symbol::Symbol;

fn expand_cross_checks(cx: &mut ExtCtxt,
                       _sp: Span,
                       mi: &ast::MetaItem,
                       item: Annotatable) -> Annotatable {
    let config = CrossCheckConfig::new(mi);
    match item {
        Annotatable::Item(i) => Annotatable::Item(
            CrossChecker{ cx: cx, config: config }
            .fold_item(i).expect_one("too many items returned")),
        // TODO: handle TraitItem
        // TODO: handle ImplItem
        _ => panic!("Unexpected item: {:?}", item),
    }
}

struct CrossCheckConfig {
    enabled: bool,
    name: Option<String>,
    id: Option<u32>,
}

impl CrossCheckConfig {
    fn new(mi: &ast::MetaItem) -> CrossCheckConfig {
        assert!(mi.name == "cross_check");
        let mut res = CrossCheckConfig {
            enabled: true,
            name: None,
            id: None,
        };
        match mi.node {
            ast::MetaItemKind::Word => { } // Use the defaults for #[cross_check]
            ast::MetaItemKind::List(ref items) => {
                for ref nested_item in items {
                    if let Some(ref item) = nested_item.meta_item() {
                        match item.name.as_str().as_ref() {
                            "never" |
                            "disable" |
                            "no" => {
                                res.enabled = false
                            }
                            "always" |
                            "enable" |
                            "yes" => {
                                res.enabled = true
                            }
                            "name" => {
                                res.name = item.value_str().map(|s| String::from(s.as_str().as_ref()))
                            }
                            "id" => {
                                if let ast::MetaItemKind::NameValue(ref lit) = item.node {
                                    if let ast::LitKind::Int(id128, _) = lit.node {
                                        if let Ok(id32) = id128.try_into() {
                                            res.id = Some(id32);
                                        } else {
                                            panic!("Invalid u32 for cross_check id: {}", id128);
                                        }
                                    } else {
                                        panic!("Invalid literal for cross_check id: {:?}", lit.node);
                                    }
                                }
                            }
                            name@_ => panic!("Unknown cross_check item: {}", name)
                        }
                    }
                }
            }
            _ => panic!("Unknown cross_check item: {:?}", mi.node)
        }
        res
    }
}

struct CrossChecker<'a, 'cx: 'a> {
    cx: &'a mut ExtCtxt<'cx>,
    config: CrossCheckConfig,
}

fn djb2_hash(s: &str) -> u32 {
    s.bytes().fold(5381u32, |h, c| h.wrapping_mul(33).wrapping_add(c as u32))
}

impl<'a, 'cx> Folder for CrossChecker<'a, 'cx> {
    fn fold_item_simple(&mut self, item: ast::Item) -> ast::Item {
        if !self.config.enabled {
            return fold::noop_fold_item_simple(item, self);
        }
        if item.attrs.iter().any(|attr| attr.name().map_or(false, |name| name == "cross_check")) {
            // If we have cross-check attrs at multiple levels, e.g.,
            // one per crate and one per function, we'll get called multiple times
            // and might end up adding multiple cross-checks to each function.
            // If we get called from the crate-level #![cross_check] attr, we'll
            // also see the #[cross_check] attributes here for each function;
            // if that's the case, we can skip inserting cross-checks here,
            // and let each function insert its own check calls later.
            // This allows each function to override the global cross-check
            // settings with its own.
            return fold::noop_fold_item_simple(item, self);
        }
        if let ast::ItemKind::Fn(fn_decl, unsafety, constness, abi, generics, block) = item.node {
            // Add the cross-check to the beginning of the function
            // TODO: only add the checks to C abi functions???
            // Allow clients to specify the id or name manually, like this:
            // #[cross_check(name = "foo")]
            // #[cross_check(id = 0x12345678)]
            let fn_ident = self.fold_ident(item.ident);
            let check_id = if let Some(id) = self.config.id {
                id
            } else if let Some(ref name) = self.config.name {
                djb2_hash(name)
            } else {
                djb2_hash(fn_ident.name.as_str().as_ref())
            };
            let checked_block = self.fold_block(block).map(|block| {
                quote_block!(self.cx, {
                    extern crate xcheck_runtime;
                    xcheck_runtime::xcheck::xcheck(
                        xcheck_runtime::xcheck::FUNCTION_CALL_TAG,
                        $check_id as u64);
                    $block
                }).unwrap()
            });
            let checked_fn = ast::ItemKind::Fn(
                self.fold_fn_decl(fn_decl),
                unsafety,
                constness,
                abi,
                self.fold_generics(generics),
                checked_block);
            // Build and return the replacement function item
            ast::Item {
                id: self.new_id(item.id),
                vis: self.fold_vis(item.vis),
                ident: fn_ident,
                attrs: fold::fold_attrs(item.attrs, self),
                node: checked_fn,
                span: self.new_span(item.span),
                tokens: item.tokens,
            }
        } else {
            fold::noop_fold_item_simple(item, self)
        }
    }

    fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
        mac
    }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_syntax_extension(
        Symbol::intern("cross_check"),
        SyntaxExtension::MultiModifier(Box::new(expand_cross_checks)));
}
