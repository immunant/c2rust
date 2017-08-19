#![feature(plugin_registrar, quote, rustc_private)]

extern crate rustc_plugin;
extern crate syntax;

use rustc_plugin::Registry;
use syntax::ast;
use syntax::fold;

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
            CrossChecker{ cx: cx, config: config, ident: i.ident }
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
                                if let Some(val) = item.value_str() {
                                    // FIXME: for now, we only support decimal ids
                                    // It would be nice to use libsyntax's integer parser
                                    let id = val.as_str().parse::<u32>();
                                    if let Ok(id) = id {
                                        res.id = Some(id)
                                    } else {
                                        println!("Warning!!! Could not parse cross_check id: \"{}\"", val);
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
    ident: ast::Ident,
}

fn djb2_hash(s: &str) -> u32 {
    s.bytes().fold(5381u32, |h, c| h.wrapping_mul(33).wrapping_add(c as u32))
}

impl<'a, 'cx> Folder for CrossChecker<'a, 'cx> {
    fn fold_item_kind(&mut self, ik: ast::ItemKind) -> ast::ItemKind {
        if !self.config.enabled {
            fold::noop_fold_item_kind(ik, self)
        } else if let ast::ItemKind::Fn(fn_decl, unsafety, constness, abi, generics, block) = ik {
            // Add the cross-check to the beginning of the function
            // TODO: only add the checks to C abi functions???
            // Allow clients to specify the id or name manually, like this:
            // #[cross_check(name = "foo")]
            // #[cross_check(id = "12345678")]
            let check_id = if let Some(id) = self.config.id {
                id
            } else if let Some(ref name) = self.config.name {
                djb2_hash(name)
            } else {
                djb2_hash(self.ident.name.as_str().as_ref())
            };
            let checked_block = self.fold_block(block).map(|block| {
                quote_block!(self.cx, {
                    unsafe { rb_xcheck($check_id); }
                    $block
                }).unwrap()
            });
            
            ast::ItemKind::Fn(
                self.fold_fn_decl(fn_decl),
                unsafety,
                constness,
                abi,
                self.fold_generics(generics),
                checked_block)
        } else {
            fold::noop_fold_item_kind(ik, self)
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
