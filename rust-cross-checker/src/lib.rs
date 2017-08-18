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
                       _meta_item: &ast::MetaItem,
                       item: Annotatable) -> Annotatable {
    match item {
        Annotatable::Item(i) => Annotatable::Item(
            CrossChecker{ cx: cx, ident: i.ident }.fold_item(i).expect_one("too many items returned")),
        // TODO: handle TraitItem
        // TODO: handle ImplItem
        _ => panic!("Unexpected item: {:?}", item),
    }
}

struct CrossChecker<'a, 'cx: 'a> {
    cx: &'a mut ExtCtxt<'cx>,
    ident: ast::Ident,
}

impl<'a, 'cx> Folder for CrossChecker<'a, 'cx> {
    fn fold_item_kind(&mut self, ik: ast::ItemKind) -> ast::ItemKind {
        if let ast::ItemKind::Fn(fn_decl, unsafety, constness, abi, generics, block) = ik {
            // Add the cross-check to the beginning of the function
            // TODO: only add the checks to C abi functions???
            let check_id = 0x12345678u32; // TODO
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
