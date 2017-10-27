#![feature(plugin_registrar, quote, rustc_private, try_from)]

extern crate rustc_plugin;
extern crate syntax;

extern crate cross_check_config as xcfg;

use rustc_plugin::Registry;
use syntax::ast;
use syntax::fold;

use std::convert::TryInto;
use std::path::PathBuf;

use syntax::ext::base::{SyntaxExtension, ExtCtxt, Annotatable, MultiItemModifier};
use syntax::codemap::{Span, FileLoader, RealFileLoader};
use syntax::fold::Folder;
use syntax::symbol::Symbol;

struct CrossCheckExpander {
    // Arguments passed to plugin
    // TODO: pre-parse them???
    args: Vec<ast::NestedMetaItem>,
    config_files: Vec<xcfg::Config>,
}

impl CrossCheckExpander {
    fn new(args: &[ast::NestedMetaItem]) -> CrossCheckExpander {
        CrossCheckExpander {
            args: args.to_vec(),
            config_files: CrossCheckExpander::parse_config_files(args),
        }
    }

    fn parse_config_files(args: &[ast::NestedMetaItem]) -> Vec<xcfg::Config> {
        // Parse arguments of the form
        // #[plugin(cross_check_plugin(config_file = "..."))]
        let fl = RealFileLoader;
        args.iter()
            .filter(|nmi| nmi.check_name("config_file"))
            .map(|mi| mi.value_str().expect("invalid string for config_file"))
            .map(|fsym| PathBuf::from(&*fsym.as_str()))
            .map(|fp| fl.abs_path(&fp)
                        .expect(&format!("invalid path to config file: {:?}", fp)))
            .map(|fp| fl.read_file(&fp)
                        .expect(&format!("could not read config file: {:?}", fp)))
            // TODO: use a Reader to read&parse each configuration file
            // without storing its contents in an intermediate String buffer???
            .map(|fd| xcfg::parse_string(&fd).expect("could not parse config file"))
            .collect()
    }
}

impl MultiItemModifier for CrossCheckExpander {
    fn expand(&self,
              cx: &mut ExtCtxt,
              _sp: Span,
              mi: &ast::MetaItem,
              item: Annotatable) -> Vec<Annotatable> {
        let config = CrossCheckConfig::new(mi);
        match item {
            Annotatable::Item(i) => Annotatable::Item(
                CrossChecker{ cx: cx, config: config }
                .fold_item(i)
                .expect_one("too many items returned")).into(),
            // TODO: handle TraitItem
            // TODO: handle ImplItem
            _ => panic!("Unexpected item: {:?}", item),
        }
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
                        match &*item.name.as_str() {
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
                                res.name = item.value_str().map(|s| String::from(&*s.as_str()))
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
        match item.node {
            ast::ItemKind::Fn(fn_decl, unsafety, constness, abi, generics, block) => {
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
                    djb2_hash(&*fn_ident.name.as_str())
                };

                // Insert cross-checks for function arguments,
                // if enabled via the "xcheck-args" feature
                let mut arg_xchecks: Vec<ast::Block> = vec![];
                if cfg!(feature = "xcheck-args") {
                    fn_decl.inputs.iter().for_each(|ref arg| {
                        match arg.pat.node {
                            ast::PatKind::Ident(_, ident, _) => {
                                // Parameter pattern is just an identifier,
                                // so we can reference it directly by name
                                arg_xchecks.push(quote_block!(self.cx, {
                                    cross_check_value!(FUNCTION_ARG_TAG, $ident);
                                }).unwrap());
                            }
                            _ => unimplemented!()
                        }
                    });
                }

                let checked_block = self.fold_block(block).map(|block| {
                    quote_block!(self.cx, {
                        #[allow(dead_code)]
                        mod cross_check_types {
                            pub type DefaultAggHasher    = ::cross_check_runtime::hash::jodyhash::JodyHasher;
                            pub type DefaultSimpleHasher = ::cross_check_runtime::hash::simple::SimpleHasher;
                        };
                        cross_check_raw!(FUNCTION_ENTRY_TAG, $check_id);
                        $arg_xchecks
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
            }
            ast::ItemKind::Enum(_, _) |
            ast::ItemKind::Struct(_, _) |
            ast::ItemKind::Union(_, _) => {
                // Prepend #[derive(CrossCheckHash)] automatically
                // to every structure definition
                let mut item_attrs = fold::fold_attrs(item.attrs, self);
                let xcheck_hash_attr = quote_attr!(self.cx, #[derive(CrossCheckHash)]);
                item_attrs.push(xcheck_hash_attr);
                ast::Item {
                    id: self.new_id(item.id),
                    vis: self.fold_vis(item.vis),
                    ident: self.fold_ident(item.ident),
                    attrs: item_attrs,
                    node: self.fold_item_kind(item.node),
                    span: self.new_span(item.span),
                    tokens: item.tokens,
                }
            }
            _ => fold::noop_fold_item_simple(item, self)
        }
    }

    fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
        mac
    }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    let ecc = CrossCheckExpander::new(reg.args());
    // TODO: parse args
    reg.register_syntax_extension(
        Symbol::intern("cross_check"),
        SyntaxExtension::MultiModifier(Box::new(ecc)));
}
