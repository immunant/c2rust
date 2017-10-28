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
        let config = CrossCheckConfig::default().parse_config(mi);
        match item {
            Annotatable::Item(i) => {
                // If we're seeing #![cross_check] at the top of the crate or a module,
                // create a fresh configuration and perform a folding; otherwise, just
                // ignore this expansion and let the higher level one do everything
                let ni = match i.node {
                    ast::ItemKind::Mod(_) =>
                        CrossChecker{ cx: cx, config: config }
                        .fold_item(i)
                        .expect_one("too many items returned"),
                    _ => i
                };
                Annotatable::Item(ni).into()
            }
            // TODO: handle TraitItem
            // TODO: handle ImplItem
            _ => panic!("Unexpected item: {:?}", item),
        }
    }
}

#[derive(Clone)]
struct CrossCheckConfig {
    enabled: bool,
    name: Option<String>,
    id: Option<u32>,
}

impl Default for CrossCheckConfig {
    fn default() -> CrossCheckConfig {
        CrossCheckConfig {
            enabled: true,
            name: None,
            id: None,
        }
    }
}

impl CrossCheckConfig {
    fn parse_config(&self, mi: &ast::MetaItem) -> Self {
        assert!(mi.name == "cross_check");
        let mut res = self.clone();
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

    // Allow clients to specify the id or name manually, like this:
    // #[cross_check(name = "foo")]
    // #[cross_check(id = 0x12345678)]
    fn get_hash(&self) -> Option<u32> {
        self.id.or_else(|| self.name.as_ref().map(|ref name| djb2_hash(name)))
    }
}

struct CrossChecker<'a, 'cx: 'a> {
    cx: &'a mut ExtCtxt<'cx>,
    config: CrossCheckConfig,
}

impl<'a, 'cx> CrossChecker<'a, 'cx> {
    fn parse_config(&mut self, item: &ast::Item) -> Option<CrossCheckConfig> {
        let xcheck_attr = item.attrs.iter().find(
            |attr| attr.name().map_or(false, |name| name == "cross_check"));
        xcheck_attr.map(|attr| self.config.parse_config(&attr.meta().unwrap()))
    }

    fn swap_config(&mut self, new_config: Option<CrossCheckConfig>) -> Option<CrossCheckConfig> {
        new_config.map(|mut new_config| {
            std::mem::swap(&mut self.config, &mut new_config);
            new_config
        })
    }

    fn internal_fold_item_simple(&mut self, item: ast::Item) -> ast::Item {
        match item.node {
            ast::ItemKind::Fn(fn_decl, unsafety, constness, abi, generics, block) => {
                let fn_ident = self.fold_ident(item.ident);
                let checked_block = if self.config.enabled {
                    // Add the cross-check to the beginning of the function
                    // TODO: only add the checks to C abi functions???
                    let check_id = self.config.get_hash().unwrap_or_else(
                        || djb2_hash(&*fn_ident.name.as_str()));

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

                    // Build and return the block
                    self.fold_block(block).map(|block| quote_block!(self.cx, {
                        cross_check_raw!(FUNCTION_ENTRY_TAG, $check_id);
                        $arg_xchecks
                        $block
                    }).unwrap())
                } else {
                    self.fold_block(block)
                };

                // Add our typedefs to the beginning of each function;
                // whatever the configuration says, we should always add these
                let block_with_types = quote_block!(self.cx, {
                    #[allow(dead_code)]
                    mod cross_check_types {
                        pub type DefaultAggHasher    = ::cross_check_runtime::hash::jodyhash::JodyHasher;
                        pub type DefaultSimpleHasher = ::cross_check_runtime::hash::simple::SimpleHasher;
                    };
                    $checked_block
                });
                let checked_fn = ast::ItemKind::Fn(
                    self.fold_fn_decl(fn_decl),
                    unsafety,
                    constness,
                    abi,
                    self.fold_generics(generics),
                    block_with_types);
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
}

fn djb2_hash(s: &str) -> u32 {
    s.bytes().fold(5381u32, |h, c| h.wrapping_mul(33).wrapping_add(c as u32))
}

impl<'a, 'cx> Folder for CrossChecker<'a, 'cx> {
    fn fold_item_simple(&mut self, item: ast::Item) -> ast::Item {
        let new_config = self.parse_config(&item);
        let old_config = self.swap_config(new_config);
        let new_item = self.internal_fold_item_simple(item);
        self.swap_config(old_config);
        new_item
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
