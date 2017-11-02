#![feature(plugin_registrar, quote, rustc_private, try_from)]

extern crate rustc_plugin;
extern crate syntax;

#[macro_use]
extern crate matches;

extern crate cross_check_config as xcfg;

use rustc_plugin::Registry;
use syntax::ast;
use syntax::fold;

use std::borrow::Borrow;
use std::convert::TryInto;
use std::path::PathBuf;
use std::rc::Rc;

use syntax::ext::base::{SyntaxExtension, ExtCtxt, Annotatable, MultiItemModifier};
use syntax::ext::quote::rt::{ToTokens, ExtParseUtils};
use syntax::codemap::{Span, FileLoader, RealFileLoader};
use syntax::fold::Folder;
use syntax::symbol::Symbol;
use syntax::ptr::P;
use syntax::tokenstream::TokenTree;
use syntax::util::small_vector::SmallVector;

fn djb2_hash(s: &str) -> u32 {
    s.bytes().fold(5381u32, |h, c| h.wrapping_mul(33).wrapping_add(c as u32))
}

#[derive(Clone)]
struct CrossCheckConfig {
    enabled: bool,
    name: Option<String>,
    id: Option<u32>,
    ahasher: Vec<TokenTree>,
    shasher: Vec<TokenTree>,
}

impl CrossCheckConfig {
    fn new(cx: &mut ExtCtxt) -> CrossCheckConfig {
        CrossCheckConfig {
            enabled: true,
            name: None,
            id: None,
            ahasher: quote_ty!(cx, ::cross_check_runtime::hash::jodyhash::JodyHasher).to_tokens(cx),
            shasher: quote_ty!(cx, ::cross_check_runtime::hash::simple::SimpleHasher).to_tokens(cx),
        }
    }

    fn parse_attr_config(mut self, cx: &mut ExtCtxt, mi: &ast::MetaItem) -> Self {
        assert!(mi.name == "cross_check");
        if let Some(ref items) = mi.meta_item_list() {
            for ref nested_item in items.iter() {
                if let Some(ref item) = nested_item.meta_item() {
                    match &*item.name.as_str() {
                        "never" |
                        "disable" |
                        "no" => {
                            self.enabled = false
                        }
                        "always" |
                        "enable" |
                        "yes" => {
                            self.enabled = true
                        }
                        "name" => {
                            self.name = item.value_str().map(|s| String::from(&*s.as_str()))
                        }
                        "id" => {
                            if let ast::MetaItemKind::NameValue(ref lit) = item.node {
                                if let ast::LitKind::Int(id128, _) = lit.node {
                                    if let Ok(id32) = id128.try_into() {
                                        self.id = Some(id32);
                                    } else {
                                        panic!("Invalid u32 for cross_check id: {}", id128);
                                    }
                                } else {
                                    panic!("Invalid literal for cross_check id: {:?}", lit.node);
                                }
                            }
                        }
                        "ahasher" => {
                            self.ahasher = item.value_str()
                                               .map(|s| cx.parse_tts(String::from(&*s.as_str())))
                                               .unwrap_or_default()
                        }
                        "shasher" => {
                            self.shasher = item.value_str()
                                               .map(|s| cx.parse_tts(String::from(&*s.as_str())))
                                               .unwrap_or_default()
                        }

                        // Ignore arguments for #[derive(CrossCheckHash)]
                        "custom_hash" |
                        "ahasher_override" |
                        "shasher_override" |
                        "hasher" => (),

                        name@_ => panic!("Unknown cross_check item: {}", name)
                    }
                }
            }
        }
        self
    }

    fn parse_xcfg_config(mut self, cx: &mut ExtCtxt, xcfg: &xcfg::ItemConfig) -> Self {
        match *xcfg {
            xcfg::ItemConfig::Function(ref func) => {
                if let Some(no_xchecks) = func.no_xchecks {
                    self.enabled = !no_xchecks;
                }
                if let Some(ref entry) = func.entry {
                    match *entry {
                        xcfg::XCheckType::Default => {
                            self.name = None;
                            self.id = None;
                        },
                        xcfg::XCheckType::Skip => unimplemented!(), // TODO
                        xcfg::XCheckType::Fixed(id) => {
                            self.id = Some(id as u32);
                            self.name = None;
                        },
                        xcfg::XCheckType::Djb2(ref name) => {
                            self.id = None;
                            self.name = Some(name.clone());
                        },
                    }
                }
                if let Some(ref ahasher) = func.ahasher {
                    // TODO: add a way for the external config to reset to default
                    self.ahasher = cx.parse_tts(ahasher.clone());
                }
                if let Some(ref shasher) = func.shasher {
                    // TODO: add a way for the external config to reset to default
                    self.shasher = cx.parse_tts(shasher.clone());
                }
                // TODO: parse more fields: exit, args, ret
            },
            _ => ()
        }
        self
    }

    // Allow clients to specify the id or name manually, like this:
    // #[cross_check(name = "foo")]
    // #[cross_check(id = 0x12345678)]
    fn get_hash(&self) -> Option<u32> {
        self.id.or_else(|| self.name.as_ref().map(|ref name| djb2_hash(name)))
    }
}

#[derive(Clone)]
struct ScopeConfig<'xcfg> {
    file_name: Rc<String>, // FIXME: this should be a &str
    items: Option<Rc<xcfg::NamedItemList<'xcfg>>>,
    config: Rc<CrossCheckConfig>,
}

impl<'xcfg> ScopeConfig<'xcfg> {
    fn new(cfg: &'xcfg xcfg::Config, file_name: &str,
           ccc: Rc<CrossCheckConfig>) -> ScopeConfig<'xcfg> {
        ScopeConfig {
            file_name: Rc::new(String::from(file_name)),
            items: cfg.get_file_items(file_name)
                      .map(xcfg::NamedItemList::new)
                      .map(Rc::new),
            config: ccc,
        }
    }

    fn get_item_config(&self, item: &str) -> Option<&'xcfg xcfg::ItemConfig> {
        self.items
            .as_ref()
            .and_then(|nil| nil.name_map.get(item))
            .cloned()
    }

    fn from_item(&self, item_config: Option<&'xcfg xcfg::ItemConfig>,
                 ccc: Rc<CrossCheckConfig>) -> Self {
        ScopeConfig {
            file_name: self.file_name.clone(),
            items: item_config.and_then(xcfg::ItemConfig::nested_items)
                              .map(xcfg::NamedItemList::new)
                              .map(Rc::new),
            config: ccc,
        }
    }

    fn same_file(&self, file_name: &str) -> bool {
        self.file_name.as_ref() == file_name
    }
}

struct CrossChecker<'a, 'cx: 'a, 'xcfg> {
    cx: &'a mut ExtCtxt<'cx>,
    external_config: &'xcfg xcfg::Config,
    scope_stack: Vec<ScopeConfig<'xcfg>>,
}

fn find_cross_check_attr(attrs: &[ast::Attribute]) -> Option<&ast::Attribute> {
    attrs.iter().find(|attr| attr.check_name("cross_check"))
}

impl<'a, 'cx, 'xcfg> CrossChecker<'a, 'cx, 'xcfg> {
    fn last_scope(&self) -> &ScopeConfig<'xcfg> {
        self.scope_stack.last().unwrap()
    }

    fn config(&self) -> &CrossCheckConfig {
        self.last_scope().config.borrow()
    }

    fn internal_fold_item_simple(&mut self, item: ast::Item) -> ast::Item {
        let folded_item = fold::noop_fold_item_simple(item, self);
        match folded_item.node {
            ast::ItemKind::Fn(fn_decl, unsafety, constness, abi, generics, block) => {
                let fn_ident = folded_item.ident;
                let checked_block = if self.config().enabled {
                    // Insert cross-checks for function arguments,
                    // if enabled via the "xcheck-args" feature
                    let mut arg_xchecks: Vec<P<ast::Block>> = vec![];
                    if cfg!(feature = "xcheck-args") {
                        fn_decl.inputs.iter().for_each(|ref arg| {
                            match arg.pat.node {
                                ast::PatKind::Ident(_, ident, _) => {
                                    // Parameter pattern is just an identifier,
                                    // so we can reference it directly by name
                                    arg_xchecks.push(quote_block!(self.cx, {
                                        cross_check_value!(FUNCTION_ARG_TAG, $ident);
                                    }));
                                }
                                _ => unimplemented!()
                            }
                        });
                    }

                    // Add the cross-check to the beginning of the function
                    // TODO: only add the checks to C abi functions???
                    let check_id = self.config().get_hash().unwrap_or_else(
                        || djb2_hash(&*fn_ident.name.as_str()));
                    quote_block!(self.cx, {
                        cross_check_raw!(FUNCTION_ENTRY_TAG, $check_id);
                        $arg_xchecks
                        $block
                    })
                } else {
                    block
                };

                // Add our typedefs to the beginning of each function;
                // whatever the configuration says, we should always add these
                let block_with_types = {
                    let (ahasher, shasher) = (&self.config().ahasher,
                                              &self.config().shasher);
                    quote_block!(self.cx, {
                        #[allow(dead_code)]
                        mod cross_check_types {
                            pub type DefaultAggHasher    = $ahasher;
                            pub type DefaultSimpleHasher = $shasher;
                        };
                        $checked_block
                    })
                };
                let checked_fn = ast::ItemKind::Fn(
                    fn_decl,
                    unsafety,
                    constness,
                    abi,
                    generics,
                    block_with_types);
                // Build and return the replacement function item
                ast::Item {
                    node: checked_fn,
                    ..folded_item
                }
            }
            ast::ItemKind::Enum(_, _) |
            ast::ItemKind::Struct(_, _) |
            ast::ItemKind::Union(_, _) => {
                // Prepend #[derive(CrossCheckHash)] automatically
                // to every structure definition
                let mut item_attrs = folded_item.attrs;
                let xcheck_attr = find_cross_check_attr(&item_attrs).cloned();
                let xcheck_attr_disabled = {
                    // If the structure has #[cross_check(no)] or #[cross_check(disable)],
                    // do not automatically add CrossCheckHash
                    xcheck_attr.as_ref()
                        .map(|a| a.parse_meta(self.cx.parse_sess).unwrap())
                        .and_then(|mi| mi.meta_item_list().map(
                            |items| items.iter().any(
                                |item| item.meta_item()
                                           .map(|mi| mi.name == "no" ||
                                                     mi.name == "disable" ||
                                                     mi.name == "never")
                                           .unwrap_or(false))))
                        .unwrap_or(false)
                };
                if !xcheck_attr_disabled {
                    let xcheck_hash_derive_attr = quote_attr!(self.cx, #[derive(CrossCheckHash)]);
                    item_attrs.push(xcheck_hash_derive_attr);

                    // Rename #[cross_check] to #[cross_check_hash] internally
                    // and pass it to derive-macros (for some reason,
                    // if we try to pass it as #[cross_check], it disappears)
                    let xcheck_hash_attr = xcheck_attr.map(|attr| ast::Attribute {
                        path: quote_path!(self.cx, cross_check_hash),
                        ..attr
                    });
                    item_attrs.extend(xcheck_hash_attr.into_iter());
                }
                ast::Item {
                    attrs: item_attrs,
                    ..folded_item
                }
            }
            _ => folded_item
        }
    }
}

impl<'a, 'cx, 'xcfg> Folder for CrossChecker<'a, 'cx, 'xcfg> {
    fn fold_item_simple(&mut self, item: ast::Item) -> ast::Item {
        let new_scope = {
            let xcheck_attr = find_cross_check_attr(item.attrs.as_slice());
            let item_xcfg_config = {
                let item_name = item.ident.name.as_str();
                self.last_scope().get_item_config(&*item_name)
            };
            let new_config = if xcheck_attr.is_some() || item_xcfg_config.is_some() {
                // We have either a #[cross_check] attribute
                // or external config, so create a new CrossCheckConfig
                let nc = self.config().clone();
                // TODO: order???
                let nc = xcheck_attr.iter().fold(nc, |nc, attr| {
                    let mi = attr.parse_meta(self.cx.parse_sess).unwrap();
                    nc.parse_attr_config(self.cx, &mi)
                });
                let nc = item_xcfg_config.iter().fold(nc, |nc, xcfg| {
                    nc.parse_xcfg_config(self.cx, xcfg)
                });
                Rc::new(nc)
            } else {
                // If the new config is the same as the previous one,
                // just take a reference to it via Rc
                self.last_scope().config.clone()
            };

            let span = match item.node {
                ast::ItemKind::Mod(ref m) => m.inner,
                _ => item.span
            };
            let mod_file_name = self.cx.codemap().span_to_filename(span);
            if !self.last_scope().same_file(&mod_file_name) {
                // We should only ever get a file name mismatch
                // at the top of a module
                assert_matches!(item.node, ast::ItemKind::Mod(_));
                ScopeConfig::new(self.external_config, &mod_file_name, new_config)
            } else {
                self.last_scope().from_item(item_xcfg_config, new_config)
            }
        };
        self.scope_stack.push(new_scope);
        let new_item = self.internal_fold_item_simple(item);
        self.scope_stack.pop();
        new_item
    }

    fn fold_stmt(&mut self, s: ast::Stmt) -> SmallVector<ast::Stmt> {
       let folded_stmt = fold::noop_fold_stmt(s, self);
       folded_stmt.into_iter().flat_map(|s| {
           let new_stmt = match s.node {
               ast::StmtKind::Local(ref local) => {
                   let attr = find_cross_check_attr(&*local.attrs);
                   // TODO: check that the cross_check attr is "yes"
                   attr.and_then(|_| {
                       // TODO: only add cross-checks for initialized locals???
                       // (in other words, check local.init.is_some())
                       match local.pat.node {
                           ast::PatKind::Ident(_, ident, _) => {
                               Some(quote_stmt!(self.cx, cross_check_value!($ident)).unwrap())
                           },
                           // TODO: handle more pattern types
                           _ => None
                       }
                   })
               },
               _ => None
           };
           let mut res = vec![s];
           res.extend(new_stmt.into_iter());
           res
       }).collect()
    }

    fn fold_mod(&mut self, m: ast::Mod) -> ast::Mod {
        let folded_mod = fold::noop_fold_mod(m, self);
        folded_mod
    }

    fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
        mac
    }
}

struct CrossCheckExpander {
    // Arguments passed to plugin
    // TODO: pre-parse them???
    external_config: xcfg::Config,
}

impl CrossCheckExpander {
    fn new(args: &[ast::NestedMetaItem]) -> CrossCheckExpander {
        CrossCheckExpander {
            external_config: CrossCheckExpander::parse_config_files(args),
        }
    }

    fn parse_config_files(args: &[ast::NestedMetaItem]) -> xcfg::Config {
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
            .fold(Default::default(), |acc, fc| acc.merge(fc))
    }
}

impl MultiItemModifier for CrossCheckExpander {
    fn expand(&self,
              cx: &mut ExtCtxt,
              sp: Span,
              mi: &ast::MetaItem,
              item: Annotatable) -> Vec<Annotatable> {
        let config = CrossCheckConfig::new(cx).parse_attr_config(cx, mi);
        let top_file_name = cx.codemap().span_to_filename(sp);
        let top_scope = ScopeConfig::new(&self.external_config,
                                         &top_file_name,
                                         Rc::new(config));
        match item {
            Annotatable::Item(i) => {
                // If we're seeing #![cross_check] at the top of the crate or a module,
                // create a fresh configuration and perform a folding; otherwise, just
                // ignore this expansion and let the higher level one do everything
                let ni = match i.node {
                    ast::ItemKind::Mod(_) =>
                        CrossChecker {
                            cx: cx,
                            external_config: &self.external_config,
                            scope_stack: vec![top_scope]
                        }.fold_item(i)
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

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    let ecc = CrossCheckExpander::new(reg.args());
    // TODO: parse args
    reg.register_syntax_extension(
        Symbol::intern("cross_check"),
        SyntaxExtension::MultiModifier(Box::new(ecc)));
}
