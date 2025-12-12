use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use ra_ap_hir::Semantics;
use ra_ap_ide_db::RootDatabase;
use ra_ap_load_cargo::{self, LoadCargoConfig, ProcMacroServerChoice};
use ra_ap_project_model::CargoConfig;
use ra_ap_syntax::SyntaxNode;
use rust_util::rewrite::{FlatTokens, OutputBuffer, TokenIndex, render_output};
use std::env;
use std::fs;
use std::iter;
use std::mem;
use std::path::Path;
use std::str::FromStr;
use syn;
use syn::spanned::Spanned;
use syn::visit_mut::{self, VisitMut};

struct AddDerivedItemVisitor<F>(F);

impl<F: FnMut(&mut syn::Item) -> Option<syn::Item>> AddDerivedItemVisitor<F> {
    fn visit_items(&mut self, items: &mut Vec<syn::Item>) {
        let new_items = Vec::with_capacity(items.len());
        let old_items = mem::replace(items, new_items);
        for mut item in old_items {
            let derived_item = (self.0)(&mut item);
            items.push(item);
            if let Some(derived_item) = derived_item {
                items.push(derived_item);
            }
        }
    }
}

impl<F: FnMut(&mut syn::Item) -> Option<syn::Item>> VisitMut for AddDerivedItemVisitor<F> {
    fn visit_item_mod_mut(&mut self, im: &mut syn::ItemMod) {
        let items = match im.content {
            Some((_, ref mut x)) => x,
            None => return,
        };
        self.visit_items(items);
        visit_mut::visit_item_mod_mut(self, im);
    }

    fn visit_file_mut(&mut self, f: &mut syn::File) {
        self.visit_items(&mut f.items);
        visit_mut::visit_file_mut(self, f);
    }
}

fn add_ffi_wrapper(
    _db: &RootDatabase,
    _sema: &Semantics<RootDatabase>,
    _root: SyntaxNode,
    item: &mut syn::Item,
) -> Option<syn::Item> {
    let fn_item = match *item {
        syn::Item::Fn(ref mut x) => x,
        _ => return None,
    };
    let fn_name = fn_item.sig.ident.to_string();

    // Example of gathering semantic information from rust-analyzer:
    /*
    let range = span_to_text_range(fn_item.span());
    let cover = root.covering_element(range);
    // If this assert fails, we might need to look for a `FN` ancestor of `cover` instead.
    debug_assert_eq!(cover.kind(), SyntaxKind::FN);
    let cover = cover.into_node().unwrap();
    let f_ast = ast::Fn::cast(cover).unwrap();
    eprintln!("f_ast = {f_ast:?}");

    let f = sema.to_fn_def(&f_ast).unwrap();
    eprintln!("f = {f:?}");

    let attrs = f.attrs(db);
    // etc...
    */

    // Walk over the attributes of `fn_item`, sorting them into attrs that should remain on the
    // inner function and ones that should be moved or copied onto the newly-generated wrapper.
    let mut inner_attrs = Vec::with_capacity(fn_item.attrs.len());
    let mut wrapper_attrs = Vec::new();
    let mut need_wrapper = false;

    for mut attr in mem::take(&mut fn_item.attrs) {
        let mut pm = ParsedMeta::from(attr.meta);

        let mut move_to_wrapper = false;
        pm.with_innermost_mut(&mut |pm| match pm {
            ParsedMeta::NoMangle(..) => {
                move_to_wrapper = true;
                *pm = ParsedMeta::ExportName(ParsedMetaExportName {
                    ident: syn::Ident::new("export_name", Span::call_site()),
                    eq: syn::Token![=](Span::call_site()),
                    name: syn::LitStr::new(&fn_name, Span::call_site()),
                });
            }
            ParsedMeta::ExportName(..) => {
                move_to_wrapper = true;
            }
            _ => {}
        });

        attr.meta = pm.into();

        if move_to_wrapper {
            wrapper_attrs.push(attr);
            need_wrapper = true;
        } else {
            inner_attrs.push(attr);
        }
    }

    fn_item.attrs = inner_attrs;

    if !need_wrapper {
        return None;
    }

    let wrapper_name = format!("{fn_name}_ffi");
    let mut fn_wrapper = syn::ItemFn {
        attrs: wrapper_attrs,
        vis: fn_item.vis.clone(),
        sig: fn_item.sig.clone(),
        block: Box::new(syn::Block {
            brace_token: syn::token::Brace::default(),
            stmts: Vec::new(),
        }),
    };
    fn_wrapper.sig.ident = syn::Ident::new(&wrapper_name, Span::call_site());

    let mut arg_exprs = Vec::new();
    for (i, arg) in fn_wrapper.sig.inputs.iter_mut().enumerate() {
        let ident = match *arg {
            syn::FnArg::Receiver(_) => syn::Ident::new("self", Span::call_site()),
            syn::FnArg::Typed(ref mut pt) => match *pt.pat {
                syn::Pat::Ident(ref pi) => pi.ident.clone(),
                _ => {
                    let ident = syn::Ident::new(&format!("arg{i}"), Span::call_site());
                    *pt.pat = syn::Pat::Ident(syn::PatIdent {
                        attrs: Vec::new(),
                        by_ref: None,
                        mutability: None,
                        ident: ident.clone(),
                        subpat: None,
                    });
                    ident
                }
            },
        };
        let expr = syn::Expr::Path(syn::ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: syn::Path::from(ident),
        });
        arg_exprs.push(expr);
    }

    let wrapper_expr = syn::Expr::Call(syn::ExprCall {
        attrs: Vec::new(),
        func: Box::new(syn::Expr::Path(syn::ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: syn::Path::from(syn::Ident::new(&fn_name, Span::call_site())),
        })),
        paren_token: syn::token::Paren::default(),
        args: arg_exprs.into_iter().collect(),
    });
    let wrapper_stmt = syn::Stmt::Expr(wrapper_expr, None);
    fn_wrapper.block.stmts.push(wrapper_stmt);

    Some(fn_wrapper.into())
}

/*
fn span_to_text_range(span: Span) -> TextRange {
    let span_range = span.byte_range();
    let lo = TextSize::try_from(span_range.start).unwrap();
    let hi = TextSize::try_from(span_range.end).unwrap();
    TextRange::new(lo, hi)
}
*/

// Helpers for dealing with nested meta items in attrs, like `#[unsafe(no_mangle)]`

#[derive(Clone)]
enum ParsedMeta {
    Meta(syn::Meta),
    Unsafe(Box<ParsedMetaUnsafe>),
    NoMangle(ParsedMetaNoMangle),
    ExportName(ParsedMetaExportName),
}

#[derive(Clone)]
struct ParsedMetaUnsafe {
    ident: syn::Ident,
    paren: syn::token::Paren,
    inner: ParsedMeta,
}

#[derive(Clone)]
struct ParsedMetaNoMangle {
    ident: syn::Ident,
}

#[derive(Clone)]
struct ParsedMetaExportName {
    ident: syn::Ident,
    eq: syn::Token![=],
    name: syn::LitStr,
}

impl ParsedMeta {
    pub fn parse(meta: &syn::Meta) -> syn::Result<ParsedMeta> {
        let ident = meta.path().require_ident()?;
        let ident_str = ident.to_string();
        match ident_str.as_str() {
            "unsafe" => {
                let ml = meta.require_list()?;
                let ident = ml.path.require_ident()?.clone();
                let paren = match ml.delimiter {
                    syn::MacroDelimiter::Paren(p) => p,
                    _ => {
                        return Err(syn::Error::new(
                            ml.delimiter.span().open(),
                            "expected parens",
                        ));
                    }
                };
                let meta: syn::Meta = syn::parse2(ml.tokens.clone())?;
                let inner = ParsedMeta::from(meta);
                Ok(ParsedMeta::Unsafe(Box::new(ParsedMetaUnsafe {
                    ident,
                    paren,
                    inner,
                })))
            }
            "no_mangle" => {
                let _ = meta.require_path_only()?;
                let ident = ident.clone();
                Ok(ParsedMeta::NoMangle(ParsedMetaNoMangle { ident }))
            }
            "export_name" => {
                let mnv = meta.require_name_value()?;
                let ident = mnv.path.require_ident()?.clone();
                let eq = mnv.eq_token;
                let expr_lit = match mnv.value {
                    syn::Expr::Lit(ref el) => el,
                    _ => return Err(syn::Error::new(mnv.value.span(), "expected Lit")),
                };
                let syn::ExprLit { ref attrs, ref lit } = *expr_lit;
                if attrs.len() > 0 {
                    return Err(syn::Error::new(expr_lit.span(), "name must not have attrs"));
                }
                let name = match *lit {
                    syn::Lit::Str(ref ls) => ls.clone(),
                    _ => return Err(syn::Error::new(lit.span(), "expected Str")),
                };
                Ok(ParsedMeta::ExportName(ParsedMetaExportName {
                    ident,
                    eq,
                    name,
                }))
            }
            _ => Ok(ParsedMeta::Meta(meta.clone())),
        }
    }

    pub fn with_innermost_mut(&mut self, f: &mut impl FnMut(&mut ParsedMeta)) {
        match *self {
            ParsedMeta::Meta(..) | ParsedMeta::NoMangle(..) | ParsedMeta::ExportName(..) => f(self),
            ParsedMeta::Unsafe(ref mut pmu) => pmu.inner.with_innermost_mut(f),
        }
    }
}

impl From<syn::Meta> for ParsedMeta {
    fn from(meta: syn::Meta) -> ParsedMeta {
        match Self::parse(&meta) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("warning: failed to parse `{}`: {e}", meta.to_token_stream());
                ParsedMeta::Meta(meta)
            }
        }
    }
}

impl From<ParsedMeta> for syn::Meta {
    fn from(pm: ParsedMeta) -> syn::Meta {
        syn::parse2(pm.into_token_stream()).unwrap()
    }
}

impl ToTokens for ParsedMeta {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match *self {
            ParsedMeta::Meta(ref x) => x.to_tokens(tokens),
            ParsedMeta::Unsafe(ref x) => x.to_tokens(tokens),
            ParsedMeta::NoMangle(ref x) => x.to_tokens(tokens),
            ParsedMeta::ExportName(ref x) => x.to_tokens(tokens),
        }
    }
}

impl ToTokens for ParsedMetaUnsafe {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ident.to_tokens(tokens);
        self.paren.surround(tokens, |tokens| {
            self.inner.to_tokens(tokens);
        });
    }
}

impl ToTokens for ParsedMetaNoMangle {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ident.to_tokens(tokens);
    }
}

impl ToTokens for ParsedMetaExportName {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ident.to_tokens(tokens);
        self.eq.to_tokens(tokens);
        self.name.to_tokens(tokens);
    }
}

fn main() {
    let cargo_dir_path = env::args().nth(1).unwrap();
    let cargo_dir_path = Path::new(&cargo_dir_path);

    let cargo_config = CargoConfig::default();

    let load_cargo_config: LoadCargoConfig = LoadCargoConfig {
        load_out_dirs_from_check: true,
        with_proc_macro_server: ProcMacroServerChoice::Sysroot,
        prefill_caches: false,
    };

    let (db, vfs, _proc_macro_client) = ra_ap_load_cargo::load_workspace_at(
        cargo_dir_path,
        &cargo_config,
        &load_cargo_config,
        &|_msg| {},
    )
    .unwrap();

    // Assume the first file in `vfs` is the crate root.
    let (first_file_id, _) = vfs.iter().next().unwrap();

    let sema = Semantics::new(&db);

    eprintln!("processing crate...");
    let krate = sema.first_crate(first_file_id).unwrap();

    let mut files = Vec::new();
    for m in krate.modules(&db) {
        let src = m.definition_source(&db);
        let node = src.value.node();
        if let Some(editioned_file_id) = m.as_source_file_id(&db) {
            sema.parse(editioned_file_id);
            let file_id = editioned_file_id.file_id(&db);
            let vfs_path = vfs.file_path(file_id);
            if let Some(path) = vfs_path.as_path() {
                files.push((path.to_path_buf(), node));
            }
        }
    }

    for (path, root) in files {
        // Only rewrite files that are inside the provided cargo dir.  If our strategy for finding
        // the main crate is wrong, this will keep us from overwriting files unexpectedly.
        let mut ancestors = iter::successors(Some(path.as_path()), |p| p.parent());
        if !ancestors.any(|a| a == cargo_dir_path) {
            eprintln!("skip {path:?}: outside cargo dir {cargo_dir_path:?}");
            continue;
        }

        let code = fs::read_to_string(&path).unwrap();

        let ts = TokenStream::from_str(&code).unwrap();
        let orig_tokens = FlatTokens::new(ts.clone()).collect::<Vec<_>>();
        let ti = TokenIndex::new(&orig_tokens);

        let mut ast: syn::File = syn::parse2(ts.clone()).unwrap();
        let mut v = AddDerivedItemVisitor(|i: &mut syn::Item| -> Option<syn::Item> {
            add_ffi_wrapper(&db, &sema, root.clone(), i)
        });
        v.visit_file_mut(&mut ast);

        let new_ts = ast.into_token_stream();

        let mut buf = OutputBuffer::new();
        render_output(&code, &orig_tokens, &ti, new_ts, &mut buf);
        let s = buf.finish();
        fs::write(&path, &s).unwrap();
        eprintln!("wrote {:?}", path);
    }
}
