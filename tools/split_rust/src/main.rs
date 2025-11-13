use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;
use proc_macro2::Span;
use rust_util::collect::FileCollector;
use serde_json;
use syn;
use syn::spanned::Spanned;
use syn::visit::{self, Visit};

struct ItemSpanVisitor<'a> {
    file_path: &'a Path,
    cur_path: Vec<String>,
    item_spans: Vec<(Vec<String>, &'a Path, usize, usize)>,
}

impl<'a> ItemSpanVisitor<'a> {
    pub fn new(file_path: &'a Path, mod_path: Vec<String>) -> ItemSpanVisitor<'a> {
        ItemSpanVisitor {
            file_path,
            cur_path: mod_path,
            item_spans: Vec::new(),
        }
    }

    fn _emit(&mut self, name: String, sp: Span) {
        self.enter(name, sp, |_| {});
    }

    fn enter<R>(&mut self, name: String, sp: Span, f: impl FnOnce(&mut Self) -> R) -> R {
        self.cur_path.push(name);

        let range = sp.byte_range();
        self.item_spans.push((self.cur_path.clone(), self.file_path, range.start, range.end));
        let r = f(self);

        self.cur_path.pop();
        r
    }
}

impl Visit<'_> for ItemSpanVisitor<'_> {
    fn visit_item(&mut self, item: &syn::Item) {
        match *item {
            syn::Item::Fn(ref ifn) => {
                let name = ifn.sig.ident.to_string();
                self.enter(name, ifn.span(), |v| v.visit_item_fn(ifn));
            },
            syn::Item::Mod(ref im) => {
                let name = im.ident.to_string();
                self.enter(name, im.span(), |v| v.visit_item_mod(im));
            },
            _ => {
                visit::visit_item(self, item);
            },
        }
    }
}

fn main() {
    let mut fc = FileCollector::default();
    fc.parse(env::args().nth(1).unwrap(), vec![], true).unwrap();
    let mut out = HashMap::new();
    for &(ref name, ref mod_path, ref ast) in &fc.files {
        eprintln!("visit {:?}", name);
        let mut v = ItemSpanVisitor::new(name, mod_path.to_owned());
        v.visit_file(ast);

        let src = fs::read_to_string(name).unwrap();
        for &(ref item_path, _, lo, hi) in &v.item_spans {
            let snippet = &src[lo .. hi];
            out.insert(item_path.join("::"), snippet.to_owned());
        }
    }
    serde_json::to_writer(std::io::stdout(), &out).unwrap();
}
