use std::env;
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
    pub fn new(file_path: &'a Path) -> ItemSpanVisitor<'a> {
        ItemSpanVisitor {
            file_path,
            cur_path: Vec::new(),
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
    fc.parse(env::args().nth(1).unwrap(), true).unwrap();
    let mut out = Vec::new();
    for &(ref name, ref ast) in &fc.files {
        eprintln!("visit {:?}", name);
        let mut v = ItemSpanVisitor::new(name);
        v.visit_file(ast);
        out.extend(v.item_spans);
    }
    serde_json::to_writer(std::io::stdout(), &out).unwrap();
}
