use proc_macro2::Span;
use syn;
use syn::spanned::Spanned;
use syn::visit::{self, Visit};

struct ItemSpanVisitor {
    cur_path: Vec<String>,
    item_spans: Vec<(Vec<String>, usize, usize)>,
}

impl ItemSpanVisitor {
    pub fn new(mod_path: Vec<String>) -> ItemSpanVisitor {
        ItemSpanVisitor {
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
        self.item_spans
            .push((self.cur_path.clone(), range.start, range.end));
        let r = f(self);

        self.cur_path.pop();
        r
    }
}

impl Visit<'_> for ItemSpanVisitor {
    fn visit_item(&mut self, item: &syn::Item) {
        match *item {
            syn::Item::Fn(ref ifn) => {
                let name = ifn.sig.ident.to_string();
                self.enter(name, ifn.span(), |v| v.visit_item_fn(ifn));
            }
            syn::Item::Mod(ref im) => {
                let name = im.ident.to_string();
                self.enter(name, im.span(), |v| v.visit_item_mod(im));
            }
            // TODO: handle other items that can contain nested items.  Note that any expr or type
            // can contain items, e.g. `type T = [u8; { fn f(){} 10 }];`
            _ => {
                visit::visit_item(self, item);
            }
        }
    }
}

pub fn item_spans(mod_path: Vec<String>, ast: &syn::File) -> Vec<(Vec<String>, usize, usize)> {
    let mut v = ItemSpanVisitor::new(mod_path);
    v.visit_file(ast);
    v.item_spans
}
