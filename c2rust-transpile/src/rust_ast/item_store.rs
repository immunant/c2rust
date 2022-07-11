use c2rust_ast_builder::{mk, Builder};
use indexmap::{IndexMap, IndexSet};
use syn::{ForeignItem, Ident, Item};

use std::borrow::Cow;
use std::mem::swap;

#[derive(Debug)]
pub struct MultiImport {
    attrs: Option<Builder>,
    leaves: IndexSet<String>,
}

impl MultiImport {
    fn new() -> Self {
        MultiImport {
            attrs: None,
            leaves: IndexSet::new(),
        }
    }

    pub fn insert<'a, S>(&mut self, leaf: S)
    where
        S: Into<Cow<'a, str>>,
    {
        self.leaves.insert(leaf.into().into_owned());
    }

    pub fn insert_with_attr<'a, S>(&mut self, leaf: S, attrs: Builder)
    where
        S: Into<Cow<'a, str>>,
    {
        self.insert(leaf);
        self.attrs = Some(attrs);
    }
}

#[derive(Debug, Default)]
pub struct PathedMultiImports(IndexMap<Vec<String>, MultiImport>);

impl PathedMultiImports {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_mut(&mut self, path: Vec<String>) -> &mut MultiImport {
        self.0.entry(path).or_insert(MultiImport::new())
    }

    pub fn into_items(self) -> Vec<Box<Item>> {
        fn build_items((mut path, imports): (Vec<String>, MultiImport)) -> Box<Item> {
            let mut leaves = imports.leaves;
            let attrs = imports.attrs.unwrap_or_else(mk);

            if leaves.len() == 1 {
                path.push(leaves.pop().unwrap());

                attrs.use_simple_item(path, None as Option<Ident>)
            } else {
                attrs.use_multiple_item(path, leaves.into_iter())
            }
        }

        self.0.into_iter().map(build_items).collect()
    }
}

#[derive(Debug, Default)]
pub struct ItemStore {
    // Fixing this would require major refactors for marginal benefit.
    #[allow(clippy::vec_box)]
    items: Vec<Box<Item>>,
    foreign_items: Vec<ForeignItem>,
    uses: PathedMultiImports,
}

impl ItemStore {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_item(&mut self, item: Box<Item>) {
        self.items.push(item);
    }

    pub fn add_foreign_item(&mut self, item: ForeignItem) {
        self.foreign_items.push(item);
    }

    pub fn add_use(&mut self, path: Vec<String>, ident: &str) {
        self.uses.get_mut(path).insert(ident)
    }

    pub fn add_use_with_attr(&mut self, path: Vec<String>, ident: &str, attrs: Builder) {
        self.uses.get_mut(path).insert_with_attr(ident, attrs)
    }

    pub fn drain(&mut self) -> (Vec<Box<Item>>, Vec<ForeignItem>, PathedMultiImports) {
        let mut items = Vec::new();
        let mut foreign_items = Vec::new();
        let mut uses = PathedMultiImports::new();

        swap(&mut items, &mut self.items);
        swap(&mut foreign_items, &mut self.foreign_items);
        swap(&mut uses, &mut self.uses);

        (items, foreign_items, uses)
    }
}
