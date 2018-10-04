use indexmap::{IndexMap, IndexSet};
use rust_ast::mk;
use syntax::ast::{Attribute, Item, ForeignItem};
use syntax::ptr::P;

use std::mem::swap;

#[derive(Debug)]
pub struct MultiImport {
    pub attrs: Option<Attribute>,
    pub leaves: IndexSet<String>,
}

impl MultiImport {
    fn new() -> Self {
        MultiImport {
            attrs: None,
            leaves: IndexSet::new(),
        }
    }
}

#[derive(Debug)]
pub struct PathedMultiImports(IndexMap<Vec<String>, MultiImport>);

impl PathedMultiImports {
    pub fn new() -> Self {
        PathedMultiImports(IndexMap::new())
    }

    pub fn get_mut(&mut self, path: Vec<String>) -> &mut MultiImport {
        self.0.entry(path).or_insert(MultiImport::new())
    }

    pub fn into_items(self) -> Vec<P<Item>> {
        // TODO: Apply attributes

        self.0
            .into_iter()
            .map(|(path, imports)| mk().use_multiple_item(path, imports.leaves.iter().collect()))
            .collect()
    }
}

#[derive(Debug)]
pub struct ItemStore {
    pub items: Vec<P<Item>>,
    pub foreign_items: Vec<ForeignItem>,
    pub uses: PathedMultiImports,
}

impl ItemStore {
    pub fn new() -> Self {
        ItemStore {
            items: Vec::new(),
            foreign_items: Vec::new(),
            uses: PathedMultiImports::new(),
        }
    }

    pub fn drain(&mut self) -> (Vec<P<Item>>, Vec<ForeignItem>, PathedMultiImports) {
        let mut items = Vec::new();
        let mut foreign_items = Vec::new();
        let mut uses = PathedMultiImports::new();

        swap(&mut items, &mut self.items);
        swap(&mut foreign_items, &mut self.foreign_items);
        swap(&mut uses, &mut self.uses);

        (items, foreign_items, uses)
    }
}
