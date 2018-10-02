use indexmap::{IndexMap, IndexSet};
use syntax::ast::{Item, ForeignItem};
use syntax::ptr::P;
use c_ast::Attribute;
use std::mem::swap;

#[derive(Debug)]
pub struct MultiImport {
    pub attrs: Option<Attribute>,
    pub leaves: IndexSet<String>,
}

impl MultiImport {
    pub fn new() -> Self {
        MultiImport {
            attrs: None,
            leaves: IndexSet::new(),
        }
    }
}

pub type PathedMultiImports = IndexMap<Vec<String>, MultiImport>;

// REVIEW: We might be able to use ItemStore in the Translation struct
// as a replacement for the uses, items, and foreign items fields

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
