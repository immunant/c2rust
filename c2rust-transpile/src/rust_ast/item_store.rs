use c2rust_ast_builder::{mk, Builder};
use indexmap::{IndexMap, IndexSet};
use syn::{ForeignItem, Ident, Item};

use std::borrow::Cow;
use std::collections::HashSet;
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
pub struct PathedMultiImports(IndexMap<(bool, Vec<String>), MultiImport>);

impl PathedMultiImports {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_mut(&mut self, is_absolute: bool, path: Vec<String>) -> &mut MultiImport {
        self.0
            .entry((is_absolute, path))
            .or_insert(MultiImport::new())
    }

    pub fn into_items(self) -> Vec<Box<Item>> {
        self.0
            .into_iter()
            .map(|((is_absolute, mut path), imports)| {
                let mut leaves = imports.leaves;
                let attrs = imports.attrs.unwrap_or_else(mk);

                if leaves.len() == 1 {
                    path.push(leaves.pop().unwrap());

                    let path = if is_absolute {
                        mk().abs_path(path)
                    } else {
                        mk().path(path)
                    };

                    attrs.use_simple_item(path, None as Option<Ident>)
                } else {
                    let path = if is_absolute {
                        mk().abs_path(path)
                    } else {
                        mk().path(path)
                    };

                    attrs.use_multiple_item(path, leaves.into_iter())
                }
            })
            .collect()
    }

    /// Remove all imports covered by the other [`PathedMultiImports`].
    pub fn remove(&mut self, other: &PathedMultiImports) {
        for (k, v) in &mut self.0 {
            // We don't consider attributes, just subtract leaf sets.
            let other_items = other
                .0
                .get(k)
                .map(|imports| imports.leaves.iter().collect::<HashSet<_>>())
                .unwrap_or_default();
            v.leaves.retain(|leaf| !other_items.contains(leaf));
        }
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

    pub fn add_use(&mut self, is_absolute: bool, path: Vec<String>, ident: &str) {
        self.uses.get_mut(is_absolute, path).insert(ident)
    }

    pub fn add_use_with_attr(
        &mut self,
        is_absolute: bool,
        path: Vec<String>,
        ident: &str,
        attrs: Builder,
    ) {
        self.uses
            .get_mut(is_absolute, path)
            .insert_with_attr(ident, attrs)
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
