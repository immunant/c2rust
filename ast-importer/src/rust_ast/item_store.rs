use syntax::ast::*;
use syntax::ptr::P;
use std::mem::swap;

#[derive(Debug)]
pub struct ItemStore {
    pub items: Vec<P<Item>>,
    pub foreign_items: Vec<ForeignItem>,
}

impl ItemStore {
    pub fn new() -> Self {
        ItemStore {
            items: Vec::new(),
            foreign_items: Vec::new(),
        }
    }

    pub fn drain(&mut self) -> (Vec<P<Item>>, Vec<ForeignItem>) {
        let mut items = Vec::new();
        let mut foreign_items = Vec::new();

        swap(&mut items, &mut self.items);
        swap(&mut foreign_items, &mut self.foreign_items);

        (items, foreign_items)
    }
}
