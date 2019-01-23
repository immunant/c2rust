use std::collections::HashMap;
use syntax::ast::*;
use syntax::attr::HasAttrs;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::visit::{self, Visitor};

use crate::ast_manip::{Fold, GetNodeId, Visit};


struct CollectCfgAttrs {
    node_attrs: HashMap<NodeId, Vec<Attribute>>,
}

impl CollectCfgAttrs {
    fn collect<T: HasAttrs + GetNodeId>(&mut self, x: &T) {
        let attrs = x.attrs().iter()
            .filter(|attr| attr.check_name("cfg_attr"))
            .cloned()
            .collect::<Vec<_>>();
        if attrs.len() > 0 {
            self.node_attrs.insert(x.get_node_id(), attrs);
        }
    }
}

macro_rules! collect_cfg_attrs {
    ($($visit_thing:ident($Thing:ty), $walk_thing:ident;)*) => {
        impl<'ast> Visitor<'ast> for CollectCfgAttrs {
            $(
                fn $visit_thing(&mut self, x: &'ast $Thing) {
                    self.collect(x);
                    visit::$walk_thing(self, x);
                }
            )*

            fn visit_mac(&mut self, mac: &'ast Mac) {
                visit::walk_mac(self, mac)
            }
        }
    };
}

collect_cfg_attrs! {
    visit_item(Item), walk_item;
    visit_impl_item(ImplItem), walk_impl_item;
    visit_trait_item(TraitItem), walk_trait_item;
    visit_foreign_item(ForeignItem), walk_foreign_item;
    visit_stmt(Stmt), walk_stmt;
    visit_expr(Expr), walk_expr;
    // TODO: extend this list with the remaining node types
}

pub fn collect_cfg_attrs(krate: &Crate) -> HashMap<NodeId, Vec<Attribute>> {
    let mut v = CollectCfgAttrs {
        node_attrs: HashMap::new(),
    };
    krate.visit(&mut v);
    v.node_attrs
}


struct RestoreCfgAttrs {
    node_attrs: HashMap<NodeId, Vec<Attribute>>,
}

impl RestoreCfgAttrs {
    fn restore<T: HasAttrs + GetNodeId>(&mut self, x: T) -> T {
        if let Some(cfg_attrs) = self.node_attrs.get(&x.get_node_id()) {
            info!("RESTORE ATTRS {:?} onto {:?}",
                  cfg_attrs.iter().map(|a| ::syntax::print::pprust::attr_to_string(a))
                    .collect::<Vec<_>>(),
                  x.attrs().iter().map(|a| ::syntax::print::pprust::attr_to_string(a))
                    .collect::<Vec<_>>());
            let x2 =
            x.map_attrs(|mut attrs| {
                // Drop attrs that were produced by evaluation of one of the `#[cfg_attr]`s.
                attrs.retain(|a| !cfg_attrs.iter().any(|ca| ca.span.contains(a.span)));
                // Now put the #[cfg_attr]s themselves back in.
                attrs.extend(cfg_attrs.iter().cloned());
                attrs
            });
            info!("  attrs changed to {:?}",
                  x2.attrs().iter().map(|a| ::syntax::print::pprust::attr_to_string(a))
                    .collect::<Vec<_>>());
            x2
        } else {
            x
        }
    }
}

macro_rules! restore_cfg_attrs {
    ($slf:ident, $thing:ident;
     $($fold_thing:ident($Thing:ty), $noop_fold_thing:expr;)*) => {
        impl Folder for RestoreCfgAttrs {
            $(
                fn $fold_thing(&mut $slf, $thing: $Thing) -> <$Thing as Fold>::Result {
                    let $thing = $slf.restore($thing);
                    $noop_fold_thing
                }
            )*

            fn fold_mac(&mut self, mac: Mac) -> Mac {
                fold::noop_fold_mac(mac, self)
            }
        }
    };
}

restore_cfg_attrs! {
    self, x;
    fold_item(P<Item>), fold::noop_fold_item(x, self);
    fold_impl_item(ImplItem), fold::noop_fold_impl_item(x, self);
    fold_trait_item(TraitItem), fold::noop_fold_trait_item(x, self);
    fold_foreign_item(ForeignItem), fold::noop_fold_foreign_item(x, self);
    fold_stmt(Stmt), fold::noop_fold_stmt(x, self);
    fold_expr(P<Expr>), x.map(|x| fold::noop_fold_expr(x, self));
    // TODO: extend this list with the remaining node types
}

pub fn restore_cfg_attrs(krate: Crate, node_attrs: HashMap<NodeId, Vec<Attribute>>) -> Crate {
    let mut f = RestoreCfgAttrs { node_attrs };
    krate.fold(&mut f)
}
