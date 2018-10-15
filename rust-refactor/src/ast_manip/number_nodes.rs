use syntax::ast::{NodeId, Mac};
use syntax::fold::{self, Folder};

use ast_manip::Fold;



struct NumberNodes {
    next_id: u32,
}

impl NumberNodes {
    fn next_id(&mut self) -> NodeId {
        let id = NodeId::from_u32(self.next_id);
        self.next_id += 1;
        id
    }
}

impl Folder for NumberNodes {
    fn new_id(&mut self, _i: NodeId) -> NodeId {
        self.next_id()
    }

    fn fold_mac(&mut self, mac: Mac) -> Mac {
        fold::noop_fold_mac(mac, self)
    }
}

/// Assign new `NodeId`s to all nodes in `x`.
pub fn number_nodes<T: Fold>(x: T) -> <T as Fold>::Result {
    // 0 is a valid node id.  DUMMY_NODE_ID is -1.
    x.fold(&mut NumberNodes { next_id: 0 })
}
