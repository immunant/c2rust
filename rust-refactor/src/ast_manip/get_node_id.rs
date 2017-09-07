//! `GetNodeId` trait for obtaining the `NodeId` of a generic AST node.
use syntax::ast::*;
use syntax::ptr::P;


/// Trait for obtaining the `NodeId` of a generic AST node.
pub trait GetNodeId {
    fn get_node_id(&self) -> NodeId;
}

impl<T: GetNodeId> GetNodeId for P<T> {
    fn get_node_id(&self) -> NodeId {
        <T as GetNodeId>::get_node_id(self)
    }
}

include!(concat!(env!("OUT_DIR"), "/get_node_id_gen.inc.rs"));
