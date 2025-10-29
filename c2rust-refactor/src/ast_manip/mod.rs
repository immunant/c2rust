//! General-purpose AST manipulation utilities.  Everything in here works strictly on the AST, with
//! no reliance on HIR or TyCtxt information.

// Modules with simple APIs are private, with their public definitions reexported.
mod ast_deref;
mod ast_equiv;
mod ast_map;
mod ast_names;
mod ast_node;
mod fold;
mod get_node_id;
mod get_span;
mod list_node_ids;
mod load_modules;
mod output_exprs;
mod remove_paren;
mod seq_edit;
mod span_maps;
mod visit;
mod visit_node;

pub use self::ast_deref::AstDeref;
pub use self::ast_equiv::AstEquiv;
pub use self::ast_map::{
    map_ast, map_ast_into, map_ast_into_unified, map_ast_unified, AstMap, NodeTable, UnifiedAstMap,
};
pub use self::ast_names::AstName;
pub use self::ast_node::{AstNode, AstNodeRef};
pub use self::comments::{collect_comments, gather_comments, Comment, CommentMap, CommentStyle};
pub use self::fold::{FlatMapNodes, MutVisit, MutVisitNodes, WalkAst};
pub use self::get_node_id::{GetNodeId, MaybeGetNodeId};
pub use self::get_span::GetSpan;
pub use self::list_node_ids::ListNodeIds;
pub use self::load_modules::load_modules;
pub use self::output_exprs::fold_output_exprs;
pub use self::remove_paren::remove_paren;
pub use self::seq_edit::{fold_blocks, fold_modules};
pub use self::span_maps::{
    child_slot, AstSpanMaps, NodeContextKey, NodeSpan, SpanNodeKind, StructuralContext,
};
pub use self::visit::Visit;
pub use self::visit_node::{visit_nodes, visit_nodes_post, VisitNode};

// Modules with more complex APIs are left as `pub`.
pub mod comments;
pub mod fn_edit;
pub mod lr_expr;
pub mod number_nodes;
pub mod util;
