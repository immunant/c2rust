
use syntax::ast;
use syntax::ast::*;
use syntax::tokenstream::{TokenStream};
use syntax::parse::token::{DelimToken,Token};
use syntax::abi::Abi;
use renamer::Renamer;
use convert_type::TypeConverter;
use loops::*;
use syntax::ptr::P;
use idiomize::ast_manip::make_ast::*;
use std::collections::HashMap;
use c_ast::CLabelId;


#[derive(Copy,Clone,PartialEq,Eq,PartialOrd,Ord,Debug,Hash)]
pub struct Label(pub u64);

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub body: Vec<Stmt>,
    pub terminator: Terminator,
}

#[derive(Clone, Debug)]
pub enum Terminator {
    /// End of control-flow. For example: the last statement in a function, or a return
    End,

    /// Unconditional branch to another block
    Jump(Label),

    /// Conditional branch to another block. The expression is expected to be a boolean Rust
    /// expression
    Branch(P<Expr>, Label, Label),
}

#[derive(Clone, Debug)]
pub struct Cfg {
    entry: Label,
    graph: HashMap<Label, BasicBlock>,

    prev_label: u64,

    pub labels: HashMap<CLabelId, Label>,      // Map of all labels encountered

    pub break_label: Option<Label>,
    pub continue_label: Option<Label>,
}

impl Cfg {
    pub fn add_block(&mut self, lbl: Label, bb: BasicBlock) -> () {
        match self.graph.insert(lbl, bb) {
            None => { },
            Some(_) => panic!("Label {:?} cannot identify two basic blocks", lbl),
        }
    }

    pub fn fresh_label(&mut self) -> Label {
        self.prev_label += 1;
        Label(self.prev_label)
    }
}
