use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::fs::canonicalize;
use serde_cbor::{Value, from_value};
use serde_cbor::error;
use std;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LRValue {
    LValue, RValue
}

impl LRValue {
    pub fn is_lvalue(&self) -> bool { *self == LRValue::LValue }
    pub fn is_rvalue(&self) -> bool { *self == LRValue::RValue }
}

#[derive(Debug,Clone)]
pub struct AstNode {
    pub tag: ASTEntryTag,
    pub children: Vec<Option<u64>>,
    pub fileid: u64,
    pub line: u64,
    pub column: u64,
    pub file_path: Option<PathBuf>,
    pub type_id: Option<u64>,
    pub rvalue: LRValue,
    pub extras: Vec<Value>,
}

#[derive(Debug,Clone)]
pub struct TypeNode {
    pub tag: TypeTag,
    pub extras: Vec<Value>,
}

#[derive(Debug,Clone)]
pub struct CommentNode {
    pub fileid: u64,
    pub line: u64,
    pub column: u64,
    pub string: String,
}

impl TypeNode {
    // Masks used to decode the IDs given to type nodes
    pub const ID_MASK: u64 = !0b111;
    pub const CONST_MASK: u64 = 0b001;
    pub const RESTRICT_MASK: u64 = 0b010;
    pub const VOLATILE_MASK: u64 = 0b100;
}

#[derive(Debug, Clone)]
pub struct AstContext {
    pub ast_nodes: HashMap<u64, AstNode>,
    pub type_nodes: HashMap<u64, TypeNode>,
    pub top_nodes: Vec<u64>,
    pub comments: Vec<CommentNode>,
}

pub fn expect_opt_str(val: &Value) -> Option<Option<&str>> {
    match *val {
        Value::Null => Some(None),
        Value::String(ref s) => Some(Some(s)),
        _ => None,
    }
}

pub fn expect_opt_u64(val: &Value) -> Option<Option<u64>> {
    match *val {
        Value::Null => Some(None),
        Value::U64(n) => Some(Some(n)),
        _ => None,
    }
}

fn import_ast_tag(tag: u64) -> ASTEntryTag {
    unsafe {
        return std::mem::transmute::<u32, ASTEntryTag>(tag as u32);
    }
}

fn import_type_tag(tag: u64) -> TypeTag {
    unsafe {
        return std::mem::transmute::<u32, TypeTag>(tag as u32);
    }
}

pub fn process(items: Value) -> error::Result<AstContext> {

    let mut asts: HashMap<u64, AstNode> = HashMap::new();
    let mut types: HashMap<u64, TypeNode> = HashMap::new();
    let mut comments: Vec<CommentNode> = vec![];

    let (all_nodes, top_nodes, file_paths, raw_comments):
        (Vec<Vec<Value>>,
         Vec<u64>,
         Vec<String>,
         Vec<(u64, u64, u64, String)>,
        ) = from_value(items)?;

    for (fileid, line, column, string) in raw_comments {
        comments.push(CommentNode{fileid, line, column, string})
    }

    for entry in all_nodes {
        let entry_id = entry[0].as_u64().unwrap();
        let tag = entry[1].as_u64().unwrap();

        if tag < 400 {

            let children =
                entry[2].as_array().unwrap()
                    .iter()
                    .map(|x| expect_opt_u64(x).unwrap())
                    .collect::<Vec<Option<u64>>>();

            let type_id: Option<u64> = expect_opt_u64(&entry[6]).unwrap();
            let fileid = entry[3].as_u64().unwrap();
            let file_path = match file_paths[fileid as usize].as_str() {
                "" => None,
                "?" => None,
                path => Some(canonicalize(Path::new(path)).expect("Could not canonicalize file")),
            };

            let node = AstNode {
                tag: import_ast_tag(tag),
                children,
                fileid,
                line: entry[4].as_u64().unwrap(),
                column: entry[5].as_u64().unwrap(),
                type_id,
                file_path,
                rvalue: if entry[7].as_boolean().unwrap() { LRValue::RValue } else { LRValue::LValue },
                extras: entry[8..].to_vec(),
            };

            asts.insert(entry_id, node);
        } else {
            let node = TypeNode {
                tag: import_type_tag(tag),
                extras: entry[2..].to_vec(),
            };

            types.insert(entry_id, node);
        }
    }
    Ok(AstContext {
        top_nodes,
        ast_nodes: asts,
        type_nodes: types,
        comments,
    })
}
