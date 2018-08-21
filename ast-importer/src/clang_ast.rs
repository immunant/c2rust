use std::collections::HashMap;
use serde_cbor::{Value, from_value};
use serde_cbor::error;
use std;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[derive(Debug,Clone)]
pub struct AstNode {
    pub tag: ASTEntryTag,
    pub children: Vec<Option<u64>>,
    pub fileid: u64,
    pub line: u64,
    pub column: u64,
    pub type_id: Option<u64>,
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

pub fn expect_vec8(val: &Value) -> Option<&Vec<u8>> {
    val.as_bytes()
}

pub fn expect_array(val: &Value) -> Option<&Vec<Value>> {
    val.as_array()
}

pub fn expect_string(val: &Value) -> Option<String> {
    val.as_string().map(|x| x.to_owned())
}

pub fn expect_u64(val: &Value) -> Option<u64> {
    val.as_u64()
}

pub fn expect_i64(val: &Value) -> Option<i64> {
    val.as_i64()
}

pub fn expect_f64(val: &Value) -> Option<f64> {
    val.as_f64()
}

pub fn expect_str(val: &Value) -> Option<&str> {
    val.as_string().map(|x| x.as_str())
}

pub fn expect_opt_str(val: &Value) -> Option<Option<&str>> {
    match *val {
        Value::Null => Some(None),
        Value::String(ref s) => Some(Some(s)),
        _ => None,
    }
}

pub fn expect_bool(val: &Value) -> Option<bool> {
    val.as_boolean()
}

pub fn expect_opt_u64(val: &Value) -> Option<Option<u64>> {
    if let &Value::Null = val {
        Some(None)
    } else {
        val.as_u64().map(Some)
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

    let (all_nodes, top_nodes, _filenames, raw_comments):
        (Vec<Vec<Value>>,
         Vec<u64>,
         Vec<String>,
         Vec<(u64, u64, u64, String)>,
        ) = from_value(items)?;

    for (fileid, line, column, string) in raw_comments {
        comments.push(CommentNode{fileid, line, column, string})
    }

    for entry in all_nodes {
        let entry_id = expect_u64(&entry[0]).unwrap();
        let tag = expect_u64(&entry[1]).unwrap();

        if tag < 400 {

            let children =
                expect_array(&entry[2]).unwrap()
                    .iter()
                    .map(|x| expect_opt_u64(x).unwrap())
                    .collect::<Vec<Option<u64>>>();

            let type_id: Option<u64> = expect_opt_u64(&entry[6]).unwrap();

            let node = AstNode {
                tag: import_ast_tag(tag),
                children,
                fileid: expect_u64(&entry[3]).unwrap(),
                line: expect_u64(&entry[4]).unwrap(),
                column: expect_u64(&entry[5]).unwrap(),
                type_id,
                extras: entry[7..].to_vec(),
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
