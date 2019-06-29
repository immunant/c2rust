use serde_bytes::ByteBuf;
use serde_cbor::error;
use serde_cbor::{from_value, Value};
use std;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LRValue {
    LValue,
    RValue,
}

impl LRValue {
    pub fn is_lvalue(&self) -> bool {
        *self == LRValue::LValue
    }
    pub fn is_rvalue(&self) -> bool {
        *self == LRValue::RValue
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Ord, Eq)]
pub struct SrcLoc {
    pub fileid: u64,
    pub line: u64,
    pub column: u64,
}

#[derive(Debug, Clone)]
pub struct AstNode {
    pub tag: ASTEntryTag,
    pub children: Vec<Option<u64>>,
    pub loc: SrcLoc,
    pub type_id: Option<u64>,
    pub rvalue: LRValue,

    // Stack of macros this node was expanded from, beginning with the initial
    // macro call and ending with the leaf. This needs to be a stack for nested
    // macro definitions.
    pub macro_expansions: Vec<u64>,
    pub extras: Vec<Value>,
}

#[derive(Debug, Clone)]
pub struct TypeNode {
    pub tag: TypeTag,
    pub extras: Vec<Value>,
}

#[derive(Debug, Clone)]
pub struct CommentNode {
    pub loc: SrcLoc,
    pub string: String,
}

#[derive(Debug, Clone)]
pub struct SrcFile {
    pub path: Option<PathBuf>,
    pub include_loc: Option<SrcLoc>,
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
    pub files: Vec<SrcFile>,
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

    let (all_nodes, top_nodes, files, raw_comments): (
        Vec<Vec<Value>>,
        Vec<u64>,
        Vec<(String, Option<(u64, u64, u64)>)>,
        Vec<(u64, u64, u64, ByteBuf)>,
    ) = from_value(items)?;

    for (fileid, line, column, bytes) in raw_comments {
        comments.push(CommentNode {
            loc: SrcLoc { fileid, line, column },
            string: String::from_utf8_lossy(&bytes).to_string(),
        })
    }

    let files = files.into_iter()
        .map(|(path, loc)| {
            let path = match path.as_str() {
                "" => None,
                "?" => None,
                path => Some(Path::new(path).to_path_buf()),
            };
            SrcFile {
                path,
                include_loc: loc.map(|(fileid, line, column)| SrcLoc { fileid, line, column }),
            }
        })
        .collect::<Vec<_>>();

    for entry in all_nodes {
        let entry_id = entry[0].as_u64().unwrap();
        let tag = entry[1].as_u64().unwrap();

        if tag < 400 {
            let children = entry[2]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| expect_opt_u64(x).unwrap())
                .collect::<Vec<Option<u64>>>();

            let type_id: Option<u64> = expect_opt_u64(&entry[6]).unwrap();
            let fileid = entry[3].as_u64().unwrap();

            let macro_expansions = entry[8]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| x.as_u64().unwrap())
                .collect::<Vec<u64>>();

            let node = AstNode {
                tag: import_ast_tag(tag),
                children,
                loc: SrcLoc {
                    fileid,
                    line: entry[4].as_u64().unwrap(),
                    column: entry[5].as_u64().unwrap(),
                },
                type_id,
                rvalue: if entry[7].as_boolean().unwrap() {
                    LRValue::RValue
                } else {
                    LRValue::LValue
                },
                macro_expansions,
                extras: entry[9..].to_vec(),
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
        files,
    })
}
