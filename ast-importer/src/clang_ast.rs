use std::collections::HashMap;
use std::io::Cursor;
use cbor::Items;
use cbor::Cbor;
use cbor::CborError;
use std;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[derive(Debug)]
pub struct AstNode {
    pub tag: ASTEntryTag,
    pub children: Vec<Option<u64>>,
    pub fileid: u64,
    pub line: u64,
    pub column: u64,
    pub type_id: Option<u64>,
    pub extras: Vec<Cbor>,
}

#[derive(Debug,Clone)]
pub struct TypeNode {
    pub tag: TypeTag,
    pub constant: bool,
    pub extras: Vec<Cbor>,
}

#[derive(Debug)]
pub struct AstContext {
    pub ast_nodes: HashMap<u64, AstNode>,
    pub type_nodes: HashMap<u64, TypeNode>,
}

#[derive(Debug)]
pub enum DecodeError {
    DecodeCborError(CborError),
    TypeMismatch,
}

impl AstContext {
    pub fn get_type(&self, node_id: u64) -> Option<TypeNode> {
        self.type_nodes
            .get(&(node_id & !1u64))
            .cloned()
            .map(|mut x| {
                if node_id & 1 == 1 {
                    x.constant = true
                }
                x
            })
    }
}

fn expect_array<'a>(val: &'a Cbor) -> Result<&'a Vec<Cbor>, DecodeError> {
    match val {
        &Cbor::Array(ref xs) => Ok(xs),
        _ => Err(DecodeError::TypeMismatch)
    }
}

pub fn expect_u64(val: &Cbor) -> Result<u64, DecodeError> {
    match val {
        &Cbor::Unsigned(x) => Ok(x.into_u64()),
        _ => { println!("{:?}", val); Err(DecodeError::TypeMismatch) }
    }
}

pub fn expect_str(val: &Cbor) -> Result<&str, DecodeError> {
    match val {
        &Cbor::Unicode(ref s) => Ok(s),
        _ => { println!("{:?}", val); Err(DecodeError::TypeMismatch) }
    }
}

fn expect_bool(val: &Cbor) -> Result<bool, DecodeError> {
    match val {
        &Cbor::Bool(b) => Ok(b),
        _ => { println!("{:?}", val); Err(DecodeError::TypeMismatch) }
    }
}

fn expect_opt_u64(val: &Cbor) -> Result<Option<u64>, DecodeError> {
    match val {
        &Cbor::Null => Ok(None),
        &Cbor::Unsigned(x) => Ok(Some(x.into_u64())),
        _ => { println!("{:?}", val); Err(DecodeError::TypeMismatch) }
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

pub fn process(items: Items<Cursor<Vec<u8>>>) -> Result<AstContext, DecodeError> {

    let mut asts: HashMap<u64, AstNode> = HashMap::new();
    let mut types: HashMap<u64, TypeNode> = HashMap::new();

    for val in items.take(1) {
        let val1 = val.map_err(DecodeError::DecodeCborError)?;

        for x in expect_array(&val1)? {

            let entry = expect_array(x)?;
            println!("{:?}", entry);
            let entry_id = expect_u64(&entry[0])?;
            let tag = expect_u64(&entry[1])?;

            if tag < 400 {
                let mut kids = vec![];
                for x in expect_array(&entry[2])? {
                    kids.push(expect_opt_u64(&x)?)
                }

                let type_id: Option<u64> = expect_opt_u64(&entry[6])?;

                let node = AstNode {
                    tag: import_ast_tag(tag),
                    children: kids,
                    fileid: expect_u64(&entry[3])?,
                    line: expect_u64(&entry[4])?,
                    column: expect_u64(&entry[5])?,
                    type_id,
                    extras: entry[7..].to_vec(),
                };

                asts.insert(entry_id, node);

            } else {

                let node = TypeNode {
                    tag: import_type_tag(tag),
                    constant: false,
                    extras: entry[2..].to_vec(),
                };

                types.insert(entry_id, node);
            }
        }
    }
    Ok(AstContext {
        ast_nodes: asts,
        type_nodes: types,
    })
}
