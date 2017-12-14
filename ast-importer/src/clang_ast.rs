use std::collections::HashMap;
use std::io::Cursor;
use cbor::Items;
use cbor::Cbor;
use cbor::CborBytes;
use cbor::CborError;
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
    pub extras: Vec<Cbor>,
}

#[derive(Debug,Clone)]
pub struct TypeNode {
    pub tag: TypeTag,
    pub extras: Vec<Cbor>,
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
}

#[derive(Debug)]
pub enum DecodeError {
    DecodeCborError(CborError),
    TypeMismatch,
}

pub fn expect_vec8<'a>(val: &'a Cbor) -> Result<&'a Vec<u8>, DecodeError> {
    match val {
        &Cbor::Bytes(CborBytes(ref bytes)) => Ok(bytes),
        _ => Err(DecodeError::TypeMismatch),
    }
}

pub fn expect_array<'a>(val: &'a Cbor) -> Result<&'a Vec<Cbor>, DecodeError> {
    match val {
        &Cbor::Array(ref xs) => Ok(xs),
        _ => Err(DecodeError::TypeMismatch)
    }
}

pub fn expect_string(val: &Cbor) -> Result<String, DecodeError> {
    match val {
        &Cbor::Unicode(ref xs) => Ok(xs.clone()),
        _ => Err(DecodeError::TypeMismatch)
    }
}

pub fn expect_u64(val: &Cbor) -> Result<u64, DecodeError> {
    match val {
        &Cbor::Unsigned(x) => Ok(x.into_u64()),
        _ => { println!("{:?}", val); Err(DecodeError::TypeMismatch) }
    }
}

pub fn expect_f64(val: &Cbor) -> Result<f64, DecodeError> {
    match val {
        &Cbor::Float(x) => Ok(x.into_f64()),
        _ => { println!("{:?}", val); Err(DecodeError::TypeMismatch) }
    }
}

pub fn expect_str(val: &Cbor) -> Result<&str, DecodeError> {
    match val {
        &Cbor::Unicode(ref s) => Ok(s),
        _ => { println!("Got, {:?}; Expected string", val); Err(DecodeError::TypeMismatch) }
    }
}

pub fn expect_opt_str(val: &Cbor) -> Result<Option<&str>, DecodeError> {
    match val {
        &Cbor::Null => Ok(None),
        &Cbor::Unicode(ref s) => Ok(Some(s)),
        _ => { println!("Got, {:?}; Expected string", val); Err(DecodeError::TypeMismatch) }
    }
}

pub fn expect_bool(val: &Cbor) -> Result<bool, DecodeError> {
    match val {
        &Cbor::Bool(b) => Ok(b),
        _ => { println!("{:?}", val); Err(DecodeError::TypeMismatch) }
    }
}

pub fn expect_opt_u64(val: &Cbor) -> Result<Option<u64>, DecodeError> {
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

    let mut top_cbors : Vec<Cbor> = vec![];
    for item in items {
        top_cbors.push(item.unwrap());
    }

    let filenames = top_cbors.remove(2);
    let filenames = expect_array(&filenames).expect("Bad filename array");

    let top_nodes = top_cbors.remove(1);
    let top_nodes = expect_array(&top_nodes).expect("Bad all nodes array");
    let top_nodes : Vec<u64> = top_nodes.iter().map(|x| expect_u64(x).expect("top node list must contain node ids")).collect();

    let all_nodes = top_cbors.remove(0);
    let all_nodes = expect_array(&all_nodes).expect("Bad top nodes array");




    for x in all_nodes {
        let entry = expect_array(x).expect("All nodes entry not array");
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
                extras: entry[2..].to_vec(),
            };

            types.insert(entry_id, node);
        }
    }
    Ok(AstContext {
        top_nodes,
        ast_nodes: asts,
        type_nodes: types,
    })
}
