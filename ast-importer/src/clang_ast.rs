use std::collections::HashMap;
use std::io::Cursor;
use cbor::Items;
use cbor::Cbor;
use cbor::CborError;
use std;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[derive(Debug)]
struct AstNode {
    tag: ASTEntryTag,
    children: Vec<Option<u64>>,
    line: u64,
    column: u64,
    type_id: Option<u64>,
    extras: Vec<Cbor>,
}

#[derive(Debug)]
struct TypeNode {
    tag: TypeTag,
    extras: Vec<Cbor>,
}

#[derive(Debug)]
pub struct AstContext {
    ast_nodes: HashMap<u64, AstNode>,
    type_nodes: HashMap<u64, TypeNode>,
}

#[derive(Debug)]
pub enum DecodeError {
    DecodeCborError(CborError),
    TypeMismatch,
}

fn expect_array<'a>(val: &'a Cbor) -> Result<&'a Vec<Cbor>, DecodeError> {
    match val {
        &Cbor::Array(ref xs) => Ok(xs),
        _ => Err(DecodeError::TypeMismatch)
    }
}

fn expect_u64(val: &Cbor) -> Result<u64, DecodeError> {
    match val {
        &Cbor::Unsigned(x) => Ok(x.into_u64()),
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

    for val in items {
        let val1 = val.map_err(DecodeError::DecodeCborError)?;

        for x in expect_array(&val1)? {

            let entry = expect_array(x)?;

            let entry_id = expect_u64(&entry[0])?;
            let tag = expect_u64(&entry[1])?;

            if tag < 400 {
                let mut kids = vec![];
                for x in expect_array(&entry[2])? {
                    kids.push(expect_opt_u64(&x)?)
                }

                let type_id: Option<u64> = expect_opt_u64(&entry[5])?;

                let node = AstNode {
                    tag: import_ast_tag(tag),
                    children: kids,
                    line: expect_u64(&entry[3])?,
                    column: expect_u64(&entry[4])?,
                    type_id: type_id,
                    extras: entry[6..].to_vec(),
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
    }
    Ok(AstContext {
        ast_nodes: asts,
        type_nodes: types,
    })
}
