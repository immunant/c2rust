use std::collections::HashMap;
use std::io::Cursor;
use cbor::Items;
use cbor::Cbor;
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
    pub constant: bool,
    pub extras: Vec<Cbor>,
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

    pub fn resolve_type(&self, node: TypeNode) -> TypeNode {
        match node.tag {
            TypeTag::TagElaboratedType | TypeTag::TagDecayedType | TypeTag::TagTypeOfType => {
                let child_id = expect_u64(&node.extras[0]).expect("child id");
                let node = self.get_type(child_id).expect("child node");
                self.resolve_type(node)
            }
            TypeTag::TagTypedefType => {
                let child_id = expect_u64(&node.extras[0]).expect("child id");
                let decl = self.ast_nodes.get(&child_id).expect("child node");
                assert_eq!(decl.tag, ASTEntryTag::TagTypedefDecl);
                let type_id = decl.type_id.expect("typedef type");
                let type_node = self.get_type(type_id).expect("type node");
                self.resolve_type(type_node)
            }
            _ => node,
        }
    }
}

impl AstNode {
    pub fn get_decl_name(&self) -> Option<&str> {
        match self.tag {
            ASTEntryTag::TagVarDecl => Some(expect_str(&self.extras[0]).unwrap()),
            ASTEntryTag::TagFunctionDecl => Some(expect_str(&self.extras[0]).unwrap()),
            _ => None,
        }
    }
}

impl TypeNode {
    pub fn is_pointer(&self) -> bool {
        match self.tag {
            TypeTag::TagPointer => true,
            _ => false,
        }
    }
    pub fn is_unsigned_integral_type(&self) -> bool {
        match self.tag {
            TypeTag::TagUInt | TypeTag::TagUShort | TypeTag::TagULong | TypeTag::TagULongLong => true,
            _ => false
        }

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

pub fn expect_bool(val: &Cbor) -> Result<bool, DecodeError> {
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
                constant: false,
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
