use clang_ast::*;
use syntax::ast::*;

pub struct TypeConverter {

    cint_type: Ty,

}

impl TypeConverter {

    pub fn new() -> TypeConverter {
        unimplemented!();
    }

    pub fn convert(&self, ctxt: &AstContext, ctype: &TypeNode) -> Ty {
        match ctype.tag {
            TypeTag::TagInt => self.cint_type,
            _ => panic!("Unsupported type"),
        }
    }
}