use clang_ast::*;
use syntax::ast::*;
use idiomize::ast_manip::make_ast::*;
use syntax::ptr::P;

pub struct TypeConverter {
}

impl TypeConverter {

    pub fn new() -> TypeConverter {
        TypeConverter {
            //cint_type:
        }
    }

    pub fn convert(&self, ctxt: &AstContext, ctype: &TypeNode) -> P<Ty> {
        match ctype.tag {
            TypeTag::TagInt => mk().path_ty(mk().path(vec!["libc","c_int"])),

            TypeTag::TagPointer => {
                let child = ctxt.type_nodes.get(&expect_u64(&ctype.extras[0]).unwrap()).expect("Pointer child not found");
                let child_ty = self.convert(ctxt, child);
                mk().set_mutbl(Mutability::Mutable).ptr_ty(child_ty)
            }

            /*
            TypeTag::TagConstantArray => {
                let child = ctxt.type_nodes.get(ctype.extras[0]).expect("Array child not found");
                let len = ctype.extras[1];

                mk().array_ty()
            }
            */

            _ => panic!("Unsupported type"),
        }
    }
}