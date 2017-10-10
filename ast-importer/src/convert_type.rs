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

    pub fn convert(&self, ctxt: &AstContext, ctype_id: u64) -> P<Ty> {

        let ctype = ctxt.get_type(ctype_id).unwrap();

        match ctype.tag {
            TypeTag::TagVoid => mk().tuple_ty(vec![] as Vec<P<Ty>>),
            TypeTag::TagBool => mk().path_ty(mk().path(vec!["bool"])),
            TypeTag::TagInt => mk().path_ty(mk().path(vec!["libc","c_int"])),

            TypeTag::TagPointer => {
                let child_id = expect_u64(&ctype.extras[0]).expect("Pointer child not found");
                let child_ty = self.convert(ctxt, child_id);
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