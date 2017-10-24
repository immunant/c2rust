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
            TypeTag::TagUInt => mk().path_ty(mk().path(vec!["libc","c_uint"])),
            TypeTag::TagSChar => mk().path_ty(mk().path(vec!["libc","c_schar"])),
            TypeTag::TagUChar => mk().path_ty(mk().path(vec!["libc","c_uchar"])),
            TypeTag::TagChar => mk().path_ty(mk().path(vec!["libc","c_char"])),
            TypeTag::TagDouble => mk().path_ty(mk().path(vec!["libc","c_double"])),
            TypeTag::TagFloat => mk().path_ty(mk().path(vec!["libc","c_float"])),

            TypeTag::TagPointer => {
                let child_id = expect_u64(&ctype.extras[0]).expect("Pointer child not found");
                let child_ty = self.convert(ctxt, child_id);
                mk().set_mutbl(Mutability::Mutable).ptr_ty(child_ty)
            }

            TypeTag::TagElaboratedType => {
                let child_id = expect_u64(&ctype.extras[0]).expect("Elaborated child not found");
                self.convert(ctxt, child_id)
            }

            TypeTag::TagRecordType => {
                let child_id = expect_u64(&ctype.extras[0]).expect("Record child not found");
                let decl = ctxt.ast_nodes.get(&child_id).expect("Expected declaration node");
                assert_eq!(decl.tag, ASTEntryTag::TagRecordDecl);
                let name = expect_str(&decl.extras[0]).expect("record name");
                mk().path_ty(mk().path(vec![name]))
            }

            /*
            TypeTag::TagConstantArray => {
                let child = ctxt.type_nodes.get(ctype.extras[0]).expect("Array child not found");
                let len = ctype.extras[1];

                mk().array_ty()
            }
            */

            t => panic!("Unsupported type {:?}", t),
        }
    }
}