use c_ast::*;
use syntax::ast::*;
use idiomize::ast_manip::make_ast::*;
use syntax::ptr::P;
use std::ops::Index;

pub struct TypeConverter {
}

fn mk_qualified(quals: &Qualifiers) -> Builder {
    mk().set_mutbl(if quals.is_const { Mutability::Immutable } else { Mutability:: Mutable })
}

impl TypeConverter {

    pub fn new() -> TypeConverter {
        TypeConverter {
        }
    }

    /// Convert a `C` type to a `Rust` one. For the moment, these are expected to have compatible
    /// memory layouts.
    pub fn convert(&self, ctxt: &TypedAstContext, ctype: CTypeId) -> P<Ty> {

        match ctxt.index(ctype).kind {
            CTypeKind::Void => mk().tuple_ty(vec![] as Vec<P<Ty>>),
            CTypeKind::Bool => mk().path_ty(mk().path(vec!["bool"])),
            CTypeKind::Int => mk().path_ty(mk().path(vec!["libc","c_int"])),
            CTypeKind::Long => mk().path_ty(mk().path(vec!["libc","c_long"])),
            CTypeKind::LongLong => mk().path_ty(mk().path(vec!["libc","c_longlong"])),
            CTypeKind::UInt => mk().path_ty(mk().path(vec!["libc","c_uint"])),
            CTypeKind::ULong => mk().path_ty(mk().path(vec!["libc","c_ulong"])),
            CTypeKind::ULongLong => mk().path_ty(mk().path(vec!["libc","c_ulonglong"])),
            CTypeKind::SChar => mk().path_ty(mk().path(vec!["libc","c_schar"])),
            CTypeKind::UChar => mk().path_ty(mk().path(vec!["libc","c_uchar"])),
            CTypeKind::Char => mk().path_ty(mk().path(vec!["libc","c_char"])),
            CTypeKind::Double => mk().path_ty(mk().path(vec!["libc","c_double"])),
            CTypeKind::Float => mk().path_ty(mk().path(vec!["libc","c_float"])),

            CTypeKind::Pointer(CQualTypeId { ref qualifiers, ref ctype }) => {
                match ctxt.resolve_type(*ctype).kind {

                    // While void converts to () in function returns, it converts to c_void
                    // in the case of pointers.
                    CTypeKind::Void =>
                        mk().ptr_ty(mk().path_ty(vec!["libc","c_void"])),

                    CTypeKind::Function(ref ret, ref params) => {
                        let inputs = params.iter().map(|x|
                            mk().arg(self.convert(ctxt, x.ctype), mk().wild_pat())
                        ).collect();
                        let output = self.convert(ctxt, ret.ctype);
                        mk().unsafe_().barefn_ty(mk().fn_decl(inputs, FunctionRetTy::Ty(output)))
                    }

                    _ => {
                        let child_ty = self.convert(ctxt, *ctype);
                        mk_qualified(qualifiers).ptr_ty(child_ty)
                    }
                }
            }

            CTypeKind::Elaborated(ref ctype) => self.convert(ctxt, *ctype),
            CTypeKind::Decayed(ref ctype) => self.convert(ctxt, *ctype),
            CTypeKind::Paren(ref ctype) => self.convert(ctxt, *ctype),

            CTypeKind::Record(ref decl) => {
                if let CDeclKind::Record { ref name, .. } = ctxt.index(*decl).kind {
                    mk().path_ty(mk().path(vec![name.clone().unwrap()]))
                } else {
                    panic!("{:?} in record type does not point to a record decl", decl)
                }
            }

            CTypeKind::Typedef(ref decl) => {
                if let CDeclKind::Typedef { ref name, .. }  = ctxt.index(*decl).kind {
                    mk().path_ty(mk().path(vec![name]))
                } else {
                    panic!("{:?} in typedef type does not point to a typedef decl", decl)
                }
            }

            CTypeKind::ConstantArray(ref element, count) => {
                let ty = self.convert(ctxt, element.ctype);
                mk().array_ty(ty, mk().lit_expr(mk().int_lit(count as u128, LitIntType::Unsuffixed)))
            }

            ref t => panic!("Unsupported type {:?}", t),
        }
    }
}