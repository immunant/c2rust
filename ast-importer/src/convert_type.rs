use c_ast::*;
use syntax::ast::*;
use syntax::abi::Abi;
use idiomize::ast_manip::make_ast::*;
use syntax::ptr::P;
use std::ops::Index;
use renamer::Renamer;

pub struct TypeConverter {
}

impl TypeConverter {

    pub fn new() -> TypeConverter {
        TypeConverter {
        }
    }

    /// Convert a `C` type to a `Rust` one. For the moment, these are expected to have compatible
    /// memory layouts.
    pub fn convert(&self, ctxt: &TypedAstContext, renamer: &Renamer<String>, ctype: CTypeId) -> P<Ty> {

        match ctxt.index(ctype).kind {
            CTypeKind::Void => mk().tuple_ty(vec![] as Vec<P<Ty>>),
            CTypeKind::Bool => mk().path_ty(mk().path(vec!["bool"])),
            CTypeKind::Short => mk().path_ty(mk().path(vec!["libc","c_short"])),
            CTypeKind::Int => mk().path_ty(mk().path(vec!["libc","c_int"])),
            CTypeKind::Long => mk().path_ty(mk().path(vec!["libc","c_long"])),
            CTypeKind::LongLong => mk().path_ty(mk().path(vec!["libc","c_longlong"])),
            CTypeKind::UShort => mk().path_ty(mk().path(vec!["libc","c_ushort"])),
            CTypeKind::UInt => mk().path_ty(mk().path(vec!["libc","c_uint"])),
            CTypeKind::ULong => mk().path_ty(mk().path(vec!["libc","c_ulong"])),
            CTypeKind::ULongLong => mk().path_ty(mk().path(vec!["libc","c_ulonglong"])),
            CTypeKind::SChar => mk().path_ty(mk().path(vec!["libc","c_schar"])),
            CTypeKind::UChar => mk().path_ty(mk().path(vec!["libc","c_uchar"])),
            CTypeKind::Char => mk().path_ty(mk().path(vec!["libc","c_char"])),
            CTypeKind::Double => mk().path_ty(mk().path(vec!["libc","c_double"])),
            CTypeKind::Float => mk().path_ty(mk().path(vec!["libc","c_float"])),
            CTypeKind::Int128 => mk().path_ty(mk().path(vec!["i128"])),
            CTypeKind::UInt128 => mk().path_ty(mk().path(vec!["u128"])),

            CTypeKind::Pointer(CQualTypeId { ref qualifiers, ref ctype }) => {
                match ctxt.resolve_type(*ctype).kind {

                    // While void converts to () in function returns, it converts to c_void
                    // in the case of pointers.
                    CTypeKind::Void => {
                            let mutbl = if qualifiers.is_const { Mutability::Immutable } else { Mutability::Mutable };
                            mk().set_mutbl(mutbl).ptr_ty(mk().path_ty(vec!["libc","c_void"]))
                    }

                    // Function pointers are translated to Option applied to the function type
                    // in order to support NULL function pointers natively
                    CTypeKind::Function(ref ret, ref params) => {
                        let inputs = params.iter().map(|x|
                            mk().arg(self.convert(ctxt, renamer, x.ctype), mk().wild_pat())
                        ).collect();
                        let output = self.convert(ctxt, renamer, ret.ctype);
                        let fn_ptr = mk().unsafe_().abi(Abi::C).barefn_ty(mk().fn_decl(inputs, FunctionRetTy::Ty(output)));
                        let param = mk().angle_bracketed_param_types(vec![fn_ptr]);
                        mk().path_ty(vec![mk().path_segment_with_params("Option", param)])
                    }

                    _ => {
                        let child_ty = self.convert(ctxt, renamer, *ctype);
                        let mutbl = if qualifiers.is_const { Mutability::Immutable } else { Mutability::Mutable };
                        mk().set_mutbl(mutbl).ptr_ty(child_ty)
                    }
                }
            }

            CTypeKind::Elaborated(ref ctype) => self.convert(ctxt, renamer, *ctype),
            CTypeKind::Decayed(ref ctype) => self.convert(ctxt, renamer, *ctype),
            CTypeKind::Paren(ref ctype) => self.convert(ctxt, renamer, *ctype),

            CTypeKind::Struct(ref decl) => {
                if let CDeclKind::Struct { name: Some(ref name), .. } = ctxt.index(*decl).kind {
                    let new_name = renamer.get(name).expect("Struct is not already renamed");
                    mk().path_ty(mk().path(vec![new_name]))
                } else {
                    panic!("{:?} in struct type does not point to a record decl", decl)
                }
            }

            CTypeKind::Union(ref decl) => {
                if let CDeclKind::Union { name: Some(ref name), .. } = ctxt.index(*decl).kind {
                    let new_name = renamer.get(name).expect("Union is not already renamed");
                    mk().path_ty(mk().path(vec![new_name]))
                } else {
                    panic!("{:?} in union type does not point to a record decl", decl)
                }
            }

            CTypeKind::Enum(ref decl) => {
                if let CDeclKind::Enum { name: Some(ref name), .. } = ctxt.index(*decl).kind {
                    let new_name = renamer.get(name).expect("Enum is not already renamed");
                    mk().path_ty(mk().path(vec![new_name]))
                } else {
                    panic!("{:?} in enum type does not point to an enum decl", decl)
                }
            }

            CTypeKind::Typedef(ref decl) => {
                if let CDeclKind::Typedef { ref name, .. }  = ctxt.index(*decl).kind {
                    let new_name = renamer.get(name).expect("Typedef is not already renamed");
                    mk().path_ty(mk().path(vec![new_name]))
                } else {
                    panic!("{:?} in typedef type does not point to a typedef decl", decl)
                }
            }

            CTypeKind::ConstantArray(element, count) => {
                let ty = self.convert(ctxt, renamer, element);
                mk().array_ty(ty, mk().lit_expr(mk().int_lit(count as u128, LitIntType::Unsuffixed)))
            }

            ref t => panic!("Unsupported type {:?}", t),
        }
    }
}
