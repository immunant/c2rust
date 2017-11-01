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
                let child_ty = self.convert(ctxt, *ctype);
                mk_qualified(qualifiers).ptr_ty(child_ty)
            }

            CTypeKind::Elaborated(ref ctype) => self.convert(ctxt, *ctype),
            CTypeKind::Decayed(ref ctype) => self.convert(ctxt, *ctype),

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
                    panic!("{:?} in typdef type does not point to a typdef decl", decl)
                }
            }

            ref t => panic!("Unsupported type {:?}", t),
        }
    }
}