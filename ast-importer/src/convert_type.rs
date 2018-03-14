use c_ast::*;
use syntax::ast::*;
use syntax::abi::Abi;
use idiomize::ast_manip::make_ast::*;
use syntax::ptr::P;
use std::ops::Index;
use renamer::*;
use std::collections::HashMap;
use c_ast::CDeclId;

pub struct TypeConverter {
    renamer: Renamer<CDeclId>,
    fields: HashMap<CDeclId, Renamer<CFieldId>>,
}

static RESERVED_NAMES: [&str; 52] = [
    // Keywords currently in use
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
    "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub",
    "ref", "return", "Self", "self", "static", "struct", "super", "trait", "true",
    "type", "unsafe", "use", "where", "while",

    // Keywords reserved for future use
    "abstract", "alignof", "become", "box", "do", "final", "macro", "offsetof",
    "override", "priv", "proc", "pure", "sizeof", "typeof", "unsized", "virtual",
    "yield",
];

impl TypeConverter {

    pub fn new() -> TypeConverter {

        TypeConverter {
            renamer: Renamer::new(&RESERVED_NAMES),
            fields: HashMap::new(),
        }
    }

    pub fn declare_decl_name(&mut self, decl_id: CDeclId, name: &str) -> String {
        self.renamer.insert(decl_id, name).expect("Name already assigned")
    }

    pub fn resolve_decl_name(&self, decl_id: CDeclId) -> Option<String> {
        self.renamer.get(&decl_id)
    }

    pub fn declare_field_name(&mut self, record_id: CRecordId, field_id: CFieldId, name: &str) -> String {

        let name = if name.is_empty() { "unnamed" } else { name };

        if !self.fields.contains_key(&record_id) {
            self.fields.insert(record_id, Renamer::new(&RESERVED_NAMES));
        }

        self.fields.get_mut(&record_id).unwrap()
            .insert(field_id, name).expect("Field already declared")
    }

    /** Resolve the Rust name associated with a field declaration. The optional record_id
    is used as a hint to speed up the process of finding the field's name.
    */
    pub fn resolve_field_name(&self, record_id: Option<CRecordId>, field_id: CFieldId) -> Option<String> {
        match record_id {
            Some(record_id) => self.fields.get(&record_id).and_then(|x| x.get(&field_id)),
            None => self.fields.values().flat_map(|x| x.get(&field_id)).next(),
        }

    }

    /// Helper function handling conversion of function types in `convert`.
    fn convert_function(&mut self, ctxt: &TypedAstContext,  ret: &CQualTypeId, params: &Vec<CQualTypeId>) -> Result<P<Ty>, String> {
        let inputs = params.iter().map(|x|
            mk().arg(self.convert(ctxt, x.ctype).unwrap(),
                                 mk().wild_pat())
        ).collect();
        let output = self.convert(ctxt, ret.ctype)?;
        let fn_ty = mk().fn_decl(inputs, FunctionRetTy::Ty(output));
        return Ok(mk().unsafe_().abi(Abi::C).barefn_ty(fn_ty));
    }

    /// Convert a `C` type to a `Rust` one. For the moment, these are expected to have compatible
    /// memory layouts.
    pub fn convert(&mut self, ctxt: &TypedAstContext, ctype: CTypeId) -> Result<P<Ty>, String> {

        match ctxt.index(ctype).kind {
            CTypeKind::Void => Ok(mk().tuple_ty(vec![] as Vec<P<Ty>>)),
            CTypeKind::Bool => Ok(mk().path_ty(mk().path(vec!["bool"]))),
            CTypeKind::Short => Ok(mk().path_ty(mk().path(vec!["libc","c_short"]))),
            CTypeKind::Int => Ok(mk().path_ty(mk().path(vec!["libc","c_int"]))),
            CTypeKind::Long => Ok(mk().path_ty(mk().path(vec!["libc","c_long"]))),
            CTypeKind::LongLong => Ok(mk().path_ty(mk().path(vec!["libc","c_longlong"]))),
            CTypeKind::UShort => Ok(mk().path_ty(mk().path(vec!["libc","c_ushort"]))),
            CTypeKind::UInt => Ok(mk().path_ty(mk().path(vec!["libc","c_uint"]))),
            CTypeKind::ULong => Ok(mk().path_ty(mk().path(vec!["libc","c_ulong"]))),
            CTypeKind::ULongLong => Ok(mk().path_ty(mk().path(vec!["libc","c_ulonglong"]))),
            CTypeKind::SChar => Ok(mk().path_ty(mk().path(vec!["libc","c_schar"]))),
            CTypeKind::UChar => Ok(mk().path_ty(mk().path(vec!["libc","c_uchar"]))),
            CTypeKind::Char => Ok(mk().path_ty(mk().path(vec!["libc","c_char"]))),
            CTypeKind::Double => Ok(mk().path_ty(mk().path(vec!["libc","c_double"]))),
            CTypeKind::LongDouble => Ok(mk().path_ty(mk().path(vec!["libc","c_double"]))),
            CTypeKind::Float => Ok(mk().path_ty(mk().path(vec!["libc","c_float"]))),
            CTypeKind::Int128 => Ok(mk().path_ty(mk().path(vec!["i128"]))),
            CTypeKind::UInt128 => Ok(mk().path_ty(mk().path(vec!["u128"]))),

            CTypeKind::Pointer(CQualTypeId { ref qualifiers, ref ctype }) => {
                match ctxt.resolve_type(*ctype).kind {

                    // While void converts to () in function returns, it converts to c_void
                    // in the case of pointers.
                    CTypeKind::Void => {
                            let mutbl = if qualifiers.is_const { Mutability::Immutable } else { Mutability::Mutable };
                            Ok(mk().set_mutbl(mutbl).ptr_ty(mk().path_ty(vec!["libc","c_void"])))
                    }

                    CTypeKind::VariableArray(mut elt,_len) => {
                        while let CTypeKind::VariableArray(elt_, _) = ctxt.resolve_type(elt).kind {
                            elt = elt_
                        }
                        let child_ty = self.convert(ctxt, elt)?;
                        let mutbl = if qualifiers.is_const { Mutability::Immutable } else { Mutability::Mutable };
                        Ok(mk().set_mutbl(mutbl).ptr_ty(child_ty))
                    }

                    // Function pointers are translated to Option applied to the function type
                    // in order to support NULL function pointers natively
                    CTypeKind::Function(ref ret, ref params) => {
                        let fn_ty = self.convert_function(ctxt, ret, params)?;
                        let param = mk().angle_bracketed_param_types(vec![fn_ty]);
                        let optn_ty = mk().path_ty(vec![mk().path_segment_with_params("Option", param)]);
                        Ok(optn_ty)
                    }

                    _ => {
                        let child_ty = self.convert(ctxt, *ctype)?;
                        let mutbl = if qualifiers.is_const { Mutability::Immutable } else { Mutability::Mutable };
                        Ok(mk().set_mutbl(mutbl).ptr_ty(child_ty))
                    }
                }
            }

            CTypeKind::Elaborated(ref ctype) => self.convert(ctxt, *ctype),
            CTypeKind::Decayed(ref ctype) => self.convert(ctxt, *ctype),
            CTypeKind::Paren(ref ctype) => self.convert(ctxt, *ctype),

            CTypeKind::Struct(decl_id) => {
                let new_name = self.resolve_decl_name(decl_id).ok_or_else(|| format!("Unknown decl id {:?}", decl_id))?;
                Ok(mk().path_ty(mk().path(vec![new_name])))
            }

            CTypeKind::Union(decl_id) => {
                let new_name = self.resolve_decl_name(decl_id).unwrap();
                Ok(mk().path_ty(mk().path(vec![new_name])))
            }

            CTypeKind::Enum(decl_id) => {
                let new_name = self.resolve_decl_name(decl_id).unwrap();
                Ok(mk().path_ty(mk().path(vec![new_name])))
            }

            CTypeKind::Typedef(decl_id) => {
                let new_name = self.resolve_decl_name(decl_id).unwrap();
                Ok(mk().path_ty(mk().path(vec![new_name])))
            }

            CTypeKind::ConstantArray(element, count) => {
                let ty = self.convert(ctxt, element)?;
                Ok(mk().array_ty(ty, mk().lit_expr(mk().int_lit(count as u128, LitIntType::Unsuffixed))))
            }

            CTypeKind::IncompleteArray(element) => {
                // FIXME: handle translation of incomplete arrays
                let ty = self.convert(ctxt, element)?;
                Ok(mk().slice_ty(ty))
            }

            CTypeKind::VariableArray(element, _) => {
                let child_ty = self.convert(ctxt, element)?;
                Ok(mk().mutbl().ptr_ty(child_ty))
            }

            CTypeKind::Attributed(ty) => self.convert(ctxt, ty.ctype),

            CTypeKind::Function(ref ret, ref params) => {
                let fn_ty = self.convert_function(ctxt, ret, params)?;
                Ok(fn_ty)
            }

            ref t => Err(format!("Unsupported type {:?}", t)),
        }
    }
}
