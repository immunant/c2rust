use crate::c_ast::CDeclId;
use crate::c_ast::*;
use crate::renamer::*;
use crate::diagnostics::TranslationError;
use c2rust_ast_builder::mk;
use std::collections::{HashMap, HashSet};
use std::ops::Index;
use syntax::ast::*;
use syntax::ptr::P;

pub struct TypeConverter {
    pub translate_valist: bool,
    renamer: Renamer<CDeclId>,
    fields: HashMap<CDeclId, Renamer<CFieldId>>,
    features: HashSet<&'static str>,
    emit_no_std: bool,
}

static RESERVED_NAMES: [&str; 103] = [
    // Keywords currently in use
    "as",
    "break",
    "const",
    "continue",
    "crate",
    "else",
    "enum",
    "extern",
    "false",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "match",
    "mod",
    "move",
    "mut",
    "pub",
    "ref",
    "return",
    "Self",
    "self",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "type",
    "unsafe",
    "use",
    "where",
    "while",
    "dyn",
    // Keywords reserved for future use
    "abstract",
    "alignof",
    "become",
    "box",
    "do",
    "final",
    "macro",
    "offsetof",
    "override",
    "priv",
    "proc",
    "pure",
    "sizeof",
    "typeof",
    "unsized",
    "virtual",
    "yield",
    "async",
    "try",
    // Types exported in prelude
    "Copy",
    "Send",
    "Sized",
    "Sync",
    "Drop",
    "Fn",
    "FnMut",
    "FnOnce",
    "Box",
    "ToOwned",
    "Clone",
    "PartialEq",
    "PartialOrd",
    "Eq",
    "Ord",
    "AsRef",
    "AsMut",
    "Into",
    "From",
    "Default",
    "Iterator",
    "Extend",
    "IntoIterator",
    "DoubleEndedIterator",
    "ExactSizeIterator",
    "Option",
    "Result",
    "SliceConcatExt",
    "String",
    "ToString",
    "Vec",
    "bool",
    "char",
    "f32",
    "f64",
    "i8",
    "i16",
    "i32",
    "i64",
    "i128",
    "isize",
    "u8",
    "u16",
    "u32",
    "u64",
    "u128",
    "usize",
    "str",
];

impl TypeConverter {
    pub fn new(emit_no_std: bool) -> TypeConverter {
        TypeConverter {
            translate_valist: false,
            renamer: Renamer::new(&RESERVED_NAMES),
            fields: HashMap::new(),
            features: HashSet::new(),
            emit_no_std,
        }
    }

    pub fn features_used(&self) -> &HashSet<&'static str> {
        &self.features
    }

    pub fn declare_decl_name(&mut self, decl_id: CDeclId, name: &str) -> String {
        self.renamer
            .insert(decl_id, name)
            .expect("Name already assigned")
    }

    pub fn alias_decl_name(&mut self, new_decl_id: CDeclId, old_decl_id: CDeclId) {
        self.renamer.alias(new_decl_id, &old_decl_id)
    }

    pub fn resolve_decl_name(&self, decl_id: CDeclId) -> Option<String> {
        self.renamer.get(&decl_id)
    }

    pub fn declare_field_name(
        &mut self,
        record_id: CRecordId,
        field_id: CFieldId,
        name: &str,
    ) -> String {
        let name = if name.is_empty() { "unnamed" } else { name };

        if !self.fields.contains_key(&record_id) {
            self.fields.insert(record_id, Renamer::new(&RESERVED_NAMES));
        }

        self.fields
            .get_mut(&record_id)
            .unwrap()
            .insert(field_id, name)
            .expect("Field already declared")
    }

    /** Resolve the Rust name associated with a field declaration. The optional record_id
    is used as a hint to speed up the process of finding the field's name.
    */
    pub fn resolve_field_name(
        &self,
        record_id: Option<CRecordId>,
        field_id: CFieldId,
    ) -> Option<String> {
        match record_id {
            Some(record_id) => self.fields.get(&record_id).and_then(|x| x.get(&field_id)),
            None => self.fields.values().flat_map(|x| x.get(&field_id)).next(),
        }
    }

    /// Helper function handling conversion of function types in `convert`.
    /// Optional return type excludes a ty when a function doesn't return.
    fn convert_function(
        &mut self,
        ctxt: &TypedAstContext,
        ret: Option<CQualTypeId>,
        params: &Vec<CQualTypeId>,
        is_variadic: bool,
    ) -> Result<P<Ty>, TranslationError> {
        let mut inputs = params
            .iter()
            .map(|x| mk().arg(self.convert(ctxt, x.ctype).unwrap(), mk().wild_pat()))
            .collect::<Vec<_>>();

        let output = match ret {
            None => mk().never_ty(),
            Some(ret) => self.convert(ctxt, ret.ctype)?,
        };

        if is_variadic {
            // For variadic functions, we need to add `_: ...` as an explicit argument
            inputs.push(mk().arg(mk().cvar_args_ty(), mk().wild_pat()))
        };

        let fn_ty = mk().fn_decl(inputs, FunctionRetTy::Ty(output), is_variadic);
        return Ok(mk().unsafe_().abi("C").barefn_ty(fn_ty));
    }

    pub fn convert_pointer(
        &mut self,
        ctxt: &TypedAstContext,
        qtype: CQualTypeId,
    ) -> Result<P<Ty>, TranslationError> {
        match ctxt.resolve_type(qtype.ctype).kind {
            // While void converts to () in function returns, it converts to c_void
            // in the case of pointers.
            CTypeKind::Void => {
                let mutbl = if qtype.qualifiers.is_const {
                    Mutability::Immutable
                } else {
                    Mutability::Mutable
                };
                return Ok(mk()
                    .set_mutbl(mutbl)
                    .ptr_ty(mk().path_ty(vec!["libc", "c_void"])));
            }

            CTypeKind::VariableArray(mut elt, _len) => {
                while let CTypeKind::VariableArray(elt_, _) = ctxt.resolve_type(elt).kind {
                    elt = elt_
                }
                let child_ty = self.convert(ctxt, elt)?;
                let mutbl = if qtype.qualifiers.is_const {
                    Mutability::Immutable
                } else {
                    Mutability::Mutable
                };
                return Ok(mk().set_mutbl(mutbl).ptr_ty(child_ty));
            }

            // Function pointers are translated to Option applied to the function type
            // in order to support NULL function pointers natively
            CTypeKind::Function(ret, ref params, is_var, is_noreturn, has_proto) => {
                if !has_proto {
                    return Err(TranslationError::generic(
                        "Unable to convert function pointer type without prototype",
                    ));
                }

                let opt_ret = if is_noreturn { None } else { Some(ret) };
                let fn_ty = self.convert_function(ctxt, opt_ret, params, is_var)?;
                let param = mk().angle_bracketed_args(vec![fn_ty]);
                let optn_ty = mk().path_ty(vec![mk().path_segment_with_args("Option", param)]);
                return Ok(optn_ty);
            }

            CTypeKind::Struct(struct_id) => {
                if self.translate_valist {
                    if let CDeclKind::Struct {
                        name: Some(ref struct_name),
                        ..
                    } = ctxt[struct_id].kind
                    {
                        if struct_name == "__va_list_tag" {
                            self.features.insert("c_variadic");

                            let std_or_core = if self.emit_no_std { "core" } else { "std" };
                            let path = vec!["", std_or_core, "ffi", "VaList"];
                            let ty = mk().path_ty(path);
                            return Ok(ty);
                        }
                    }
                }
            }

            _ => {}
        }

        let child_ty = self.convert(ctxt, qtype.ctype)?;
        let mutbl = if qtype.qualifiers.is_const {
            Mutability::Immutable
        } else {
            Mutability::Mutable
        };
        Ok(mk().set_mutbl(mutbl).ptr_ty(child_ty))
    }

    pub fn is_inner_type_valist(ctxt: &TypedAstContext, qtype: CQualTypeId) -> bool {
        match ctxt.resolve_type(qtype.ctype).kind {
            CTypeKind::Struct(struct_id) => {
                if let CDeclKind::Struct {
                    name: Some(ref struct_name),
                    ..
                } = ctxt[struct_id].kind
                {
                    if struct_name == "__va_list_tag" {
                        return true;
                    }
                }
                false
            }
            CTypeKind::Pointer(pointer_id) => Self::is_inner_type_valist(ctxt, pointer_id),
            _ => false,
        }
    }

    /// Convert a `C` type to a `Rust` one. For the moment, these are expected to have compatible
    /// memory layouts.
    pub fn convert(
        &mut self,
        ctxt: &TypedAstContext,
        ctype: CTypeId,
    ) -> Result<P<Ty>, TranslationError> {
        match ctxt.index(ctype).kind {
            CTypeKind::Void => Ok(mk().tuple_ty(vec![] as Vec<P<Ty>>)),
            CTypeKind::Bool => Ok(mk().path_ty(mk().path(vec!["bool"]))),
            CTypeKind::Short => Ok(mk().path_ty(mk().path(vec!["libc", "c_short"]))),
            CTypeKind::Int => Ok(mk().path_ty(mk().path(vec!["libc", "c_int"]))),
            CTypeKind::Long => Ok(mk().path_ty(mk().path(vec!["libc", "c_long"]))),
            CTypeKind::LongLong => Ok(mk().path_ty(mk().path(vec!["libc", "c_longlong"]))),
            CTypeKind::UShort => Ok(mk().path_ty(mk().path(vec!["libc", "c_ushort"]))),
            CTypeKind::UInt => Ok(mk().path_ty(mk().path(vec!["libc", "c_uint"]))),
            CTypeKind::ULong => Ok(mk().path_ty(mk().path(vec!["libc", "c_ulong"]))),
            CTypeKind::ULongLong => Ok(mk().path_ty(mk().path(vec!["libc", "c_ulonglong"]))),
            CTypeKind::SChar => Ok(mk().path_ty(mk().path(vec!["libc", "c_schar"]))),
            CTypeKind::UChar => Ok(mk().path_ty(mk().path(vec!["libc", "c_uchar"]))),
            CTypeKind::Char => Ok(mk().path_ty(mk().path(vec!["libc", "c_char"]))),
            CTypeKind::Double => Ok(mk().path_ty(mk().path(vec!["libc", "c_double"]))),
            CTypeKind::LongDouble => Ok(mk().path_ty(mk().path(vec!["f128", "f128"]))),
            CTypeKind::Float => Ok(mk().path_ty(mk().path(vec!["libc", "c_float"]))),
            CTypeKind::Int128 => Ok(mk().path_ty(mk().path(vec!["i128"]))),
            CTypeKind::UInt128 => Ok(mk().path_ty(mk().path(vec!["u128"]))),

            CTypeKind::Pointer(qtype) => self.convert_pointer(ctxt, qtype),

            CTypeKind::Elaborated(ref ctype) => self.convert(ctxt, *ctype),
            CTypeKind::Decayed(ref ctype) => self.convert(ctxt, *ctype),
            CTypeKind::Paren(ref ctype) => self.convert(ctxt, *ctype),

            CTypeKind::Struct(decl_id) => {
                let new_name = self
                    .resolve_decl_name(decl_id)
                    .ok_or_else(|| format_err!("Unknown decl id {:?}", decl_id))?;
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
                Ok(mk().array_ty(
                    ty,
                    mk().lit_expr(mk().int_lit(count as u128, LitIntType::Unsuffixed)),
                ))
            }

            CTypeKind::IncompleteArray(element) => {
                let ty = self.convert(ctxt, element)?;
                let zero_lit = mk().int_lit(0, LitIntType::Unsuffixed);
                let zero = mk().lit_expr(zero_lit);
                Ok(mk().array_ty(ty, zero))
            }

            CTypeKind::VariableArray(mut elt, _) => {
                while let CTypeKind::VariableArray(elt_, _) = ctxt.resolve_type(elt).kind {
                    elt = elt_
                }
                let child_ty = self.convert(ctxt, elt)?;
                Ok(mk().mutbl().ptr_ty(child_ty))
            }

            CTypeKind::Attributed(ty, _) => self.convert(ctxt, ty.ctype),

            CTypeKind::Function(ret, ref params, is_var, is_noreturn, true) => {
                let opt_ret = if is_noreturn { None } else { Some(ret) };
                let fn_ty = self.convert_function(ctxt, opt_ret, params, is_var)?;
                Ok(fn_ty)
            }

            CTypeKind::TypeOf(ty) => self.convert(ctxt, ty),

            ref t => Err(format_err!("Unsupported type {:?}", t).into()),
        }
    }
}
