use crate::c_ast::CDeclId;
use crate::c_ast::*;
use crate::diagnostics::TranslationResult;
use crate::renamer::*;
use c2rust_ast_builder::{mk, properties::*};
use failure::format_err;
use std::collections::{HashMap, HashSet};
use std::ops::Index;
use syn::*;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
enum FieldKey {
    Field(CFieldId),
    Padding(usize),
}

pub struct TypeConverter {
    pub translate_valist: bool,
    renamer: Renamer<CDeclId>,
    fields: HashMap<CDeclId, Renamer<FieldKey>>,
    suffix_names: HashMap<(CDeclId, &'static str), String>,
    features: HashSet<&'static str>,
}

pub const RESERVED_NAMES: [&str; 103] = [
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
    // We don't provide a `Default` impl to simplify future compatibility:
    // if `TypeConverter` ever gets fields incompatible with `Default`, then
    // cleaning out the uses of `impl Default for TypeConverter` can be a pain.
    // More practically, there is a single use of `TypeConverter::new` and no
    // current plans to use a `Default` impl, so providing it isn't worth the
    // potential breakage.
    #[allow(clippy::new_without_default)]
    pub fn new() -> TypeConverter {
        TypeConverter {
            translate_valist: false,
            renamer: Renamer::new(&RESERVED_NAMES),
            fields: HashMap::new(),
            suffix_names: HashMap::new(),
            features: HashSet::new(),
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

    pub fn resolve_decl_suffix_name(&mut self, decl_id: CDeclId, suffix: &'static str) -> &str {
        let key = (decl_id, suffix);
        self.suffix_names.entry(key).or_insert_with(|| {
            let mut suffix_name = self
                .renamer
                .get(&decl_id)
                .unwrap_or_else(|| "C2RustUnnamed".to_string());
            suffix_name += suffix;

            self.renamer.pick_name(&suffix_name)
        })
    }

    pub fn declare_field_name(
        &mut self,
        record_id: CRecordId,
        field_id: CFieldId,
        name: &str,
    ) -> String {
        let name = if name.is_empty() {
            "c2rust_unnamed"
        } else {
            name
        };

        self.fields
            .entry(record_id)
            .or_insert_with(|| Renamer::new(&RESERVED_NAMES))
            .insert(FieldKey::Field(field_id), name)
            .expect("Field already declared")
    }

    pub fn declare_padding(&mut self, record_id: CRecordId, padding_idx: usize) -> String {
        let field = self
            .fields
            .entry(record_id)
            .or_insert_with(|| Renamer::new(&RESERVED_NAMES));
        let key = FieldKey::Padding(padding_idx);
        if let Some(name) = field.get(&key) {
            name
        } else {
            field.insert(key, "c2rust_padding").unwrap()
        }
    }

    /** Resolve the Rust name associated with a field declaration. The optional record_id
    is used as a hint to speed up the process of finding the field's name.
    */
    pub fn resolve_field_name(
        &self,
        record_id: Option<CRecordId>,
        field_id: CFieldId,
    ) -> Option<String> {
        let key = FieldKey::Field(field_id);
        match record_id {
            Some(record_id) => self.fields.get(&record_id).and_then(|x| x.get(&key)),
            None => self.fields.values().flat_map(|x| x.get(&key)).next(),
        }
    }

    /// Helper function handling conversion of function types in `convert`.
    /// Optional return type excludes a ty when a function doesn't return.
    pub fn convert_function(
        &mut self,
        ctxt: &TypedAstContext,
        ret: Option<CQualTypeId>,
        params: &[CQualTypeId],
        is_variadic: bool,
    ) -> TranslationResult<Box<Type>> {
        let barefn_inputs = params
            .iter()
            .map(|x| mk().bare_arg(self.convert(ctxt, x.ctype).unwrap(), None::<Box<Ident>>))
            .collect::<Vec<_>>();

        let output = match ret {
            None => mk().never_ty(),
            Some(ret) => self.convert(ctxt, ret.ctype)?,
        };

        let variadic = is_variadic.then(|| mk().variadic_arg(vec![]));

        let fn_ty = (
            barefn_inputs,
            variadic,
            ReturnType::Type(Default::default(), output),
        );
        Ok(mk().unsafe_().extern_("C").barefn_ty(fn_ty))
    }

    pub fn convert_pointer(
        &mut self,
        ctxt: &TypedAstContext,
        qtype: CQualTypeId,
    ) -> TranslationResult<Box<Type>> {
        let mutbl = if qtype.qualifiers.is_const {
            Mutability::Immutable
        } else {
            Mutability::Mutable
        };

        match ctxt.resolve_type(qtype.ctype).kind {
            // While void converts to () in function returns, it converts to c_void
            // in the case of pointers.
            CTypeKind::Void => Ok(mk()
                .set_mutbl(mutbl)
                .ptr_ty(mk().path_ty(vec!["libc", "c_void"]))),

            CTypeKind::VariableArray(mut elt, _len) => {
                while let CTypeKind::VariableArray(elt_, _) = ctxt.resolve_type(elt).kind {
                    elt = elt_
                }
                let child_ty = self.convert(ctxt, elt)?;
                Ok(mk().set_mutbl(mutbl).ptr_ty(child_ty))
            }

            // Function pointers are translated to Option applied to the function type
            // in order to support NULL function pointers natively
            CTypeKind::Function(..) => {
                let fn_ty = self.convert(ctxt, qtype.ctype)?;
                let param = mk().angle_bracketed_args(vec![fn_ty]);
                Ok(mk().path_ty(vec![mk().path_segment_with_args("Option", param)]))
            }

            _ => {
                let child_ty = self.convert(ctxt, qtype.ctype)?;
                Ok(mk().set_mutbl(mutbl).ptr_ty(child_ty))
            }
        }
    }

    /// Convert a `C` type to a `Rust` one. For the moment, these are expected to have compatible
    /// memory layouts.
    pub fn convert(
        &mut self,
        ctxt: &TypedAstContext,
        ctype: CTypeId,
    ) -> TranslationResult<Box<Type>> {
        if self.translate_valist && ctxt.is_va_list(ctype) {
            let path = vec!["core", "ffi", "VaList"];
            let ty = mk().path_ty(mk().abs_path(path));
            return Ok(ty);
        }

        match ctxt.index(ctype).kind {
            CTypeKind::Void => Ok(mk().tuple_ty(vec![])),
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
            CTypeKind::BFloat16 => Ok(mk().path_ty(mk().path(vec!["bf16"]))),

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
                Ok(mk().array_ty(ty, mk().lit_expr(mk().int_unsuffixed_lit(count as u128))))
            }

            CTypeKind::IncompleteArray(element) => {
                let ty = self.convert(ctxt, element)?;
                let zero_lit = mk().int_unsuffixed_lit(0);
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
            CTypeKind::Atomic(ty) => self.convert(ctxt, ty.ctype),

            // ANSI/ISO C-style function
            CTypeKind::Function(ret, ref params, is_var, is_noreturn, true) => {
                let opt_ret = if is_noreturn { None } else { Some(ret) };
                let fn_ty = self.convert_function(ctxt, opt_ret, params, is_var)?;
                Ok(fn_ty)
            }

            // K&R-style function
            CTypeKind::Function(ret, _, is_var, is_noreturn, false) => {
                let opt_ret = if is_noreturn { None } else { Some(ret) };
                let fn_ty = self.convert_function(ctxt, opt_ret, &[], is_var)?;
                Ok(fn_ty)
            }

            CTypeKind::TypeOf(ty) => self.convert(ctxt, ty),

            ref t => Err(format_err!("Unsupported type {:?}", t).into()),
        }
    }

    /// Add the given parameters to a K&R function pointer type,
    /// returning a full signature or `None` if the function isn't K&R.
    pub fn knr_function_type_with_parameters(
        &mut self,
        ctxt: &TypedAstContext,
        ctype: CTypeId,
        params: &Vec<CParamId>,
    ) -> TranslationResult<Option<Box<Type>>> {
        match ctxt.index(ctype).kind {
            // ANSI/ISO C-style function
            CTypeKind::Function(.., true) => Ok(None),

            // K&R-style function
            CTypeKind::Function(ret, ref _params, is_var, is_noreturn, false) => {
                // _params is empty here -> get params from function definition instead
                let params = params
                    .iter()
                    .map(|p| {
                        let decl = &ctxt.get_decl(p).unwrap().kind;
                        match decl {
                            CDeclKind::Variable { typ, .. } => *typ,
                            _ => panic!("parameter referenced non-variable decl."),
                        }
                    })
                    .collect::<Vec<_>>();

                let opt_ret = if is_noreturn { None } else { Some(ret) };
                let fn_ty = self.convert_function(ctxt, opt_ret, &params, is_var)?;
                Ok(Some(fn_ty))
            }

            CTypeKind::Elaborated(ref ctype) => {
                self.knr_function_type_with_parameters(ctxt, *ctype, params)
            }
            CTypeKind::Decayed(ref ctype) => {
                self.knr_function_type_with_parameters(ctxt, *ctype, params)
            }
            CTypeKind::Paren(ref ctype) => {
                self.knr_function_type_with_parameters(ctxt, *ctype, params)
            }
            CTypeKind::TypeOf(ty) => self.knr_function_type_with_parameters(ctxt, ty, params),

            CTypeKind::Typedef(decl) => match &ctxt.index(decl).kind {
                CDeclKind::Typedef { typ, .. } => {
                    self.knr_function_type_with_parameters(ctxt, typ.ctype, params)
                }
                _ => panic!("Typedef decl did not point to a typedef"),
            },

            ref kind => panic!("ctype parameter must be a function instead of {:?}", kind),
        }
    }
}
