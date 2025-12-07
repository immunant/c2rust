use crate::c_ast::CDeclId;
use crate::c_ast::*;
use crate::diagnostics::TranslationResult;
use crate::renamer::*;
use crate::{CrateSet, ExternCrate};
use c2rust_ast_builder::{mk, properties::*};
use failure::format_err;
use indexmap::IndexSet;
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
    extern_crates: CrateSet,
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
            extern_crates: IndexSet::new(),
        }
    }

    fn use_crate(&mut self, extern_crate: ExternCrate) {
        self.extern_crates.insert(extern_crate);
    }

    pub fn features_used(&self) -> &HashSet<&'static str> {
        &self.features
    }

    pub fn extern_crates_used(&self) -> &CrateSet {
        &self.extern_crates
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
            .map(|x| {
                let ty = self.convert_function_param(ctxt, x.ctype).unwrap();
                mk().bare_arg(ty, None::<Box<Ident>>)
            })
            .collect::<Vec<_>>();

        let output = match ret {
            None => mk().never_ty(),
            Some(ret) => self.convert(ctxt, ret.ctype)?,
        };

        let variadic = is_variadic.then(|| mk().bare_variadic_arg());

        let fn_ty = (
            barefn_inputs,
            variadic,
            ReturnType::Type(Default::default(), output),
        );
        Ok(mk().unsafe_().extern_("C").barefn_ty(fn_ty))
    }

    /// Converts the qualified type of a pointer.
    pub fn convert_pointer(
        &mut self,
        ctxt: &TypedAstContext,
        qtype: CQualTypeId,
    ) -> TranslationResult<Box<Type>> {
        let pointee_ty = self.convert_pointee(ctxt, qtype.ctype)?;

        if let CTypeKind::Function(..) = ctxt.resolve_type(qtype.ctype).kind {
            // Function pointers are translated to Option applied to the function type
            // in order to support NULL function pointers natively
            let param = mk().angle_bracketed_args(vec![pointee_ty]);
            Ok(mk().path_ty(vec![mk().path_segment_with_args("Option", param)]))
        } else {
            let mutbl = if qtype.qualifiers.is_const {
                Mutability::Immutable
            } else {
                Mutability::Mutable
            };

            Ok(mk().set_mutbl(mutbl).ptr_ty(pointee_ty))
        }
    }

    /// Converts the pointee type of a pointer.
    pub fn convert_pointee(
        &mut self,
        ctxt: &TypedAstContext,
        ctype: CTypeId,
    ) -> TranslationResult<Box<Type>> {
        match ctxt.resolve_type(ctype).kind {
            // While void converts to () in function returns, it converts to c_void
            // in the case of pointers.
            CTypeKind::Void => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_void"])),

            CTypeKind::VariableArray(mut elt, _len) => {
                while let CTypeKind::VariableArray(elt_, _) = ctxt.resolve_type(elt).kind {
                    elt = elt_
                }
                self.convert(ctxt, elt)
            }

            _ => self.convert(ctxt, ctype),
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
            let ty = mk().abs_path_ty(vec!["core", "ffi", "VaListImpl"]);
            return Ok(ty);
        }

        match ctxt.index(ctype).kind {
            CTypeKind::Void => Ok(mk().tuple_ty(vec![])),
            CTypeKind::Bool => Ok(mk().path_ty(vec!["bool"])),
            CTypeKind::Short => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_short"])),
            CTypeKind::Int => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_int"])),
            CTypeKind::Long => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_long"])),
            CTypeKind::LongLong => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_longlong"])),
            CTypeKind::UShort => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_ushort"])),
            CTypeKind::UInt => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_uint"])),
            CTypeKind::ULong => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_ulong"])),
            CTypeKind::ULongLong => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_ulonglong"])),
            CTypeKind::SChar => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_schar"])),
            CTypeKind::UChar => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_uchar"])),
            CTypeKind::Char => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_char"])),
            CTypeKind::Double => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_double"])),
            CTypeKind::LongDouble | CTypeKind::Float128 => {
                self.use_crate(ExternCrate::F128);
                Ok(mk().abs_path_ty(vec!["f128", "f128"]))
            }
            CTypeKind::Float => Ok(mk().abs_path_ty(vec!["core", "ffi", "c_float"])),
            CTypeKind::Int128 => Ok(mk().path_ty(vec!["i128"])),
            CTypeKind::UInt128 => Ok(mk().path_ty(vec!["u128"])),
            CTypeKind::BFloat16 => Ok(mk().path_ty(vec!["bf16"])),

            CTypeKind::Int8 => Ok(mk().path_ty(vec!["i8"])),
            CTypeKind::Int16 => Ok(mk().path_ty(vec!["i16"])),
            CTypeKind::Int32 => Ok(mk().path_ty(vec!["i32"])),
            CTypeKind::Int64 => Ok(mk().path_ty(vec!["i64"])),
            CTypeKind::IntPtr => Ok(mk().path_ty(vec!["isize"])),
            CTypeKind::UInt8 => Ok(mk().path_ty(vec!["u8"])),
            CTypeKind::UInt16 => Ok(mk().path_ty(vec!["u16"])),
            CTypeKind::UInt32 => Ok(mk().path_ty(vec!["u32"])),
            CTypeKind::UInt64 => Ok(mk().path_ty(vec!["u64"])),
            CTypeKind::UIntPtr => Ok(mk().path_ty(vec!["usize"])),
            CTypeKind::IntMax => {
                self.use_crate(ExternCrate::Libc);
                Ok(mk().abs_path_ty(vec!["libc", "intmax_t"]))
            }
            CTypeKind::UIntMax => {
                self.use_crate(ExternCrate::Libc);
                Ok(mk().abs_path_ty(vec!["libc", "uintmax_t"]))
            }
            CTypeKind::Size => Ok(mk().path_ty(vec!["usize"])),
            CTypeKind::SSize => Ok(mk().path_ty(vec!["isize"])),
            CTypeKind::PtrDiff => Ok(mk().path_ty(vec!["isize"])),
            CTypeKind::WChar => {
                self.use_crate(ExternCrate::Libc);
                Ok(mk().abs_path_ty(vec!["libc", "wchar_t"]))
            }

            CTypeKind::Pointer(qtype) => self.convert_pointer(ctxt, qtype),

            CTypeKind::Elaborated(ref ctype) => self.convert(ctxt, *ctype),
            CTypeKind::Decayed(ref ctype) => self.convert(ctxt, *ctype),
            CTypeKind::Paren(ref ctype) => self.convert(ctxt, *ctype),

            CTypeKind::Struct(decl_id) => {
                let new_name = self
                    .resolve_decl_name(decl_id)
                    .ok_or_else(|| format_err!("Unknown decl id {:?}", decl_id))?;
                Ok(mk().path_ty(vec![new_name]))
            }

            CTypeKind::Union(decl_id) => {
                let new_name = self.resolve_decl_name(decl_id).unwrap();
                Ok(mk().path_ty(vec![new_name]))
            }

            CTypeKind::Enum(decl_id) => {
                let new_name = self.resolve_decl_name(decl_id).unwrap();
                Ok(mk().path_ty(vec![new_name]))
            }

            CTypeKind::Typedef(decl_id) => {
                let new_name = self.resolve_decl_name(decl_id).unwrap();
                Ok(mk().path_ty(vec![new_name]))
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

    // Variant of `convert` that handles types that need to be converted differently if they
    // are the type of a function parameter.
    pub fn convert_function_param(
        &mut self,
        ctxt: &TypedAstContext,
        ctype: CTypeId,
    ) -> TranslationResult<Box<Type>> {
        if ctxt.is_va_list(ctype) {
            // va_list parameters are translated as VaList rather than VaListImpl
            let ty = mk().abs_path_ty(vec!["core", "ffi", "VaList"]);
            return Ok(ty);
        }

        self.convert(ctxt, ctype)
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
