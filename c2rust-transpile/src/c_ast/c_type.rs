use crate::c_ast::c_expr::CExprId;
use crate::c_ast::{
    Attribute, CDeclId, CDeclKind, CEnumId, CRecordId, CTypedefId, Located, TypedAstContext,
};
use std::fmt::{self, Debug, Display};
use std::ops::Index;

pub use c2rust_ast_exporter::clang_ast::{BuiltinVaListKind, SrcFile, SrcLoc, SrcSpan};

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CTypeId(pub u64);

// These are references into particular variants of AST nodes
pub type CFuncTypeId = CTypeId; // Function declarations always have types which are 'TypeKind::Function'

/// Qualified type
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CQualTypeId {
    pub qualifiers: Qualifiers,
    pub ctype: CTypeId,
}

impl CQualTypeId {
    pub fn new(ctype: CTypeId) -> Self {
        Self {
            qualifiers: Qualifiers::default(),
            ctype,
        }
    }
}

/// Type qualifiers (6.7.3)
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub struct Qualifiers {
    /// The `const` qualifier, which marks lvalues as non-assignable.
    ///
    /// We make use of `const` in only two places:
    ///   * Variable and function bindings (which matches up to Rust's `mut` or not bindings)
    ///   * The pointed type in pointers (which matches up to Rust's `*const`/`*mut`)
    pub is_const: bool,

    pub is_restrict: bool,

    /// The `volatile` qualifier, which prevents the compiler from reordering accesses through such
    /// qualified lvalues past other observable side effects (other accesses, or sequence points).
    ///
    /// The part here about not reordering (or changing in any way) access to something volatile
    /// can be replicated in Rust via `std::ptr::read_volatile`  and `std::ptr::write_volatile`.
    /// Since Rust's execution model is still unclear, I am unsure that we get all of the guarantees
    /// `volatile` needs, especially regarding reordering of other side-effects.
    ///
    /// To see where we use `volatile`, check the call-sites of `Translation::volatile_write` and
    /// `Translation::volatile_read`.
    pub is_volatile: bool,
}

impl Qualifiers {
    /// Aggregate qualifier information from two sources.
    pub fn and(self, other: Qualifiers) -> Qualifiers {
        Qualifiers {
            is_const: self.is_const || other.is_const,
            is_restrict: self.is_restrict || other.is_restrict,
            is_volatile: self.is_volatile || other.is_volatile,
        }
    }
}

// TODO: these may be interesting, but I'm not sure if they fit here:
//
//  * UnaryTransformType <http://clang.llvm.org/doxygen/classclang_1_1UnaryTransformType.html>
//  * AdjustedType <http://clang.llvm.org/doxygen/classclang_1_1AdjustedType.html>

/// Represents a type in C (6.2.5 Types)
///
/// Reflects the types in <http://clang.llvm.org/doxygen/classclang_1_1Type.html>
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CTypeKind {
    Void,

    // Boolean type (6.2.5.2)
    Bool,

    // Character type (6.2.5.3)
    Char,

    // Signed types (6.2.5.4)
    SChar,
    Short,
    Int,
    Long,
    LongLong,

    // Unsigned types (6.2.5.6) (actually this also includes `_Bool`)
    UChar,
    UShort,
    UInt,
    ULong,
    ULongLong,

    // Real floating types (6.2.5.10). Ex: `double`
    Float,
    Double,
    LongDouble,

    // Clang specific types
    Int128,
    UInt128,

    Complex(CTypeId),

    // Pointer types (6.7.5.1)
    Pointer(CQualTypeId),

    // C++ Reference
    Reference(CQualTypeId),

    // Array types (6.7.5.2)
    //
    // A qualifier on an array type means the same thing as a qualifier on its element type. Since
    // Clang tracks the qualifiers in both places, we choose to discard qualifiers on the element
    // type.
    //
    // The size expression on a variable-length array is optional, it might be replaced with `*`
    ConstantArray(CTypeId, usize),
    IncompleteArray(CTypeId),
    VariableArray(CTypeId, Option<CExprId>),

    // Type of type or expression (GCC extension)
    TypeOf(CTypeId),
    TypeOfExpr(CExprId),

    // Function type (6.7.5.3)
    //
    // Note a function taking no arguments should have one `void` argument. Functions without any
    // arguments are in K&R format.
    // Flags: is_variable_argument, is_noreturn, has prototype
    Function(CQualTypeId, Vec<CQualTypeId>, bool, bool, bool),

    // Type definition type (6.7.7)
    Typedef(CTypedefId),

    // Represents a pointer type decayed from an array or function type.
    Decayed(CTypeId),
    Elaborated(CTypeId),

    // Type wrapped in parentheses
    Paren(CTypeId),

    // Struct type
    Struct(CRecordId),

    // Union type
    Union(CRecordId),

    // Enum definition type
    Enum(CEnumId),

    BuiltinFn,

    Attributed(CQualTypeId, Option<Attribute>),

    BlockPointer(CQualTypeId),

    Vector(CQualTypeId, usize),

    Half,
    BFloat16,

    // ARM Scalable Vector Extension types
    // TODO: represent all the individual types in AArch64SVEACLETypes.def
    UnhandledSveType,

    Float128,
    // Atomic types (6.7.2.4)
    Atomic(CQualTypeId),

    // Rust sized types, pullback'd into C so that we can treat uint16_t, etc. as real types.
    Int8,
    Int16,
    Int32,
    Int64,
    IntPtr,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UIntPtr,
    IntMax,
    UIntMax,
    Size,
    SSize,
    PtrDiff,
    WChar,
}

pub type CType = Located<CTypeKind>;

impl CTypeKind {
    pub const PULLBACK_KINDS: [CTypeKind; 16] = {
        use CTypeKind::*;
        [
            Int8, Int16, Int32, Int64, IntPtr, UInt8, UInt16, UInt32, UInt64, UIntPtr, IntMax,
            UIntMax, Size, SSize, PtrDiff, WChar,
        ]
    };

    pub fn is_pointer(&self) -> bool {
        matches!(*self, Self::Pointer { .. })
    }

    pub fn is_bool(&self) -> bool {
        matches!(*self, Self::Bool)
    }

    pub fn is_enum(&self) -> bool {
        matches!(*self, Self::Enum { .. })
    }

    pub fn is_integral_type(&self) -> bool {
        self.is_unsigned_integral_type() || self.is_signed_integral_type()
    }

    pub fn is_unsigned_integral_type(&self) -> bool {
        use CTypeKind::*;
        matches!(
            self,
            Bool | UChar
                | UInt
                | UShort
                | ULong
                | ULongLong
                | UInt128
                | UInt8
                | UInt16
                | UInt32
                | UInt64
                | UIntPtr
                | UIntMax
                | Size
                | WChar
        )
    }

    pub fn is_signed_integral_type(&self) -> bool {
        use CTypeKind::*;
        // `Char` is true on the platforms we handle
        matches!(
            self,
            Char | SChar
                | Int
                | Short
                | Long
                | LongLong
                | Int128
                | Int8
                | Int16
                | Int32
                | Int64
                | IntPtr
                | IntMax
                | SSize
                | PtrDiff
        )
    }

    pub fn is_floating_type(&self) -> bool {
        use CTypeKind::*;
        matches!(self, Float | Double | LongDouble | Half | BFloat16)
    }

    pub fn as_underlying_decl(&self) -> Option<CDeclId> {
        use CTypeKind::*;
        match *self {
            Struct(decl_id) | Union(decl_id) | Enum(decl_id) => Some(decl_id),
            _ => None,
        }
    }

    pub fn as_decl_or_typedef(&self) -> Option<CDeclId> {
        use CTypeKind::*;
        match *self {
            Typedef(decl_id) | Struct(decl_id) | Union(decl_id) | Enum(decl_id) => Some(decl_id),
            _ => None,
        }
    }

    pub fn is_vector(&self) -> bool {
        matches!(self, Self::Vector { .. })
    }

    /// Choose the smaller, simpler of the two types if they are cast-compatible.
    pub fn smaller_compatible_type(ty1: CTypeKind, ty2: CTypeKind) -> Option<CTypeKind> {
        let int = Self::is_integral_type;
        let float = Self::is_floating_type;

        use CTypeKind::*;
        let ty = match (&ty1, &ty2) {
            (ty, ty2) if ty == ty2 => ty1,
            (Void, _) => ty2,
            (Bool, ty) | (ty, Bool) if int(ty) => Bool,

            (Char, ty) | (ty, Char) if int(ty) => Char,
            (SChar, ty) | (ty, SChar) if int(ty) => SChar,
            (UChar, ty) | (ty, UChar) if int(ty) => UChar,

            (Short, ty) | (ty, Short) if int(ty) => Short,
            (UShort, ty) | (ty, UShort) if int(ty) => UShort,

            (Int, ty) | (ty, Int) if int(ty) => Int,
            (UInt, ty) | (ty, UInt) if int(ty) => UInt,

            (Float, ty) | (ty, Float) if float(ty) || int(ty) => Float,

            (Long, ty) | (ty, Long) if int(ty) => Long,
            (ULong, ty) | (ty, ULong) if int(ty) => ULong,

            (Double, ty) | (ty, Double) if float(ty) || int(ty) => Double,

            (LongLong, ty) | (ty, LongLong) if int(ty) => LongLong,
            (ULongLong, ty) | (ty, ULongLong) if int(ty) => ULongLong,

            (LongDouble, ty) | (ty, LongDouble) if float(ty) || int(ty) => LongDouble,

            (Int128, ty) | (ty, Int128) if int(ty) => Int128,
            (UInt128, ty) | (ty, UInt128) if int(ty) => UInt128,

            // Integer to pointer conversion. We want to keep the integer and
            // cast to a pointer at use.
            (Pointer(_), ty) if int(ty) => ty2,
            (ty, Pointer(_)) if int(ty) => ty1,

            // Array to pointer decay. We want to use the array and push the
            // decay to the use of the value.
            (Pointer(ptr_ty), ConstantArray(arr_ty, _))
            | (Pointer(ptr_ty), IncompleteArray(arr_ty))
            | (Pointer(ptr_ty), VariableArray(arr_ty, _))
                if ptr_ty.ctype == *arr_ty =>
            {
                ty2
            }
            (ConstantArray(arr_ty, _), Pointer(ptr_ty))
            | (IncompleteArray(arr_ty), Pointer(ptr_ty))
            | (VariableArray(arr_ty, _), Pointer(ptr_ty))
                if ptr_ty.ctype == *arr_ty =>
            {
                ty1
            }

            _ => return None,
        };
        Some(ty)
    }

    /// Return the element type of a pointer or array
    pub fn element_ty(&self) -> Option<CTypeId> {
        Some(match *self {
            Self::Pointer(ty) => ty.ctype,
            Self::ConstantArray(ty, _) => ty,
            Self::IncompleteArray(ty) => ty,
            Self::VariableArray(ty, _) => ty,
            _ => return None,
        })
    }

    pub fn as_str(&self) -> &'static str {
        use CTypeKind::*;
        match self {
            Void => "void",
            Bool => "_Bool",
            Char => "char",
            SChar => "signed char",
            Short => "signed short",
            Int => "int",
            Long => "long",
            LongLong => "long long",
            UChar => "unsigned char",
            UShort => "unsigned short",
            UInt => "unsigned int",
            ULong => "unsigned long",
            ULongLong => "unsigned long long",
            Float => "float",
            Double => "double",
            LongDouble => "long double",
            Int128 => "__int128",
            UInt128 => "unsigned __int128",
            Half => "half",
            BFloat16 => "bfloat16",
            Float128 => "__float128",

            Int8 => "int8_t",
            Int16 => "int16_t",
            Int32 => "int32_t",
            Int64 => "int64_t",
            IntPtr => "intptr_t",
            UInt8 => "uint8_t",
            UInt16 => "uint16_t",
            UInt32 => "uint32_t",
            UInt64 => "uint64_t",
            UIntPtr => "uintptr_t",
            IntMax => "intmax_t",
            UIntMax => "uintmax_t",
            Size => "size_t",
            SSize => "ssize_t",
            PtrDiff => "ptrdiff_t",
            WChar => "wchar_t",

            _ => unimplemented!("Printer::print_type({:?})", self),
        }
    }

    /// Whether `value` is guaranteed to be in this integer type's range.
    /// Thus, the narrowest possible range is used.
    ///
    /// For example, for [`Self::Long`], [`i32`]'s range is used,
    /// as on Linux and macOS (LP64), it's an [`i64`],
    /// but on Windows (LLP64), it's only an [`i32`].
    ///
    /// This should only be called on integer types.
    /// Other types will return `false`.
    pub fn guaranteed_integer_in_range(&self, value: u64) -> bool {
        fn in_range<T: TryFrom<u64>>(value: u64) -> bool {
            T::try_from(value).is_ok()
        }

        use CTypeKind::*;
        match *self {
            Void => false,

            // Kind of an integer type, but would definitely need an explicit cast.
            Bool => false,

            // Can be signed or unsigned, so choose the minimum range of each.
            Char => (u8::MIN as u64..=i8::MAX as u64).contains(&value),
            WChar => in_range::<i32>(value),

            // `int` is at least `i16` and `long` is at least `i32`.
            SChar => in_range::<i8>(value),
            Short => in_range::<i16>(value),
            Int => in_range::<i16>(value),
            Long => in_range::<i32>(value),
            LongLong => in_range::<i64>(value),

            // `unsigned int` is at least `u16` and `unsigned long` is at least `u32`.
            UChar => in_range::<u8>(value),
            UShort => in_range::<u16>(value),
            UInt => in_range::<u16>(value),
            ULong => in_range::<u32>(value),
            ULongLong => in_range::<u64>(value),

            Int8 => in_range::<i8>(value),
            Int16 => in_range::<i16>(value),
            Int32 => in_range::<i32>(value),
            Int64 => in_range::<i64>(value),
            Int128 => in_range::<i128>(value),

            UInt8 => in_range::<u8>(value),
            UInt16 => in_range::<u16>(value),
            UInt32 => in_range::<u32>(value),
            UInt64 => in_range::<u64>(value),
            UInt128 => in_range::<u128>(value),

            // There's no guarantee on pointer size, but `NULL` should work.
            IntPtr => value == 0,
            UIntPtr => value == 0,

            IntMax => in_range::<i64>(value),
            UIntMax => in_range::<u64>(value),

            // `size_t` is at least a `u16`, and similar for `ssize_t` and `ptrdiff_t`.
            Size => in_range::<u16>(value),
            SSize => in_range::<i16>(value),
            PtrDiff => in_range::<i16>(value),

            // Floats, see `Self::guaranteed_float_in_range`.
            Float => false,
            Double => false,
            LongDouble => false,
            Half => false,
            BFloat16 => false,
            Float128 => false,

            // Non-scalars.
            Complex(_) => false,
            Pointer(_) => false,
            Reference(_) => false,
            ConstantArray(_, _) => false,
            IncompleteArray(_) => false,
            VariableArray(_, _) => false,
            TypeOf(_) => false,
            TypeOfExpr(_) => false,
            Function(_, _, _, _, _) => false,
            Typedef(_) => false,
            Decayed(_) => false,
            Elaborated(_) => false,
            Paren(_) => false,
            Struct(_) => false,
            Union(_) => false,
            Enum(_) => false,
            BuiltinFn => false,
            Attributed(_, _) => false,
            BlockPointer(_) => false,
            Vector(_, _) => false,
            UnhandledSveType => false,
            Atomic(_) => false,
        }
    }

    /// See [`Self::guaranteed_integer_in_range`].
    /// This is the same, but for floats.
    ///
    /// This should only be called on float types.
    /// Other types will return `false`.
    pub fn guaranteed_float_in_range(&self, value: f64) -> bool {
        fn in_range<T: TryFrom<f64>>(value: f64) -> bool {
            T::try_from(value).is_ok()
        }

        use CTypeKind::*;
        match *self {
            // `f32: TryFrom<f64>` is not implemented.
            // C `float`s are not guaranteed to be `f32`,
            // but Rust (namely `libc`) doesn't support any platform where this isn't the case.
            Float => value >= f32::MIN as f64 && value <= f32::MAX as f64,

            // Similarly to `float`, `double` is not guaranteed to be `f64`,
            // but `libc` doesn't support any platform where this isn't the case.
            Double => in_range::<f64>(value),

            // `long double` (not `f128`) is only guaranteed to be at least as precise as a `double`.
            LongDouble => in_range::<f64>(value),

            // All `f64`s are valid `f128`s.
            Float128 => in_range::<f64>(value),

            // TODO Would like to depend on `half`.
            Half => todo!("f16 range"),
            BFloat16 => todo!("bf16 range"),

            Void => false,
            Bool => false,
            Char => false,
            SChar => false,
            Short => false,
            Int => false,
            Long => false,
            LongLong => false,
            UChar => false,
            UShort => false,
            UInt => false,
            ULong => false,
            ULongLong => false,
            Int128 => false,
            UInt128 => false,
            Complex(_) => false,
            Pointer(_) => false,
            Reference(_) => false,
            ConstantArray(_, _) => false,
            IncompleteArray(_) => false,
            VariableArray(_, _) => false,
            TypeOf(_) => false,
            TypeOfExpr(_) => false,
            Function(_, _, _, _, _) => false,
            Typedef(_) => false,
            Decayed(_) => false,
            Elaborated(_) => false,
            Paren(_) => false,
            Struct(_) => false,
            Union(_) => false,
            Enum(_) => false,
            BuiltinFn => false,
            Attributed(_, _) => false,
            BlockPointer(_) => false,
            Vector(_, _) => false,
            UnhandledSveType => false,
            Atomic(_) => false,
            Int8 => false,
            Int16 => false,
            Int32 => false,
            Int64 => false,
            IntPtr => false,
            UInt8 => false,
            UInt16 => false,
            UInt32 => false,
            UInt64 => false,
            UIntPtr => false,
            IntMax => false,
            UIntMax => false,
            Size => false,
            SSize => false,
            PtrDiff => false,
            WChar => false,
        }
    }
}

impl Display for CTypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl TypedAstContext {
    /// Predicate for struct, union, and enum declarations without
    /// bodies. These forward declarations are suitable for use as
    /// the targets of pointers
    pub fn is_forward_declared_type(&self, typ: CTypeId) -> bool {
        use CDeclKind::*;
        || -> Option<()> {
            let decl_id = self.resolve_type(typ).kind.as_underlying_decl()?;
            matches!(
                self[decl_id].kind,
                Struct { fields: None, .. }
                    | Union { fields: None, .. }
                    | Enum {
                        integral_type: None,
                        ..
                    }
            )
            .then(|| ())
        }()
        .is_some()
    }

    /// Returns whether `typ` is a chain of typedefs that ends in `__builtin_va_list`,
    /// thus naming the type clang uses to represent `va_list`s.
    /// This works on all architectures, but does not work in situations where typedefs are
    /// resolved/bypassed, such as with array-to-pointer decay.
    pub fn is_builtin_va_list(&self, mut typ: CTypeId) -> bool {
        loop {
            // Skip over Elaborated types
            let mut kind = &self.index(typ).kind;

            while let &CTypeKind::Elaborated(typ) = kind {
                kind = &self.index(typ).kind;
            }

            // TODO: Rust 1.65: use let-else
            let decl = match kind {
                &CTypeKind::Typedef(decl) => decl,
                _ => return false,
            };
            let (name, qtyp) = match &self.index(decl).kind {
                &CDeclKind::Typedef { ref name, typ, .. } => (name, typ),
                _ => panic!("Typedef decl did not point to a typedef"),
            };

            if name == "__builtin_va_list" {
                return true;
            }

            typ = qtyp.ctype;
        }
    }

    /// Returns whether `typ` is the architecture-specific type used for `va_list`.
    /// Returns `false` for architectures where `va_list` is a generic pointer type.
    pub fn is_va_list_struct(&self, typ: CTypeId) -> bool {
        use BuiltinVaListKind::*;

        match self.va_list_kind {
            // No special identification is possible with generic types.
            CharPtrBuiltinVaList | VoidPtrBuiltinVaList => false,

            // ARM32:
            // typedef struct __va_list {
            //     void *__ap;
            // } __builtin_va_list;

            // ARM64:
            // typedef struct __va_list {
            //     void *__stack;
            //     void *__gr_top;
            //     void *__vr_top;
            //     int __gr_offs;
            //     int __vr_offs;
            // } __builtin_va_list;
            AAPCSABIBuiltinVaList | AArch64ABIBuiltinVaList => {
                // TODO: Rust 1.65: use let-else
                let decl = match self.resolve_type(typ).kind {
                    CTypeKind::Struct(decl) => decl,
                    _ => return false,
                };
                let name = match &self[decl].kind {
                    CDeclKind::Struct {
                        name: Some(name), ..
                    } => name,
                    _ => return false,
                };

                name == "__va_list"
            }

            // X86-64:
            // typedef struct __va_list_tag {
            //     unsigned int gp_offset;
            //     unsigned int fp_offset;
            //     void *overflow_arg_area;
            //     void *reg_save_area;
            // } __builtin_va_list[1];

            // Power:
            // typedef struct __va_list_tag {
            //     unsigned char gpr;
            //     unsigned char fpr;
            //     unsigned short reserved;
            //     char *overflow_arg_area;
            //     char *reg_save_area;
            // } __builtin_va_list[1];
            X86_64ABIBuiltinVaList | PowerABIBuiltinVaList => {
                // TODO: Rust 1.65: use let-else
                let inner = match self.resolve_type(typ).kind {
                    CTypeKind::ConstantArray(inner, 1) => inner,
                    // Account for array-to-pointer decay in function parameters.
                    CTypeKind::Pointer(CQualTypeId { ctype: inner, .. }) => inner,
                    _ => return false,
                };
                let decl = match self.resolve_type(inner).kind {
                    CTypeKind::Struct(decl) => decl,
                    _ => return false,
                };
                let name = match &self[decl].kind {
                    CDeclKind::Struct {
                        name: Some(name), ..
                    } => name,
                    _ => return false,
                };

                name == "__va_list_tag"
            }

            kind => unimplemented!("va_list type {:?} not yet implemented", kind),
        }
    }

    /// Returns whether `typ` is a C `va_list`.
    pub fn is_va_list(&self, typ: CTypeId) -> bool {
        self.is_builtin_va_list(typ) || self.is_va_list_struct(typ)
    }

    /// Predicate for function pointers
    pub fn is_function_pointer(&self, typ: CTypeId) -> bool {
        let resolved_ctype = self.resolve_type(typ);
        use CTypeKind::*;
        if let Pointer(p) = resolved_ctype.kind {
            matches!(self.resolve_type(p.ctype).kind, Function { .. })
        } else {
            false
        }
    }

    /// Returns the length of an array type, or panics.
    pub fn array_len(&self, typ: CTypeId) -> usize {
        match self.resolve_type(typ).kind {
            CTypeKind::ConstantArray(_, len) => len,
            ref kind => panic!("CTypeId {typ:?} is {kind:?}, not a ConstantArray"),
        }
    }

    /// Can the given field decl be a flexible array member?
    pub fn maybe_flexible_array(&self, typ: CTypeId) -> bool {
        let field_ty = self.resolve_type(typ);
        use CTypeKind::*;
        matches!(field_ty.kind, IncompleteArray(_) | ConstantArray(_, 0 | 1))
    }

    pub fn get_pointee_qual_type(&self, typ: CTypeId) -> Option<CQualTypeId> {
        let resolved_ctype = self.resolve_type(typ);
        if let CTypeKind::Pointer(p) = resolved_ctype.kind {
            Some(p)
        } else {
            None
        }
    }

    pub fn type_for_kind(&self, kind: &CTypeKind) -> Option<CTypeId> {
        self.c_types
            .iter()
            .find_map(|(id, k)| if kind == &k.kind { Some(*id) } else { None })
    }

    pub fn resolve_type_id(&self, typ: CTypeId) -> CTypeId {
        use CTypeKind::*;
        let ty = match self.index(typ).kind {
            Attributed(ty, _) => ty.ctype,
            Elaborated(ty) => ty,
            Decayed(ty) => ty,
            TypeOf(ty) => ty,
            Paren(ty) => ty,
            Typedef(decl) => match self.index(decl).kind {
                CDeclKind::Typedef { typ: ty, .. } => ty.ctype,
                _ => panic!("Typedef decl did not point to a typedef"),
            },
            _ => return typ,
        };
        self.resolve_type_id(ty)
    }

    pub fn resolve_type(&self, typ: CTypeId) -> &CType {
        let resolved_typ_id = self.resolve_type_id(typ);
        self.index(resolved_typ_id)
    }

    pub fn is_aligned_struct_type(&self, typ: CTypeId) -> bool {
        if let Some(decl_id) = self.resolve_type(typ).kind.as_underlying_decl() {
            if let CDeclKind::Struct {
                manual_alignment: Some(_),
                ..
            } = self.index(decl_id).kind
            {
                return true;
            }
        }
        false
    }
}

impl Index<CTypeId> for TypedAstContext {
    type Output = CType;

    fn index(&self, index: CTypeId) -> &CType {
        match self.c_types.get(&index) {
            None => panic!("Could not find {:?} in TypedAstContext", index),
            Some(ty) => ty,
        }
    }
}
