use std::collections::HashMap;
use std::io::Cursor;
use cbor::Items;
use cbor::Cbor;
use cbor::CborError;
use std;
use syntax::ptr::P;

use clang_ast;


/// Represents a position inside a C source file
#[derive(Debug)]
pub struct SrcLoc {
  line: u64,
  column: u64,
  file: &str,
}



/// Represents a type in C 
#[derive(Debug)]
pub struct CType {
  loc: SrcLoc,
  kind: CTypeKind,  
}

/// Type qualifiers (6.7.3)
#[derive(Debug)]
pub struct Qualifiers {
  is_const: bool,
  is_restrict: bool,
  is_volatile: bool,
}

/// Qualified type
#[derive(Debug)]
pub struct CQualType {
  qualifiers: Qualifiers,
  ctype: P<CType>,
}


// TODO: these may be interesting, but I'm not sure if they fit here:
//  
//  * ElaboratedType <http://clang.llvm.org/doxygen/classclang_1_1ElaboratedType.html>
//  * UnaryTranformType <http://clang.llvm.org/doxygen/classclang_1_1UnaryTransformType.html>
//  * AdjustedType <http://clang.llvm.org/doxygen/classclang_1_1AdjustedType.html>

/// Represents a CType
///
/// Reflects the types in <http://clang.llvm.org/doxygen/classclang_1_1Type.html>
#[derive(Debug)]
enum CTypeKind {
  /* Builtin types: <https://github.com/llvm-mirror/clang/include/clang/AST/BuiltinTypes.def> */

  /// Void type (6.2.5.19)
  Void,
  
  /// Boolean type (6.2.5.2)
  Bool,
  
  Size,
  
  /// Character type (6.2.5.3)
  Char,
  
  /// Signed types (6.2.5.4)
  SChar, Short, Int, Long, LongLong
  
  /// Unsigned types (6.2.5.6) (actually this also includes `_Bool`)
  UChar, UShort, UInt, ULong, ULongLong

  /// Real floating types (6.2.5.10). Ex: `double`
  Float, Double, LongDouble,

  
  /* Compound types <https://github.com/llvm-mirror/clang/include/clang/AST/TypeNodes.def> */ 
  
  /// Complex types (6.2.5.11). Ex: `float _Complex`.
  Complex(P<CType>),

  /// Pointer types (6.7.5.1)
  Pointer(CQualType),

  /// Array types (6.7.5.2)
  ConstantArray(CQualType, usize),
  IncompleteArray(CQualType),
  VariableArray(CQualType, /* CExpr */),

  /// Type of type or expression (GCC extension)
  TypeOf(CQualType),
  TypeOf(/* CExpr */),

  /// K&R stype function type (6.7.5.3). Ex: `int foo()`.
  FunctionNoProto(CQualType),

  /// Function type always with arguments (6.7.5.3). Ex: `int bar(void)`
  FunctionProto(CQualType, Vec<CQualType>),

  /// Type definition type (6.7.7)
  Typedef(/* CDecl */),

  /// Represents a pointer type decayed from an array or function type.
  Decayed(QualType, QualType),

  /// Struct or union type
  RecordType(/* CDecl */),
  EnumType(/* CDecl */),
}



