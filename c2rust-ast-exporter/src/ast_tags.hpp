//
//  ast_tags.hpp
//  LLVM
//
//  Created by Eric Mertens on 8/15/17.
//
//

#ifndef ast_tags_h
#define ast_tags_h

enum ASTEntryTag {
    TagFunctionDecl = 0,
    TagParmVarDecl,
    TagVarDecl,
    TagStructDecl,
    TagFieldDecl,

    TagEnumDecl,
    TagEnumConstantDecl,
    TagTypedefDecl,
    TagUnionDecl,

    TagNonCanonicalDecl,

    TagStaticAssertDecl,

    TagMacroObjectDef,
    TagMacroFunctionDef,

    TagCompoundStmt = 100,
    TagReturnStmt,
    TagIfStmt,
    TagGotoStmt,
    TagLabelStmt,

    TagNullStmt,
    TagForStmt,
    TagWhileStmt,
    TagSwitchStmt,
    TagDeclStmt,

    TagBreakStmt,
    TagCaseStmt,
    TagContinueStmt,
    TagDefaultStmt,
    TagDoStmt,

    TagAsmStmt,
    TagAttributedStmt,

    TagBinaryOperator = 200,
    TagUnaryOperator,
    TagDeclRefExpr,
    TagImplicitCastExpr,
    TagCallExpr,

    TagInitListExpr,
    TagImplicitValueInitExpr,
    TagArraySubscriptExpr,
    TagCStyleCastExpr,
    TagConditionalOperator,

    TagBinaryConditionalOperator,
    TagMemberExpr,
    TagParenExpr,
    TagUnaryExprOrTypeTraitExpr,
    TagOffsetOfExpr,

    TagCompoundLiteralExpr,
    TagPredefinedExpr,
    TagVAArgExpr,
    TagShuffleVectorExpr,

    TagConvertVectorExpr,
    TagDesignatedInitExpr,

    TagBuiltinBitCastExpr,
    TagMaterializeTemporaryExpr,
    TagExprWithCleanups,

    // Wrapper expressions
    TagFullExpr, // unused
    TagConstantExpr,

    // GNU extensions
    TagStmtExpr,
    TagChooseExpr,

    TagAtomicExpr,

    TagIntegerLiteral = 300,
    TagStringLiteral,
    TagCharacterLiteral,
    TagFloatingLiteral,
};

enum TypeTag {
    TagTypeUnknown = 400,

    TagInt = 500,
    TagShort,
    TagLong,
    TagLongLong,
    TagUInt,

    TagUShort,
    TagULong,
    TagULongLong,
    TagPointer,
    TagReference,
    TagStructType,

    TagUnionType,
    TagDouble,
    TagLongDouble,
    TagFloat,
    TagConstantArrayType,

    TagVariableArrayType,
    TagIncompleteArrayType,
    TagEnumType,
    TagFunctionType,
    TagTypeOfType,

    TagVectorType,
    TagTypedefType,
    TagElaboratedType,
    TagUChar,
    TagSChar,

    TagChar,
    TagVoid,
    TagBool,
    TagDecayedType,
    TagParenType,

    TagSWChar,
    TagUWChar,
    TagInt128,
    TagUInt128,
    TagBuiltinFn,

    TagAttributedType,
    TagBlockPointer,
    TagComplexType,
    TagHalf,
    TagBFloat16,

    TagSveCount,
    TagSveBool,
    TagSveBoolx2,
    TagSveBoolx4,

    TagFloat128,
    TagAtomicType,
};

enum StringTypeTag {
    TagAscii = 600,
    TagWide,
    TagUTF8,
    TagUTF16,
    TagUTF32,
    TagUnevaluated,
};

// From `clang/Basic/TargetInfo.h`
/// The different kinds of `__builtin_va_list` types defined by
/// the target implementation.
enum BuiltinVaListKind {
    /// `typedef char* __builtin_va_list;`
    CharPtrBuiltinVaList = 0,

    /// `typedef void* __builtin_va_list;`
    VoidPtrBuiltinVaList,

    /// `__builtin_va_list` as defined by the [AArch64 ABI](http://infocenter.arm.com/help/topic/com.arm.doc.ihi0055a/IHI0055A_aapcs64.pdf)
    AArch64ABIBuiltinVaList,

    /// `__builtin_va_list` as defined by the [PNaCl ABI](http://www.chromium.org/nativeclient/pnacl/bitcode-abi#TOC-Machine-Types)
    PNaClABIBuiltinVaList,

    /// `__builtin_va_list` as defined by the [Power ABI](https://www.power.org/resources/downloads/Power-Arch-32-bit-ABI-supp-1.0-Embedded.pdf)
    PowerABIBuiltinVaList,

    /// `__builtin_va_list` as defined by the [x86-64 ABI](http://refspecs.linuxbase.org/elf/x86_64-abi-0.21.pdf)
    X86_64ABIBuiltinVaList,

    /// `__builtin_va_list` as defined by the [ARM AAPCS ABI](http://infocenter.arm.com/help/topic/com.arm.doc.ihi0042d/IHI0042D_aapcs.pdf)
    AAPCSABIBuiltinVaList,

    /// ```C
    /// typedef struct __va_list_tag {
    ///     long __gpr;
    ///     long __fpr;
    ///     void *__overflow_arg_area;
    ///     void *__reg_save_area;
    /// } va_list[1];
    /// ```
    SystemZBuiltinVaList
};

#endif /* ast_tags_h */
