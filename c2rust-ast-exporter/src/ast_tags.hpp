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
};

enum StringTypeTag {
    TagAscii = 600,
    TagWide,
    TagUTF8,
    TagUTF16,
    TagUTF32,
};
#endif /* ast_tags_h */
