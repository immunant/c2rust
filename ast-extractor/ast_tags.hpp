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
    TagRecordDecl,
    TagFieldDecl,
    
    TagEnumDecl,
    TagEnumConstantDecl,
    TagTypedefDecl,
    
    
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
    TagRecordType,
    
    TagDouble,
    TagLongDouble,
    TagFloat,
    TagConstantArrayType,
    TagVariableArrayType,
    
    TagIncompleteArrayType,
    TagEnumType,
    TagFunctionType,
    TagTypeOfType,
    TagTypedefType,
    
    TagElaboratedType,
};
#endif /* ast_tags_h */
