#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <fstream>

#include "llvm/Support/Debug.h"
// Declares clang::SyntaxOnlyAction.
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/Tooling.h"

#include <tinycbor/cbor.h>
#include "ast_tags.hpp"

using namespace llvm;
using namespace clang;
using namespace clang::tooling;

#define DEBUG_TYPE "ast-extractor"

using std::string;
using clang::QualType;
using clang::ASTContext;

// Encode a string object assuming that it is valid UTF-8 encoded text
static void cbor_encode_string(CborEncoder *encoder, const std::string &str) {
    auto ptr = str.data();
    auto len = str.size();
    cbor_encode_text_string(encoder, ptr, len);
}

class TranslateASTVisitor;

class TypeEncoder final : public TypeVisitor<TypeEncoder>
{
    ASTContext *Context;
    CborEncoder *encoder;
    std::unordered_map<void*, QualType> *sugared;
    TranslateASTVisitor *astEncoder;
    
    // Bounds recursion when visiting self-referential record declarations
    std::unordered_set<const clang::RecordDecl*> recordDeclsUnderVisit;
   
    std::unordered_set<const clang::Type*> exports;
    
    bool isUnexported(const clang::Type *ptr) {
        return exports.emplace(ptr).second;
    }
    
    void encodeType(const clang::Type *T, TypeTag tag,
                    std::function<void(CborEncoder*)> extra = [](CborEncoder*){}) {
        if (!isUnexported(T)) return;
        
        CborEncoder local;
        cbor_encoder_create_array(encoder, &local, CborIndefiniteLength);
        
        // 1 - Entity ID
        cbor_encode_uint(&local, uintptr_t(T));
        
        // 2 - Type tag
        cbor_encode_uint(&local, tag);
        
        // 3 - extras
        extra(&local);
        
        cbor_encoder_close_container(encoder, &local);
    }

public:
    uintptr_t encodeQualType(QualType t) {
        auto s = t.split();

        auto desugared = sugared->find((void*) s.Ty);
        if (desugared != sugared->end())
          return encodeQualType(desugared->second);

        auto i = uintptr_t(s.Ty);

        if (t.isConstQualified()) {
          i |= 1;
        }
        if (t.isRestrictQualified()) {
          i |= 2;
        }
        if (t.isVolatileQualified()) {
          i |= 4;
        }

        return i;
    }
    
    explicit TypeEncoder
      (ASTContext *Context,
       CborEncoder *encoder,
       std::unordered_map<void*, QualType> *sugared,
       TranslateASTVisitor *ast)
      : Context(Context), encoder(encoder), sugared(sugared), astEncoder(ast) {}
    
    void VisitQualType(const QualType &QT) {
        if (!QT.isNull()) {
            auto s = QT.split();
            
            auto desugared = sugared->find((void*) s.Ty);
            if (desugared != sugared->end())
              VisitQualType(desugared->second);
            else
              Visit(s.Ty);
        }
    }
    
    void VisitAttributedType(const AttributedType *T) {
        auto t = T->getModifiedType();
        auto qt = encodeQualType(t);
        
        encodeType(T, TagAttributedType, [T,qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
        });
        
        VisitQualType(t);
    }
    
    void VisitParenType(const ParenType *T) {
        auto t = T->getInnerType();
        auto qt = encodeQualType(t);
        
        encodeType(T, TagParenType, [T,qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
        });
        
        VisitQualType(t);
    }
    
    void VisitEnumType(const EnumType *T) {
        encodeType(T, TagEnumType, [T](CborEncoder *local) {
            cbor_encode_uint(local, uintptr_t(T->getDecl()->getCanonicalDecl()));
        });
    }
    
    void VisitConstantArrayType(const ConstantArrayType *T) {
        auto t = T->getElementType();
        auto qt = encodeQualType(t);
        
        encodeType(T, TagConstantArrayType, [T,qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
            cbor_encode_uint(local, T->getSize().getLimitedValue());
        });
        
        VisitQualType(t);
    }
    
    void VisitVariableArrayType(const VariableArrayType *T);
    
    void VisitIncompleteArrayType(const IncompleteArrayType *T) {
        auto t = T->getElementType();
        auto qt = encodeQualType(t);

        encodeType(T, TagIncompleteArrayType, [qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
        });
        
        VisitQualType(t);
    }
    
    void VisitBlockPointerType(const BlockPointerType *T) {
        auto t = T->getPointeeType();
        auto qt = encodeQualType(t);
        
        encodeType(T, TagBlockPointer, [qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
        });
        
        VisitQualType(t);
    }
    
    // definition below due to recursive call into AST translator
    void VisitRecordType(const RecordType *T);
    
    void VisitBuiltinType(const BuiltinType *T) {
        TypeTag tag;
        auto kind = T->getKind();
        switch (kind) {
            default:                      tag = TagTypeUnknown; break;
            case BuiltinType::BuiltinFn:  tag = TagBuiltinFn;   break;
            case BuiltinType::UInt128:    tag = TagUInt128;     break;
            case BuiltinType::Int128:     tag = TagInt128;      break;
            case BuiltinType::Short:      tag = TagShort;       break;
            case BuiltinType::Int:        tag = TagInt;         break;
            case BuiltinType::Long:       tag = TagLong;        break;
            case BuiltinType::LongLong:   tag = TagLongLong;    break;
            case BuiltinType::UShort:     tag = TagUShort;      break;
            case BuiltinType::UInt:       tag = TagUInt;        break;
            case BuiltinType::ULong:      tag = TagULong;       break;
            case BuiltinType::ULongLong:  tag = TagULongLong;   break;
            case BuiltinType::Double:     tag = TagDouble;      break;
            case BuiltinType::LongDouble: tag = TagLongDouble;  break;
            case BuiltinType::Float:      tag = TagFloat;       break;
            case BuiltinType::SChar:      tag = TagSChar;       break;
            case BuiltinType::UChar:      tag = TagUChar;       break;
            case BuiltinType::Char_U:     tag = TagChar;        break;
            case BuiltinType::Char_S:     tag = TagChar;        break;
            case BuiltinType::Void:       tag = TagVoid;        break;
            case BuiltinType::Bool:       tag = TagBool;        break;
            case BuiltinType::WChar_S:    tag = TagSWChar;      break;
            case BuiltinType::WChar_U:    tag = TagUWChar;      break;
        }
        
        encodeType(T, tag);
    }
    
    // Clang represents function declarations with parameters as `FunctionProtoType`
    // instances whereas functions w/o parameters are handled as `FunctionNoPrototype`
    // instances. Note: we could handle both cases by overriding `VisitFunctionType`
    // instead of the current two-function solution.
    void VisitFunctionProtoType(const FunctionProtoType *T) {
        auto EPI = T->getExtProtoInfo();
        static bool firstWarning = true;
        if(firstWarning && EPI.Variadic) {
            std::cerr << "Warning: variadic functions are not fully supported.\n";
            firstWarning = false;
        }
        DEBUG(dbgs() << "Visit ");
        DEBUG(T->dump());

        encodeType(T, TagFunctionType, [T, this](CborEncoder *local) {
            CborEncoder arrayEncoder;

            // Function types are encoded with an extra list of types. The return type
            // is always the first element of the list followed by the parameters.
            size_t elts = T->getNumParams()+1;
            cbor_encoder_create_array(local, &arrayEncoder, elts);

            cbor_encode_uint(&arrayEncoder, encodeQualType(T->getReturnType()));
            for (auto t : T->param_types()) {
                cbor_encode_uint(&arrayEncoder, encodeQualType(t));
            }
            
            cbor_encoder_close_container(local, &arrayEncoder);

            cbor_encode_boolean(local, T->getExtProtoInfo().Variadic);
        });

        VisitQualType(T->getReturnType());
        for (auto x : T->param_types()) {
            VisitQualType(x);
        }

    }

    // See `VisitFunctionProtoType`.
    void VisitFunctionNoProtoType(const FunctionNoProtoType *T) {
        encodeType(T, TagFunctionType, [T](CborEncoder *local) {
            CborEncoder arrayEncoder;

            cbor_encoder_create_array(local, &arrayEncoder, 1);

            cbor_encode_uint(&arrayEncoder, uintptr_t(T->getReturnType().getTypePtrOrNull()));

            cbor_encoder_close_container(local, &arrayEncoder);

            cbor_encode_boolean(local, false);
        });

        VisitQualType(T->getReturnType());
    }
    
    void VisitPointerType(const clang::PointerType *T) {
        auto pointee = T->getPointeeType();
        auto qt = encodeQualType(pointee);
        
        encodeType(T, TagPointer, [qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
        });
        
        VisitQualType(pointee);
    }
    
    void VisitTypedefType(const TypedefType *T);
    
    void VisitTypeOfType(const TypeOfType *T) {
        auto t = T->desugar();
        auto qt = encodeQualType(t);
        encodeType(T, TagTypeOfType, [qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
        });
        VisitQualType(t);
    }
    
    void VisitTypeOfExprType(const TypeOfExprType *T) {
        auto t = T->desugar();
        auto qt = encodeQualType(t);
        encodeType(T, TagTypeOfType, [qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
        });
        VisitQualType(t);
    }
    
    void VisitElaboratedType(const ElaboratedType *T) {
        auto t = T->desugar();
        auto qt = encodeQualType(t);
        encodeType(T, TagElaboratedType, [qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
        });

        VisitQualType(t);
    }
    
    void VisitDecayedType(const DecayedType *T) {
        auto t = T->desugar();
        auto qt = encodeQualType(t);
        encodeType(T, TagDecayedType, [qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
        });
        
        VisitQualType(t);
    }
};

class TranslateASTVisitor final
  : public RecursiveASTVisitor<TranslateASTVisitor> {
      
      ASTContext *Context;
      TypeEncoder typeEncoder;
      CborEncoder *encoder;
      std::unordered_map<string, uint64_t> filenames;
      std::set<std::pair<void*, ASTEntryTag>> exportedTags;
      
      // Returns true when a new entry is added to exportedTags
      bool markForExport(void* ptr, ASTEntryTag tag) {
          return exportedTags.emplace(ptr,tag).second;
      }
      
      void encodeSourcePos(CborEncoder *enc, SourceLocation loc) {
          auto& manager = Context->getSourceManager();
          auto line = manager.getPresumedLineNumber(loc);
          auto col  = manager.getPresumedColumnNumber(loc);
          auto fileid = manager.getFileID(loc);
          auto entry = manager.getFileEntryForID(fileid);
          
          auto filename = string("?");
          if (entry) {
              filename = entry->getName().str();
          }
          
          auto pair = filenames.insert(std::make_pair(filename, filenames.size()));
          
          cbor_encode_uint(enc, pair.first->second);
          cbor_encode_uint(enc, line);
          cbor_encode_uint(enc, col);
      }
      
      // Template required because Decl and Stmt don't share a common base class
      void encode_entry_raw
             (void *ast,
              ASTEntryTag tag,
              SourceLocation loc,
              const QualType ty,
              const std::vector<void *> &childIds,
              std::function<void(CborEncoder*)> extra
             )
      {
          if (!markForExport(ast, tag)) return;
          
          CborEncoder local, childEnc;
          cbor_encoder_create_array(encoder, &local, CborIndefiniteLength);
          
          // 1 - Entry ID
          cbor_encode_uint(&local, uintptr_t(ast));
          
          // 2 - Entry Tag
          cbor_encode_uint(&local, tag);
          
          // 3 - Entry Children
          cbor_encoder_create_array(&local, &childEnc, childIds.size());
          for (auto x : childIds) {
              if (x == nullptr) {
                  cbor_encode_null(&childEnc);
              } else {
                  cbor_encode_uint(&childEnc, uintptr_t(x));
              }
          }
          cbor_encoder_close_container(&local , &childEnc);
          
          // 4 - File number
          // 5 - Line number
          // 6 - Column number
          encodeSourcePos(&local, loc);

          // 7 - Type ID (only for expressions)
          encode_qualtype(&local, ty);
          
          // 7 - Extra entries
          extra(&local);
          
          cbor_encoder_close_container(encoder, &local);
      }
      
      void encode_qualtype(CborEncoder *enc, QualType ty) {
          if (ty.getTypePtrOrNull()) {
              cbor_encode_uint(enc, typeEncoder.encodeQualType(ty));
          } else {
              cbor_encode_null(enc);
          }
      }

      void encode_entry
      (Expr *ast,
       ASTEntryTag tag,
       const std::vector<void *> &childIds,
       std::function<void(CborEncoder*)> extra = [](CborEncoder*){}
       ) {
          auto ty = ast->getType();
          encode_entry_raw(ast, tag, ast->getLocStart(), ty, childIds, extra);
          typeEncoder.VisitQualType(ty);
      }

      void encode_entry
      (Stmt *ast,
       ASTEntryTag tag,
       const std::vector<void *> &childIds,
       std::function<void(CborEncoder*)> extra = [](CborEncoder*){}
       ) {
          QualType s = QualType(static_cast<clang::Type*>(nullptr), 0);
          encode_entry_raw(ast, tag, ast->getLocStart(), s, childIds, extra);
      }
      
      void encode_entry
      (Decl *ast,
       ASTEntryTag tag,
       const std::vector<void *> &childIds,
       const QualType T,
       std::function<void(CborEncoder*)> extra = [](CborEncoder*){}
       ) {
          encode_entry_raw(ast, tag, ast->getLocStart(), T, childIds, extra);
      }
      
      
  public:
      explicit TranslateASTVisitor(ASTContext *Context, CborEncoder *encoder, std::unordered_map<void*, QualType> *sugared)
      : Context(Context), typeEncoder(Context, encoder, sugared, this), encoder(encoder) {
      }
      
      // Override the default behavior of the RecursiveASTVisitor
      bool shouldVisitImplicitCode() const {
          return true;
      }
      
      const std::unordered_map<string,uint64_t> &getFilenames() const {
          return filenames;
      }
      
      //
      // Statements
      //
      
      bool VisitCompoundStmt(CompoundStmt *CS) {
          std::vector<void*> childIds;
          for (auto x : CS->children()) {
              childIds.push_back(x);
          }

          encode_entry(CS, TagCompoundStmt, childIds);
          return true;
      }

      
      bool VisitReturnStmt(ReturnStmt *RS) {
          std::vector<void*> childIds =
          { RS->getRetValue() } ;
          encode_entry(RS, TagReturnStmt, childIds);
          return true;
      }

      bool VisitDoStmt(DoStmt *S) {
          std::vector<void*> childIds = { S->getBody(), S->getCond() } ;
          encode_entry(S, TagDoStmt, childIds);
          return true;
      }
      
      bool VisitGotoStmt(GotoStmt *GS) {
          std::vector<void*> childIds = { GS->getLabel()->getStmt() };
          encode_entry(GS, TagGotoStmt, childIds);
          return true;
      }
      
      bool VisitLabelStmt(LabelStmt *LS) {
          
          std::vector<void*> childIds = { LS->getSubStmt() };
          encode_entry(LS, TagLabelStmt, childIds,
                             [LS](CborEncoder *array){
                                 cbor_encode_text_stringz(array, LS->getName());
                             });
          return true;
      }

      
      bool VisitNullStmt(NullStmt *NS) {
          std::vector<void*> childIds;
          encode_entry(NS, TagNullStmt, childIds);
          return true;
      }
      
      bool VisitIfStmt(IfStmt *IS) {
          std::vector<void*> childIds = { IS->getCond(), IS->getThen(), IS->getElse() } ;
          encode_entry(IS, TagIfStmt, childIds);
          return true;
      }
      
      bool VisitForStmt(ForStmt *FS) {
          std::vector<void*> childIds =
          { FS->getInit(), FS->getCond(), FS->getInc(), FS->getBody() };
          encode_entry(FS, TagForStmt, childIds);
          return true;
      }
      
      bool VisitWhileStmt(WhileStmt *WS) {
          std::vector<void*> childIds =
          { WS->getCond(), WS->getBody() };
          encode_entry(WS, TagWhileStmt, childIds);
          return true;
      }
      

      bool VisitDeclStmt(DeclStmt *DS) {

          DEBUG(dbgs() << "Visit ");
          DEBUG(DS->dumpColor());

          // We copy only canonical decls and VarDecl's that are extern/local. For more on the
          // latter, see the comment at the top of `VisitVarDecl`
          std::vector<void*> childIds;
          std::copy_if(
              DS->decl_begin(),
              DS->decl_end(),
              std::back_inserter(childIds),
              [](Decl *decl){
                  if (decl->isCanonicalDecl())
                      return true;

                  if (VarDecl *var_decl = dyn_cast<VarDecl>(decl))
                      return var_decl->isExternC() && var_decl->isLocalVarDecl();

                  return false;
              }
          );

          encode_entry(DS, TagDeclStmt, childIds);
          return true;
      }

      
      bool VisitBreakStmt(BreakStmt *BS) {
          std::vector<void*> childIds;
          encode_entry(BS, TagBreakStmt, childIds);
          return true;
      }
      
      bool VisitContinueStmt(ContinueStmt *S) {
          std::vector<void*> childIds;
          encode_entry(S, TagContinueStmt, childIds);
          return true;
      }
      
      bool VisitCaseStmt(CaseStmt *CS) {
          auto expr = CS->getLHS();

          APSInt value;
          if (!expr->isIntegerConstantExpr(value, *Context)) {
              abort();
          }

          std::vector<void*> childIds { expr, CS->getSubStmt() };
          encode_entry(CS, TagCaseStmt, childIds, [value](CborEncoder *extra) {
              if (value.isSigned()) {
                  cbor_encode_int(extra, value.getSExtValue());
              } else {
                  cbor_encode_uint(extra, value.getZExtValue());
              }
          });
          return true;
      }
      
      bool VisitSwitchStmt(SwitchStmt *SS) {
          std::vector<void*> childIds =
          { SS->getCond(), SS->getBody() };
          encode_entry(SS, TagSwitchStmt, childIds);
          return true;
      }
      
      bool VisitDefaultStmt(DefaultStmt *DS) {
          std::vector<void*> childIds = { DS->getSubStmt() };
          encode_entry(DS, TagDefaultStmt, childIds);
          return true;
      }
      
      //
      // Expressions
      //
      
      bool VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *E) {
          std::vector<void*> childIds { E->isArgumentType() ? nullptr : E->getArgumentExpr() };
          auto t = E->getTypeOfArgument();
          auto qt = typeEncoder.encodeQualType(t);
          encode_entry(E, TagUnaryExprOrTypeTraitExpr, childIds, [E,qt](CborEncoder *extras){
              switch(E->getKind()) {
                  case UETT_SizeOf: cbor_encode_text_stringz(extras, "sizeof"); break;
                  case UETT_AlignOf: cbor_encode_text_stringz(extras, "alignof"); break;
                  case UETT_VecStep: cbor_encode_text_stringz(extras, "vecstep"); break;
                  case UETT_OpenMPRequiredSimdAlign: cbor_encode_text_stringz(extras, "openmprequiredsimdalign"); break;
              }
              cbor_encode_uint(extras, qt);
          });
          typeEncoder.VisitQualType(t);
          return true;
      }
      
      bool VisitParenExpr(ParenExpr *E) {
          std::vector<void*> childIds { E->getSubExpr() };
          encode_entry(E, TagParenExpr, childIds);
          return true;
      }
      
      /*
       [C99 6.5.2.3] Structure and Union Members.
       Children:
       - base expression
       - field declaration
       Extras:
       - true: is arrow; false: is dot
       */
      bool VisitMemberExpr(MemberExpr *E) {
          std::vector<void*> childIds
            { E->getBase(), E->getMemberDecl()->getCanonicalDecl() };
          encode_entry(E, TagMemberExpr, childIds, [E](CborEncoder *extras) {
              cbor_encode_boolean(extras, E->isArrow());
          });
          return true;
      }
      
      /*
       [C99 6.5.2.5] Compound literal expression
       Children:
       - initializer expression
       Extras: (none)
       */
      bool VisitCompoundLiteralExpr(CompoundLiteralExpr *E) {
          std::vector<void*> childIds { E->getInitializer() };
          encode_entry(E, TagCompoundLiteralExpr, childIds);
          return true;
      }
      
      /*
       Describes a C initializer list
       Children: expressions
       Extras: (none)
       */
      bool VisitInitListExpr(InitListExpr *ILE) {
          auto inits = ILE->inits();
          std::vector<void*> childIds(inits.begin(), inits.end());
          encode_entry(ILE, TagInitListExpr, childIds, [ILE](CborEncoder *extras) {
              auto union_field = ILE->getInitializedFieldInUnion();
              if (union_field) {
                  cbor_encode_uint(extras, uintptr_t(union_field));
              } else {
                  cbor_encode_null(extras);
              }
          });
          
          return true;
      }
      
      bool VisitPredefinedExpr(PredefinedExpr *E) {
          std::vector<void*> childIds { E->getFunctionName() };
          encode_entry(E, TagPredefinedExpr, childIds);
          return true;
      }
      
      bool VisitImplicitValueInitExpr(ImplicitValueInitExpr *E) {
          std::vector<void*> childIds;
          encode_entry(E, TagImplicitValueInitExpr, childIds);
          return true;
      }
      
      bool VisitImplicitCastExpr(ImplicitCastExpr *ICE) {
          std::vector<void*> childIds = { ICE->getSubExpr() };
          encode_entry(ICE, TagImplicitCastExpr, childIds,
                             [ICE](CborEncoder *array){
                                 auto cast_name = ICE->getCastKindName();
                                 
                                 if (ICE->getCastKind() == CastKind::CK_BitCast) {
                                     auto source_type = ICE->getSubExpr()->getType();
                                     auto target_type = ICE->getType();
                                                                         
                                     if (target_type.getTypePtr()->isPointerType() && source_type.getTypePtr()->isPointerType()) {
                                         auto target_pointee = static_cast<const clang::PointerType*>(target_type.getTypePtr())->getPointeeType();
                                         auto source_pointee = static_cast<const clang::PointerType*>(source_type.getTypePtr())->getPointeeType();


                                         if (target_pointee.isConstQualified() &&
                                             source_pointee->getUnqualifiedDesugaredType() == target_pointee->getUnqualifiedDesugaredType()) {
                                                cast_name = "ConstCast";
                                        }
                                     }
                                 }
                                 
                                 cbor_encode_text_stringz(array, cast_name);
                             });
          return true;
      }
      
      bool VisitCStyleCastExpr(CStyleCastExpr *E) {
          std::vector<void*> childIds = { E->getSubExpr() };
          
          
          if (E->getCastKind() == CastKind::CK_ToUnion) {
              
              FieldDecl *target_field = nullptr;
              auto src_type = E->getSubExpr()->getType()->getUnqualifiedDesugaredType();
              
              for (auto&& field : E->getType()->getAsUnionType()->getDecl()->fields()) {
                  auto field_type = field->getType()->getUnqualifiedDesugaredType();
               
                  if (field_type == src_type) {
                      target_field = field;
                      break;
                  }
              }
              
              childIds.push_back(target_field);
          }
          
          encode_entry(E, TagCStyleCastExpr, childIds,
                       [E](CborEncoder *array){
                           cbor_encode_text_stringz(array, E->getCastKindName());
                       });
          return true;
      }
      
      bool VisitUnaryOperator(UnaryOperator *UO) {
          std::vector<void*> childIds = { UO->getSubExpr() };
          encode_entry(UO, TagUnaryOperator, childIds,
                             [UO](CborEncoder *array) {
                                 cbor_encode_string(array, UO->getOpcodeStr(UO->getOpcode()).str());
                                 cbor_encode_boolean(array, UO->isPrefix());
                             });
          return true;
      }
      
      bool VisitBinaryOperator(BinaryOperator *BO) {
          std::vector<void*> childIds = { BO->getLHS(), BO->getRHS() };
          
          QualType computationLHSType, computationResultType;
          
          if (auto cao = dyn_cast_or_null<CompoundAssignOperator>(BO)) {
              computationLHSType = cao->getComputationLHSType();
              computationResultType = cao->getComputationResultType();
              typeEncoder.VisitQualType(computationLHSType);
              typeEncoder.VisitQualType(computationResultType);
          }
          
          encode_entry(BO, TagBinaryOperator, childIds,
                             [this, BO, computationLHSType, computationResultType](CborEncoder *array) {
                                 cbor_encode_string(array, BO->getOpcodeStr().str());
                                 
                                 encode_qualtype(array, computationLHSType);
                                 encode_qualtype(array, computationResultType);
                             });
          return true;
      }
      
      bool VisitConditionalOperator(ConditionalOperator *CO) {
          std::vector<void*> childIds = { CO->getCond(), CO->getTrueExpr(), CO->getFalseExpr() };
          encode_entry(CO, TagConditionalOperator, childIds);
          return true;
      }
      
      bool VisitBinaryConditionalOperator(BinaryConditionalOperator *CO) {
          std::vector<void*> childIds = { CO->getCommon(), CO->getFalseExpr() };
          encode_entry(CO, TagBinaryConditionalOperator, childIds);
          return true;
      }
      
      bool VisitDeclRefExpr(DeclRefExpr *DRE) {
          DEBUG(dbgs() << "Visiting ");
          DEBUG(DRE->dumpColor());
          DEBUG(DRE->getDecl()->getType()->dump());
          DEBUG(DRE->getType()->dump());
          std::vector<void*> childIds = { DRE->getDecl()->getCanonicalDecl() };
          encode_entry(DRE, TagDeclRefExpr, childIds);
          return true;
      }
      
      bool VisitCallExpr(CallExpr *CE) {
          std::vector<void*> childIds = { CE->getCallee() };
          for (auto x : CE->arguments()) {
              childIds.push_back(x);
          }
          encode_entry(CE, TagCallExpr, childIds);
          return true;
      }
      
      bool VisitArraySubscriptExpr(ArraySubscriptExpr *E) {
          std::vector<void*> childIds = { E->getLHS(), E->getRHS() };
          encode_entry(E, TagArraySubscriptExpr, childIds);
          return true;
      }
 
      
      //
      // Declarations
      //
      
      // Some function declarations are also function definitions.
      // This method handles both types of declarations.
      bool VisitFunctionDecl(FunctionDecl *FD)
      {              
          // Skip non-canonical decls
          if(!FD->isCanonicalDecl())
              return true;

          // Use the parameters from the function declaration
          // the defines the body, if one exists.
          const FunctionDecl *paramsFD = FD;
          auto body = FD->getBody(paramsFD); // replaces its argument if body exists
          
          std::vector<void*> childIds;
          for (auto x : paramsFD->parameters()) {
              auto cd = x->getCanonicalDecl();
              childIds.push_back(cd);
              TraverseDecl(cd);
          }

          childIds.push_back(body);

          auto functionType = FD->getType();
          encode_entry(FD, TagFunctionDecl, childIds, functionType,
                             [FD](CborEncoder *array) {
                                 auto name = FD->getNameAsString();
                                 cbor_encode_string(array, name);

                                 auto is_extern = FD->isExternC();
                                 cbor_encode_boolean(array, is_extern);

                                 auto def = FD->getDefinition();
                                 bool is_inline = def && def->isInlineSpecified();
                                 cbor_encode_boolean(array, is_inline);

                                 auto is_main = FD->isMain();
                                 cbor_encode_boolean(array, is_main);
                             });
          typeEncoder.VisitQualType(functionType);

          return true;
      }
      
      /* I don't think this adds anything that we don't get from VarDecl
      bool VisitParmVarDecl(ParmVarDecl *PVD)
      {
          std::vector<void*> childIds = { PVD->getDefinition() };
          encode_entry_extra(encoder, PVD, TagParmVarDecl, childIds,
                             [PVD](CborEncoder *array){
                                 auto name = PVD->getNameAsString();
                                 cbor_encode_text_stringz(array, name.c_str());
                             });
          return true;
      }*/
      
      
      bool VisitVarDecl(VarDecl *VD)
      {
          // Skip non-canonical decls, as long as they aren't 'extern'. Unfortunately, if there
          // are two 'extern' variables in different functions that should be the same at link
          // time, Clang groups them. That is unhelpful for us though, since we need to convert
          // them into two seperate `extern` blocks.
          if(!VD->isCanonicalDecl() && !(VD->isExternC() && VD->isLocalVarDecl()))
              return true;

          std::vector<void*> childIds =
          { VD->getInit() } ;
          auto T = VD->getType();
          
          encode_entry(VD, TagVarDecl, childIds, T,
                             [VD](CborEncoder *array){
                                 auto name = VD->getNameAsString();
                                 cbor_encode_string(array, name);

                                 auto is_static = VD->getStorageDuration() == clang::SD_Static;
                                 cbor_encode_boolean(array, is_static);

                                 auto is_extern = VD->isExternC();
                                 cbor_encode_boolean(array, is_extern);

                                 auto is_defn = VD->isThisDeclarationADefinition() != clang::VarDecl::DefinitionKind::DeclarationOnly;
                                 cbor_encode_boolean(array, is_defn);
                             });
          
          typeEncoder.VisitQualType(T);

          return true;
      }
      
      /*
       Represents a struct/union
       Children:
       - canonical field declarations
       Extras:
       - name as string
       */
      bool VisitRecordDecl(RecordDecl *D)
      {
          // Skip non-canonical decls
          if (!D->isCanonicalDecl()) {
              return true;
          }
          
          auto def = D->getDefinition();
          
          std::vector<void*> childIds;
          if (def) {
              for (auto x : def->fields()) {
                  childIds.push_back(x->getCanonicalDecl());
              }
          }
          
          auto tag = D->isStruct() ? TagStructDecl : TagUnionDecl;
          
          encode_entry(D, tag, childIds, QualType(),
          [D,def](CborEncoder *local){
              auto name = D->getNameAsString();
              if (name.empty()) {
                  cbor_encode_null(local);
              } else {
                  cbor_encode_string(local, name);
              }
              cbor_encode_boolean(local, !!def);
          });
          
          return true;
      }
      
      bool VisitEnumDecl(EnumDecl *D)
      {
          // Skip non-canonical decls
          if(!D->isCanonicalDecl())
              return true;

          std::vector<void*> childIds;
          for (auto x : D->enumerators()) {
              childIds.push_back(x->getCanonicalDecl());
          }
          
          auto underlying_type = D->getIntegerType();
          typeEncoder.VisitQualType(underlying_type);
          
          encode_entry(D, TagEnumDecl, childIds, underlying_type,
          [D](CborEncoder *local){
              auto name = D->getNameAsString();
              if (name.empty()) {
                  cbor_encode_null(local);
              } else {
                  cbor_encode_string(local, name);
              }
          });
          
          return true;
      }
      
      bool VisitEnumConstantDecl(EnumConstantDecl *D)
      {
          // Skip non-canonical decls
          if(!D->isCanonicalDecl())
              return true;

          std::vector<void*> childIds; // = { D->getInitExpr() };
          
          encode_entry(D, TagEnumConstantDecl, childIds, QualType(),
            [D](CborEncoder *local){
              auto name = D->getNameAsString();
              cbor_encode_string(local, name);

              auto val = D->getInitVal().getSExtValue();
              cbor_encode_int(local, val);
          });
          return true;
      }
      
      bool VisitFieldDecl(FieldDecl *D)
      {
          // Skip non-canonical decls
          if(!D->isCanonicalDecl())
              return true;

          std::vector<void*> childIds;
          auto t = D->getType();
          encode_entry(D, TagFieldDecl, childIds, t,
                             [D, this](CborEncoder *array) {
                                 auto name = D->getNameAsString();
                                 cbor_encode_string(array, name);
                                 
                                 if (D->isBitField()) {
                                     cbor_encode_uint(array, D->getBitWidthValue(*this->Context));
                                 } else {
                                     cbor_encode_null(array);
                                 };
                             });
          
          // This might be the only occurence of this type in the translation unit
          typeEncoder.VisitQualType(t);
          
          return true;
      }
      
      bool VisitTypedefNameDecl(TypedefNameDecl *D)
      {
          // Skip non-canonical decls
          if(!D->isCanonicalDecl())
              return true;

          std::vector<void*> childIds;
          auto typeForDecl = D->getUnderlyingType();
          encode_entry(D, TagTypedefDecl, childIds, typeForDecl,
                             [D](CborEncoder *array) {
                                 auto name = D->getNameAsString();
                                 cbor_encode_string(array, name);
                             });

          typeEncoder.VisitQualType(typeForDecl);
          
          return true;
      }
      
      //
      // Literals
      //
      
      bool VisitIntegerLiteral(IntegerLiteral *IL) {
          std::vector<void*> childIds;
          encode_entry(IL, TagIntegerLiteral, childIds,
                             [IL](CborEncoder *array){
                                 cbor_encode_uint(array, IL->getValue().getLimitedValue());
                             });
          return true;
      }
      
      bool VisitCharacterLiteral(CharacterLiteral *L) {
          std::vector<void*> childIds;
          encode_entry(L, TagCharacterLiteral, childIds,
                             [L](CborEncoder *array){
                                 auto lit = L->getValue();
                                 cbor_encode_uint(array, lit);
                             });
          return true;
      }
      
      bool VisitStringLiteral(clang::StringLiteral *SL) {
          std::vector<void*> childIds;
          encode_entry(SL, TagStringLiteral, childIds,
                             [SL](CborEncoder *array){
                                // C and C++ supports different string types, so 
                                // we need to identify the string literal type
                                switch(SL->getKind()) {
                                    case clang::StringLiteral::StringKind::Ascii:
                                        cbor_encode_uint(array, StringTypeTag::TagAscii);
                                        break;
                                    case clang::StringLiteral::StringKind::Wide:
                                        cbor_encode_uint(array, StringTypeTag::TagWide);
                                        break;
                                    case clang::StringLiteral::StringKind::UTF8:
                                        cbor_encode_uint(array, StringTypeTag::TagUTF8);
                                        break;
                                    case clang::StringLiteral::StringKind::UTF16:
                                        cbor_encode_uint(array, StringTypeTag::TagUTF16);
                                        break;
                                    case clang::StringLiteral::StringKind::UTF32:
                                        cbor_encode_uint(array, StringTypeTag::TagUTF32);
                                        break;
                                }
                                // The size of the wchar_t type in C is implementation defined
                                cbor_encode_uint(array, SL->getCharByteWidth());

                                // String literals can contain arbitrary bytes, so  
                                // we encode these as byte strings rather than text.

                                const uint8_t* bytes = reinterpret_cast<const uint8_t*>(SL->getBytes().data());
                                cbor_encode_byte_string(array, bytes, SL->getByteLength());
                             });
          return true;
      }
      
      bool VisitFloatingLiteral(clang::FloatingLiteral *L) {
          std::vector<void*> childIds;
          encode_entry(L, TagFloatingLiteral, childIds,
                       [L](CborEncoder *array){
                           auto lit = L->getValueAsApproximateDouble();
                           cbor_encode_double(array, lit);
                       });
          return true;
      }
};

void TypeEncoder::VisitRecordType(const RecordType *T) {
  
    // Should only ever be reached during the first pass
    if (T->isSugared()) {
      auto qt = T->desugar();
      sugared->emplace((void*) T, qt);
      VisitQualType(qt);
    }

    auto tag = T->isStructureType() ? TagStructType : TagUnionType;
    
    encodeType(T, tag, [T](CborEncoder *local) {
        cbor_encode_uint(local, uintptr_t(T->getDecl()->getCanonicalDecl()));
    });
    
    // record type might be anonymous and have no top-level declaration
    // structure declarations can reference themselves, so we need
    // a way to guard against unbounded recursion.
    clang::RecordDecl *D = T->getDecl();
    if(recordDeclsUnderVisit.emplace(D).second) {
        astEncoder->TraverseDecl(D);
        recordDeclsUnderVisit.erase(D);
    }
}

void TypeEncoder::VisitTypedefType(const TypedefType *T) {
    
    auto D = T->getDecl()->getCanonicalDecl();

    encodeType(T, TagTypedefType, [D, T](CborEncoder *local) {
        cbor_encode_uint(local, uintptr_t(D));
    });
    astEncoder->TraverseDecl(D);
}

void TypeEncoder::VisitVariableArrayType(const VariableArrayType *T) {
    auto t = T->getElementType();
    auto qt = encodeQualType(t);
    
    auto c = T->getSizeExpr();
    astEncoder->TraverseStmt(c);
    
    encodeType(T, TagVariableArrayType, [qt, c](CborEncoder *local) {
        cbor_encode_uint(local, qt);
        if (c) {
            cbor_encode_uint(local, uintptr_t(c));
        } else {
            // This case occurs when the expression omitted and * is used:
            // void a_function(int example[][*]);
            cbor_encode_null(local);
        }
    });
    
    VisitQualType(t);
}

class TranslateConsumer : public clang::ASTConsumer {
    const std::string outfile;
public:
    explicit TranslateConsumer(llvm::StringRef InFile) 
        : outfile(InFile.str().append(".cbor")) { }
    
    virtual void HandleTranslationUnit(clang::ASTContext &Context) {
        
        CborEncoder encoder;

        // There are some type nodes (see `TypedefType` and `RecordType`) which
        // can be "sugared". That means we should not follow the declarations we
        // normally would follow for those types, but we should use the `desugared`
        // type instead.
        std::unordered_map<void*, QualType> sugared;
        
        auto process = [&encoder, &Context, &sugared](uint8_t *buffer, size_t len)
        {
            cbor_encoder_init(&encoder, buffer, len, 0);
            
            CborEncoder array;
            
            // Encode all of the reachable AST nodes and types
            cbor_encoder_create_array(&encoder, &array, CborIndefiniteLength);
            TranslateASTVisitor visitor(&Context, &array, &sugared);
            auto translation_unit = Context.getTranslationUnitDecl();
            visitor.TraverseDecl(translation_unit);
            cbor_encoder_close_container(&encoder, &array);
            
            // Track all of the top-level declarations
            cbor_encoder_create_array(&encoder, &array, CborIndefiniteLength);
            for (auto d : translation_unit->decls()) {
                if (d->isCanonicalDecl())
                  cbor_encode_uint(&array, reinterpret_cast<std::uintptr_t>(d));
            }
            cbor_encoder_close_container(&encoder, &array);
            
            // Encode all of the visited file names
            auto filenames = visitor.getFilenames();
            cbor_encoder_create_array(&encoder, &array, filenames.size());
            for (auto &kv : filenames) {
                auto str = kv.first;
                cbor_encode_string(&array, str);
            }
            cbor_encoder_close_container(&encoder, &array);
        };
        
        process(NULL, 0);
        
        auto needed = cbor_encoder_get_extra_bytes_needed(&encoder);
        std::vector<uint8_t> buf(needed);
        
        process(buf.data(), buf.size());
        
        {   
            std::ofstream out(outfile, out.binary | out.trunc);
            out.write(reinterpret_cast<char*>(buf.data()), buf.size());
        }
    }
};

class TranslateAction : public clang::ASTFrontendAction {
    
public:
  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
    clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    return std::unique_ptr<clang::ASTConsumer>(new TranslateConsumer(InFile));
  }
};

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("my-tool options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...");

int main(int argc, const char **argv) {
  CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());
  return Tool.run(newFrontendActionFactory<TranslateAction>().get());
}
