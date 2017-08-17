#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <fstream>

// Declares clang::SyntaxOnlyAction.
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
// Declares llvm::cl::extrahelp.
#include "llvm/Support/CommandLine.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

#include <tinycbor/cbor.h>
#include "ast_tags.hpp"

using namespace llvm;
using namespace clang;
using namespace clang::tooling;


using std::string;
using clang::QualType;
using clang::ASTContext;

class TranslateASTVisitor;

class TypeEncoder final : public TypeVisitor<TypeEncoder>
{
    ASTContext *Context;
    CborEncoder *encoder;
    TranslateASTVisitor *astEncoder;
    
    std::unordered_set<const Type*> exports;
    
    bool isUnexported(const Type *ptr) {
        return exports.emplace(ptr).second;
    }
    
    void encodeType(const Type *T, TypeTag tag,
                    std::function<void(CborEncoder*)> extra = [](CborEncoder*){}) {
        if (!isUnexported(T)) return;
        
        CborEncoder local;
        cbor_encoder_create_array(encoder, &local, CborIndefiniteLength);
        
        // 1 - Entity ID
        cbor_encode_uint(&local, uintptr_t(T));
        
        // 2 - Type tag
        cbor_encode_uint(&local, tag);
        
        // 3- extras
        extra(&local);
        
        cbor_encoder_close_container(encoder, &local);
    }
    
public:
    explicit TypeEncoder(ASTContext *Context, CborEncoder *encoder, TranslateASTVisitor *ast)
      : Context(Context), encoder(encoder), astEncoder(ast) {}
    
    void VisitEnumType(const EnumType *T) {
        encodeType(T, TagEnumType, [T](CborEncoder *local) {
            cbor_encode_uint(local, uintptr_t(T->getDecl()));
        });
    }
    
    //Constant, Variable, DependentSized, Incomplete
    void VisitArrayType(const ArrayType *T) {
        auto s = T->getPointeeType().split();
        
        encodeType(T, TagArrayType, [T,&s](CborEncoder *local) {
            cbor_encode_uint(local, uintptr_t(s.Ty));
        });
        
        if(s.Ty) {
            Visit(s.Ty);
        }
    }
    
    // definition below due to recursive call into AST translator
    void VisitRecordType(const RecordType *T);

    
    void VisitBuiltinType(const BuiltinType *T) {
        TypeTag tag;
        using clang::BuiltinType;
        switch (T->getKind()) {
            default:                      tag = TagTypeUnknown; break;
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
        }
        
        encodeType(T, tag);
    }
    
    /* Function types are encoded with an extra list of types. The return type
     is always the first element of the list followed by
     */
    void VisitFunctionProtoType(const FunctionProtoType *T) {
        encodeType(T, TagFunctionType, [T](CborEncoder *local) {
            CborEncoder arrayEncoder;
            
            size_t elts = T->getNumParams()+1;
            cbor_encoder_create_array(local, &arrayEncoder, elts);
            
            cbor_encode_uint(&arrayEncoder, uintptr_t(T->getReturnType().getTypePtr()));
            for (auto x : T->param_types()) {
                auto s = x.split();
                cbor_encode_uint(&arrayEncoder, uintptr_t(s.Ty));
            }
            
            cbor_encoder_close_container(local, &arrayEncoder);
            
            
        });
        
        Visit(T->getReturnType().split().Ty);
        for (auto x : T->param_types()) {
            Visit(x.split().Ty);
        }
    }
    
    void VisitPointerType(const PointerType *T) {
        auto pointee = T->getPointeeType();
        auto s = pointee.split();
        
        encodeType(T, TagPointer, [T, &s](CborEncoder *local) {
            cbor_encode_uint(local, uintptr_t(s.Ty));
        });
        
        Visit(s.Ty);
        
    }
    
    void VisitTypedefType(const TypedefType *T) {
        encodeType(T, TagTypedefType, [T](CborEncoder *local) {
        });
    }
    
    void VisitTypeOfType(const TypeOfType *T) {
        encodeType(T, TagTypeOfType, [T](CborEncoder *local) {
        });
    }
    
    void VisitElaboratedType(const ElaboratedType *T) {
        auto T1 = T->desugar().getTypePtr();
        encodeType(T, TagElaboratedType, [T,T1](CborEncoder *local) {
            cbor_encode_uint(local, uintptr_t(T1));
        });

        Visit(T1);
    }
};

class TranslateASTVisitor final
  : public RecursiveASTVisitor<TranslateASTVisitor> {
      
      ASTContext *Context;
      TypeEncoder typeEncoder;
      CborEncoder *encoder;
      std::set<std::pair<void*, ASTEntryTag>> exportedTags;
      
      // Returns true when a new entry is added to exportedTags
      bool markForExport(void* ptr, ASTEntryTag tag) {
          return exportedTags.emplace(ptr,tag).second;
      }
      
      void encodeSourcePos(CborEncoder *enc, SourceLocation loc) {
          auto& manager = Context->getSourceManager();
          auto line = manager.getPresumedLineNumber(loc);
          auto col  = manager.getPresumedColumnNumber(loc);
          
          cbor_encode_uint(enc, line);
          cbor_encode_uint(enc, col);
      }
      
      // Template required because Decl and Stmt don't share a common base class
      void encode_entry_raw
             (void *ast,
              ASTEntryTag tag,
              SourceLocation loc,
              const Type *ty,
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
          
          // 4 - line number
          // 5 - column number
          encodeSourcePos(&local, loc);
          
          // 6 - type (only for expressions)
          if (nullptr == ty) {
              cbor_encode_null(&local);
          } else {
              cbor_encode_uint(&local, uintptr_t(ty));
          }
          
          // 7.. extra entries
          extra(&local);
          
          cbor_encoder_close_container(encoder, &local);
      }
      
      void encode_entry
      (Expr *ast,
       ASTEntryTag tag,
       const std::vector<void *> &childIds,
       std::function<void(CborEncoder*)> extra = [](CborEncoder*){}
       ) {
          auto ty = ast->getType().split().Ty;
          encode_entry_raw(ast, tag, ast->getLocStart(), ty, childIds, extra);
          typeEncoder.Visit(ty);
      }
      
      void encode_entry
      (Stmt *ast,
       ASTEntryTag tag,
       const std::vector<void *> &childIds,
       std::function<void(CborEncoder*)> extra = [](CborEncoder*){}
       ) {
          encode_entry_raw(ast, tag, ast->getLocStart(), nullptr, childIds, extra);
      }
      
      void encode_entry
      (Decl *ast,
       ASTEntryTag tag,
       const std::vector<void *> &childIds,
       const Type *T,
       std::function<void(CborEncoder*)> extra = [](CborEncoder*){}
       ) {
          encode_entry_raw(ast, tag, ast->getLocStart(), T, childIds, extra);
      }
      
      
  public:
      explicit TranslateASTVisitor(ASTContext *Context, CborEncoder *encoder)
      : Context(Context), typeEncoder(Context, encoder, this), encoder(encoder) {
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
          std::vector<void*> childIds = { GS->getLabel() };
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
          { FS->getInit(), FS->getCond(), FS->getBody() };
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
          std::vector<void*> childIds(DS->decl_begin(), DS->decl_end());
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
          std::vector<void*> childIds =
          { CS->getLHS(), CS->getSubStmt() };
          encode_entry(CS, TagCaseStmt, childIds);
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
      
      bool VisitMemberExpr(MemberExpr *E) {
          std::vector<void*> childIds =
            { E->getBase(), E->getMemberDecl() };
          encode_entry(E, TagMemberExpr, childIds);
          return true;
      }
      
      bool VisitInitListExpr(InitListExpr *ILE) {
          std::vector<void*> childIds =
          { ILE->getArrayFiller() };
          for (auto x : ILE->inits()) {
              childIds.push_back(x);
          }
          encode_entry(ILE, TagInitListExpr, childIds);
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
                                 cbor_encode_uint(array, ICE->getCastKind());
                             });
          return true;
      }
      
      bool VisitCStyleCastExpr(CStyleCastExpr *E) {
          std::vector<void*> childIds = { E->getSubExpr() };
          encode_entry(E, TagCStyleCastExpr, childIds);
          return true;
      }
      
      bool VisitUnaryOperator(UnaryOperator *UO) {
          std::vector<void*> childIds = { UO->getSubExpr() };
          encode_entry(UO, TagUnaryOperator, childIds,
                             [UO](CborEncoder *array) {
                                 cbor_encode_uint(array, UO->getOpcode());
                             });
          return true;
      }
      
      bool VisitBinaryOperator(BinaryOperator *BO) {
          std::vector<void*> childIds = { BO->getLHS(), BO->getRHS() };
          encode_entry(BO, TagBinaryOperator, childIds,
                             [BO](CborEncoder *array) {
                                 cbor_encode_uint(array, BO->getOpcode());
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
          std::vector<void*> childIds = { DRE->getDecl() };
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
      
      bool VisitFunctionDecl(FunctionDecl *FD)
      {
          std::vector<void*> childIds;
          for (auto x : FD->parameters()) {
              childIds.push_back(x);
          }
          childIds.push_back(FD->getBody());
          encode_entry(FD, TagFunctionDecl, childIds, FD->getType().getTypePtr(),
                             [FD](CborEncoder *array) {
                                 auto name = FD->getNameAsString();
                                 cbor_encode_text_stringz(array, name.c_str());
                             });
          typeEncoder.Visit(FD->getType().getTypePtr());

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
          std::vector<void*> childIds =
          { VD->getInit() } ;
          auto T = VD->getType().getTypePtr();
          
          encode_entry(VD, TagVarDecl, childIds, T,
                             [VD](CborEncoder *array){
                                 auto name = VD->getNameAsString();
                                 cbor_encode_text_stringz(array, name.c_str());
                             });
          
          typeEncoder.Visit(T);

          return true;
      }
      
      
      bool VisitRecordDecl(RecordDecl *D) {
          std::vector<void*> childIds;
          for (auto x : D->fields()) {
              childIds.push_back(x);
          }
          encode_entry(D, TagRecordDecl, childIds, nullptr,
          [D](CborEncoder *local){
              auto name = D->getNameAsString();
              cbor_encode_text_string(local, name.c_str(), name.size());
          });
          return true;
      }
      
      bool VisitEnumDecl(EnumDecl *D) {
          std::vector<void*> childIds;
          for (auto x : D->enumerators()) {
              childIds.push_back(x);
          }
          
          encode_entry(D, TagEnumDecl, childIds, nullptr,
          [D](CborEncoder *local){
              auto name = D->getNameAsString();
              cbor_encode_text_string(local, name.c_str(), name.size());
          });
          return true;
      }
      
      bool VisitEnumConstantDecl(EnumConstantDecl *D) {
          std::vector<void*> childIds = { D->getInitExpr() };
          
          encode_entry(D, TagEnumConstantDecl, childIds, nullptr,
            [D](CborEncoder *local){
              auto name = D->getNameAsString();
              cbor_encode_text_string(local, name.c_str(), name.size());
          });
          return true;
      }
      
      bool VisitFieldDecl(FieldDecl *D) {
          std::vector<void*> childIds;
          encode_entry(D, TagFieldDecl, childIds, nullptr,
                             [D](CborEncoder *array) {
                                 auto name = D->getNameAsString();
                                 cbor_encode_text_stringz(array, name.c_str());
                             });
          return true;
      }
      
      bool VisitTypedefDecl(TypedefDecl *D) {
          std::vector<void*> childIds;
          encode_entry(D, TagTypedefDecl, childIds, D->getTypeForDecl(),
                             [D](CborEncoder *array) {
                                 auto name = D->getNameAsString();
                                 cbor_encode_text_stringz(array, name.c_str());
                             });
          typeEncoder.Visit(D->getTypeForDecl());
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
                                 auto lit = SL->getString().str();
                                 cbor_encode_text_string(array, lit.c_str(), lit.size());
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
    
    encodeType(T, TagRecordType, [T](CborEncoder *local) {
        cbor_encode_uint(local, uintptr_t(T->getDecl()));
    });
    
    // record type might be anonymous and have no top-level declaration
    astEncoder->TraverseDecl(T->getDecl());
}

class TranslateConsumer : public clang::ASTConsumer {
    const std::string outfile;
public:
    explicit TranslateConsumer(llvm::StringRef InFile) 
        : outfile(InFile.str().append(".cbor")) { }
    
    virtual void HandleTranslationUnit(clang::ASTContext &Context) {
        
        CborEncoder encoder;
        
        auto process = [&encoder, &Context](uint8_t *buffer, size_t len)
        {
            cbor_encoder_init(&encoder, buffer, len, 0);
            
            CborEncoder array;
            cbor_encoder_create_array(&encoder, &array, CborIndefiniteLength);
            
            TranslateASTVisitor(&Context, &array).TraverseDecl(Context.getTranslationUnitDecl());
            
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
