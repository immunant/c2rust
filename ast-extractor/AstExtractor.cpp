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
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

#include <tinycbor/cbor.h>

using namespace llvm;
using namespace clang;
using namespace clang::tooling;


using std::string;
using clang::QualType;
using clang::ASTContext;



enum ASTEntryTag {
    TagFunctionDecl = 0,
    TagParmVarDecl,
    TagVarDecl,
    TagRecordDecl,
    TagFieldDecl,
    
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

    
    TagBinaryOperator = 200,
    TagUnaryOperator,
    TagDeclRefExpr,
    TagImplicitCastExpr,
    TagCallExpr,

    TagInitListExpr,
    TagImplicitValueInitExpr,
    TagArraySubscriptExpr,
    TagCStyleCastExpr,
    
    
    TagIntegerLiteral = 300,
    TagStringLiteral,
    TagCharacterLiteral,
};



/*
class TypeTranslator final
{
public:
    explicit TypeTranslator(ASTContext *Context) : fresh(0), Context(Context) {}
    
    string convertType(QualType QT);
    
private:
    unordered_map<string, string> tag_namespace, type_namespace;
    unordered_set<string> names;
    int fresh;
    ASTContext *Context;
};

string TypeTranslator::convertType(QualType QT)
{
    
    auto qts = QT.split();
    auto basetype = qts.Ty;
    auto quals = qts.Quals;
    
    auto BT = dyn_cast_or_null<BuiltinType>(basetype);
    
    if (BT) {
        switch (BT->getKind()) {
            default:
                return "builtin";
            case clang::BuiltinType::Short:
                return "i16";
            case clang::BuiltinType::Int:
                return "i32";
            case clang::BuiltinType::UShort:
                return "u16";
        }
    } else {
        return "unknown";
    }
} */


class TranslateASTVisitor final
  : public RecursiveASTVisitor<TranslateASTVisitor> {
      
      ASTContext *Context;
      CborEncoder *encoder;
      std::set<std::pair<void*, ASTEntryTag>> exported_tags;
      
      // Returns true when a new entry is added
      bool markForExport(void* ptr, ASTEntryTag tag) {
          return exported_tags.emplace(ptr,tag).second;
      }
      
      void encodeSourcePos(CborEncoder *enc, SourceLocation loc) {
          auto& manager = Context->getSourceManager();
          auto line = manager.getPresumedLineNumber(loc);
          auto col  = manager.getPresumedColumnNumber(loc);
          
          cbor_encode_uint(enc, line);
          cbor_encode_uint(enc, col);
      }
      
      void encode_entry_extra
      (CborEncoder *encoder,
       Stmt *S, ASTEntryTag tag, const std::vector<void *> &childIds,
       std::function<void(CborEncoder*)> extra
       )
      {
          if (!markForExport(S, tag)) return;
          
          CborEncoder local, childEnc;
          cbor_encoder_create_array(encoder, &local, CborIndefiniteLength);
          
          // 1 - Entry ID
          cbor_encode_uint(&local, uintptr_t(S));
          
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
          encodeSourcePos(&local, S->getLocStart());
          
          // 6.. extra entries
          extra(&local);
          
          cbor_encoder_close_container(encoder, &local);
      }
      
      void encode_entry_extra
      (CborEncoder *encoder,
       Decl *D, ASTEntryTag tag, const std::vector<void *> &childIds,
       std::function<void(CborEncoder*)> extra
       )
      {
          if (!markForExport(D, tag)) return;

          CborEncoder local, childEnc;
          cbor_encoder_create_array(encoder, &local, CborIndefiniteLength);
          
          // 1 - Entry ID
          cbor_encode_uint(&local, uintptr_t(D));
          
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
          encodeSourcePos(&local, D->getLocStart());
          
          // 6.. extra entries
          extra(&local);
          
          cbor_encoder_close_container(encoder, &local);
      }
      
      void encode_entry
      (CborEncoder *encoder,
       Decl *entryId, ASTEntryTag tag, const std::vector<void *> &childIds)
      {
          encode_entry_extra(encoder, entryId, tag, childIds, [](CborEncoder*){});
      }
      
      void encode_entry
      (CborEncoder *encoder,
       Stmt *entryId, ASTEntryTag tag, const std::vector<void *> &childIds)
      {
          encode_entry_extra(encoder, entryId, tag, childIds, [](CborEncoder*){});
      }

      
  public:
      explicit TranslateASTVisitor(ASTContext *Context, CborEncoder *encoder)
      : Context(Context), encoder(encoder) {
      }
      
      //
      // Statements
      //
      
      bool VisitCompoundStmt(CompoundStmt *CS) {
          std::vector<void*> childIds;
          for (auto x : CS->children()) {
              childIds.push_back(x);
          }
          encode_entry(encoder, CS, TagCompoundStmt, childIds);
          return true;
      }

      
      bool VisitReturnStmt(ReturnStmt *RS) {
          std::vector<void*> childIds =
          { RS->getRetValue() } ;
          encode_entry(encoder, RS, TagReturnStmt, childIds);
          return true;
      }

      
      
      bool VisitGotoStmt(GotoStmt *GS) {
          std::vector<void*> childIds = { GS->getLabel() };
          encode_entry(encoder, GS, TagGotoStmt, childIds);
          return true;
      }
      
      bool VisitLabelStmt(LabelStmt *LS) {
          
          std::vector<void*> childIds = { LS->getSubStmt() };
          encode_entry_extra(encoder, LS, TagLabelStmt, childIds,
                             [LS](CborEncoder *array){
                                 cbor_encode_text_stringz(array, LS->getName());
                             });
          return true;
      }

      
      bool VisitNullStmt(NullStmt *NS) {
          std::vector<void*> childIds;
          encode_entry(encoder, NS, TagNullStmt, childIds);
          return true;
      }
      
      bool VisitIfStmt(IfStmt *IS) {
          std::vector<void*> childIds = { IS->getCond(), IS->getThen(), IS->getElse() } ;
          encode_entry(encoder, IS, TagIfStmt, childIds);
          return true;
      }
      
      bool VisitForStmt(ForStmt *FS) {
          std::vector<void*> childIds =
          { FS->getInit(), FS->getCond(), FS->getBody() };
          encode_entry(encoder, FS, TagForStmt, childIds);
          return true;
      }
      
      bool VisitWhileStmt(WhileStmt *WS) {
          std::vector<void*> childIds =
          { WS->getCond(), WS->getBody() };
          encode_entry(encoder, WS, TagWhileStmt, childIds);
          return true;
      }
      

      bool VisitDeclStmt(DeclStmt *DS) {
          std::vector<void*> childIds(DS->decl_begin(), DS->decl_end());
          encode_entry(encoder, DS, TagDeclStmt, childIds);
          return true;
      }

      
      bool VisitBreakStmt(BreakStmt *BS) {
          std::vector<void*> childIds;
          encode_entry(encoder, BS, TagBreakStmt, childIds);
          return true;
      }
      
      bool VisitContinueStmt(ContinueStmt *S) {
          std::vector<void*> childIds;
          encode_entry(encoder, S, TagContinueStmt, childIds);
          return true;
      }
      
      bool VisitCaseStmt(CaseStmt *CS) {
          std::vector<void*> childIds =
          { CS->getLHS(), CS->getSubStmt() };
          encode_entry(encoder, CS, TagCaseStmt, childIds);
          return true;
      }
      
      bool VisitSwitchStmt(SwitchStmt *SS) {
          std::vector<void*> childIds =
          { SS->getCond(), SS->getBody() };
          encode_entry(encoder, SS, TagSwitchStmt, childIds);
          return true;
      }
      
      bool VisitDefaultStmt(DefaultStmt *DS) {
          std::vector<void*> childIds = { DS->getSubStmt() };
          encode_entry(encoder, DS, TagDefaultStmt, childIds);
          return true;
      }
      
      //
      // Expressions
      //
      
      bool VisitInitListExpr(InitListExpr *ILE) {
          std::vector<void*> childIds =
          { ILE->getArrayFiller() };
          for (auto x : ILE->inits()) {
              childIds.push_back(x);
          }
          encode_entry(encoder, ILE, TagInitListExpr, childIds);
          return true;
      }
      
      bool VisitImplicitValueInitExpr(ImplicitValueInitExpr *E) {
          std::vector<void*> childIds;
          encode_entry(encoder, E, TagImplicitValueInitExpr, childIds);
          return true;
      }
      
      bool VisitImplicitCastExpr(ImplicitCastExpr *ICE) {
          std::vector<void*> childIds = { ICE->getSubExpr() };
          encode_entry_extra(encoder, ICE, TagImplicitCastExpr, childIds,
                             [ICE](CborEncoder *array){
                                 cbor_encode_uint(array, ICE->getCastKind());
                             });
          return true;
      }
      
      bool VisitCStyleCastExpr(CStyleCastExpr *E) {
          std::vector<void*> childIds = { E->getSubExpr() };
          encode_entry(encoder, E, TagCStyleCastExpr, childIds);
          return true;
      }
      
      bool VisitUnaryOperator(UnaryOperator *UO) {
          std::vector<void*> childIds = { UO->getSubExpr() };
          encode_entry_extra(encoder, UO, TagUnaryOperator, childIds,
                             [UO](CborEncoder *array) {
                                 cbor_encode_uint(array, UO->getOpcode());
                             });
          return true;
      }
      
      bool VisitBinaryOperator(BinaryOperator *BO) {
          std::vector<void*> childIds = { BO->getLHS(), BO->getRHS() };
          encode_entry_extra(encoder, BO, TagBinaryOperator, childIds,
                             [BO](CborEncoder *array) {
                                 cbor_encode_uint(array, BO->getOpcode());
                             });
          return true;
      }
      
      bool VisitDeclRefExpr(DeclRefExpr *DRE) {
          std::vector<void*> childIds = { DRE->getDecl() };
          encode_entry(encoder, DRE, TagDeclRefExpr, childIds);
          return true;
      }
      
      bool VisitCallExpr(CallExpr *CE) {
          std::vector<void*> childIds = { CE->getCallee() };
          for (auto x : CE->arguments()) {
              childIds.push_back(x);
          }
          encode_entry(encoder, CE, TagCallExpr, childIds);
          return true;
      }
      
      bool VisitArraySubscriptExpr(ArraySubscriptExpr *E) {
          std::vector<void*> childIds = { E->getLHS(), E->getRHS() };
          encode_entry(encoder, E, TagArraySubscriptExpr, childIds);
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
          encode_entry_extra(encoder, FD, TagFunctionDecl, childIds,
                             [FD](CborEncoder *array) {
                                 auto name = FD->getNameAsString();
                                 cbor_encode_text_stringz(array, name.c_str());
                             });
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
          encode_entry_extra(encoder, VD, TagVarDecl, childIds,
                             [VD](CborEncoder *array){
                                 auto name = VD->getNameAsString();
                                 cbor_encode_text_stringz(array, name.c_str());
                             });
          return true;
      }
      
      bool VisitRecordDecl(RecordDecl *D) {
          std::vector<void*> childIds;
          for (auto x : D->fields()) {
              childIds.push_back(x);
          }
          encode_entry(encoder, D, TagRecordDecl, childIds);
          return true;
      }
      
      bool VisitFieldDecl(FieldDecl *D) {
          std::vector<void*> childIds;
          encode_entry_extra(encoder, D, TagFieldDecl, childIds,
                             [D](CborEncoder *array) {
                                 auto name = D->getNameAsString();
                                 cbor_encode_text_stringz(array, name.c_str());
                             });
          return true;
      }
      
      bool VisitTypedefDecl(TypedefDecl *D) {
          std::vector<void*> childIds;
          encode_entry_extra(encoder, D, TagTypedefDecl, childIds,
                             [D](CborEncoder *array) {
                                 auto name = D->getNameAsString();
                                 cbor_encode_text_stringz(array, name.c_str());
                             });
          return true;
      }
      
      //
      // Literals
      //
      
      bool VisitIntegerLiteral(IntegerLiteral *IL) {
          std::vector<void*> childIds;
          encode_entry_extra(encoder, IL, TagIntegerLiteral, childIds,
                             [IL](CborEncoder *array){
                                 cbor_encode_uint(array, IL->getValue().getLimitedValue());
                             });
          return true;
      }
      
      bool VisitCharacterLiteral(CharacterLiteral *L) {
          std::vector<void*> childIds;
          encode_entry_extra(encoder, L, TagStringLiteral, childIds,
                             [L](CborEncoder *array){
                                 auto lit = L->getValue();
                                 cbor_encode_uint(array, lit);
                             });
          return true;
      }
      
      bool VisitStringLiteral(clang::StringLiteral *SL) {
          std::vector<void*> childIds;
          encode_entry_extra(encoder, SL, TagStringLiteral, childIds,
                             [SL](CborEncoder *array){
                                 auto lit = SL->getString().str();
                                 cbor_encode_text_string(array, lit.c_str(), lit.size());
                             });
          return true;
      }
  };

class TranslateConsumer : public clang::ASTConsumer {
public:
    explicit TranslateConsumer() { }
    
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
        
        // TODO: Compute output filename
        {
            std::ofstream out("output.cbor", out.binary | out.trunc);
            out.write(reinterpret_cast<char*>(buf.data()), buf.size());
        }
        
        
    }
};

class TranslateAction : public clang::ASTFrontendAction {
    
public:
  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
    clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    return std::unique_ptr<clang::ASTConsumer>(new TranslateConsumer());
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
