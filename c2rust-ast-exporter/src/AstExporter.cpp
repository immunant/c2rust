#include <fstream>
#include <iterator>
#include <iostream>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <cstdlib>

#include "llvm/Support/Debug.h"
#include "llvm/Support/Path.h"
// Declares clang::SyntaxOnlyAction.
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/TypeVisitor.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/RecordLayout.h"
#include "clang/Basic/Builtins.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/Tooling.h"

#include <tinycbor/cbor.h>
#include "ast_tags.hpp"
#include "FloatingLexer.h"
#include "ExportResult.hpp"
#include "AstExporter.hpp"

using namespace llvm;
using namespace clang;
using namespace clang::tooling;

#define DEBUG_TYPE "c2rust-ast-exporter"

#ifndef LLVM_DEBUG
#define LLVM_DEBUG DEBUG
#endif

using std::string;
using clang::QualType;
using clang::ASTContext;

namespace {
    // Encode a string object assuming that it is valid UTF-8 encoded text
    void cbor_encode_string(CborEncoder *encoder, const std::string &str) {
        auto ptr = str.data();
        auto len = str.size();
        cbor_encode_text_string(encoder, ptr, len);
    }

    std::string make_realpath(std::string const &path) {
        if (auto abs_path = realpath(path.c_str(), nullptr)) {
            auto result = std::string(abs_path);
            free(abs_path);
            return result;
        } else {
            std::cerr << "make_realpath: File not found: " << path << std::endl;
            abort();
        }
    }
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

    bool markExported(const clang::Type *ptr) {
        return exports.emplace(ptr).second;
    }

    bool isExported(const clang::Type *ptr) {
        return exports.find(ptr) != exports.end();
    }

    void encodeType(const clang::Type *T, TypeTag tag,
                    std::function<void(CborEncoder*)> extra = [](CborEncoder*){}) {
        if (!markExported(T)) return;

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
            else if (!isExported(s.Ty)) {
                Visit(s.Ty);
            }
        }
    }

    void VisitAttributedType(const AttributedType *T) {
        auto t = T->getModifiedType();
        auto qt = encodeQualType(t);
        auto k = T->getAttrKind();

        encodeType(T, TagAttributedType, [qt,k](CborEncoder *local) {
            cbor_encode_uint(local, qt);

            const char *tag;
            switch (k) {
                default: tag = nullptr; break;
                case AttributedType::attr_noreturn: tag = "noreturn"; break;
                case AttributedType::attr_nonnull: tag = "notnull"; break;
                case AttributedType::attr_nullable: tag = "nullable"; break;
            }
            if (tag) {
                cbor_encode_text_stringz(local, tag);
            } else {
                cbor_encode_null(local);
            }
        });

        VisitQualType(t);
    }

    void VisitParenType(const ParenType *T) {
        auto t = T->getInnerType();
        auto qt = encodeQualType(t);

        encodeType(T, TagParenType, [qt](CborEncoder *local) {
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

    void VisitVectorType(const clang::VectorType *T) {
        auto t = T->getElementType();
        auto qt = encodeQualType(t);

        encodeType(T, TagVectorType, [T,qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
            cbor_encode_uint(local, T->getNumElements());
        });

        VisitQualType(t);
    }

    void VisitComplexType(const ComplexType *T) {
        auto t = T->getElementType();
        auto qt = encodeQualType(t);

        encodeType(T, TagComplexType, [qt](CborEncoder *local) {
            cbor_encode_uint(local, qt);
        });

        VisitQualType(t);
    }

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
            case BuiltinType::Half:       tag = TagHalf;        break;
            case BuiltinType::Float:      tag = TagFloat;       break;
            case BuiltinType::Double:     tag = TagDouble;      break;
            case BuiltinType::LongDouble: tag = TagLongDouble;  break;
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
        LLVM_DEBUG(dbgs() << "Visit ");
        LLVM_DEBUG(T->dump());

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
            cbor_encode_boolean(local, T->getNoReturnAttr());
            cbor_encode_boolean(local, true); // has arguments
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

            cbor_encode_boolean(local, false); // Variable argument function
            cbor_encode_boolean(local, T->getNoReturnAttr());
            cbor_encode_boolean(local, false); // has arguments
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
      std::unordered_set<Decl *> warnedFlexibleArrayDecls;

      // Returns true when a new entry is added to exportedTags
      bool markForExport(void* ptr, ASTEntryTag tag) {
          return exportedTags.emplace(ptr,tag).second;
      }

      bool isExported(void* ptr, ASTEntryTag tag) {
          auto search = exportedTags.find(std::make_pair(ptr,tag));
          return search != std::end(exportedTags);
      }

      // Template required because Decl and Stmt don't share a common base class
      void encode_entry_raw
             (void *ast,
              ASTEntryTag tag,
              SourceLocation loc,
              const QualType ty,
              bool rvalue,
              bool isVaList,
              const std::vector<void *> &childIds,
              std::function<void(CborEncoder*)> extra
             )
      {
          if (!markForExport(ast, tag)) return;

          CborEncoder local, childEnc;
          cbor_encoder_create_array(encoder, &local, CborIndefiniteLength);

          // 0 - Entry ID
          cbor_encode_uint(&local, uintptr_t(ast));

          // 1 - Entry Tag
          cbor_encode_uint(&local, tag);

          // 2 - Entry Children
          cbor_encoder_create_array(&local, &childEnc, childIds.size());
          for (auto x : childIds) {
              if (x == nullptr) {
                  cbor_encode_null(&childEnc);
              } else {
                  cbor_encode_uint(&childEnc, uintptr_t(x));
              }
          }
          cbor_encoder_close_container(&local , &childEnc);

          // 3 - File number
          // 4 - Line number
          // 5 - Column number
          encodeSourcePos(&local, loc, isVaList);

          // 6 - Type ID (only for expressions)
          encode_qualtype(&local, ty);

          // 7 - Is Rvalue (only for expressions)
          cbor_encode_boolean(&local, rvalue);

          // 8.. - Extra entries
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
          auto isVaList = false;
          encode_entry_raw(ast, tag, ast->getLocStart(), ty, ast->isRValue(), isVaList, childIds, extra);
          typeEncoder.VisitQualType(ty);
      }

      void encode_entry
      (Stmt *ast,
       ASTEntryTag tag,
       const std::vector<void *> &childIds,
       std::function<void(CborEncoder*)> extra = [](CborEncoder*){}
       ) {
          QualType s = QualType(static_cast<clang::Type*>(nullptr), 0);
          auto rvalue = false;
          auto isVaList = false;
          encode_entry_raw(ast, tag, ast->getLocStart(), s, rvalue, isVaList, childIds, extra);
      }

      void encode_entry
      (Decl *ast,
       ASTEntryTag tag,
       const std::vector<void *> &childIds,
       const QualType T,
       std::function<void(CborEncoder*)> extra = [](CborEncoder*){}
       ) {
          auto rvalue = false;
          encode_entry_raw(ast, tag, ast->getLocation(), T, rvalue, isVaList(ast, T), childIds, extra);
      }


  public:
      explicit TranslateASTVisitor(ASTContext *Context, CborEncoder *encoder, std::unordered_map<void*, QualType> *sugared)
      : Context(Context), typeEncoder(Context, encoder, sugared, this), encoder(encoder) {
      }

      // Override the default behavior of the RecursiveASTVisitor
      bool shouldVisitImplicitCode() const {
          return true;
      }

      // Return the filenames as a vector. Indices correspond to file IDs.
      std::vector<string> getFilenames() const {
          // Store filenames in order
          std::vector<string> ordered_filenames(filenames.size());

          for (auto const &kv : filenames) {
              ordered_filenames[kv.second] = kv.first;
          }

          return ordered_filenames;
      }

      void encodeSourcePos(CborEncoder *enc, SourceLocation loc, bool isVaList = false) {
          auto& manager = Context->getSourceManager();

          // A check to see if the Source Location is a Macro
          if (manager.isMacroArgExpansion(loc) || manager.isMacroBodyExpansion(loc))
              loc = manager.getFileLoc(loc);

          auto line = manager.getPresumedLineNumber(loc);
          auto col  = manager.getPresumedColumnNumber(loc);
          auto fileid = manager.getFileID(loc);
          auto entry = manager.getFileEntryForID(fileid);

          auto filename = string("?");
          if (entry)
              filename = entry->tryGetRealPathName().str();

          if (filename == "?" && isVaList)
              filename = "vararg";

          auto pair = filenames.insert(std::make_pair(filename, filenames.size()));

          cbor_encode_uint(enc, pair.first->second);
          cbor_encode_uint(enc, line);
          cbor_encode_uint(enc, col);
      }

      //
      // Statements
      //

      /*
      bool VisitAttributedStmt(AttributedStmt *S) {
          std::vector<void*> childIds { S->getSubStmt() };
          encode_entry(S, TagAttributedStmt, childIds,
                       [S](CborEncoder *array){
                           for (auto s: S->getAttrs()) {
                               cbor_encode_text_stringz(array, s->getSpelling());
                           }
          });
          return true;
      }
      */

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

          LLVM_DEBUG(dbgs() << "Visit ");
          LLVM_DEBUG(DS->dumpColor());

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

      // Encode ASM statements using the following encoding:
      // Child IDs: inputs expressions, output expressions
      // Extras:
      //   Boolean true if volatile, false otherwise
      //   Assembly program fragment string
      //   List of input constraints
      //   List of output constraints
      //   List of clobbers
      //
      // The number of input and output expressions in the child id list will
      // match the length of the corresponding constraint arrays.
      bool VisitGCCAsmStmt(GCCAsmStmt *E) {

          std::vector<void*> childIds;
          copy(E->begin_inputs(),  E->end_inputs(),  std::back_inserter(childIds));
          copy(E->begin_outputs(), E->end_outputs(), std::back_inserter(childIds));

          encode_entry(E, TagAsmStmt, childIds, [E](CborEncoder *local) {

              auto writeList = [E,local]
                (unsigned(AsmStmt::*NumFunc)() const,
                 llvm::StringRef(AsmStmt::*StrFunc)(unsigned) const)
              {
                  auto num = (E->*NumFunc)();

                  CborEncoder array;
                  cbor_encoder_create_array(local, &array, num);

                  for (decltype(num) i = 0; i < num; ++i) {
                      cbor_encode_string(&array, (E->*StrFunc)(i).str());
                  }

                  cbor_encoder_close_container(local, &array);
              };

              cbor_encode_boolean(local, E->isVolatile());
              cbor_encode_string(local, E->getAsmString()->getString().str());
              writeList(&AsmStmt::getNumInputs,   &AsmStmt::getInputConstraint);
              writeList(&AsmStmt::getNumOutputs,  &AsmStmt::getOutputConstraint);
              writeList(&AsmStmt::getNumClobbers, &AsmStmt::getClobber);
          });
          return true;
      }

      //
      // Expressions
      //

      bool VisitVAArgExpr(VAArgExpr *E) {
          std::vector<void*> childIds { E->getSubExpr() };
          encode_entry(E, TagVAArgExpr, childIds);
          return true;
      }

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

      bool VisitStmtExpr(StmtExpr *E) {
          std::vector<void*> childIds { E->getSubStmt() };
          encode_entry(E, TagStmtExpr, childIds);
          return true;
      }

      bool VisitOffsetOfExpr(OffsetOfExpr *E) {
          std::vector<void*> childIds;

          encode_entry(E, TagOffsetOfExpr, childIds, [E,this](CborEncoder *extras){
              APSInt value;
              bool is_contant = E->isIntegerConstantExpr(value, *this->Context);

              if (is_contant) {
                  cbor_encode_uint(extras, value.getZExtValue());
              } else {
                  cbor_encode_null(extras);
              }
          });
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

              auto syntax = ILE->getSyntacticForm();
              if (syntax) {
                  cbor_encode_uint(extras, uintptr_t(syntax));
              } else {
                  cbor_encode_null(extras);
              }
          });

          return true;
      }

      /*
       Describes a designated initializer expression
       Children: initializer
       Extras:
       - Array of designators

       Designator format:
       [1, array_index]            { [1]      = 2 }
       [2, field_id]               { .field   = 1 }
       [3, array_start, array_end] { [1 .. 2] = 3 }
       */
      bool VisitDesignatedInitExpr(DesignatedInitExpr *E) {
          std::vector<void*> childIds { E->getInit() };

          encode_entry(E, TagDesignatedInitExpr, childIds, [this,E](CborEncoder *extras) {

              CborEncoder array;
              cbor_encoder_create_array(extras, &array, E->designators().size());
              for (auto &designator: E->designators()) {
                  CborEncoder entry;
                  if (designator.isArrayDesignator()) {
                      cbor_encoder_create_array(&array, &entry, 2);
                      cbor_encode_int(&entry, 1);

                      APSInt Result;
                      bool success = E->getArrayIndex(designator)->isIntegerConstantExpr(Result, *Context);
                      assert(success && "designator array index not integer constant expr");
                      cbor_encode_int(&entry, Result.getZExtValue());

                  } else if (designator.isFieldDesignator()) {
                      cbor_encoder_create_array(&array, &entry, 2);
                      cbor_encode_int(&entry, 2);
                      cbor_encode_uint(&entry, uintptr_t(designator.getField()));
                  } else if (designator.isArrayRangeDesignator()) {
                      cbor_encoder_create_array(&array, &entry, 3);
                      cbor_encode_int(&entry, 3);

                      APSInt Result;
                      bool success = E->getArrayRangeStart(designator)->isIntegerConstantExpr(Result, *Context);
                      assert(success && "designator array range start not integer constant expr");
                      cbor_encode_int(&entry, Result.getZExtValue());

                      success = E->getArrayRangeEnd(designator)->isIntegerConstantExpr(Result, *Context);
                      assert(success && "designator array range end not integer constant expr");
                      cbor_encode_int(&entry, Result.getZExtValue());
                  } else {
                      assert(0 && "unknown designator kind");
                  }
                  cbor_encoder_close_container(&array, &entry);
              }
              cbor_encoder_close_container(extras, &array);

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

                                     if (auto *source_ptr = dyn_cast_or_null<clang::PointerType>(source_type.getTypePtrOrNull())) {
                                         if (auto *target_ptr = dyn_cast_or_null<clang::PointerType>(target_type.getTypePtrOrNull())) {

                                             auto source_pointee = source_ptr->getPointeeType();
                                             auto target_pointee = target_ptr->getPointeeType();

                                             if (target_pointee.isConstQualified() &&
                                                 source_pointee->getUnqualifiedDesugaredType() ==
                                                 target_pointee->getUnqualifiedDesugaredType()) {
                                                 cast_name = "ConstCast";
                                             }
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

          // This avoids an infinite recursive loop that can be caused by the
          // TraverseDecl below.
          if (isExported(DRE, TagDeclRefExpr)) return true;

          LLVM_DEBUG(dbgs() << "Visiting ");
          LLVM_DEBUG(DRE->dumpColor());
          LLVM_DEBUG(DRE->getDecl()->getType()->dump());
          LLVM_DEBUG(DRE->getType()->dump());

          auto decl = DRE->getDecl()->getCanonicalDecl();

          std::vector<void*> childIds { decl };
          encode_entry(DRE, TagDeclRefExpr, childIds);

          // Uses of undeclared declarations might never be traversed if we don't
          // manually traverse them from this point.
          TraverseDecl(decl);

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

      bool VisitShuffleVectorExpr(ShuffleVectorExpr *E) {
          auto children = E->children();
          std::vector<void*> childIds(std::begin(children), std::end(children));
          encode_entry(E, TagShuffleVectorExpr, childIds);
          return true;
      }

      bool VisitConvertVectorExpr(ConvertVectorExpr *E) {
          auto children = E->children();
          std::vector<void*> childIds(std::begin(children), std::end(children));
          encode_entry(E, TagConvertVectorExpr, childIds);
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
          if (!FD->isCanonicalDecl())
              return true;

          if (FD->hasBody() && FD->isVariadic()) {
            //   auto fname = FD->getNameString();
              PrintWarning("variadic functions are not fully supported.", FD);
          }

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
                             [this,FD](CborEncoder *array) {
                                 auto name = FD->getNameAsString();
                                 cbor_encode_string(array, name);

                                 auto is_extern = FD->isExternC();
                                 cbor_encode_boolean(array, is_extern);

                                 auto def = FD->getDefinition();
                                 bool is_inline = def && def->isInlineSpecified();
                                 cbor_encode_boolean(array, is_inline);

                                 auto is_main = FD->isMain();
                                 cbor_encode_boolean(array, is_main);

                                 auto bid = FD->getBuiltinID();
                                 cbor_encode_boolean(array,
                                         bid && !Context->BuiltinInfo.getHeaderName(bid));
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

          auto is_defn = false;
          auto def = VD;
          // Focus on the definition for a particular canonical declaration
          for (auto x : VD->redecls()) {
              if (!x->hasExternalStorage() || x->getInit()) { is_defn = true; def = x; }
          }

          std::vector<void*> childIds { (void*)VD->getAnyInitializer() } ;

          // Use the type from the definition in case the extern was an incomplete type
          auto T = def->getType();

          encode_entry(VD, TagVarDecl, childIds, T,
                             [VD, is_defn](CborEncoder *array){
                                 auto name = VD->getNameAsString();
                                 cbor_encode_string(array, name);

                                 auto is_static = VD->getStorageDuration() == clang::SD_Static;
                                 cbor_encode_boolean(array, is_static);

                                 auto is_extern = VD->isExternC();
                                 cbor_encode_boolean(array, is_extern);

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
          auto byteSize = 0;

          std::vector<void*> childIds;
          if (def) {
              for (auto x : def->fields()) {
                  childIds.push_back(x->getCanonicalDecl());
              }
              // Since the RecordDecl D isn't the complete definition,
              // the actual location should be given. This should handle opaque
              // types.
              auto loc = def->getLocation();
              D->setLocation(loc);

              const ASTRecordLayout &layout = this->Context->getASTRecordLayout(def);
              byteSize = layout.getSize().getQuantity();
          }

          auto tag = D->isStruct() ? TagStructDecl : TagUnionDecl;

          encode_entry(D, tag, childIds, QualType(),
          [D,def,byteSize](CborEncoder *local){

              // 1. Encode name or null
              auto name = D->getNameAsString();
              if (name.empty()) {
                  cbor_encode_null(local);
              } else {
                  cbor_encode_string(local, name);
              }

              // 2. Boolean true when definition present
              cbor_encode_boolean(local, !!def);

              // 3. Attributes stored as an array of attribute names
              CborEncoder attrs;
              size_t attrs_n = D->hasAttrs() ? D->getAttrs().size() : 0;
              cbor_encoder_create_array(local, &attrs, attrs_n);
              for (auto a: D->attrs()) {
                  cbor_encode_text_stringz(&attrs, a->getSpelling());
              }
              cbor_encoder_close_container(local, &attrs);

              // 4. Encode manually specified alignment
              auto align = D->getMaxAlignment();
              if (align == 0) {
                  cbor_encode_null(local);
              } else {
                  cbor_encode_uint(local, align);
              }

              // 5. Encode pragma pack(n)
              if (auto const mfaa = D->getAttr<MaxFieldAlignmentAttr>()) {
                  cbor_encode_uint(local, mfaa->getAlignment() / 8);
              } else {
                  cbor_encode_null(local);
              }

              // 6. Encode the platform specific size of this record
              cbor_encode_uint(local, byteSize);
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

                auto value = D->getInitVal();
                if (value.isSigned()) {
                    cbor_encode_int(local, value.getSExtValue());
                } else {
                    cbor_encode_uint(local, value.getZExtValue());
                }
          });
          return true;
      }

      bool VisitFieldDecl(FieldDecl *D)
      {
          // Skip non-canonical decls
          if (!D->isCanonicalDecl())
              return true;

          // Check to see if the FieldDecl might be a flexible array member,
          // if it is print a warning message.
          if (WarnOnFlexibleArrayDecl(D)) {
              PrintWarning("this may be an unsupported flexible array member with size of 1, "
                           "omit the size if this field is intended to be a flexible array member. "
                           "See section 6.7.2.1 of the C99 Standard for more details.", D);
          }

          std::vector<void*> childIds;
          auto t = D->getType();
          auto record = D->getParent();
          const ASTRecordLayout &layout = this->Context->getASTRecordLayout(record);
          auto index = D->getFieldIndex();
          auto bitOffset = layout.getFieldOffset(index);
          encode_entry(D, TagFieldDecl, childIds, t,
                             [D, this, bitOffset](CborEncoder *array) {
                                 // 1. Encode field name
                                 auto name = D->getNameAsString();
                                 cbor_encode_string(array, name);

                                 // 2. Encode bitfield width if any
                                 if (D->isBitField()) {
                                     cbor_encode_uint(array, D->getBitWidthValue(*this->Context));
                                 } else {
                                     cbor_encode_null(array);
                                 };

                                 // 3. Encode bit offset in its record
                                 cbor_encode_uint(array, bitOffset);
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

                                 cbor_encode_boolean(array, D->isImplicit());
                             });

          typeEncoder.VisitQualType(typeForDecl);

          return true;
      }

      //
      // Literals
      //

      bool VisitIntegerLiteral(IntegerLiteral *IL) {

          auto& sourceManager = Context->getSourceManager();
          auto prefix = sourceManager.getCharacterData(IL->getLocation());
          auto value = IL->getValue().getLimitedValue();

          auto base = (value == 0 || prefix[0] != '0')       ? 10U
                    : (prefix[1] == 'x' || prefix[1] == 'X') ? 16U
                    :                                           8U;

          std::vector<void*> childIds;
          encode_entry(IL, TagIntegerLiteral, childIds,
                             [value,base](CborEncoder *array){
                                 cbor_encode_uint(array, value);
                                 cbor_encode_uint(array, base);
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

          auto& sourceManager = Context->getSourceManager();
          auto prefix = sourceManager.getCharacterData(L->getLocation());
          auto lexeme = matchFloatingLiteral(prefix);

          std::vector<void*> childIds;
          encode_entry(L, TagFloatingLiteral, childIds,
                       [L, &lexeme](CborEncoder *array){
                           auto lit = L->getValueAsApproximateDouble();
                           cbor_encode_double(array, lit);
                           cbor_encode_string(array, lexeme);
                       });
          return true;
      }

      bool WarnOnFlexibleArrayDecl(FieldDecl* D) {
          const ASTRecordLayout &Layout = Context->getASTRecordLayout(D->getParent());
          unsigned FieldCount = Layout.getFieldCount();

          if (auto CA = dyn_cast_or_null<ConstantArrayType>(D->getType().getTypePtr())) {
              // If the array has a size of 1, and struct field count is
              // greater than 1, and if the (struct field count - 1) is equal to the index,
              // it is most likely a flexible array.
              if (CA->getSize() == 1 &&
                  FieldCount > 1 &&
                  FieldCount - 1 == D->getFieldIndex() &&
                  !warnedFlexibleArrayDecls.count(D)) {
                  // Insert the Decl into the set, if it has not been warned about yet.
                  warnedFlexibleArrayDecls.insert(D);
                  return true;
              }
          }
          return false;
      }

      // Inspired by a lambda function within `clang/lib/Sema/SemaType.cpp`
      bool isVaList(Decl *D, QualType T) {
          if (auto *RD = dyn_cast<RecordDecl>(D))
              if (auto *name = RD->getIdentifier())
                  if(name->isStr("__va_list_tag"))
                      return true;

          if (T.isNull())
              return false;

          if (auto *TD = T->getAs<TypedefType>()) {
              auto *builtinVaList = Context->getBuiltinVaListDecl();
              do {
                  if (TD->getDecl() == builtinVaList)
                      return true;
                  if (auto *name = TD->getDecl()->getIdentifier())
                      if (name->isStr("va_list"))
                          return true;
                  TD = TD->desugar()->getAs<TypedefType>();
              } while (TD);
          }

          if (auto *RT = T->getPointeeOrArrayElementType()->getAs<RecordType>())
              if (auto *name = RT->getDecl()->getIdentifier())
                  if (name->isStr("__va_list_tag"))
                      return true;

          return false;
      }

      void PrintWarning(std::string Message, Decl *D) {
          auto &DiagEngine = Context->getDiagnostics();
          const auto ID = DiagEngine.getCustomDiagID(DiagnosticsEngine::Warning, "%0");

          auto DiagBuilder = DiagEngine.Report(D->getLocation(), ID);
          DiagBuilder.AddString(Message);
          DiagBuilder.AddSourceRange(CharSourceRange::getCharRange(D->getSourceRange()));
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

    encodeType(T, TagTypedefType, [D](CborEncoder *local) {
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
    Outputs *outputs;
    const std::string outfile;

public:
    explicit TranslateConsumer(Outputs *outputs, llvm::StringRef InFile)
        : outputs(outputs), outfile(InFile.str()) { }

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

            CborEncoder outer;
            cbor_encoder_create_array(&encoder, &outer, 4);

            CborEncoder array;

            // 1. Encode all of the reachable AST nodes and types
            cbor_encoder_create_array(&outer, &array, CborIndefiniteLength);
            TranslateASTVisitor visitor(&Context, &array, &sugared);
            auto translation_unit = Context.getTranslationUnitDecl();
            visitor.TraverseDecl(translation_unit);
            cbor_encoder_close_container(&outer, &array);

            // 2. Track all of the top-level declarations
            cbor_encoder_create_array(&outer, &array, CborIndefiniteLength);
            for (auto d : translation_unit->decls()) {
                if (d->isCanonicalDecl())
                  cbor_encode_uint(&array, reinterpret_cast<std::uintptr_t>(d));
            }
            cbor_encoder_close_container(&outer, &array);

            // 3. Encode all of the visited file names
            auto filenames = visitor.getFilenames();
            cbor_encoder_create_array(&outer, &array, filenames.size());
            for (auto const &name : filenames) {
                cbor_encode_string(&array, name);
            }
            cbor_encoder_close_container(&outer, &array);

            // 4. Emit comments as array of arrays. Each comment is represented as an array
            // of source position followed by comment string.
            //
            // Getting all comments will require processing the file with -fparse-all-comments !
            auto comments = Context.getRawCommentList().getComments();
            cbor_encoder_create_array(&outer, &array, comments.size());
            for (auto comment : comments) {
                CborEncoder entry;
                cbor_encoder_create_array(&array, &entry, 4);
                visitor.encodeSourcePos(&entry, comment->getLocStart()); // emits 3 values
                cbor_encode_string(&entry, comment->getRawText(Context.getSourceManager()).str());
                cbor_encoder_close_container(&array, &entry);
            }
            cbor_encoder_close_container(&outer, &array);

            cbor_encoder_close_container(&encoder, &outer);
        };

        // A very large C file (SQLite amalgamation) produces a 18MB CBOR file.
        // Allocate a conservatively large buffer. On most operating systems,
        // the kernel just reserves the virtual address space and allocates
        // physical pages lazily on demand.
        std::vector<uint8_t> buf(64 * 1024 * 1024);

        process(buf.data(), buf.size());
        auto needed = cbor_encoder_get_extra_bytes_needed(&encoder);
        assert(needed == size_t(0) && "CBOR output buffer was too small.");
        auto written = cbor_encoder_get_buffer_size(&encoder, buf.data());
        buf.resize(written);
        buf.shrink_to_fit();

        (*outputs)[make_realpath(outfile)] = std::move(buf);
    }
};

class TranslateAction : public clang::ASTFrontendAction {
    Outputs *outputs;

public:
    TranslateAction(Outputs *outputs) : outputs(outputs) {}

  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
    clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    return std::unique_ptr<clang::ASTConsumer>(new TranslateConsumer(outputs, InFile));
  }
};

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("my-tool options");

// Added in C++ 17
template <class _Tp, size_t _Sz> constexpr size_t size(const _Tp (&)[_Sz]) noexcept { return _Sz; }

// We augment the command line arguments to ensure that comments are always
// parsed and string literals are always treated as constant.
static std::vector<const char *>augment_argv(int argc, const char *argv[]) {
    const char * const extras[] = {
        "-extra-arg=-fparse-all-comments", // always parse comments
        "-extra-arg=-Wwrite-strings",      // string literals are constant
        "-extra-arg=-D_FORTIFY_SOURCE=0",  // we don't want to use checked versions of libc.
                                           // without this we get calls to __builtin__memcpy_chk, etc.

        // Also #define C2RUST, so examples can conditionally omit C code that
        // needs special handling in the Rust version (e.g., varargs functions)
        "-extra-arg=-DC2RUST=1",
    };

    // Build a -resource-dir argument based on the path to the linked clang
    // installation. Without this, ClangTool builds the resource directory from
    // the path to the tool (in this case, the binary running the AST Exporter).
    SmallString<128> P("-extra-arg=-resource-dir=" CLANG_BIN_PATH);
    llvm::sys::path::append(P, "..", Twine("lib") + CLANG_LIBDIR_SUFFIX,
                            "clang", CLANG_VERSION_STRING);
    std::string resource_dir = P.str();
    char *resource_dir_cstr = new char [resource_dir.length()+1];
    strncpy(resource_dir_cstr, resource_dir.c_str(), resource_dir.length()+1);

    auto argv_ = std::vector<const char*>();
    argv_.reserve(argc + size(extras) + 2);

    auto pusher = std::back_inserter(argv_);
    std::copy_n(argv, argc, pusher);
    std::copy_n(extras, size(extras), pusher);
    *pusher++ = resource_dir_cstr;
    *pusher++ = nullptr; // The value of argv[argc] is guaranteed to be a null pointer.

    return argv_;
}

class MyFrontendActionFactory : public FrontendActionFactory {
    Outputs *outputs;

public:
    MyFrontendActionFactory(Outputs *outputs) : outputs(outputs) {}

    clang::FrontendAction *create() override {
        return new TranslateAction(outputs);
    }
};


// Marshal the output map into something easy to manipulate in Rust
ExportResult *make_export_result(const Outputs &outputs) {
    auto result = new ExportResult;
    auto n = outputs.size();
    result->resize(n);

    std::size_t i = 0;
    for (auto const& kv : outputs) {
        auto const& name = kv.first;
        auto const& bytes = kv.second;

        auto name_array = new char[name.size() + 1];
        strcpy(name_array, name.c_str());
        result->names[i] = name_array;

        auto byte_array = new uint8_t[bytes.size()];
        std::copy(std::begin(bytes), std::end(bytes), byte_array);
        result->bytes[i] = byte_array;
        result->sizes[i] = bytes.size();
    }

    return result;
}

Outputs process(int argc, const char *argv[], int *result)
{
    auto argv_ = augment_argv(argc, argv);
    int argc_ = argv_.size() - 1; // ignore the extra nullptr
    CommonOptionsParser OptionsParser(argc_, argv_.data(), MyToolCategory);

    ClangTool Tool(OptionsParser.getCompilations(),
                   OptionsParser.getSourcePathList());

    Outputs outputs;
    MyFrontendActionFactory myFrontendActionFactory(&outputs);

    *result = Tool.run(&myFrontendActionFactory);

    return outputs;
}

// AST-Extractor as a library interface.
extern "C" {
    ExportResult *ast_extractor(int argc, const char *argv[]) {
        int result;
        auto outputs = process(argc, argv, &result);
        return make_export_result(outputs);
    }

    void drop_export_result(ExportResult *result) {
        delete result;
    }
}
