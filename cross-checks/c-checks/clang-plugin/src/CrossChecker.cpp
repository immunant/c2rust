#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/ParseAST.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Rewrite/Frontend/Rewriters.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

using namespace clang;

uint32_t djb2_hash(const uint8_t *str);

// By implementing RecursiveASTVisitor, we can specify which AST nodes
// we're interested in by overriding relevant methods.

class MyASTVisitor : public RecursiveASTVisitor<MyASTVisitor> {

public:
  MyASTVisitor(Rewriter &R) : TheRewriter(R) {}

  // Points to the location of start of function body to
  // insert cross-check method
  SourceLocation getFunctionBodyStart(Stmt *body) {
    SourceLocation startLoc;
    startLoc = body->getLocStart();
    return startLoc.getLocWithOffset(1);
  }

  // VisitFunctionDecl is called when Function declaration in
  // AST is encountered
  bool VisitFunctionDecl(FunctionDecl *decl) {
    Stmt *stmt = decl->getBody();
    SourceLocation function_start = getFunctionBodyStart(stmt);
    auto func_name = decl->getNameInfo().getAsString();
    uint32_t hash_ret_func =
        djb2_hash(reinterpret_cast<const uint8_t *>(func_name.c_str()));

    // Rewriter methods accept only string while inserting text,
    // hence converting the hash value to string and then sending
    // it directly to crosschecks inserted in source file

    std::stringstream InsertHash;
    InsertHash << "rb_xcheck(1," << hash_ret_func << ");"
               << "\n";
    TheRewriter.InsertText(function_start, InsertHash.str(), true);

    return true;
  }

private:
  Rewriter &TheRewriter;
};

// Implementation of the ASTConsumer interface for reading an AST produced
// by the Clang parser.
class Crosschecker : public ASTConsumer {
public:
  Crosschecker(Rewriter &R) : Visitor(R) {}

  // Override the method that gets called for each parsed top-level
  // declaration.
  virtual bool HandleTopLevelDecl(DeclGroupRef DR) {
    for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b)
      // Traverse the declaration using our AST visitor.
      Visitor.TraverseDecl(*b);
    return true;
  }

private:
  MyASTVisitor Visitor;
};

// Function to calculate the hashed values for passsed arguments (function name,
// structs,unions)
uint32_t djb2_hash(const uint8_t *str) {
  uint32_t hash = 5381;
  while (uint32_t c = static_cast<uint32_t>(*str++))
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
  return hash;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    llvm::errs() << "Usage: Crosschecker <filename>\n";
    return 1;
  }

  // CompilerInstance will hold the instance of the Clang compiler for us,
  // managing the various objects needed to run the compiler.
  CompilerInstance TheCompInst;
  TheCompInst.createDiagnostics();

  // TODO: When we feed a source with an #include line into our program
  // it throws "#include<stdio.h> header not found". Although it does not
  // affect our plugin, it would be nice to resolve that.
  HeaderSearchOptions &headerSearchOptions = TheCompInst.getHeaderSearchOpts();
  headerSearchOptions.AddPath("/usr/local/include/", clang::frontend::Angled,
                              false, false);
  LangOptions &lo = TheCompInst.getLangOpts();
  lo.C99 = 1;

  // Initialize target info with the default triple.
  auto TO = std::make_shared<TargetOptions>();
  TO->Triple = llvm::sys::getDefaultTargetTriple();
  TargetInfo *TI =
      TargetInfo::CreateTargetInfo(TheCompInst.getDiagnostics(), TO);
  TheCompInst.setTarget(TI);

  TheCompInst.createFileManager();
  FileManager &FileMgr = TheCompInst.getFileManager();
  TheCompInst.createSourceManager(FileMgr);
  SourceManager &SourceMgr = TheCompInst.getSourceManager();
  TheCompInst.createPreprocessor(TU_Module);
  TheCompInst.createASTContext();
  // A Rewriter helps us manage the code rewriting task.
  Rewriter TheRewriter;
  TheRewriter.setSourceMgr(SourceMgr, TheCompInst.getLangOpts());

  // Set the main file handled by the source manager to the input file.
  const FileEntry *FileIn = FileMgr.getFile(argv[1]);
  SourceMgr.setMainFileID(
      SourceMgr.createFileID(FileIn, SourceLocation(), SrcMgr::C_User));
  TheCompInst.getDiagnosticClient().BeginSourceFile(
      TheCompInst.getLangOpts(), &TheCompInst.getPreprocessor());

  // Create an AST consumer instance which is going to get called by
  // ParseAST.
  Crosschecker TheConsumer(TheRewriter);

  // Parse the file to AST, registering our consumer as the AST consumer.
  ParseAST(TheCompInst.getPreprocessor(), &TheConsumer,
           TheCompInst.getASTContext());

  // At this point the rewriter's buffer should be full with the rewritten
  // file contents.
  const RewriteBuffer *RewriteBuf =
      TheRewriter.getRewriteBufferFor(SourceMgr.getMainFileID());
  llvm::outs() << std::string(RewriteBuf->begin(), RewriteBuf->end());

  return 0;
}
