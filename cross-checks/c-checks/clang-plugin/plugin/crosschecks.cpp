#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTMutationListener.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"

using namespace clang;

namespace {

uint32_t djb2_hash(llvm::StringRef str) {
    uint32_t hash = 5381;
    for (auto c : str.bytes())
        hash = ((hash << 5) + hash) + static_cast<uint32_t>(c); /* hash * 33 + c */
    return hash;
}

// Code saved for later
#if 0
                for (auto *attr : fd->attrs()) {
                    if (auto *aa = dyn_cast<AnnotateAttr>(attr)) {
                        auto ann = aa->getAnnotation();
                        if (ann.startswith("cross_check")) {
                            llvm::errs() << "xcheck annotation:" << ann << "\n";
                        }
                    }
                }
                auto xcheck_body = new (ctx)
                    CompoundStmt(ctx,
                                 { rb_xcheck_call },
                                 SourceLocation(),
                                 SourceLocation());

                auto tu_decl = ctx.getTranslationUnitDecl();
                std::string xcheck_name{"xcheck_"};
                xcheck_name += fd->getName();
                auto xcheck_id = &ctx.Idents.get(xcheck_name);
                DeclarationName xcheck_decl_name{xcheck_id};
                FunctionProtoType::ExtProtoInfo xcheck_epi{};
                auto xcheck_type = ctx.getFunctionType(ctx.VoidTy, {}, xcheck_epi);
                auto xcheck_decl = FunctionDecl::Create(ctx, tu_decl,
                                                        SourceLocation(),
                                                        SourceLocation(),
                                                        xcheck_decl_name,
                                                        xcheck_type,
                                                        nullptr,
                                                        SC_Static);
                xcheck_decl->setBody(xcheck_body);
                tu_decl->addDecl(xcheck_decl);
                new_funcs.push_back(xcheck_decl);

#endif

class CrossCheckInserter : public SemaConsumer {
private:
    ASTConsumer *toplevel_consumer = nullptr;

    FunctionDecl *rb_xcheck_decl = nullptr;
    std::vector<FunctionDecl*> new_funcs;

private:
    enum CrossCheckTag {
        UNKNOWN_TAG = 0,
        FUNCTION_ENTRY_TAG = 1,
        FUNCTION_EXIT_TAG = 2,
        FUNCTION_ARG_TAG = 3,
        FUNCTION_RETURN_TAG = 4,
    };

private:
    void SynthRbXcheckDecl(ASTContext &ctx) {
        if (rb_xcheck_decl != nullptr)
            return;

        auto tu_decl = ctx.getTranslationUnitDecl();
        DeclContext *parent_decl = tu_decl;
        if (ctx.getLangOpts().CPlusPlus) {
            // We're compiling C++, so we need to wrap the Decl
            // in an `extern "C"`
            auto extern_c_decl =
                LinkageSpecDecl::Create(ctx, tu_decl,
                                        SourceLocation(),
                                        SourceLocation(),
                                        LinkageSpecDecl::lang_c,
                                        false);
            tu_decl->addDecl(extern_c_decl);
            parent_decl = extern_c_decl;
        }

        auto rb_xcheck_id = &ctx.Idents.get("rb_xcheck");
        DeclarationName rb_xcheck_decl_name{rb_xcheck_id};
        FunctionProtoType::ExtProtoInfo rb_xcheck_epi{};
        auto rb_xcheck_type =
            ctx.getFunctionType(ctx.VoidTy,
                                { ctx.UnsignedCharTy, ctx.UnsignedLongTy },
                                rb_xcheck_epi);
        rb_xcheck_decl = FunctionDecl::Create(ctx, parent_decl,
                                              SourceLocation(),
                                              SourceLocation(),
                                              rb_xcheck_decl_name,
                                              rb_xcheck_type,
                                              nullptr, SC_Extern);
        // Add the parameters to the signature:
        // void rb_xcheck(unsigned char, unsigned long);
        auto rb_xcheck_tag_param =
            ParmVarDecl::Create(ctx, rb_xcheck_decl,
                                SourceLocation(), SourceLocation(),
                                nullptr, ctx.UnsignedCharTy,
                                nullptr, SC_None, nullptr);
        auto rb_xcheck_val_param =
            ParmVarDecl::Create(ctx, rb_xcheck_decl,
                                SourceLocation(), SourceLocation(),
                                nullptr, ctx.UnsignedLongTy,
                                nullptr, SC_None, nullptr);
        rb_xcheck_decl->setParams({ rb_xcheck_tag_param, rb_xcheck_val_param });
        parent_decl->addDecl(rb_xcheck_decl);
    }

public:
    void InitializeSema(Sema &S) override {
        // Grab the top-level consumer from the Sema
        toplevel_consumer = &S.getASTConsumer();
    }

    void ForgetSema() override {
        toplevel_consumer = nullptr;
    }

    bool HandleTopLevelDecl(DeclGroupRef dg) override {
        for (auto *d : dg) {
            if (FunctionDecl *fd = dyn_cast<FunctionDecl>(d)) {
                if (!fd->hasBody())
                    continue;
#if 0
                if (fd->getName() == "rb_xcheck") {
                    rb_xcheck_decl = fd;
                    continue;
                }
#endif

                auto &ctx = fd->getASTContext();
                SynthRbXcheckDecl(ctx);
                auto rb_xcheck_tag =
                    IntegerLiteral::Create(ctx,
                                           llvm::APInt(8, FUNCTION_ENTRY_TAG),
                                           ctx.UnsignedCharTy,
                                           SourceLocation());
                auto rb_xcheck_val =
                    IntegerLiteral::Create(ctx,
                                           llvm::APInt(64, djb2_hash(fd->getName())),
                                           ctx.UnsignedLongTy,
                                           SourceLocation());
                auto rb_xcheck_type = rb_xcheck_decl->getType();
                auto rb_xcheck_ptr_type = ctx.getPointerType(rb_xcheck_type);
                auto rb_xcheck_fn = new (ctx)
                    DeclRefExpr(rb_xcheck_decl, false, rb_xcheck_type,
                                VK_LValue, SourceLocation());
                auto rb_xcheck_ice =
                    ImplicitCastExpr::Create(ctx, rb_xcheck_ptr_type,
                                             CK_FunctionToPointerDecay,
                                             rb_xcheck_fn, nullptr, VK_RValue);
                auto rb_xcheck_call = new (ctx)
                    CallExpr(ctx, rb_xcheck_ice, { rb_xcheck_tag, rb_xcheck_val },
                             ctx.VoidTy, VK_RValue, SourceLocation());

                // Replace the function body
                auto old_body = fd->getBody();
                auto new_body = new (ctx)
                    CompoundStmt(ctx,
                                 { rb_xcheck_call, old_body },
                                 old_body->getLocStart(),
                                 old_body->getLocEnd());
                fd->setBody(new_body);
            }
        }
        return true;
    }

    void HandleTranslationUnit(ASTContext &ctx) override {
        assert(toplevel_consumer != nullptr);
        // Send our new xcheck_XXX functions through the ASTConsumer pipeline
        for (auto func : new_funcs)
            toplevel_consumer->HandleTopLevelDecl(DeclGroupRef(func));
        new_funcs.clear();
        rb_xcheck_decl = nullptr;
    }
};

class CrossCheckInsertionAction : public PluginASTAction {
protected:
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &ci,
                                                   llvm::StringRef) override {
        return llvm::make_unique<CrossCheckInserter>();
    }

    bool ParseArgs(const CompilerInstance &ci,
                   const std::vector<std::string> &args) override {
        return true;
    }

    PluginASTAction::ActionType getActionType() override {
        return AddBeforeMainAction;
    }

    // TODO: PrintHelp???
};

static FrontendPluginRegistry::Add<CrossCheckInsertionAction>
    X("crosschecks", "insert cross-checks");
}
