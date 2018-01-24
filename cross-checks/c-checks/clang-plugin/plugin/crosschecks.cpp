#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTMutationListener.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLTraits.h"

#include <functional>
#include <optional>

#include "config.h"

using namespace clang;
using namespace llvm::opt;

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

using StringRef = std::reference_wrapper<const std::string>;
using StringRefPair = std::pair<StringRef, StringRef>;
using FunctionConfigRef = std::reference_wrapper<FunctionConfig>;

struct StringRefPairCompare {
    bool operator()(const StringRefPair &lhs, const StringRefPair &rhs) const {
        if (lhs.first.get() == rhs.first.get()) {
            return lhs.second.get() < rhs.second.get();
        } else {
            return lhs.first.get()  < rhs.first.get();
        }
    }
};

class CrossCheckInserter : public SemaConsumer {
private:
    Config config;

    // Cache the (file, function) => config mapping
    // for fast lookup
    // FIXME: uses std::map which is O(logN), would be nice
    // to use std::unordered_map, but that one doesn't compile
    // with StringRefPair keys
    std::map<StringRefPair, FunctionConfigRef,
        StringRefPairCompare> function_configs;

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

    std::optional<FunctionConfigRef>
    get_function_config(const std::string &file_name,
                        const std::string &func_name) {
        StringRefPair key(std::cref(file_name), std::cref(func_name));
        auto file_it = function_configs.find(key);
        if (file_it != function_configs.end())
            return std::make_optional(file_it->second);
        return {};
    }

    using XCheckDefaultFn = std::function<Expr*(void)>;

    llvm::TinyPtrVector<Stmt*>
    insert_xcheck(const XCheckType &xcheck,
                  CrossCheckTag tag,
                  ASTContext &ctx,
                  XCheckDefaultFn default_fn) {
        if (xcheck.type == XCheckType::DISABLED)
            return {};

        if (xcheck.type == XCheckType::CUSTOM) {
            // TODO: implement
            llvm_unreachable("Unimplemented");
            return {};
        }

        Expr *rb_xcheck_val = nullptr;
        switch (xcheck.type) {
        case XCheckType::DEFAULT:
            rb_xcheck_val = default_fn();
            break;

        case XCheckType::FIXED: {
            assert(std::holds_alternative<uint64_t>(xcheck.data) &&
                   "Invalid type for XCheckType::data, expected uint64_t");
            auto rb_xcheck_hash = std::get<uint64_t>(xcheck.data);
            rb_xcheck_val =
                IntegerLiteral::Create(ctx,
                                       llvm::APInt(64, rb_xcheck_hash),
                                       ctx.UnsignedLongTy,
                                       SourceLocation());
            break;
        }

        case XCheckType::DJB2: {
            assert(std::holds_alternative<std::string>(xcheck.data) &&
                   "Invalid type for XCheckType::data, expected string");
            auto &xcheck_str = std::get<std::string>(xcheck.data);
            auto rb_xcheck_hash = djb2_hash(xcheck_str);
            rb_xcheck_val =
                IntegerLiteral::Create(ctx,
                                       llvm::APInt(64, rb_xcheck_hash),
                                       ctx.UnsignedLongTy,
                                       SourceLocation());
            break;
        }

        default:
            llvm_unreachable("Invalid XCheckType reached");
        }

        SynthRbXcheckDecl(ctx);
        auto rb_xcheck_tag =
            IntegerLiteral::Create(ctx,
                                   llvm::APInt(8, tag),
                                   ctx.UnsignedCharTy,
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

        llvm::TinyPtrVector<Stmt*> res;
        res.push_back(rb_xcheck_call);
        return res;
    }

public:
    CrossCheckInserter() = delete;
    CrossCheckInserter(Config &&cfg) : config(std::move(cfg)) {
        for (auto &file_config : config) {
            auto &file_name = file_config.first;
            for (auto &item : file_config.second)
                if (auto func = std::get_if<FunctionConfig>(&item)) {
                    StringRefPair key(std::cref(file_name), std::cref(func->name));
                    function_configs.emplace(key, *func);
                }
        }
    }

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
                std::optional<FunctionConfigRef> func_cfg;
                auto ploc = ctx.getSourceManager().getPresumedLoc(fd->getLocStart());
                if (ploc.isValid()) {
                    std::string file_name(ploc.getFilename());
                    std::string func_name = fd->getName().str();
                    func_cfg = get_function_config(file_name, func_name);
                }
                if (func_cfg && func_cfg->get().disable_xchecks)
                    continue;

                SmallVector<Stmt*, 8> new_body_stmts;
                auto entry_xcheck = func_cfg ? func_cfg->get().entry : XCheckType();
                auto entry_xcheck_default_fn = [&ctx, fd] (void) {
                    auto rb_xcheck_hash = djb2_hash(fd->getName());
                    return IntegerLiteral::Create(ctx,
                                                  llvm::APInt(64, rb_xcheck_hash),
                                                  ctx.UnsignedLongTy,
                                                  SourceLocation());
                };
                auto entry_xcheck_stmts = insert_xcheck(entry_xcheck,
                                                        FUNCTION_ENTRY_TAG,
                                                        ctx,
                                                        entry_xcheck_default_fn);
                std::move(entry_xcheck_stmts.begin(),
                          entry_xcheck_stmts.end(),
                          std::back_inserter(new_body_stmts));

                // Replace the function body
                auto old_body = fd->getBody();
                new_body_stmts.push_back(old_body);
                auto new_body = new (ctx) CompoundStmt(ctx, new_body_stmts,
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

enum ID {
  OPT_INVALID = 0, // This is not an option ID.
#define OPTION(PREFIX, NAME, ID, KIND, GROUP, ALIAS, ALIASARGS, FLAGS, PARAM,  \
               HELPTEXT, METAVAR, VALUES)                                      \
  OPT_##ID,
#include "Options.inc"
#undef OPTION
};

#define PREFIX(NAME, VALUE) const char *const NAME[] = VALUE;
#include "Options.inc"
#undef PREFIX

static const OptTable::Info InfoTable[] = {
#define OPTION(PREFIX, NAME, ID, KIND, GROUP, ALIAS, ALIASARGS, FLAGS, PARAM,  \
               HELPTEXT, METAVAR, VALUES)                                      \
{                                                                              \
      PREFIX,      NAME,      HELPTEXT,                                        \
      METAVAR,     OPT_##ID,  Option::KIND##Class,                             \
      PARAM,       FLAGS,     OPT_##GROUP,                                     \
      OPT_##ALIAS, ALIASARGS, VALUES},
#include "Options.inc"
#undef OPTION
};

class CrossCheckOptTable : public OptTable {
public:
    CrossCheckOptTable() : OptTable(InfoTable) {}
};

class CrossCheckInsertionAction : public PluginASTAction {
private:
    Config config;

protected:
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &ci,
                                                   llvm::StringRef) override {
        return llvm::make_unique<CrossCheckInserter>(std::move(config));
    }

    bool ParseArgs(const CompilerInstance &ci,
                   const std::vector<std::string> &args) override {
        auto &diags = ci.getDiagnostics();

        SmallVector<const char*, 8> arg_ptrs;
        for (auto &arg : args)
            arg_ptrs.push_back(arg.c_str());

        CrossCheckOptTable opt_table;
        unsigned missing_arg_index, missing_arg_count;
        auto parsed_args =
            opt_table.ParseArgs(arg_ptrs, missing_arg_index, missing_arg_count);
        if (missing_arg_count > 0) {
            unsigned diag_id =
                diags.getCustomDiagID(DiagnosticsEngine::Error,
                                      "missing %0 option(s), starting at index %1");
            diags.Report(diag_id) << missing_arg_count << missing_arg_index;
            return false;
        }

        auto config_files = parsed_args.getAllArgValues(OPT_config_files);
        for (auto &config_file : config_files) {
            auto config_data = llvm::MemoryBuffer::getFile(config_file);
            if (!config_data) {
                unsigned diag_id =
                    diags.getCustomDiagID(DiagnosticsEngine::Error,
                                          "error reading configuration file '%0': %1");
                diags.Report(diag_id) << config_file << config_data.getError().message();
                return false;
            }

            Config new_config;
            llvm::yaml::Input yin((*config_data)->getBuffer());
            yin >> new_config;
            if (auto yerr = yin.error()) {
                unsigned diag_id =
                    diags.getCustomDiagID(DiagnosticsEngine::Error,
                                          "error parsing YAML configuration: %0");
                diags.Report(diag_id) << yerr.message();
                return false;
            }
            for (auto &src_file : new_config) {
                // For every source file in new_config, append its items
                // to the corresponding file items vector in the global
                // directory
                auto &file_items = config[src_file.first];
                auto &new_file_items = src_file.second;
                file_items.insert(file_items.end(),
                                  new_file_items.begin(),
                                  new_file_items.end());
            }
        }
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
