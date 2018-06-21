#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTMutationListener.h"
#include "clang/Basic/Version.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLTraits.h"

#include <functional>
#include <optional>
#include <string_view>

#include "config.h"
#include "crosschecks.h"

namespace crosschecks {

using namespace llvm::opt;
using namespace std::literals;

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

// Ugly hack: a few structures in system headers contain unions
// or anonymous structures, which we can't handle (yet),
// so we maintain a hard-coded blacklist
std::set<std::pair<std::string_view, std::string_view>>
CrossCheckInserter::struct_xcheck_blacklist = {
    { "/usr/include/bits/types/__mbstate_t.h"sv,    "__mbstate_t"sv      },
    { "/usr/include/bits/thread-shared-types.h"sv,  "__pthread_cond_s"sv },
    { "/usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h"sv, "__mbstate_t"sv},
    { "/usr/include/x86_64-linux-gnu/bits/thread-shared-types.h"sv, "__pthread_cond_s"sv},

};

FunctionDecl *CrossCheckInserter::get_function_decl(llvm::StringRef name,
                                                    QualType result_ty,
                                                    ArrayRef<QualType> arg_tys,
                                                    StorageClass sc,
                                                    ASTContext &ctx) {
    // Retrieve the FunctionDecl from the cache by name,
    // if it exists.
    // FIXME: this assumes that the function always gets called
    // with the same argument types, so the types for the ParmVarDecls
    // always match the types of the actual arguments.
    auto it = decl_cache.find(name);
    if (it != decl_cache.end()) {
        return it->second;
    }

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

    // Build the type of the function
    FunctionProtoType::ExtProtoInfo fn_epi{};
    auto fn_type = ctx.getFunctionType(result_ty, arg_tys, fn_epi);
    auto fn_id = &ctx.Idents.get(name);
    DeclarationName fn_decl_name{fn_id};
    auto fn_decl = FunctionDecl::Create(ctx, parent_decl,
                                        SourceLocation(),
                                        SourceLocation(),
                                        fn_decl_name,
                                        fn_type,
                                        nullptr, sc);

    // Build the ParmVarDecl's of the arguments
    SmallVector<ParmVarDecl*, 16> arg_decls;
    for (auto &arg_ty : arg_tys) {
        auto arg_decl =
            ParmVarDecl::Create(ctx, fn_decl,
                                SourceLocation(), SourceLocation(),
                                nullptr, arg_ty,
                                nullptr, SC_None, nullptr);
        arg_decls.push_back(arg_decl);
    }
    fn_decl->setParams(arg_decls);

    parent_decl->addDecl(fn_decl);
    decl_cache.try_emplace(name, fn_decl);
    return fn_decl;
}

CallExpr *CrossCheckInserter::build_call(llvm::StringRef fn_name, QualType result_ty,
                                         ArrayRef<Expr*> args, ASTContext &ctx) {
    SmallVector<QualType, 16> arg_tys;
    for (auto &arg : args) {
        arg_tys.push_back(arg->getType());
    }
    auto fn_decl = get_function_decl(fn_name, result_ty, arg_tys,
                                     SC_Extern, ctx);
    auto fn_type = fn_decl->getType();
    auto fn_ptr_type = ctx.getPointerType(fn_type);
    auto fn_ref = new (ctx)
        DeclRefExpr(fn_decl, false, fn_type,
                    VK_LValue, SourceLocation());
    auto fn_ice =
        ImplicitCastExpr::Create(ctx, fn_ptr_type,
                                 CK_FunctionToPointerDecay,
                                 fn_ref, nullptr, VK_RValue);
    return new (ctx) CallExpr(ctx, fn_ice, args, result_ty,
                              VK_RValue, SourceLocation());
}

static inline void skip_sv_whitespace(std::string_view *sv) {
    assert(sv != nullptr && "Bad pointer");
    auto spaces = sv->find_first_not_of(" \t\n\v\f\r", 0);
    if (spaces == std::string_view::npos)
        spaces = sv->length();
    sv->remove_prefix(spaces);
}

CrossCheckInserter::CustomFnSig
CrossCheckInserter::parse_custom_xcheck(std::string_view sv,
                                        ASTContext &ctx) {
    auto &diags = ctx.getDiagnostics();
    skip_sv_whitespace(&sv);
    if (sv.empty() || (sv.front() != '_' && !isalpha(sv.front()))) {
        std::string found;
        if (sv.empty()) {
            found += "NUL";
        } else {
            found += sv.front();
        }
        report_clang_error(diags, "expected identifier for "
                                  "custom cross-check name, found '%0'",
                                  found);
        return {};
    }
    auto name_len = sv.find_first_not_of("_abcdefghijklmnopqrstuvwxyz"
                                         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                         "0123456789");
    if (name_len == std::string_view::npos)
        name_len = sv.length();

    CustomFnSig res{sv.substr(0, name_len), {}};
    sv.remove_prefix(name_len);
    skip_sv_whitespace(&sv);
    if (sv.empty())
        return res;

    // Read and ignore the opening parenthesis
    if (sv.front() != '(') {
        report_clang_error(diags, "expected '(' character, found '%0'", sv.front());
        return res;
    }
    sv.remove_prefix(1);
    skip_sv_whitespace(&sv);

    bool first = true;
    for (;;) {
        skip_sv_whitespace(&sv);
        if (sv.empty())
            break;
        if (sv.front() == ')') {
            sv.remove_prefix(1);
            break;
        }

        if (!first) {
            if (sv.front() != ',') {
                report_clang_error(diags, "expected ',' character, found '%0'", sv.front());
                return res;
            }
            sv.remove_prefix(1);
            skip_sv_whitespace(&sv);
        } else {
            first = false;
        }
        if (sv.empty())
            break;

        auto mods_len = sv.find_first_not_of("*&", 0);
        if (mods_len == std::string_view::npos) {
            report_clang_error(diags, "unexpected EOF in custom cross-check arguments");
            return res;
        }
        auto ident_len = sv.find_first_not_of("_abcdefghijklmnopqrstuvwxyz"
                                              "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                              "0123456789", mods_len);
        if (ident_len == std::string_view::npos)
            ident_len = sv.length();
        std::get<1>(res).emplace_back(sv.substr(0, ident_len));
        sv.remove_prefix(ident_len);
    }
    skip_sv_whitespace(&sv);
    if (!sv.empty()) {
        report_clang_error(diags, "unexpected characters in "
                                  "custom cross-check: '%0'",
                           std::string{sv});
    }
    return res;
}

template<typename DefaultFn, typename CustomArgsFn>
CrossCheckInserter::TinyStmtVec
CrossCheckInserter::build_xcheck(const XCheck &xcheck, XCheck::Tag tag,
                                 ASTContext &ctx, DefaultFn default_fn,
                                 CustomArgsFn custom_args_fn) {
    if (xcheck.type == XCheck::DISABLED)
        return {};

    Expr *rb_xcheck_val = nullptr;
    switch (xcheck.type) {
    case XCheck::DEFAULT:
        rb_xcheck_val = default_fn();
        break;

    case XCheck::FIXED: {
        assert(std::holds_alternative<uint64_t>(xcheck.data) &&
               "Invalid type for XCheck::data, expected uint64_t");
        auto rb_xcheck_hash = std::get<uint64_t>(xcheck.data);
        rb_xcheck_val =
            IntegerLiteral::Create(ctx,
                                   llvm::APInt(64, rb_xcheck_hash),
                                   ctx.UnsignedLongTy,
                                   SourceLocation());
        break;
    }

    case XCheck::DJB2: {
        assert(std::holds_alternative<std::string>(xcheck.data) &&
               "Invalid type for XCheck::data, expected string");
        auto &xcheck_str = std::get<std::string>(xcheck.data);
        auto rb_xcheck_hash = djb2_hash(xcheck_str);
        rb_xcheck_val =
            IntegerLiteral::Create(ctx,
                                   llvm::APInt(64, rb_xcheck_hash),
                                   ctx.UnsignedLongTy,
                                   SourceLocation());
        break;
    }

    case XCheck::CUSTOM: {
        assert(std::holds_alternative<std::string>(xcheck.data) &&
               "Invalid type for XCheck::data, expected string");
        auto &xcheck_fn = std::get<std::string>(xcheck.data);
        auto xcheck_fn_sig = parse_custom_xcheck(xcheck_fn, ctx);
        auto xcheck_fn_name = std::get<0>(xcheck_fn_sig);
        ExprVec xcheck_fn_args = custom_args_fn(std::get<1>(xcheck_fn_sig));
        rb_xcheck_val = build_call(llvm_string_ref_from_sv(xcheck_fn_name),
                                   ctx.UnsignedLongTy,
                                   xcheck_fn_args, ctx);
        break;
    }

    default:
        llvm_unreachable("Invalid XCheck reached");
    }
    if (rb_xcheck_val == nullptr)
        return {};

    TinyStmtVec res;
    auto rb_xcheck_tag =
        IntegerLiteral::Create(ctx,
                               llvm::APInt(8, tag),
                               ctx.UnsignedCharTy,
                               SourceLocation());
    auto rb_xcheck_call = build_call("rb_xcheck", ctx.VoidTy,
                                     { rb_xcheck_tag, rb_xcheck_val },
                                     ctx);
    res.push_back(rb_xcheck_call);
    return res;
}

CrossCheckInserter::TinyStmtVec
CrossCheckInserter::build_parameter_xcheck(ParmVarDecl *param,
                                           const DefaultsConfigOptRef file_defaults,
                                           llvm::StringRef func_name,
                                           const FunctionConfig &func_cfg,
                                           const DeclMap &param_decls,
                                           ASTContext &ctx) {
    XCheck param_xcheck{XCheck::DISABLED};
    if (file_defaults && file_defaults->get().all_args)
        param_xcheck = *file_defaults->get().all_args;
    if (func_cfg.all_args) {
        param_xcheck = *func_cfg.all_args;
    }
    parse_xcheck_attrs(param, [&param_xcheck] (llvm::yaml::Input &yin) {
        yin >> param_xcheck;
    });
    auto it = func_cfg.args.find(param->getName());
    if (it != func_cfg.args.end()) {
        param_xcheck = it->second;
    }
    auto param_xcheck_default_fn = [this, &ctx, func_name, param] (void) {
        // By default, we just call __c2rust_hash_T(x)
        // where T is the type of the parameter
        // FIXME: include shasher/ahasher
        std::string param_ty_name = func_name;
        param_ty_name += "$arg$";
        param_ty_name += param->getName();
        auto hash_fn = get_type_hash_function(param->getOriginalType(),
                                              param_ty_name, ctx, true);

        // Forward the value of the parameter to the hash function
        auto param_ref_lv =
            new (ctx) DeclRefExpr(param, false, param->getType(),
                                  VK_LValue, SourceLocation());
        auto param_ref_rv = hash_fn.forward_argument(param_ref_lv, ctx);
        auto hash_depth = build_max_hash_depth(ctx);
        // TODO: pass PODs by value, non-PODs by pointer???
        return build_call(hash_fn.name.full_name(), ctx.UnsignedLongTy,
                          { param_ref_rv, hash_depth }, ctx);
    };
    auto param_xcheck_custom_args_fn = [&ctx, &param_decls] (CustomArgVec args) {
        auto arg_build_fn = [&ctx] (DeclaratorDecl *decl) {
            return new (ctx) DeclRefExpr(decl, false, decl->getType(),
                                         VK_LValue, SourceLocation());
        };
        return generic_custom_args(ctx, param_decls, args, arg_build_fn);
    };
    return build_xcheck(param_xcheck, XCheck::Tag::FUNCTION_ARG, ctx,
                        param_xcheck_default_fn,
                        param_xcheck_custom_args_fn);
}

bool CrossCheckInserter::HandleTopLevelDecl(DeclGroupRef dg) {
    for (auto *d : dg) {
        auto &ctx = d->getASTContext();
        auto &diags = ctx.getDiagnostics();
        if (FunctionDecl *fd = dyn_cast<FunctionDecl>(d)) {
            if (!fd->hasBody())
                continue;
            auto fd_ident = fd->getIdentifier();
            if (fd_ident == nullptr) {
                // TODO: emit a warning
                continue;
            }
            auto func_name = fd_ident->getName();
            if (func_name.startswith("__c2rust")) {
                // Ignore our own functions
                continue;
            }
            if (fd->isVariadic()) {
                report_clang_warning(diags, "cross-checks not supported for variadic functions, "
                                            "disabling for '%0'", func_name);
                continue;
            }

            FunctionConfig func_cfg{llvm_string_ref_to_sv(func_name)};
            // Read the inline function configurations
            parse_xcheck_attrs(fd, [&func_cfg, func_name] (llvm::yaml::Input &yin) {
                FunctionConfig fcfg{llvm_string_ref_to_sv(func_name)};
                yin >> fcfg;
                func_cfg.update(fcfg);
            });
            // Read the external function configuration
            DefaultsConfigOptRef file_defaults;
            auto ploc = ctx.getSourceManager().getPresumedLoc(fd->getLocStart());
            if (ploc.isValid()) {
                std::string file_name(ploc.getFilename());
                auto it = defaults_configs.find(file_name);
                if (it != defaults_configs.end()) {
                    file_defaults = it->second;
                }
                auto fcfg = get_function_config(file_name, func_name);
                if (fcfg)
                    func_cfg.update(*fcfg);
            }

            bool disable_xchecks = this->disable_xchecks;
            if (file_defaults && file_defaults->get().disable_xchecks)
                disable_xchecks = *file_defaults->get().disable_xchecks;
            if (func_cfg.disable_xchecks)
                disable_xchecks = *func_cfg.disable_xchecks;
            if (disable_xchecks)
                continue;

            // Add the function entry-point cross-check
            StmtVec new_body_stmts;
            auto add_body_stmts = [&new_body_stmts] (const TinyStmtVec &stmts) {
                new_body_stmts.insert(new_body_stmts.end(),
                                      std::make_move_iterator(stmts.begin()),
                                      std::make_move_iterator(stmts.end()));
            };

            XCheck entry_xcheck{XCheck::DEFAULT};
            if (file_defaults && file_defaults->get().entry)
                entry_xcheck = *file_defaults->get().entry;
            if (func_cfg.entry)
                entry_xcheck = *func_cfg.entry;
            auto entry_xcheck_default_fn = [&ctx, fd] (void) {
                auto rb_xcheck_hash = djb2_hash(fd->getName());
                return IntegerLiteral::Create(ctx,
                                              llvm::APInt(64, rb_xcheck_hash),
                                              ctx.UnsignedLongTy,
                                              SourceLocation());
            };
            auto entry_xcheck_stmts = build_xcheck(entry_xcheck,
                                                   XCheck::Tag::FUNCTION_ENTRY,
                                                   ctx,
                                                   entry_xcheck_default_fn,
                                                   no_custom_args);
            add_body_stmts(entry_xcheck_stmts);

            // Custom cross-check functions accept either function parameters
            // or global variables as their own arguments
            // TODO: also handle static locals
            DeclMap param_decls = global_vars;
            for (auto &param : fd->parameters()) {
                param_decls.emplace(llvm_string_ref_to_sv(param->getName()), param);
            }
            auto param_custom_args_fn = [&ctx, &param_decls] (CustomArgVec args) {
                auto arg_build_fn = [&ctx] (DeclaratorDecl *decl) {
                    return new (ctx) DeclRefExpr(decl, false, decl->getType(),
                                                 VK_LValue, SourceLocation());
                };
                return generic_custom_args(ctx, param_decls, args, arg_build_fn);
            };
            // Add cross-checks for the function parameters
            for (auto &param : fd->parameters()) {
                auto param_xcheck_stmts =
                    build_parameter_xcheck(param, file_defaults, func_name,
                                           func_cfg, param_decls, ctx);
                add_body_stmts(param_xcheck_stmts);
            }

            // Add any extra cross-checks
            auto extra_xcheck_default_fn = [] (void) -> Expr* {
                llvm_unreachable("invalid XCheck::DEFAULT for extra cross-check");
                return nullptr;
            };
            for (auto &ex : func_cfg.entry_extra) {
                XCheck extra_xcheck{XCheck::CUSTOM, ex.custom};
                auto extra_xcheck_stmts = build_xcheck(extra_xcheck,
                                                       ex.tag, ctx,
                                                       extra_xcheck_default_fn,
                                                       param_custom_args_fn);
                add_body_stmts(extra_xcheck_stmts);
            }

            // Build the body function and call it
            auto dni = fd->getNameInfo();
            std::string body_fn_name{"__c2rust_fn_body_"};
            body_fn_name += dni.getName().getAsString();
            auto body_fn_id = &ctx.Idents.get(body_fn_name);
            dni.setName(DeclarationName{body_fn_id});

            auto old_body = fd->getBody();
            auto parent_dc = fd->getDeclContext();
            auto body_fn_decl =
                FunctionDecl::Create(ctx, parent_dc,
                                     fd->getLocStart(), dni,
                                     fd->getType(), fd->getTypeSourceInfo(),
                                     SC_Static, true, true,
                                     fd->isConstexpr());
            body_fn_decl->setParams(fd->parameters());
            body_fn_decl->setBody(old_body);
            parent_dc->addDecl(body_fn_decl);
            decl_cache.try_emplace(body_fn_name, body_fn_decl);
            new_funcs.push_back(body_fn_decl);

            // Build the new body from all the cross-checks, plus a call
            // to the wrapper, e.g.:
            // int foo(int x) {
            //   rb_xcheck(...);
            //   ...
            //   int __c2rust_fn_result = __c2rust_wrapper_foo(x);
            //   ...
            //   return __c2rust_fn_result;
            // }
            auto result_ty = fd->getReturnType();
            ExprVec args;
            for (auto &param : body_fn_decl->parameters()) {
                auto param_ty = param->getType();
                auto param_ref_rv =
                    new (ctx) DeclRefExpr(param, false, param_ty,
                                          VK_RValue, SourceLocation());
                args.push_back(param_ref_rv);
            }
            Expr *body_call = build_call(body_fn_name, result_ty, args, ctx);

            // Build the result variable and its value
            VarDecl *result_var = nullptr;
            Expr *result = nullptr;
            if (result_ty->isIncompleteType()) {
                // Incomplete type (probably void), which we can't store
                // in a variable, so just ignore it
                // FIXME: do we need to handle incomplete arrays here???
                new_body_stmts.push_back(body_call);
            } else {
                // Build the variable that holds the result:
                // T __c2rust_fn_result = __c2rust_wrapper_X(...);
                auto result_id = &ctx.Idents.get("__c2rust_fn_result");
                result_var =
                    VarDecl::Create(ctx, fd, SourceLocation(), SourceLocation(),
                                    result_id, result_ty, nullptr, SC_None);
                result_var->setInit(body_call);
                // Wrap the Decl in a DeclStmt and add it to our function
                auto result_decl_stmt =
                    new (ctx) DeclStmt(DeclGroupRef(result_var),
                                       SourceLocation(),
                                       SourceLocation());
                new_body_stmts.push_back(result_decl_stmt);
                // Build the DeclRefExpr for the ReturnStmt
                result = new (ctx) DeclRefExpr(result_var, false, result_ty,
                                               VK_RValue, SourceLocation());
            }

            // Add the function exit-point cross-check
            XCheck exit_xcheck{XCheck::DEFAULT};
            if (file_defaults && file_defaults->get().exit)
                exit_xcheck = *file_defaults->get().exit;
            if (func_cfg.exit)
                exit_xcheck = *func_cfg.exit;
            auto exit_xcheck_stmts =
                build_xcheck(exit_xcheck, XCheck::Tag::FUNCTION_EXIT,
                             ctx, entry_xcheck_default_fn, no_custom_args);
            add_body_stmts(exit_xcheck_stmts);

            // Post-exit return value and exit_extra checks
            if (result_var) {
                // FIXME: pick a nicer name
                param_decls.emplace("__c2rust_fn_result", result_var);

                // If we have a cross-check for the result, do it
                XCheck result_xcheck{XCheck::DEFAULT};
                if (file_defaults && file_defaults->get().ret)
                    result_xcheck = *file_defaults->get().ret;
                if (func_cfg.ret)
                    result_xcheck = *func_cfg.ret;

                auto result_xcheck_default_fn = [this, &ctx, func_name,
                                                 result_var, result_ty] (void) {
                    // By default, we just call __c2rust_hash_T(x)
                    // where T is the type of the parameter
                    // FIXME: include shasher/ahasher
                    std::string result_ty_name = func_name;
                    result_ty_name += "$result";
                    auto hash_fn = get_type_hash_function(result_ty, result_ty_name, ctx, true);
                    auto result_lv = new (ctx) DeclRefExpr(result_var, false, result_ty,
                                                           VK_LValue, SourceLocation());
                    auto result_rv = hash_fn.forward_argument(result_lv, ctx);
                    auto hash_depth = build_max_hash_depth(ctx);
                    return build_call(hash_fn.name.full_name(), ctx.UnsignedLongTy,
                                      { result_rv, hash_depth }, ctx);
                };
                auto result_xcheck_stmts =
                    build_xcheck(result_xcheck, XCheck::Tag::FUNCTION_RETURN,
                                 ctx, result_xcheck_default_fn, param_custom_args_fn);
                add_body_stmts(result_xcheck_stmts);
            }
            // Add exit_extra checks
            for (auto &ex : func_cfg.exit_extra) {
                XCheck extra_xcheck{XCheck::CUSTOM, ex.custom};
                auto extra_xcheck_stmts =
                    build_xcheck(extra_xcheck, ex.tag, ctx,
                                 extra_xcheck_default_fn, param_custom_args_fn);
                add_body_stmts(extra_xcheck_stmts);
            }

            // Add the final return
            auto return_stmt = new (ctx) ReturnStmt(SourceLocation(),
                                                    result, nullptr);
            new_body_stmts.push_back(return_stmt);

            auto new_body =
#if CLANG_VERSION_MAJOR >= 6
                CompoundStmt::Create(ctx, new_body_stmts,
#else
                new (ctx) CompoundStmt(ctx, new_body_stmts,
#endif
                                       SourceLocation(),
                                       SourceLocation());
            fd->setBody(new_body);
        } else if (VarDecl *vd = dyn_cast<VarDecl>(d)) {
            global_vars.emplace(llvm_string_ref_to_sv(vd->getName()), vd);
        } else if (RecordDecl *rd = dyn_cast<RecordDecl>(d)) {
            bool disable_xchecks = this->disable_xchecks;
            // TODO: allow override from config file
            if (disable_xchecks)
                continue;

            // Instantiate the hash function for this type
            if (rd->isCompleteDefinition() && rd->getIdentifier() != nullptr) {
                auto record_ty = ctx.getRecordType(rd);
                if (record_ty->isStructureType()) {
                    // FIXME: only structures for now
                    llvm::StringRef candidate_name;
                    get_type_hash_function(record_ty, candidate_name, ctx, true);
                }
            }
        } else if (TypedefDecl *td = dyn_cast<TypedefDecl>(d)) {
            bool disable_xchecks = this->disable_xchecks;
            // TODO: allow override from config file
            if (disable_xchecks)
                continue;

            auto typedef_ty = ctx.getTypedefType(td);
            auto under_ty = td->getUnderlyingType();
            if (under_ty->isStructureType()) {
                // FIXME: handle more types, e.g., enum
                llvm::StringRef candidate_name;
                auto td_id = td->getIdentifier();
                if (td_id != nullptr)
                    candidate_name = td_id->getName();
                get_type_hash_function(typedef_ty, candidate_name, ctx, true);
            }
        }
    }
    return true;
}

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
    bool disable_xchecks = false;
    Config config;

protected:
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &ci,
                                                   llvm::StringRef) override {
        return llvm::make_unique<CrossCheckInserter>(disable_xchecks, std::move(config));
    }

    bool ParseArgs(const CompilerInstance &ci,
                   const std::vector<std::string> &args) override;

    PluginASTAction::ActionType getActionType() override {
        return AddBeforeMainAction;
    }

    // TODO: PrintHelp???
};

bool CrossCheckInsertionAction::ParseArgs(const CompilerInstance &ci,
                                          const std::vector<std::string> &args) {
    auto &diags = ci.getDiagnostics();

    SmallVector<const char*, 8> arg_ptrs;
    for (auto &arg : args)
        arg_ptrs.push_back(arg.c_str());

    CrossCheckOptTable opt_table;
    unsigned missing_arg_index, missing_arg_count;
    auto parsed_args =
        opt_table.ParseArgs(arg_ptrs, missing_arg_index, missing_arg_count);
    if (missing_arg_count > 0) {
        report_clang_error(diags, "missing %0 option(s), starting at index %1",
                           missing_arg_count, missing_arg_index);
        return false;
    }

    if (parsed_args.hasArg(OPT_disable_xchecks)) {
        disable_xchecks = true;
    }

    auto config_files = parsed_args.getAllArgValues(OPT_config_files);
    for (auto &config_file : config_files) {
        auto config_data = llvm::MemoryBuffer::getFile(config_file);
        if (!config_data) {
            report_clang_error(diags, "error reading configuration file '%0': %1",
                               config_file, config_data.getError().message());
            return false;
        }

        Config new_config;
        llvm::yaml::Input yin((*config_data)->getBuffer());
        yin >> new_config;
        if (auto yerr = yin.error()) {
            report_clang_error(diags, "error parsing YAML configuration: %0",
                               yerr.message());
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

static FrontendPluginRegistry::Add<CrossCheckInsertionAction>
    X("crosschecks", "insert cross-checks");

} // namespace crosschecks
