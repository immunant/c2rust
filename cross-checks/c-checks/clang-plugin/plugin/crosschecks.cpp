#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTMutationListener.h"
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

using namespace clang;
using namespace llvm::opt;

using namespace std::literals;

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
using StructConfigRef = std::reference_wrapper<StructConfig>;

struct StringRefPairCompare {
    bool operator()(const StringRefPair &lhs, const StringRefPair &rhs) const {
        if (lhs.first.get() == rhs.first.get()) {
            return lhs.second.get() < rhs.second.get();
        } else {
            return lhs.first.get()  < rhs.first.get();
        }
    }
};

class HashFunctionName {
public:
    using Element = std::variant<std::string, std::string_view>;

    HashFunctionName() {}
    HashFunctionName(std::string_view sv) : elements{1, sv} {}
    HashFunctionName(std::string str) : elements{1, str} {}

    void append(std::string str) {
        elements.emplace_back(str);
    }

    void append(std::string_view sv) {
        elements.emplace_back(sv);
    }

    std::string full_name() const {
        std::string res{"__c2rust_hash"};
        for (auto &elem : elements) {
            res += '_';
            if (auto *s = std::get_if<std::string>(&elem)) {
                res += *s;
            } else if (auto *sv = std::get_if<std::string_view>(&elem)) {
                res += *sv;
            } else {
                assert(false && "Invalid HashFunctionName::Element variant type");
            }
        }
        return res;
    }

private:
    std::vector<Element> elements;
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
    std::map<StringRefPair, StructConfigRef,
        StringRefPairCompare> struct_configs;

    ASTConsumer *toplevel_consumer = nullptr;

    std::vector<FunctionDecl*> new_funcs;

    // Store a cache of name=>FunctionDecl mappings,
    // to use when building calls to our runtime functions.
    using DeclCache = llvm::StringMap<FunctionDecl*>;
    DeclCache decl_cache;

private:
    enum CrossCheckTag {
        UNKNOWN_TAG = 0,
        FUNCTION_ENTRY_TAG = 1,
        FUNCTION_EXIT_TAG = 2,
        FUNCTION_ARG_TAG = 3,
        FUNCTION_RETURN_TAG = 4,
    };

private:
    FunctionDecl *get_function_decl(llvm::StringRef name,
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

    CallExpr *build_call(llvm::StringRef fn_name, QualType result_ty,
                         ArrayRef<Expr*> args, ASTContext &ctx) {
        SmallVector<QualType, 16> arg_tys;
        for (auto &arg : args) {
            auto arg_ty = arg->getType();
            arg_tys.push_back(arg_ty);
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

    using ExprVec = SmallVector<Expr*, 4>;
    using XCheckDefaultFn = std::function<Expr*(void)>;
    using XCheckCustomArgsFn = std::function<ExprVec(void)>;

    llvm::TinyPtrVector<Stmt*>
    build_xcheck(const XCheck &xcheck, CrossCheckTag tag,
                 ASTContext &ctx, XCheckDefaultFn default_fn,
                 XCheckCustomArgsFn custom_args_fn) {
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
            auto &xcheck_fn_name = std::get<std::string>(xcheck.data);
            ExprVec xcheck_fn_args = custom_args_fn ? custom_args_fn() : ExprVec{};
            rb_xcheck_val = build_call(xcheck_fn_name, ctx.UnsignedLongTy,
                                       xcheck_fn_args, ctx);
            break;
        }

        default:
            llvm_unreachable("Invalid XCheck reached");
        }
        if (rb_xcheck_val == nullptr)
            return {};

        llvm::TinyPtrVector<Stmt*> res;
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

    HashFunctionName get_type_hash_function(QualType ty, ASTContext &ctx,
                                            bool build_it) {
        switch (ty->getTypeClass()) {
        case Type::Builtin: {
            switch (cast<BuiltinType>(ty)->getKind()) {
            case BuiltinType::Void:
                // TODO: we should never actually be able to hash a void
                return "void"sv;
            case BuiltinType::Bool:
                return "bool"sv;
            case BuiltinType::Char_S:
            case BuiltinType::Char_U:
                return "char"sv;
            case BuiltinType::UChar:
                return "uchar"sv;
            case BuiltinType::UShort:
                return "ushort"sv;
            case BuiltinType::UInt:
                return "uint"sv;
            case BuiltinType::ULong:
                return "ulong"sv;
            case BuiltinType::ULongLong:
                return "ullong"sv;
            case BuiltinType::SChar:
                return "schar"sv;
            case BuiltinType::Short:
                return "short"sv;
            case BuiltinType::Int:
                return "int"sv;
            case BuiltinType::Long:
                return "long"sv;
            case BuiltinType::LongLong:
                return "llong"sv;
            case BuiltinType::Float:
                return "float"sv;
            case BuiltinType::Double:
                return "double"sv;
            default:
                llvm_unreachable("Unknown/unhandled builtin type");
            }
            break;
        }

        case Type::Pointer: {
            auto pointee_ty = cast<PointerType>(ty)->getPointeeType();
            auto canonical_pointee_ty = ctx.getCanonicalType(pointee_ty);
            auto pointee_name = get_type_hash_function(canonical_pointee_ty, ctx, build_it);
            auto func_name = pointee_name;
            func_name.append("ptr"sv);
            if (build_it) {
                build_pointer_hash_function(func_name, ty, pointee_name,
                                            canonical_pointee_ty, ctx);
            }
            return func_name;
        }

        case Type::ConstantArray: {
            auto array_ty = cast<ConstantArrayType>(ty);
            auto element_ty = array_ty->getElementType();
            auto canonical_element_ty = ctx.getCanonicalType(element_ty);
            auto element_name = get_type_hash_function(canonical_element_ty, ctx, build_it);
            auto func_name = std::move(element_name);
            func_name.append("array"sv);
            func_name.append(llvm::utostr(array_ty->getSize().getZExtValue()));
            // TODO: build it
            return func_name;
        }

        case Type::Record: {
            // Build the type name as "name_kind", where "kind" can be
            // "struct", "class" (for C++), "union" or "enum"
            //
            // We append "kind" after "name" to avoid collisions, e.g., between
            // a structure "struct foo_ptr" and a pointer of type "struct foo*":
            // struct foo_ptr => "foo_ptr_struct"
            // struct foo*    => "foo_struct_ptr"
            auto record_ty = cast<RecordType>(ty);
            auto record_decl = record_ty->getDecl();
            auto record_name = record_decl->getDeclName().getAsString();
            HashFunctionName func_name{record_name};
            func_name.append(record_decl->getKindName().str());
            if (build_it) {
                build_record_hash_function(func_name, ty, ctx);
            }
            return func_name;
        }

        default:
            llvm_unreachable("unimplemented");
        }
    }

    using StmtVec = SmallVector<Stmt*, 16>;
    using HashFunctionBodyFn = std::function<StmtVec(FunctionDecl*)>;

    void build_generic_hash_function(const HashFunctionName &func_name,
                                     QualType ty,
                                     ASTContext &ctx,
                                     HashFunctionBodyFn body_fn) {
        auto fn_decl = get_function_decl(func_name.full_name(),
                                         ctx.UnsignedLongTy,
                                         { ty },
                                         SC_Static,
                                         ctx);
        if (fn_decl->hasBody())
            return; // We've already built it

        auto fn_body_stmts = body_fn(fn_decl);
        auto fn_body = new (ctx)
            CompoundStmt(ctx, fn_body_stmts,
                         SourceLocation(),
                         SourceLocation());

        fn_decl->setInlineSpecified(true);
        fn_decl->setBody(fn_body);
        new_funcs.push_back(fn_decl);
        // TODO: add it to the parent DeclContext???
    }

    void build_pointer_hash_function(const HashFunctionName &func_name,
                                     QualType ty,
                                     const HashFunctionName &pointee_name,
                                     QualType pointee_ty,
                                     ASTContext &ctx) {
        // Build the pointer hash function using this template:
        // uint64_t __c2rust_hash_T_ptr(T *x) {
        //   return __c2rust_pointer_is_valid(x)
        //          ? __c2rust_hash_T(*x)
        //          : __c2rust_hash_invalid_pointer(x);
        // }
        //
        // TODO: add a depth parameter and decrement it on recursion
        auto body_fn = [this, &ctx, &pointee_name] (FunctionDecl *fn_decl) -> StmtVec {
            assert(fn_decl->getNumParams() == 1 &&
                   "Invalid hash function signature");
            auto param = fn_decl->getParamDecl(0);
            auto param_ty = param->getType();
            auto param_ref_lv =
                new (ctx) DeclRefExpr(param, false, param_ty,
                                      VK_LValue, SourceLocation());
            auto param_ref_rv =
                ImplicitCastExpr::Create(ctx, param_ty,
                                         CK_LValueToRValue,
                                         param_ref_lv, nullptr, VK_RValue);
            auto is_valid_call =
                build_call("__c2rust_pointer_is_valid", ctx.BoolTy,
                           { param_ref_rv }, ctx);

            // Build the call to the pointee function
            auto param_deref =
                new (ctx) UnaryOperator(param_ref_lv, UO_Deref,
                                        param_ref_lv->getType(),
                                        VK_RValue, OK_Ordinary,
                                        SourceLocation());
            // TODO: write a function that prepends __c2rust_hash_
            auto param_hash_call =
                build_call(pointee_name.full_name(), ctx.UnsignedLongTy,
                           { param_deref }, ctx);

            // Build the call to __c2rust_hash_invalid_pointer
            auto hash_invalid_call =
                build_call("__c2rust_hash_invalid_pointer", ctx.UnsignedLongTy,
                           { param_ref_rv }, ctx);

            // Build the conditional expression and return statement
            auto cond_expr =
                new (ctx) ConditionalOperator(is_valid_call,
                                              SourceLocation(),
                                              param_hash_call,
                                              SourceLocation(),
                                              hash_invalid_call,
                                              ctx.UnsignedLongTy,
                                              VK_RValue,
                                              OK_Ordinary);
            auto return_stmt =
                new (ctx) ReturnStmt(SourceLocation(), cond_expr, nullptr);
            return { return_stmt };
        };
        build_generic_hash_function(func_name, ty, ctx, body_fn);
    }

    // TODO: build_array_hash_function

    void build_record_hash_function(const HashFunctionName &func_name,
                                    QualType ty,
                                    ASTContext &ctx) {
        auto &diags = ctx.getDiagnostics();
        auto record_ty = cast<RecordType>(ty);
        auto record_decl = record_ty->getDecl();
        if (record_decl->isUnion()) {
            unsigned diag_id =
                diags.getCustomDiagID(DiagnosticsEngine::Error,
                                      "default cross-checking is not supported for unions, "
                                      "please use a custom cross-check");
            diags.Report(diag_id);
            return;
        }
        assert((record_decl->isStruct() || record_decl->isClass()) &&
               "Called build_record_hash_function on neither a struct nor a class");

        // Build the following code:
        // uint64_t __c2rust_hash_T_struct(struct T x) {
        //   char hasher[__c2rust_hasher_H_size()];
        //   __c2rust_hasher_H_init(hasher);
        //   __c2rust_hasher_H_update(hasher, __c2rust_hash_F1(x.field1));
        //   __c2rust_hasher_H_update(hasher, __c2rust_hash_F2(x.field2));
        //   ...
        //   return __c2rust_hasher_H_finish(hasher);
        // }
        //
        // TODO: allow custom hashers instead of the default "jodyhash"
        // TODO: add support for the "field_hasher" configuration override
        // TODO: add support for the complete override of this function
        // using "custom_hash"
        // TODO: allow per-field cross-check configuration
        auto record_def = record_decl->getDefinition();
        if (record_def == nullptr) {
            unsigned diag_id =
                diags.getCustomDiagID(DiagnosticsEngine::Error,
                                      "default cross-checking is not supported for undefined structures, "
                                      "please use a custom cross-check");
            diags.Report(diag_id);
            return;
        }

        std::optional<StructConfigRef> record_cfg;
        auto ploc = ctx.getSourceManager().getPresumedLoc(record_def->getLocStart());
        if (ploc.isValid()) {
            std::string file_name(ploc.getFilename());
            std::string record_name = record_def->getName().str();
            record_cfg = get_struct_config(file_name, record_name);
        }
        if (record_cfg && !record_cfg->get().custom_hash.empty()) {
            // The user specified a "custom_hash" function, so just forward
            // the structure to it
            // FIXME: would be nice to not have to emit a function body,
            // and instead declare our function using "alias", e.g.:
            // uint64_t __c2rust_hash_T_struct(struct T x) __attribute__((alias("...")));
            auto &hash_fn_name = record_cfg->get().custom_hash;
            auto body_fn = [this, &ctx, &hash_fn_name] (FunctionDecl *fn_decl) -> StmtVec {
                auto param = fn_decl->getParamDecl(0);
                auto param_ty = param->getType();
                auto param_ref_rv =
                    new (ctx) DeclRefExpr(param, false, param_ty,
                                          VK_RValue, SourceLocation());
                auto hash_fn_call = build_call(hash_fn_name,
                                               ctx.UnsignedLongTy,
                                               { param_ref_rv }, ctx);
                auto return_stmt =
                    new (ctx) ReturnStmt(SourceLocation(), hash_fn_call, nullptr);
                return { return_stmt };
            };
            build_generic_hash_function(func_name, ty, ctx, body_fn);
            return;
        }

        std::string hasher_name{"jodyhash"};
        if (record_cfg && !record_cfg->get().field_hasher.empty()) {
            hasher_name = record_cfg->get().field_hasher;
        }
        std::string hasher_prefix{"__c2rust_hasher_"};
        hasher_prefix += hasher_name;
        auto body_fn =
                [this, &ctx, &record_def,
                 record_cfg = std::move(record_cfg),
                 hasher_prefix = std::move(hasher_prefix)]
                (FunctionDecl *fn_decl) -> StmtVec {
            StmtVec stmts;
            // TODO: read and apply the configuration settings for each field:
            // "disabled", "fixed" and "custom"
            auto hasher_size_call =
                build_call(hasher_prefix + "_size", ctx.UnsignedIntTy,
                           {}, ctx);
            auto hasher_ty = ctx.getVariableArrayType(ctx.CharTy,
                                                      hasher_size_call,
                                                      ArrayType::Normal,
                                                      0, SourceRange());
            auto hasher_ptr_ty = ctx.getArrayDecayedType(hasher_ty);
            auto hasher_id = &ctx.Idents.get("hasher");
            auto hasher_var =
                VarDecl::Create(ctx, fn_decl, SourceLocation(), SourceLocation(),
                                hasher_id, hasher_ty, nullptr, SC_None);
            auto hasher_var_decl_stmt =
                new (ctx) DeclStmt(DeclGroupRef(hasher_var),
                                   SourceLocation(),
                                   SourceLocation());
            stmts.push_back(hasher_var_decl_stmt);

            // Call the initializer
            auto hasher_var_ref =
                new (ctx) DeclRefExpr(hasher_var, false, hasher_ty,
                                      VK_LValue, SourceLocation());
            auto hasher_var_ptr =
                ImplicitCastExpr::Create(ctx, hasher_ptr_ty,
                                         CK_ArrayToPointerDecay,
                                         hasher_var_ref, nullptr, VK_RValue);
            auto init_call = build_call(hasher_prefix + "_init",
                                        ctx.VoidTy,
                                        { hasher_var_ptr }, ctx);
            stmts.push_back(init_call);

            // Add the field calls
            auto param = fn_decl->getParamDecl(0);
            for (auto *field : record_def->fields()) {
                if (field->isBitField()) {
                    auto &diags = ctx.getDiagnostics();
                    unsigned diag_id =
                        diags.getCustomDiagID(DiagnosticsEngine::Error,
                                              "default cross-checking is not supported for bitfields, "
                                              "please use a custom cross-check");
                    diags.Report(diag_id);
                    return {};
                }

                XCheck field_xcheck;
                if (record_cfg) {
                    auto &fields = record_cfg->get().fields;
                    auto it = fields.find(field->getName());
                    if (it != fields.end()) {
                        field_xcheck = it->second;
                    }
                }
                if (field_xcheck.type == XCheck::DISABLED)
                    continue;

                Expr *field_hash = nullptr;
                if (field_xcheck.type == XCheck::FIXED) {
                    auto field_hash_val = std::get<uint64_t>(field_xcheck.data);
                    field_hash = IntegerLiteral::Create(ctx,
                                                        llvm::APInt(64, field_hash_val),
                                                        ctx.UnsignedLongTy,
                                                        SourceLocation());
                } else {
                    auto param_ref_lv =
                        new (ctx) DeclRefExpr(param, false, param->getType(),
                                              VK_LValue, SourceLocation());
                    auto field_ref_lv =
                        new (ctx) MemberExpr(param_ref_lv, false, SourceLocation(),
                                             field, SourceLocation(),
                                             field->getType(), VK_LValue, OK_Ordinary);
                    auto canonical_field_ty = ctx.getCanonicalType(field->getType());
                    auto field_hash_fn =
                        field_xcheck.type == XCheck::CUSTOM
                        ? std::get<std::string>(field_xcheck.data)
                        : get_type_hash_function(canonical_field_ty, ctx, true).full_name();
                    auto field_ref_rv =
                        ImplicitCastExpr::Create(ctx, canonical_field_ty,
                                                 CK_LValueToRValue,
                                                 field_ref_lv, nullptr, VK_RValue);
                    field_hash = build_call(field_hash_fn,
                                            ctx.UnsignedLongTy,
                                            { field_ref_rv }, ctx);
                }
                auto field_update_call = build_call(hasher_prefix + "_update",
                                                    ctx.VoidTy,
                                                    { hasher_var_ptr, field_hash },
                                                    ctx);
                stmts.push_back(field_update_call);
            }

            // Return the result of the finish function
            auto finish_call = build_call(hasher_prefix + "_finish",
                                          ctx.UnsignedLongTy,
                                          { hasher_var_ptr }, ctx);
            auto return_stmt =
                new (ctx) ReturnStmt(SourceLocation(), finish_call, nullptr);
            stmts.push_back(return_stmt);
            return stmts;
        };
        build_generic_hash_function(func_name, ty, ctx, body_fn);
    }

    llvm::TinyPtrVector<Stmt*>
    build_parameter_xcheck(ParmVarDecl *param,
                           const std::optional<FunctionConfigRef> &func_cfg,
                           ASTContext &ctx) {
        XCheck param_xcheck{XCheck::DISABLED};
        if (func_cfg) {
            auto &func_cfg_ref = func_cfg->get();
            param_xcheck = func_cfg_ref.all_args;

            auto it = func_cfg_ref.args.find(param->getName());
            if (it != func_cfg_ref.args.end()) {
                param_xcheck = it->second;
            }
        }
        auto param_xcheck_default_fn = [this, &ctx, param] (void) {
            // By default, we just call __c2rust_hash_T(x)
            // where T is the type of the parameter
            // FIXME: include shasher/ahasher
            auto param_canonical_type = ctx.getCanonicalType(param->getType());
            auto hash_fn_name = get_type_hash_function(param_canonical_type, ctx, true);

            // Forward the value of the parameter to the hash function
            auto param_ref_rv =
                new (ctx) DeclRefExpr(param, false, param->getType(),
                                      VK_RValue, SourceLocation());
            // TODO: pass PODs by value, non-PODs by pointer???
            return build_call(hash_fn_name.full_name(), ctx.UnsignedLongTy,
                              { param_ref_rv }, ctx);
        };
        auto param_xcheck_custom_args_fn = [&ctx, param] (void) {
            // Forward the value of the parameter to the custom function
            auto param_ref_rv =
                new (ctx) DeclRefExpr(param, false, param->getType(),
                                      VK_RValue, SourceLocation());
            // TODO: pass PODs by value, non-PODs by pointer???
            //
            // TODO: we might need a way to pass additional
            // arguments to the custom function, e.g., if it hashes
            // an array and requires the array's length, we should
            // call it as `custom(a, len)`. For this to work, we'll
            // need a way to customize which arguments get passed.
            return ExprVec{param_ref_rv};
        };
        return build_xcheck(param_xcheck, FUNCTION_ARG_TAG, ctx,
                            param_xcheck_default_fn,
                            param_xcheck_custom_args_fn);
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

    std::optional<StructConfigRef>
    get_struct_config(const std::string &file_name,
                      const std::string &struct_name) {
        StringRefPair key(std::cref(file_name), std::cref(struct_name));
        auto file_it = struct_configs.find(key);
        if (file_it != struct_configs.end())
            return std::make_optional(file_it->second);
        return {};
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
                } else if (auto struc = std::get_if<StructConfig>(&item)) {
                    StringRefPair key(std::cref(file_name), std::cref(struc->name));
                    struct_configs.emplace(key, *struc);
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
                if (fd->getName().startswith("__c2rust")) {
                    // Ignore our own functions
                    continue;
                }

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

                // Add the function entry-point cross-check
                SmallVector<Stmt*, 8> new_body_stmts;
                auto entry_xcheck = func_cfg ? func_cfg->get().entry : XCheck();
                auto entry_xcheck_default_fn = [&ctx, fd] (void) {
                    auto rb_xcheck_hash = djb2_hash(fd->getName());
                    return IntegerLiteral::Create(ctx,
                                                  llvm::APInt(64, rb_xcheck_hash),
                                                  ctx.UnsignedLongTy,
                                                  SourceLocation());
                };
                auto entry_xcheck_stmts = build_xcheck(entry_xcheck,
                                                       FUNCTION_ENTRY_TAG,
                                                       ctx,
                                                       entry_xcheck_default_fn,
                                                       nullptr);
                std::move(entry_xcheck_stmts.begin(),
                          entry_xcheck_stmts.end(),
                          std::back_inserter(new_body_stmts));

                // Add cross-checks for the function parameters
                for (auto &param : fd->parameters()) {
                    auto param_xcheck_stmts =
                        build_parameter_xcheck(param, func_cfg, ctx);
                    std::move(param_xcheck_stmts.begin(),
                              param_xcheck_stmts.end(),
                              std::back_inserter(new_body_stmts));
                }

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
        decl_cache.clear();
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
