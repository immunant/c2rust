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

// Helper function for report_clang_error
// that inserts each argument into the given stream
template<typename Stream>
static inline void
args_to_stream(Stream &s) {}

template<typename Stream, typename Arg, typename... Args>
static inline void
args_to_stream(Stream &s, Arg &&arg, Args&&... args) {
    s << std::forward<Arg>(arg);
    args_to_stream(s, args...);
}

template<unsigned N, typename... Args>
static inline void
report_clang_error(DiagnosticsEngine &diags,
                   const char (&fmt)[N],
                   Args&&... args) {
    unsigned diag_id =
        diags.getCustomDiagID(DiagnosticsEngine::Error, fmt);
    auto db = diags.Report(diag_id);
    args_to_stream(db, std::forward<Args>(args)...);
}

using StringRef = std::reference_wrapper<const std::string>;
using StringRefPair = std::pair<StringRef, StringRef>;
using DefaultsConfigRef = std::reference_wrapper<DefaultsConfig>;
using DefaultsConfigOptRef = std::optional<DefaultsConfigRef>;
using FunctionConfigRef = std::reference_wrapper<FunctionConfig>;
using StructConfigRef = std::reference_wrapper<StructConfig>;

struct StringRefCompare {
    bool operator()(const StringRef &lhs, const StringRef &rhs) const {
        return lhs.get() < rhs.get();
    }
};

struct StringRefPairCompare {
    bool operator()(const StringRefPair &lhs, const StringRefPair &rhs) const {
        if (lhs.first.get() == rhs.first.get()) {
            return lhs.second.get() < rhs.second.get();
        } else {
            return lhs.first.get()  < rhs.first.get();
        }
    }
};

static inline
llvm::StringRef llvm_string_ref_from_sv(std::string_view sv) {
    return { sv.data(), sv.length() };
}

static inline
std::string_view llvm_string_ref_to_sv(llvm::StringRef sr) {
    return { sr.data(), sr.size() };
}

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
    std::map<StringRef, DefaultsConfig,
        StringRefCompare> defaults_configs;
    std::map<StringRefPair, FunctionConfigRef,
        StringRefPairCompare> function_configs;
    std::map<StringRefPair, StructConfigRef,
        StringRefPairCompare> struct_configs;

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

    ASTConsumer *toplevel_consumer = nullptr;

    std::vector<FunctionDecl*> new_funcs;

    using DeclMap = std::map<std::string_view, DeclaratorDecl*>;

    DeclMap global_vars;

private:
    // Store a cache of name=>FunctionDecl mappings,
    // to use when building calls to our runtime functions.
    using DeclCache = llvm::StringMap<FunctionDecl*>;
    DeclCache decl_cache;

    FunctionDecl *get_function_decl(llvm::StringRef name,
                                    QualType result_ty,
                                    ArrayRef<QualType> arg_tys,
                                    StorageClass sc,
                                    ASTContext &ctx);

    CallExpr *build_call(llvm::StringRef fn_name, QualType result_ty,
                         ArrayRef<Expr*> args, ASTContext &ctx);

    using ExprVec = SmallVector<Expr*, 4>;

    // Argument passed to custom cross-check
    struct CustomArg {
        enum Modifier {
            NONE,
            ADDR,
            DEREF,
        };

        std::string_view ident;
        Modifier mod;

        CustomArg() = delete;
        CustomArg(std::string_view sv) : ident(sv), mod(NONE) {
            if (ident.empty())
                return;
            if (ident.front() == '&') {
                mod = ADDR;
                ident.remove_prefix(1);
            } else if (ident.front() == '*') {
                mod = DEREF;
                ident.remove_prefix(1);
            }
            // TODO: support a more complex argument format
        }
    };

    using CustomArgVec = std::vector<CustomArg>;
    using CustomFnSig = std::tuple<std::string_view, CustomArgVec>;

    CustomFnSig parse_custom_xcheck(std::string_view sv,
                                    ASTContext &ctx);

    static inline ExprVec no_custom_args(CustomArgVec args) {
        return {};
    }

    template<typename BuildFn>
    static ExprVec generic_custom_args(ASTContext &ctx,
                                       const DeclMap &decls,
                                       CustomArgVec args,
                                       BuildFn build_fn);

    using TinyStmtVec = llvm::TinyPtrVector<Stmt*>;

    template<typename DefaultFn, typename CustomArgsFn>
    TinyStmtVec
    build_xcheck(const XCheck &xcheck, XCheck::Tag tag,
                 ASTContext &ctx, DefaultFn default_fn,
                 CustomArgsFn custom_args_fn);

    HashFunctionName get_type_hash_function(QualType ty, ASTContext &ctx,
                                            bool build_it);

    using StmtVec = SmallVector<Stmt*, 16>;

    // Set of functions we're in the process of building
    // We need to keep track of which hash functions we've started
    // building, so we avoid an infinite recursion when we build
    // hash functions for recursive structures, e.g.:
    // struct Foo {
    //   struct Foo *p;
    // }
    // __c2rust_hash_Foo_struct calls __c2rust_hash_Foo_struct_ptr
    // which in turns calls __c2rust_hash_Foo_struct, so we need
    // to be careful when building them to avoid infinite recursion
    std::set<StringRef, StringRefCompare> pending_hash_functions;

    template<typename BodyFn>
    void build_generic_hash_function(const HashFunctionName &func_name,
                                     QualType ty,
                                     ASTContext &ctx,
                                     BodyFn body_fn);

    void build_pointer_hash_function(const HashFunctionName &func_name,
                                     QualType ty,
                                     const HashFunctionName &pointee_name,
                                     QualType pointee_ty,
                                     ASTContext &ctx);

    // TODO: build_array_hash_function

    void build_record_hash_function(const HashFunctionName &func_name,
                                    QualType ty,
                                    ASTContext &ctx);

    TinyStmtVec
    build_parameter_xcheck(ParmVarDecl *param,
                           const DefaultsConfigOptRef file_defaults,
                           const std::optional<FunctionConfigRef> &func_cfg,
                           const DeclMap &param_decls,
                           ASTContext &ctx);

public:
    CrossCheckInserter() = delete;
    CrossCheckInserter(Config &&cfg) : config(std::move(cfg)) {
        for (auto &file_config : config) {
            auto &file_name = file_config.first;
            for (auto &item : file_config.second)
                if (auto defs = std::get_if<DefaultsConfig>(&item)) {
                    defaults_configs[file_name].update(*defs);
                } else if (auto func = std::get_if<FunctionConfig>(&item)) {
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

    bool HandleTopLevelDecl(DeclGroupRef dg) override;

    void HandleTranslationUnit(ASTContext &ctx) override {
        assert(toplevel_consumer != nullptr);
        // Send our new xcheck_XXX functions through the ASTConsumer pipeline
        for (auto func : new_funcs)
            toplevel_consumer->HandleTopLevelDecl(DeclGroupRef(func));
        new_funcs.clear();
        decl_cache.clear();
    }
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
        return { std::string{sv}, {} };

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

HashFunctionName
CrossCheckInserter::get_type_hash_function(QualType ty, ASTContext &ctx,
                                           bool build_it) {
    ty = ctx.getCanonicalType(ty);
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
        auto pointee_name = get_type_hash_function(pointee_ty, ctx, build_it);
        auto func_name = pointee_name;
        func_name.append("ptr"sv);
        if (build_it) {
            build_pointer_hash_function(func_name, ty, pointee_name,
                                        pointee_ty, ctx);
        }
        return func_name;
    }

    case Type::ConstantArray: {
        auto array_ty = cast<ConstantArrayType>(ty);
        auto element_ty = array_ty->getElementType();
        auto element_name = get_type_hash_function(element_ty, ctx, build_it);
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

template<typename BodyFn>
void CrossCheckInserter::build_generic_hash_function(const HashFunctionName &func_name,
                                                     QualType ty,
                                                     ASTContext &ctx,
                                                     BodyFn body_fn) {
    auto full_name = func_name.full_name();
    auto fn_decl = get_function_decl(full_name,
                                     ctx.UnsignedLongTy,
                                     { ty },
                                     SC_Static,
                                     ctx);
    if (fn_decl->hasBody())
        return; // We've already built it

    // If we're already in the process of building this function,
    // just leave it as a body-less Decl
    auto [it, inserted] = pending_hash_functions.insert(std::cref(full_name));
    if (!inserted)
        return; // Function is already pending

    auto fn_body_stmts = body_fn(fn_decl);
    auto fn_body = new (ctx)
        CompoundStmt(ctx, fn_body_stmts,
                     SourceLocation(),
                     SourceLocation());

    fn_decl->setInlineSpecified(true);
    fn_decl->setBody(fn_body);
    new_funcs.push_back(fn_decl);
    // TODO: add it to the parent DeclContext???

    pending_hash_functions.erase(full_name);
}

void CrossCheckInserter::build_pointer_hash_function(const HashFunctionName &func_name,
                                                     QualType ty,
                                                     const HashFunctionName &pointee_name,
                                                     QualType pointee_ty,
                                                     ASTContext &ctx) {
    // __c2rust_hash_void_ptr is implemented in the runtime
    if (pointee_ty->isVoidType())
        return;

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

void CrossCheckInserter::build_record_hash_function(const HashFunctionName &func_name,
                                                    QualType ty,
                                                    ASTContext &ctx) {
    auto &diags = ctx.getDiagnostics();
    auto record_ty = cast<RecordType>(ty);
    auto record_decl = record_ty->getDecl();
    if (record_decl->isUnion()) {
        report_clang_error(diags, "default cross-checking is not supported for unions, "
                                  "please use a custom cross-check");
        return;
    }
    assert((record_decl->isStruct() || record_decl->isClass()) &&
           "Called build_record_hash_function on neither a struct nor a class");

    // TODO: handle disable_xchecks == true here

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
    auto record_def = record_decl->getDefinition();
    if (record_def == nullptr) {
        report_clang_error(diags, "default cross-checking is not supported for undefined structures, "
                                  "please use a custom cross-check");
        return;
    }

    std::optional<StructConfigRef> record_cfg;
    auto ploc = ctx.getSourceManager().getPresumedLoc(record_def->getLocStart());
    if (ploc.isValid()) {
        std::string file_name(ploc.getFilename());
        std::string record_name = record_def->getName().str();
        record_cfg = get_struct_config(file_name, record_name);
    }
    if (record_cfg && record_cfg->get().custom_hash) {
        // The user specified a "custom_hash" function, so just forward
        // the structure to it
        // FIXME: would be nice to not have to emit a function body,
        // and instead declare our function using "alias", e.g.:
        // uint64_t __c2rust_hash_T_struct(struct T x) __attribute__((alias("...")));
        auto &hash_fn_name = *record_cfg->get().custom_hash;
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
    if (record_cfg && record_cfg->get().field_hasher) {
        hasher_name = *record_cfg->get().field_hasher;
    }
    std::string hasher_prefix{"__c2rust_hasher_"};
    hasher_prefix += hasher_name;
    auto body_fn =
            [this, &ctx, &record_def,
             record_cfg = std::move(record_cfg),
             hasher_prefix = std::move(hasher_prefix)]
            (FunctionDecl *fn_decl) -> StmtVec {
        StmtVec stmts;
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
        DeclMap field_decls;
        for (auto *field : record_def->fields()) {
            field_decls.emplace(llvm_string_ref_to_sv(field->getName()), field);
        }
        for (auto *field : record_def->fields()) {
            if (field->isBitField()) {
                auto &diags = ctx.getDiagnostics();
                report_clang_error(diags, "default cross-checking is not supported for bitfields, "
                                          "please use a custom cross-check");
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
                std::string field_hash_fn;
                ExprVec field_hash_args;
                if (field_xcheck.type == XCheck::CUSTOM) {
                    auto xcheck_data = std::get<std::string>(field_xcheck.data);
                    auto field_hash_fn_sig = parse_custom_xcheck(xcheck_data, ctx);
                    field_hash_fn = std::string{std::get<0>(field_hash_fn_sig)};

                    // Build the argument vector
                    auto &args = std::get<1>(field_hash_fn_sig);
                    auto arg_build_fn = [&ctx, param_ref_lv] (DeclaratorDecl *decl) {
                        return new (ctx) MemberExpr(param_ref_lv, false, SourceLocation(),
                                                    decl, SourceLocation(),
                                                    decl->getType(), VK_LValue,
                                                    OK_Ordinary);
                    };
                    field_hash_args = generic_custom_args(ctx, field_decls,
                                                          args, arg_build_fn);
                } else {
                    field_hash_fn =
                        get_type_hash_function(field->getType(), ctx, true).full_name();
                    auto field_ref_lv =
                        new (ctx) MemberExpr(param_ref_lv, false, SourceLocation(),
                                             field, SourceLocation(),
                                             field->getType(), VK_LValue, OK_Ordinary);
                    auto field_ref_rv =
                        ImplicitCastExpr::Create(ctx, field->getType(),
                                                 CK_LValueToRValue,
                                                 field_ref_lv, nullptr, VK_RValue);
                    field_hash_args.push_back(field_ref_rv);
                }
                field_hash = build_call(field_hash_fn,
                                        ctx.UnsignedLongTy,
                                        field_hash_args, ctx);
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

template<typename BuildFn>
CrossCheckInserter::ExprVec
CrossCheckInserter::generic_custom_args(ASTContext &ctx,
                                        const DeclMap &decls,
                                        CustomArgVec args,
                                        BuildFn build_fn) {
    ExprVec res;
    for (auto &arg : args) {
        auto it = decls.find(arg.ident);
        if (it == decls.end()) {
            auto &diags = ctx.getDiagnostics();
            report_clang_error(diags, "unknown parameter: '%0'",
                               std::string{arg.ident});
            return res;
        }
        auto arg_ref_lv = build_fn(it->second);
        Expr *arg_ref_rv;
        switch (arg.mod) {
        // arg
        case CustomArg::NONE:
            arg_ref_rv = ImplicitCastExpr::Create(ctx, arg_ref_lv->getType(),
                                                  CK_LValueToRValue,
                                                  arg_ref_lv, nullptr, VK_RValue);
            break;

        // &arg
        case CustomArg::ADDR:
            arg_ref_rv = new (ctx) UnaryOperator(arg_ref_lv, UO_AddrOf,
                                                 ctx.getPointerType(arg_ref_lv->getType()),
                                                 VK_RValue, OK_Ordinary,
                                                 SourceLocation());
            break;

        // *arg
        case CustomArg::DEREF:
            arg_ref_rv = new (ctx) UnaryOperator(arg_ref_lv, UO_Deref,
                                                 arg_ref_lv->getType(),
                                                 VK_RValue, OK_Ordinary,
                                                 SourceLocation());
            break;

        default:
            llvm_unreachable("Unknown CustomArg::Modifier case");
        }
        res.push_back(arg_ref_rv);
    }
    return res;
}

CrossCheckInserter::TinyStmtVec
CrossCheckInserter::build_parameter_xcheck(ParmVarDecl *param,
                                           const DefaultsConfigOptRef file_defaults,
                                           const std::optional<FunctionConfigRef> &func_cfg,
                                           const DeclMap &param_decls,
                                           ASTContext &ctx) {
    XCheck param_xcheck{XCheck::DISABLED};
    if (file_defaults && file_defaults->get().all_args)
        param_xcheck = *file_defaults->get().all_args;
    if (func_cfg) {
        auto &func_cfg_ref = func_cfg->get();
        if (func_cfg_ref.all_args) {
            param_xcheck = *func_cfg_ref.all_args;
        }

        auto it = func_cfg_ref.args.find(param->getName());
        if (it != func_cfg_ref.args.end()) {
            param_xcheck = it->second;
        }
    }
    auto param_xcheck_default_fn = [this, &ctx, param] (void) {
        // By default, we just call __c2rust_hash_T(x)
        // where T is the type of the parameter
        // FIXME: include shasher/ahasher
        auto hash_fn_name = get_type_hash_function(param->getType(), ctx, true);

        // Forward the value of the parameter to the hash function
        auto param_ref_rv =
            new (ctx) DeclRefExpr(param, false, param->getType(),
                                  VK_RValue, SourceLocation());
        // TODO: pass PODs by value, non-PODs by pointer???
        return build_call(hash_fn_name.full_name(), ctx.UnsignedLongTy,
                          { param_ref_rv }, ctx);
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
        if (FunctionDecl *fd = dyn_cast<FunctionDecl>(d)) {
            if (!fd->hasBody())
                continue;
            if (fd->getName().startswith("__c2rust")) {
                // Ignore our own functions
                continue;
            }

            auto &ctx = fd->getASTContext();
            DefaultsConfigOptRef file_defaults;
            std::optional<FunctionConfigRef> func_cfg;
            auto ploc = ctx.getSourceManager().getPresumedLoc(fd->getLocStart());
            if (ploc.isValid()) {
                std::string file_name(ploc.getFilename());
                auto it = defaults_configs.find(file_name);
                if (it != defaults_configs.end()) {
                    file_defaults = it->second;
                }

                std::string func_name = fd->getName().str();
                func_cfg = get_function_config(file_name, func_name);

            }

            bool disable_xchecks = false;
            if (file_defaults && file_defaults->get().disable_xchecks)
                disable_xchecks = *file_defaults->get().disable_xchecks;
            if (func_cfg && func_cfg->get().disable_xchecks)
                disable_xchecks = *func_cfg->get().disable_xchecks;
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
            if (func_cfg && func_cfg->get().entry)
                entry_xcheck = *func_cfg->get().entry;
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
                    build_parameter_xcheck(param, file_defaults, func_cfg,
                                           param_decls, ctx);
                add_body_stmts(param_xcheck_stmts);
            }

            // Add any extra cross-checks
            auto extra_xcheck_default_fn = [] (void) -> Expr* {
                llvm_unreachable("invalid XCheck::DEFAULT for extra cross-check");
                return nullptr;
            };
            if (func_cfg) {
                for (auto &ex : func_cfg->get().entry_extra) {
                    XCheck extra_xcheck{XCheck::CUSTOM, ex.custom};
                    auto extra_xcheck_stmts = build_xcheck(extra_xcheck,
                                                           ex.tag, ctx,
                                                           extra_xcheck_default_fn,
                                                           param_custom_args_fn);
                    add_body_stmts(extra_xcheck_stmts);
                }
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
            if (func_cfg && func_cfg->get().exit)
                exit_xcheck = *func_cfg->get().exit;
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
                if (func_cfg && func_cfg->get().ret)
                    result_xcheck = *func_cfg->get().ret;

                auto result_xcheck_default_fn = [this, &ctx, result, result_ty] (void) {
                    // By default, we just call __c2rust_hash_T(x)
                    // where T is the type of the parameter
                    // FIXME: include shasher/ahasher
                    auto hash_fn_name = get_type_hash_function(result_ty, ctx, true);
                    return build_call(hash_fn_name.full_name(), ctx.UnsignedLongTy,
                                      { result }, ctx);
                };
                auto result_xcheck_stmts =
                    build_xcheck(result_xcheck, XCheck::Tag::FUNCTION_RETURN,
                                 ctx, result_xcheck_default_fn, param_custom_args_fn);
                add_body_stmts(result_xcheck_stmts);
            }
            if (func_cfg) {
                // Add exit_extra checks
                for (auto &ex : func_cfg->get().exit_extra) {
                    XCheck extra_xcheck{XCheck::CUSTOM, ex.custom};
                    auto extra_xcheck_stmts =
                        build_xcheck(extra_xcheck, ex.tag, ctx,
                                     extra_xcheck_default_fn, param_custom_args_fn);
                    add_body_stmts(extra_xcheck_stmts);
                }
            }

            // Add the final return
            auto return_stmt = new (ctx) ReturnStmt(SourceLocation(),
                                                    result, nullptr);
            new_body_stmts.push_back(return_stmt);

            auto new_body = new (ctx) CompoundStmt(ctx, new_body_stmts,
                                                   SourceLocation(),
                                                   SourceLocation());
            fd->setBody(new_body);
        } else if (VarDecl *vd = dyn_cast<VarDecl>(d)) {
            global_vars.emplace(llvm_string_ref_to_sv(vd->getName()), vd);
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
    Config config;

protected:
    std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &ci,
                                                   llvm::StringRef) override {
        return llvm::make_unique<CrossCheckInserter>(std::move(config));
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
}
