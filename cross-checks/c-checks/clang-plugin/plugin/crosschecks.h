#ifndef CROSSCHECK_PLUGIN_CROSSCHECKS_H
#define CROSSCHECK_PLUGIN_CROSSCHECKS_H

#include <algorithm>
#include <variant>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTMutationListener.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/Regex.h"

#include "config.h"

namespace crosschecks {

using namespace clang;

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

template<unsigned N, typename... Args>
static inline void
report_clang_warning(DiagnosticsEngine &diags,
                     const char (&fmt)[N],
                     Args&&... args) {
    unsigned diag_id =
        diags.getCustomDiagID(DiagnosticsEngine::Warning, fmt);
    auto db = diags.Report(diag_id);
    args_to_stream(db, std::forward<Args>(args)...);
}

using StringRef = std::reference_wrapper<const std::string>;

struct StringRefCompare {
    bool operator()(const StringRef &lhs, const StringRef &rhs) const {
        return lhs.get() < rhs.get();
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
    using Element = std::variant<std::string, std::string_view, llvm::StringRef>;

    HashFunctionName() {}
    HashFunctionName(std::string_view sv) : elements{1, sv}  {}
    HashFunctionName(std::string str)     : elements{1, str} {}
    HashFunctionName(llvm::StringRef sr)  : elements{1, sr}  {}

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
            } else if (auto *sr = std::get_if<llvm::StringRef>(&elem)) {
                res += *sr;
            } else {
                assert(false && "Invalid HashFunctionName::Element variant type");
            }
        }
        return res;
    }

private:
    std::vector<Element> elements;
};

struct HashFunction {
    HashFunctionName name;

    // The original type of the hashed value, as passed to get_type_hash_function
    QualType orig_ty;

    // The actual type of the main hash function argument "x",
    // as it appears in the function declaration
    QualType actual_ty;

    HashFunction() = delete;
    HashFunction(const HashFunctionName &n, QualType oty, QualType aty)
        : name(n), orig_ty(oty), actual_ty(aty) {
    }

    // Convert the main argument "x" from its original type
    // to the type in the hash function's signature
    Expr *forward_argument(Expr *arg, ASTContext &ctx) const {
        assert(arg->isLValue() && "Passed non-lvalue to forward_argument");
        CastKind ck = CK_LValueToRValue;
        QualType rv_ty = arg->getType().getCanonicalType();
        bool by_pointer = false;
        if (rv_ty->isRecordType()) {
            // We forward structure/union arguments by pointer, not by value
            by_pointer = true;
        } else if (rv_ty->isArrayType()) {
            // If the field is an array type T[...], decay it to
            // a pointer T*, and pass that pointer to the field
            // hash function
            ck = CK_ArrayToPointerDecay;
            rv_ty = ctx.getDecayedType(rv_ty);
        } else if (rv_ty->isFunctionType()) {
            // FIXME: we could instead set by_pointer=true here and
            // force a conversion to "void*", but the result
            // seems to be the same
            ck = CK_FunctionToPointerDecay;
            rv_ty = ctx.getDecayedType(rv_ty);
        }
        Expr *rv;
        if (by_pointer) {
            rv_ty = ctx.getPointerType(rv_ty);
            rv = new (ctx) UnaryOperator(arg, UO_AddrOf, rv_ty,
                                         VK_RValue, OK_Ordinary,
#if CLANG_VERSION_MAJOR >= 7
                                         SourceLocation(), false);
#else
                                         SourceLocation());
#endif
        } else {
            rv = ImplicitCastExpr::Create(ctx, rv_ty, ck, arg, nullptr, VK_RValue);
        }
        if (rv_ty != actual_ty) {
            rv = ImplicitCastExpr::Create(ctx, actual_ty, CK_BitCast, rv,
                                          nullptr, VK_RValue);
        }
        return rv;
    }
};

class CrossCheckInsertionAction;

class CrossCheckInserter : public SemaConsumer {
private:
    bool disable_xchecks;

    const config::Config *config;
    config::ScopeStack *config_stack;

    static const char default_config[];
    friend class CrossCheckInsertionAction;

    // Regex that matches cross-check annotations
    llvm::Regex xcheck_ann_regex{"^[:space:]*cross_check[:space:]*:(.*)$"};

    template<typename Fn>
    void parse_xcheck_attrs(Decl *decl, Fn fn) {
        for (const auto *aa : decl->specific_attrs<clang::AnnotateAttr>()) {
            auto ann = aa->getAnnotation();
            llvm::SmallVector<llvm::StringRef, 2> groups;
            if (xcheck_ann_regex.match(ann, &groups)) {
                fn(groups[1]);
            }
        }
    }

    // For now, the attributes we accept may optionally be enclosed in spaces
    // like a YAML mapping, e.g., "{ foo: bar }"; when passing the mapping to
    // the Rust configuration parser, we need to strip away the braces
    llvm::Regex xcheck_brace_regex{"^[:space:]*\\{(.*)\\}[:space:]*$"};

    template<typename Iter>
    llvm::SmallVector<std::string, 16>
    build_xcfg_yaml(llvm::StringRef item_name, llvm::StringRef item_kind,
                    clang::Decl *item_decl, llvm::StringRef extras_name, Iter extras) {
        using namespace std::literals;
        llvm::SmallVector<std::string, 16> yaml_strings;
        if (this->disable_xchecks) {
            auto xc = llvm::formatv("{{ name: {0}, item: {1}, disable_xchecks: true }",
                                    item_name, item_kind);
            yaml_strings.push_back(xc);
        }
        for (auto e_decl : extras) {
            auto e_name = e_decl->getName();
            parse_xcheck_attrs(e_decl, [this, &yaml_strings, item_name,
                                        item_kind, item_decl,
                                        extras_name, e_name] (llvm::StringRef s) {
                if (!s.empty()) {
                    auto xc = llvm::formatv("{{ name: {0}, item: {1}, {2}: {{ {3}: {4} } }",
                                            item_name, item_kind, extras_name, e_name, s);
                    yaml_strings.push_back(xc);
                }
            });
        }
        parse_xcheck_attrs(item_decl, [this, &yaml_strings, item_name,
                                       item_kind, item_decl] (llvm::StringRef s) {
            llvm::StringRef xchecks = s;
            llvm::SmallVector<llvm::StringRef, 2> groups;
            if (xcheck_brace_regex.match(s, &groups))
                xchecks = groups[1]; 
            if (!xchecks.empty()) {
                auto xc = llvm::formatv("{{ name: {0}, item: {1}, {2} }",
                                        item_name, item_kind, xchecks);
                yaml_strings.push_back(xc);
            }
        });
        return yaml_strings;
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
                                    llvm::ArrayRef<QualType> arg_tys,
                                    StorageClass sc,
                                    ASTContext &ctx);

    CallExpr *build_call(llvm::StringRef fn_name,
                         QualType result_ty,
                         llvm::ArrayRef<Expr*> args,
                         ASTContext &ctx);

    using ExprVec = llvm::SmallVector<Expr*, 4>;

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
#if CLANG_VERSION_MAJOR >= 7
                                                     SourceLocation(), false);
#else
                                                     SourceLocation());
#endif
                break;

            // *arg
            case CustomArg::DEREF:
                arg_ref_rv = new (ctx) UnaryOperator(arg_ref_lv, UO_Deref,
                                                     arg_ref_lv->getType()->getPointeeType(),
                                                     VK_RValue, OK_Ordinary,
#if CLANG_VERSION_MAJOR >= 7
                                                     SourceLocation(), false);
#else
                                                     SourceLocation());
#endif
                break;

            default:
                llvm_unreachable("Unknown CustomArg::Modifier case");
            }
            res.push_back(arg_ref_rv);
        }
        return res;
    }

    using TinyStmtVec = llvm::TinyPtrVector<Stmt*>;

    template<typename DefaultFn, typename CustomArgsFn>
    TinyStmtVec
    build_xcheck(const XCheck&, config::XCheckTag tag,
                 ASTContext &ctx, DefaultFn default_fn,
                 CustomArgsFn custom_args_fn);

    const HashFunction
    get_type_hash_function(QualType ty,
                           llvm::StringRef candidate_name,
                           ASTContext &ctx,
                           bool build_it);

    using StmtVec = llvm::SmallVector<Stmt*, 16>;

    // TODO: make it configurable via both a plugin argument
    // and an external configuration item
    static const size_t MAX_HASH_DEPTH = 8;

    Expr *build_max_hash_depth(ASTContext &ctx) {
        auto hash_depth_ty = ctx.getSizeType();
        llvm::APInt hash_depth(ctx.getTypeSize(hash_depth_ty), MAX_HASH_DEPTH);
        return IntegerLiteral::Create(ctx, hash_depth,
                                      hash_depth_ty,
                                      SourceLocation());
    }

    Expr *get_depth(FunctionDecl *fn_decl, bool sub1, ASTContext &ctx) {
        // Build `depth` or `depth - 1` as an Expr
        auto depth = fn_decl->getParamDecl(1);
        auto depth_ty = depth->getType();
        auto depth_rv =
            new (ctx) DeclRefExpr(depth, false, depth_ty,
                                         VK_RValue, SourceLocation());
        if (!sub1)
            return depth_rv;

        llvm::APInt one(ctx.getTypeSize(depth_ty), 1);
        auto one_lit = IntegerLiteral::Create(ctx, one, depth_ty,
                                                     SourceLocation());
        return new (ctx) BinaryOperator(depth_rv, one_lit,
                                               BO_Sub, depth_ty,
                                               VK_RValue, OK_Ordinary,
                                               SourceLocation(),
                                               FPOptions{});
    }

    Stmt *build_depth_check(FunctionDecl *fn_decl,
                            std::string_view item,
                            ASTContext &ctx);

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

    std::tuple<VarDecl*, Expr*, StmtVec>
    build_hasher_init(const std::string &hasher_prefix,
                      FunctionDecl *parent,
                      ASTContext &ctx);

    template<typename BodyFn>
    void build_generic_hash_function(const HashFunction &func,
                                     ASTContext &ctx,
                                     BodyFn body_fn);

    void build_pointer_hash_function(const HashFunction &func,
                                     const HashFunction &pointee,
                                     ASTContext &ctx);

    void build_array_hash_function(const HashFunction &func,
                                   const HashFunction &element,
                                   const llvm::APInt &num_elements,
                                   ASTContext &ctx);

    void build_record_hash_function(const HashFunction &func,
                                    const std::string &record_name,
                                    ASTContext &ctx);

    TinyStmtVec
    build_parameter_xcheck(ParmVarDecl *param,
                           llvm::StringRef func_name,
                           const config::ScopeConfig *func_cfg,
                           const DeclMap &param_decls,
                           ASTContext &ctx);

    class ZeroInitVisitor : public RecursiveASTVisitor<ZeroInitVisitor> {
    public:
        ZeroInitVisitor() = delete;
        ZeroInitVisitor(ASTContext &ctx_) : ctx(ctx_) {}

        bool VisitVarDecl(VarDecl *vd) {
            if (vd->isLocalVarDecl() && vd->hasLocalStorage() &&
                !vd->isImplicit()    && !vd->hasInit()) {
                // Zero-initialize each uninitialized local
                // by setting it to an ImplicitValueInitExpr
                auto init = new (ctx) ImplicitValueInitExpr(vd->getType());
                vd->setInit(init);
            }
            return true;
        }

    private:
        ASTContext &ctx;
    };

public:
    CrossCheckInserter() = delete;
    CrossCheckInserter(bool dx, const config::Config *cfg)
            : disable_xchecks(dx), config(cfg) {
        config_stack = config::xcfg_scope_stack_new(nullptr);
    }

    ~CrossCheckInserter() {
        if (config_stack) {
            config::xcfg_scope_stack_destroy(config_stack);
            config_stack = nullptr;
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

} // namespace crosschecks

#endif // CROSSCHECK_PLUGIN_CROSSCHECKS_H
