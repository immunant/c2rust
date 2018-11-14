#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTMutationListener.h"
#include "clang/Basic/Version.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaConsumer.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/StringExtras.h"

#include "config.h"
#include "crosschecks.h"

namespace crosschecks {

using namespace std::literals;

const HashFunction
CrossCheckInserter::get_type_hash_function(QualType ty, llvm::StringRef candidate_name,
                                           ASTContext &ctx, bool build_it) {
    switch (ty->getTypeClass()) {
    case Type::Builtin: {
        switch (cast<BuiltinType>(ty)->getKind()) {
        case BuiltinType::Void:
            // TODO: we should never actually be able to hash a void
            return HashFunction{"void"sv,    ty, ctx.VoidTy};
        case BuiltinType::Bool:
            return HashFunction{"bool"sv,    ty, ctx.BoolTy};
        case BuiltinType::Char_S:
        case BuiltinType::Char_U:
            return HashFunction{"char"sv,    ty, ctx.CharTy};
        case BuiltinType::UChar:
            return HashFunction{"uchar"sv,   ty, ctx.UnsignedCharTy};
        case BuiltinType::UShort:
            return HashFunction{"ushort"sv,  ty, ctx.UnsignedShortTy};
        case BuiltinType::UInt:
            return HashFunction{"uint"sv,    ty, ctx.UnsignedIntTy};
        case BuiltinType::ULong:
            return HashFunction{"ulong"sv,   ty, ctx.UnsignedLongTy};
        case BuiltinType::ULongLong:
            return HashFunction{"ullong"sv,  ty, ctx.UnsignedLongLongTy};
        case BuiltinType::SChar:
            return HashFunction{"schar"sv,   ty, ctx.SignedCharTy};
        case BuiltinType::Short:
            return HashFunction{"short"sv,   ty, ctx.ShortTy};
        case BuiltinType::Int:
            return HashFunction{"int"sv,     ty, ctx.IntTy};
        case BuiltinType::Long:
            return HashFunction{"long"sv,    ty, ctx.LongTy};
        case BuiltinType::LongLong:
            return HashFunction{"llong"sv,   ty, ctx.LongLongTy};
        case BuiltinType::Float:
            return HashFunction{"float"sv,   ty, ctx.FloatTy};
        case BuiltinType::Double:
            return HashFunction{"double"sv,  ty, ctx.DoubleTy};
        case BuiltinType::LongDouble:
            return HashFunction{"ldouble"sv, ty, ctx.LongDoubleTy};
        default:
            ty->dump();
            llvm_unreachable("Unknown/unhandled builtin type");
        }
        break;
    }

    case Type::Enum: {
        // Cross-check an enum type as the underlying integer type
        auto *ed = cast<EnumType>(ty)->getDecl();
        return get_type_hash_function(ed->getIntegerType(),
                                      candidate_name, ctx, build_it);
    }

    case Type::FunctionNoProto:
    case Type::FunctionProto:
        return HashFunction{"function"sv, ty, ctx.getPointerType(ctx.VoidTy)};

    case Type::Pointer: {
        auto pointee_ty = cast<PointerType>(ty)->getPointeeType();
        auto pointee = get_type_hash_function(pointee_ty, candidate_name, ctx, build_it);
        HashFunction func{pointee.name, ty, ctx.getPointerType(pointee.actual_ty)};
        func.name.append("ptr"sv);
        if (build_it) {
            build_pointer_hash_function(func, pointee, ctx);
        }
        return func;
    }

    case Type::ConstantArray: {
        auto array_ty = cast<ConstantArrayType>(ty);
        auto element_ty = array_ty->getElementType();
        auto element = get_type_hash_function(element_ty, candidate_name, ctx, build_it);
        auto num_elements = array_ty->getSize();
        HashFunction func{element.name, ty, ctx.getPointerType(element.actual_ty)};
        func.name.append("array"sv);
        func.name.append(llvm::utostr(num_elements.getZExtValue()));
        if (build_it) {
            build_array_hash_function(func, element, num_elements, ctx);
        }
        return func;
    }

    case Type::IncompleteArray: {
        // For now, we hash incomplete arrays as 1-element arrays
        // FIXME: the array may be empty
        auto array_ty = cast<IncompleteArrayType>(ty);
        auto element_ty = array_ty->getElementType();
        auto element = get_type_hash_function(element_ty, candidate_name, ctx, build_it);
        HashFunction func{element.name, ty, ctx.getPointerType(element.actual_ty)};
        func.name.append("incarray"sv);
        if (build_it) {
            llvm::APInt one(ctx.getTypeSize(ctx.getSizeType()), 1);
            build_array_hash_function(func, element, one, ctx);
        }
        return func;
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
        auto record_id = record_decl->getIdentifier();
        if (record_id != nullptr)
            candidate_name = record_id->getName();
        if (candidate_name.empty()) {
            record_ty->dump();
            report_clang_error(ctx.getDiagnostics(),
                               "unable to retrieve record name");
        }
        HashFunction func{candidate_name, ty,
            ctx.getPointerType(ty.getUnqualifiedType())};
        func.name.append(record_decl->getKindName().str());
        if (build_it) {
            build_record_hash_function(func, candidate_name, ctx);
        }
        return func;
    }

    case Type::Typedef: {
        // If this type is an anonymous record, e.g.
        // `struct { ... };`, then we try to use the name
        // of the inner-most typedef instead, since a lot of C code
        // defines structures using the `typedef struct` pattern:
        // `typedef struct { ... } struct_name;`
        auto *td = ty->getAs<TypedefType>();
        auto td_id = td->getDecl()->getIdentifier();
        if (td_id != nullptr)
            candidate_name = td_id->getName();
        return get_type_hash_function(td->desugar(), candidate_name, ctx, build_it);
    }

    case Type::Elaborated:
        return get_type_hash_function(ty->getAs<ElaboratedType>()->desugar(),
                                      candidate_name, ctx, build_it);

    case Type::Paren:
        return get_type_hash_function(ty->getAs<ParenType>()->desugar(),
                                      candidate_name, ctx, build_it);

    case Type::Attributed:
        return get_type_hash_function(ty->getAs<AttributedType>()->desugar(),
                                      candidate_name, ctx, build_it);

    case Type::Adjusted:
        return get_type_hash_function(ty->getAs<AdjustedType>()->getOriginalType(),
                                      candidate_name, ctx, build_it);

    default:
        ty->dump(llvm::errs());
        llvm_unreachable("unimplemented");
    }
}

Stmt *CrossCheckInserter::build_depth_check(FunctionDecl *fn_decl,
                                            std::string_view item,
                                            ASTContext &ctx) {
    // Build the following code:
    // if (depth == 0)
    //   return __c2rust_hash_$item_leaf();
    auto depth = get_depth(fn_decl, false, ctx);
    auto depth_ty = depth->getType();
    llvm::APInt zero(ctx.getTypeSize(depth_ty), 0);
    auto zero_lit = IntegerLiteral::Create(ctx, zero, depth_ty, SourceLocation());
    auto depth_cmp = new (ctx) BinaryOperator(depth, zero_lit,
                                              BO_EQ, ctx.IntTy,
                                              VK_RValue, OK_Ordinary,
                                              SourceLocation(),
                                              FPOptions{});

    HashFunctionName hash_leaf_func{item};
    hash_leaf_func.append("leaf"sv);
    auto hash_leaf_call = build_call(hash_leaf_func.full_name(),
                                     ctx.UnsignedLongTy, { }, ctx);
    auto return_hash_leaf =
        new (ctx) ReturnStmt(SourceLocation(), hash_leaf_call, nullptr);
    return new (ctx) IfStmt(ctx, SourceLocation(), false,
                            nullptr, nullptr, depth_cmp,
                            return_hash_leaf, SourceLocation(), nullptr);

}

std::tuple<VarDecl*, Expr*, CrossCheckInserter::StmtVec>
CrossCheckInserter::build_hasher_init(const std::string &hasher_prefix,
                                      FunctionDecl *parent,
                                      ASTContext &ctx) {
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
        VarDecl::Create(ctx, parent, SourceLocation(), SourceLocation(),
                        hasher_id, hasher_ty, nullptr, SC_None);
    auto hasher_var_decl_stmt =
        new (ctx) DeclStmt(DeclGroupRef(hasher_var),
                           SourceLocation(),
                           SourceLocation());

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
    return { hasher_var, hasher_var_ptr, { hasher_var_decl_stmt, init_call } };
}

template<typename BodyFn>
void CrossCheckInserter::build_generic_hash_function(const HashFunction &func,
                                                     ASTContext &ctx,
                                                     BodyFn body_fn) {
    auto full_name = func.name.full_name();
    auto fn_decl = get_function_decl(full_name,
                                     ctx.UnsignedLongTy,
                                     { func.actual_ty, ctx.getSizeType() },
                                     SC_Extern,
                                     ctx);
    if (fn_decl->hasBody())
        return; // We've already built it

    // If we're already in the process of building this function,
    // just leave it as a body-less Decl
    auto [it, inserted] = pending_hash_functions.insert(std::cref(full_name));
    if (!inserted)
        return; // Function is already pending

    auto fn_body_stmts = body_fn(fn_decl);
    auto fn_body =
#if CLANG_VERSION_MAJOR >= 6
        CompoundStmt::Create(ctx, fn_body_stmts,
#else
        new (ctx) CompoundStmt(ctx, fn_body_stmts,
#endif
                               SourceLocation(),
                               SourceLocation());

    // Put this function in a linkonce section, so the linker merges
    // all duplicate copies of it into one during linking
    auto fn_section = ".gnu.linkonce.t."s + full_name;
    fn_decl->addAttr(SectionAttr::CreateImplicit(ctx, SectionAttr::GNU_section,
                                                 fn_section));
    fn_decl->addAttr(VisibilityAttr::CreateImplicit(ctx, VisibilityAttr::Hidden));
    fn_decl->setInlineSpecified(true);
    fn_decl->setBody(fn_body);
    new_funcs.push_back(fn_decl);
    // TODO: add it to the parent DeclContext???

    pending_hash_functions.erase(full_name);
}

void CrossCheckInserter::build_pointer_hash_function(const HashFunction &func,
                                                     const HashFunction &pointee,
                                                     ASTContext &ctx) {
    // __c2rust_hash_void_ptr is implemented in the runtime
    if (pointee.orig_ty->isVoidType())
        return;
    if (pointee.orig_ty->isIncompleteType() &&
        !pointee.orig_ty->isRecordType()) {
        // We only allow pointers to incomplete structures
        return;
    }

    // Build the pointer hash function using this template:
    // uint64_t __c2rust_hash_T_ptr(T *x, size_t depth) {
    //   if (__c2rust_pointer_is_invalid(x))
    //      return __c2rust_hash_invalid_pointer(x);
    //   if (depth == 0)
    //      return __c2rust_hash_pointer_leaf();
    //   return __c2rust_hash_T(*x, depth - 1);
    // }
    //
    // TODO: add a depth parameter and decrement it on recursion
    auto body_fn = [this, &ctx, &pointee] (FunctionDecl *fn_decl) -> StmtVec {
        assert(fn_decl->getNumParams() == 2 &&
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
        // Convert from T* to a void*
        auto param_void_ref_rv =
            ImplicitCastExpr::Create(ctx, ctx.getPointerType(ctx.VoidTy),
                                     CK_BitCast, param_ref_rv,
                                     nullptr, VK_RValue);
        auto is_invalid_call =
            build_call("__c2rust_pointer_is_invalid", ctx.BoolTy,
                       { param_void_ref_rv }, ctx);
        // Build the call to __c2rust_hash_invalid_pointer
        auto hash_invalid_call =
            build_call("__c2rust_hash_invalid_pointer", ctx.UnsignedLongTy,
                       { param_void_ref_rv }, ctx);
        auto return_hash_invalid =
            new (ctx) ReturnStmt(SourceLocation(), hash_invalid_call, nullptr);
        auto if_invalid =
            new (ctx) IfStmt(ctx, SourceLocation(), false,
                             nullptr, nullptr, is_invalid_call,
                             return_hash_invalid, SourceLocation(), nullptr);

        auto depth_check = build_depth_check(fn_decl, "pointer", ctx);

        // Build the call to the pointee function
        auto param_deref_lv =
            new (ctx) UnaryOperator(param_ref_lv, UO_Deref, pointee.orig_ty,
                                    VK_LValue, OK_Ordinary,
#if CLANG_VERSION_MAJOR >= 7
                                    SourceLocation(), false);
#else
                                    SourceLocation());
#endif
        auto param_deref_rv = pointee.forward_argument(param_deref_lv, ctx);
        auto param_depth = get_depth(fn_decl, true, ctx);
        auto param_hash_call =
            build_call(pointee.name.full_name(), ctx.UnsignedLongTy,
                       { param_deref_rv, param_depth }, ctx);

        // Build the conditional expression and return statement
        auto return_hash_stmt =
            new (ctx) ReturnStmt(SourceLocation(), param_hash_call, nullptr);
        return { if_invalid, depth_check, return_hash_stmt };
    };
    build_generic_hash_function(func, ctx, body_fn);
}

void CrossCheckInserter::build_array_hash_function(const HashFunction &func,
                                                   const HashFunction &element,
                                                   const llvm::APInt &num_elements,
                                                   ASTContext &ctx) {
    if (element.orig_ty->isIncompleteType()) {
        // TODO: figure out what to do about this
        // for now, we just expect the user to provide a custom function
        return;
    }

    // Build the following code:
    // uint64_t __c2rust_hash_T_array_N(T x[N], size_t depth) {
    //   if (depth == 0)
    //      return __c2rust_hash_array_leaf();
    //
    //   char hasher[__c2rust_hasher_H_size()];
    //   __c2rust_hasher_H_init(hasher);
    //   for (size_t i = 0; i < N; i++)
    //      __c2rust_hasher_H_update(hasher, __c2rust_hash_T(x[i]));
    //   return __c2rust_hasher_H_finish(hasher);
    // }
    //
    // TODO: allow custom hashers instead of the default "jodyhash"
    std::string hasher_name{"jodyhash"};
    std::string hasher_prefix{"__c2rust_hasher_"};
    hasher_prefix += hasher_name;
    auto body_fn =
            [this, &ctx, &element, &num_elements,
             hasher_prefix = std::move(hasher_prefix)]
            (FunctionDecl *fn_decl) -> StmtVec {
        StmtVec stmts;
        auto depth_check = build_depth_check(fn_decl, "array"sv, ctx);
        stmts.push_back(depth_check);

        auto [hasher_var, hasher_var_ptr, hasher_init_stmts] =
            build_hasher_init(hasher_prefix, fn_decl, ctx);
        stmts.insert(stmts.end(),
                     std::make_move_iterator(hasher_init_stmts.begin()),
                     std::make_move_iterator(hasher_init_stmts.end()));

        // size_t i = 0;
        auto i_id = &ctx.Idents.get("i");
        auto i_ty = ctx.getSizeType();
        auto i_var = VarDecl::Create(ctx, fn_decl, SourceLocation(), SourceLocation(),
                                     i_id, i_ty, nullptr, SC_None);
        llvm::APInt zero(ctx.getTypeSize(i_ty), 0);
        auto i_init = IntegerLiteral::Create(ctx, zero, i_ty, SourceLocation());
        i_var->setInit(i_init);
        auto i_decl_stmt = new (ctx) DeclStmt(DeclGroupRef(i_var),
                                              SourceLocation(),
                                              SourceLocation());
        // i < N
        auto i_var_lv = new (ctx) DeclRefExpr(i_var, false, i_ty,
                                              VK_LValue, SourceLocation());
        auto i_var_rv = ImplicitCastExpr::Create(ctx, i_var_lv->getType(),
                                                 CK_LValueToRValue,
                                                 i_var_lv, nullptr, VK_RValue);
        auto num_elems_lit = IntegerLiteral::Create(ctx, num_elements, i_ty,
                                                    SourceLocation());
        auto loop_cond = new (ctx) BinaryOperator(i_var_rv, num_elems_lit,
                                                  BO_LT, ctx.IntTy,
                                                  VK_RValue, OK_Ordinary,
                                                  SourceLocation(),
                                                  FPOptions{});
        // i++
        auto i_incr = new (ctx) UnaryOperator(i_var_lv, UO_PostInc,
                                              i_ty, VK_RValue, OK_Ordinary,
#if CLANG_VERSION_MAJOR >= 7
                                              SourceLocation(),
                                              num_elements.isMaxValue());
#else
                                              SourceLocation());
#endif
        // Loop body: __c2rust_hasher_H_update(hasher, __c2rust_hash_T(x[i]));
        assert(!element.orig_ty->isIncompleteType() &&
               "Attempting to dereference incomplete type");
        auto param = fn_decl->getParamDecl(0);
        auto param_ty = param->getType();
        auto param_ref_lv =
            new (ctx) DeclRefExpr(param, false, param_ty,
                                  VK_LValue, SourceLocation());
        auto param_i_lv = new (ctx) ArraySubscriptExpr(param_ref_lv, i_var_rv,
                                                       element.orig_ty, VK_LValue,
                                                       OK_Ordinary, SourceLocation());
        auto param_i_rv = element.forward_argument(param_i_lv, ctx);
        auto param_i_hash_fn = element.name.full_name();
        auto param_i_depth = get_depth(fn_decl, true, ctx);
        auto param_i_hash = build_call(param_i_hash_fn, ctx.UnsignedLongTy,
                                       { param_i_rv, param_i_depth }, ctx);
        auto update_call = build_call(hasher_prefix + "_update", ctx.VoidTy,
                                      { hasher_var_ptr, param_i_hash }, ctx);

        // Put everything together
        auto loop = new (ctx) ForStmt(ctx, i_decl_stmt, loop_cond,
                                      nullptr, i_incr, update_call,
                                      SourceLocation(), SourceLocation(),
                                      SourceLocation());
        stmts.push_back(loop);

        // Return the result of the finish function
        auto finish_call = build_call(hasher_prefix + "_finish",
                                      ctx.UnsignedLongTy,
                                      { hasher_var_ptr }, ctx);
        auto return_stmt =
            new (ctx) ReturnStmt(SourceLocation(), finish_call, nullptr);
        stmts.push_back(return_stmt);
        return stmts;
    };
    build_generic_hash_function(func, ctx, body_fn);
}

void CrossCheckInserter::build_record_hash_function(const HashFunction &func,
                                                    const std::string &record_name,
                                                    ASTContext &ctx) {
    auto &diags = ctx.getDiagnostics();
    auto record_ty = cast<RecordType>(func.orig_ty);
    auto record_decl = record_ty->getDecl();

    llvm::StringRef file_name;
    auto ploc = ctx.getSourceManager().getPresumedLoc(record_decl->getLocStart());
    if (ploc.isValid()) {
        file_name = ploc.getFilename();
    }

    unsigned pushed_files = 0; // FIXME: use a scope guard
    auto file_cfg = xcfg_scope_stack_push_file(config_stack, config, file_name);
    if (file_cfg != nullptr)
        pushed_files++;

    auto pre_xcfg_strings = build_xcfg_yaml(record_name, "struct",
                                            record_decl, "fields",
                                            record_decl->fields());
    llvm::SmallVector<config::StringLenPtr, 16> pre_xcfg_slps;
    for (auto &s : pre_xcfg_strings)
        pre_xcfg_slps.push_back(config::StringLenPtr{s});
    auto record_cfg =
        xcfg_scope_stack_push_item(config_stack, config::ITEM_KIND_STRUCT,
                                   file_name, record_name,
                                   config::StringVec::from_vector(pre_xcfg_slps),
                                   {});
    pushed_files++;

    if (!config::xcfg_scope_enabled(record_cfg)) {
        // Cross-checks are disabled for this record
        xcfg_scope_stack_pop_multi(config_stack, pushed_files);
        return;
    }

    auto custom_hash = xcfg_scope_struct_custom_hash(record_cfg);
    if (!custom_hash.is_empty()) {
        // The user specified a "custom_hash" function, so just forward
        // the structure to it
        // FIXME: would be nice to not have to emit a function body,
        // and instead declare our function using "alias", e.g.:
        // uint64_t __c2rust_hash_T_struct(struct T *x) __attribute__((alias("...")));
        auto body_fn = [this, &ctx, custom_hash, &func] (FunctionDecl *fn_decl) -> StmtVec {
            auto param = fn_decl->getParamDecl(0);
            auto param_ty = param->getType();
            auto param_ref_lv =
                new (ctx) DeclRefExpr(param, false, param_ty,
                                      VK_LValue, SourceLocation());
            auto param_ref_rv = func.forward_argument(param_ref_lv, ctx);
            auto new_depth = get_depth(fn_decl, false, ctx);
            auto hash_fn_call = build_call(custom_hash,
                                           ctx.UnsignedLongTy,
                                           { param_ref_rv, new_depth }, ctx);
            auto return_stmt =
                new (ctx) ReturnStmt(SourceLocation(), hash_fn_call, nullptr);
            return { return_stmt };
        };
        build_generic_hash_function(func, ctx, body_fn);
        xcfg_scope_stack_pop_multi(config_stack, pushed_files);
        return;
    }

    // Build the following code:
    // uint64_t __c2rust_hash_T_struct(struct T *x, size_t depth) {
    //   if (depth == 0)
    //      return __c2rust_hash_record_leaf();
    //
    //   char hasher[__c2rust_hasher_H_size()];
    //   __c2rust_hasher_H_init(hasher);
    //   __c2rust_hasher_H_update(hasher, __c2rust_hash_F1(x.field1, depth - 1));
    //   __c2rust_hasher_H_update(hasher, __c2rust_hash_F2(x.field2, depth - 1));
    //   ...
    //   return __c2rust_hasher_H_finish(hasher);
    // }
    //
    // TODO: allow custom hashers instead of the default "jodyhash"
    auto record_def = record_decl->getDefinition();
    if (record_def == nullptr) {
#if 0 // Assume some other file provides an implementation for this
        report_clang_error(diags, "default cross-checking is not supported for incomplete structures, "
                                  "please use a custom cross-check for '%0'",
                                  record_decl->getDeclName().getAsString());
#endif
        xcfg_scope_stack_pop_multi(config_stack, pushed_files);
        return;
    }
    if (record_def->isUnion()) {
        report_clang_warning(diags, "emitting generic 'AnyUnion' cross-check for union, "
                                    "please use a custom cross-check for '%0'",
                                    record_name);
        // uint64_t __c2rust_hash_T_union(struct T *x, size_t depth) {
        //   if (depth == 0)
        //      return __c2rust_hash_record_leaf();
        //   return __c2rust_hash_anyunion();
        // }
        auto body_fn = [this, &ctx] (FunctionDecl *fn_decl) -> StmtVec {
            auto depth_check = build_depth_check(fn_decl, "record"sv, ctx);
            auto anyunion_call = build_call("__c2rust_hash_anyunion",
                                             ctx.UnsignedLongTy, { }, ctx);
            auto return_stmt =
                new (ctx) ReturnStmt(SourceLocation(), anyunion_call, nullptr);
            return { depth_check, return_stmt };
        };
        build_generic_hash_function(func, ctx, body_fn);
        xcfg_scope_stack_pop_multi(config_stack, pushed_files);
        return;
    }
    assert((record_def->isStruct() || record_def->isClass()) &&
           "Called build_record_hash_function on neither a struct nor a class");

    std::string hasher_name{"jodyhash"};
    auto field_hasher = xcfg_scope_struct_field_hasher(record_cfg);
    if (!field_hasher.is_empty()) {
        hasher_name = field_hasher;
    }
    std::string hasher_prefix{"__c2rust_hasher_"};
    hasher_prefix += hasher_name;
    auto body_fn =
            [this, &ctx, &record_def, &record_name, record_cfg,
             hasher_prefix = std::move(hasher_prefix)]
            (FunctionDecl *fn_decl) -> StmtVec {
        StmtVec stmts;
        auto depth_check = build_depth_check(fn_decl, "record"sv, ctx);
        stmts.push_back(depth_check);

        auto [hasher_var, hasher_var_ptr, hasher_init_stmts] =
            build_hasher_init(hasher_prefix, fn_decl, ctx);
        stmts.insert(stmts.end(),
                     std::make_move_iterator(hasher_init_stmts.begin()),
                     std::make_move_iterator(hasher_init_stmts.end()));

        // Add the field calls
        auto param = fn_decl->getParamDecl(0);
        auto field_depth = get_depth(fn_decl, true, ctx);
        DeclMap field_decls;
        for (auto *field : record_def->fields()) {
            field_decls.emplace(llvm_string_ref_to_sv(field->getName()), field);
        }
        for (auto *field : record_def->fields()) {
            if (field->isUnnamedBitfield())
                continue; // Unnamed bitfields only affect layout, not contents
            if (field->isBitField()) {
                auto &diags = ctx.getDiagnostics();
                auto record_decl = field->getParent();
                report_clang_error(diags, "default cross-checking is not supported for bitfields, "
                                          "please use a custom cross-check for '%0'",
                                          record_name);
                return {};
            }

            XCheck field_xcheck{xcfg_scope_struct_field(record_cfg, field->getName())};
            if (field_xcheck.type == config::XCHECK_TYPE_DISABLED)
                continue;

            Expr *field_hash = nullptr;
            if (field_xcheck.type == config::XCHECK_TYPE_FIXED) {
                auto field_hash_val = field_xcheck.data_u64;
                field_hash = IntegerLiteral::Create(ctx,
                                                    llvm::APInt(64, field_hash_val),
                                                    ctx.UnsignedLongTy,
                                                    SourceLocation());
            } else {
                auto param_ref_rv =
                    new (ctx) DeclRefExpr(param, false, param->getType(),
                                          VK_RValue, SourceLocation());
                std::string field_hash_fn_name;
                ExprVec field_hash_args;
                if (field_xcheck.type == config::XCHECK_TYPE_CUSTOM) {
                    std::string_view xcheck_data = field_xcheck.data_str;
                    auto field_hash_fn_sig = parse_custom_xcheck(xcheck_data, ctx);
                    field_hash_fn_name = std::string{std::get<0>(field_hash_fn_sig)};

                    // Build the argument vector
                    auto &args = std::get<1>(field_hash_fn_sig);
                    auto arg_build_fn = [&ctx, param_ref_rv] (DeclaratorDecl *decl) {
                        return new (ctx) MemberExpr(param_ref_rv, true, SourceLocation(),
                                                    decl, SourceLocation(),
                                                    decl->getType(), VK_LValue,
                                                    OK_Ordinary);
                    };
                    field_hash_args = generic_custom_args(ctx, field_decls,
                                                          args, arg_build_fn);
                } else {
                    std::string field_ty_name = record_name;
                    field_ty_name += "$field$";
                    field_ty_name += field->getName();
                    auto field_ty = field->getType();
                    auto field_hash_fn = get_type_hash_function(field_ty, field_ty_name, ctx, true);
                    field_hash_fn_name = field_hash_fn.name.full_name();
                    auto field_ref_lv =
                        new (ctx) MemberExpr(param_ref_rv, true, SourceLocation(),
                                             field, SourceLocation(),
                                             field->getType(), VK_LValue, OK_Ordinary);
                    auto field_ref_rv = field_hash_fn.forward_argument(field_ref_lv, ctx);
                    field_hash_args.push_back(field_ref_rv);
                }
                field_hash_args.push_back(field_depth);
                field_hash = build_call(field_hash_fn_name,
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
    build_generic_hash_function(func, ctx, body_fn);
    xcfg_scope_stack_pop_multi(config_stack, pushed_files);
}

} // namespace crosschecks
