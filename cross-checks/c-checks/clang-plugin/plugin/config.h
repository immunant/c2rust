#ifndef CROSSCHECK_PLUGIN_CONFIG_H
#define CROSSCHECK_PLUGIN_CONFIG_H

#include "llvm/ADT/StringRef.h"

#include <string>

namespace crosschecks {
namespace config {

enum XCheckType : unsigned {
    XCHECK_TYPE_DEFAULT  = 0,
    XCHECK_TYPE_DISABLED = 1,
    XCHECK_TYPE_FIXED    = 2,
    XCHECK_TYPE_DJB2     = 3,
    XCHECK_TYPE_AS_TYPE  = 4,
    XCHECK_TYPE_CUSTOM   = 5,
};

enum XCheckTag : unsigned {
    XCHECK_TAG_UNKNOWN         = 0,
    XCHECK_TAG_FUNCTION_ENTRY  = 1,
    XCHECK_TAG_FUNCTION_EXIT   = 2,
    XCHECK_TAG_FUNCTION_ARG    = 3,
    XCHECK_TAG_FUNCTION_RETURN = 4,
};

enum ItemKind : unsigned {
    ITEM_KIND_FUNCTION = 0,
    ITEM_KIND_STRUCT   = 1,
    ITEM_KIND_IMPL     = 2,
};

struct Config;
struct ExtraXCheck;
struct ScopeConfig;
struct ScopeStack;
struct XCheck;

struct StringLenPtr {
    const char *ptr;
    unsigned len;

    StringLenPtr(const std::string &s)
        : ptr(s.data()), len(s.size()) {
    }

    StringLenPtr(const llvm::StringRef sr)
        : ptr(sr.data()), len(sr.size()) {
    }

    StringLenPtr(const std::string_view sv)
        : ptr(sv.data()), len(sv.size()) {
    }

    operator llvm::StringRef() const {
        return llvm::StringRef{ptr, len};
    }

    operator std::string_view() const {
        return std::string_view{ptr, len};
    }

    bool is_empty() const {
        return ptr == nullptr || len == 0;
    }
};

template<typename T>
struct VecLenPtr {
    const T *ptr;
    unsigned elem_size;
    unsigned len;

    VecLenPtr() : ptr(nullptr), elem_size(sizeof(T)), len(0) {
    }
    VecLenPtr(T *p, unsigned l) : ptr(p), elem_size(sizeof(T)), len(l) {
    }

    template<typename V>
    static VecLenPtr from_vector(V &v) {
        return VecLenPtr{&*v.begin(), std::distance(v.begin(), v.end())};
    }

    class Iterator {
    public:
        Iterator() = delete;
        Iterator(const VecLenPtr<T> &v, unsigned i) : vec(v), idx(i) {
        }

        bool operator==(const Iterator &o) const {
            return vec.ptr == o.vec.ptr && vec.elem_size == o.vec.elem_size &&
                   vec.len == o.vec.len && idx == o.idx;
        }

        bool operator!=(const Iterator &o) const {
            return !(*this == o);
        }

        T &operator*() {
            auto base_ptr = reinterpret_cast<uint8_t*>(const_cast<T*>(vec.ptr));
            auto offset = vec.elem_size * idx;
            return *reinterpret_cast<T*>(base_ptr + offset);
        }

        T &operator++() {
            if (idx < vec.len)
                ++idx;
            return **this;
        }

    private:
        const VecLenPtr &vec;
        unsigned idx;
    };

    Iterator begin() const {
        return Iterator{*this, 0};
    }

    Iterator end() const {
        return Iterator{*this, len};
    }
};

using StringVec = VecLenPtr<StringLenPtr>;

extern "C" {
const Config *xcfg_config_new(void);
const Config *xcfg_config_parse(const Config*, StringLenPtr);
void xcfg_config_destroy(Config*);
unsigned xcfg_xcheck_type(const XCheck*);
uint64_t xcfg_xcheck_data_u64(const XCheck*);
StringLenPtr xcfg_xcheck_data_string(const XCheck*);
unsigned xcfg_extra_xcheck_tag(const ExtraXCheck*);
StringLenPtr xcfg_extra_xcheck_custom(const ExtraXCheck*);
ScopeStack *xcfg_scope_stack_new(ScopeConfig*);
void xcfg_scope_stack_destroy(ScopeStack*);
ScopeConfig *xcfg_scope_stack_push_file(ScopeStack*, const Config*, StringLenPtr);
ScopeConfig *xcfg_scope_stack_push_item(ScopeStack*, unsigned, StringLenPtr, StringLenPtr,
                                        StringVec, StringVec);
void xcfg_scope_stack_pop(ScopeStack*);
void xcfg_scope_stack_pop_multi(ScopeStack*, unsigned);
ScopeConfig *xcfg_scope_stack_last(ScopeStack*);
unsigned xcfg_scope_enabled(const ScopeConfig*);
XCheck *xcfg_scope_entry_xcheck(const ScopeConfig*);
XCheck *xcfg_scope_exit_xcheck(const ScopeConfig*);
XCheck *xcfg_scope_all_args_xcheck(const ScopeConfig*);
XCheck *xcfg_scope_ret_xcheck(const ScopeConfig*);
StringLenPtr xcfg_scope_ahasher(const ScopeConfig*);
StringLenPtr xcfg_scope_shasher(const ScopeConfig*);
XCheck *xcfg_scope_function_arg(const ScopeConfig*, StringLenPtr, unsigned);
VecLenPtr<ExtraXCheck> xcfg_scope_function_entry_extra(const ScopeConfig*);
VecLenPtr<ExtraXCheck> xcfg_scope_function_exit_extra(const ScopeConfig*);
StringLenPtr xcfg_scope_struct_custom_hash(const ScopeConfig*);
StringLenPtr xcfg_scope_struct_field_hasher(const ScopeConfig*);
XCheck *xcfg_scope_struct_field(const ScopeConfig*, StringLenPtr);
} // extern "C"

struct Config {
public:
    // std::unique_ptr calls this
    static inline void operator delete(void *ptr) {
        if (ptr != nullptr)
            xcfg_config_destroy(reinterpret_cast<Config*>(ptr));
    }

private:
    // We should never construct one of these from C++
    Config() = delete;
    Config(const Config&) = delete;
    Config(Config&&) = delete;
    Config &operator=(const Config&) = delete;
    Config &operator=(Config&&) = delete;
};

struct ScopeStack {
public:
    // std::unique_ptr calls this
    static inline void operator delete(void *ptr) {
        if (ptr != nullptr)
            xcfg_scope_stack_destroy(reinterpret_cast<ScopeStack*>(ptr));
    }

private:
    // We should never construct one of these from C++
    ScopeStack() = delete;
    ScopeStack(const ScopeStack&) = delete;
    ScopeStack(ScopeStack&&) = delete;
    ScopeStack &operator=(const ScopeStack&) = delete;
    ScopeStack &operator=(ScopeStack&&) = delete;
};

} // namespace config

// Our own wrapper over Rust's XCheckType values;
// this stores a local copy of the data, so we can
// build our own XCheck values without calling into Rust
struct XCheck {
    unsigned type;
    union {
        uint64_t data_u64;
        config::StringLenPtr data_str;
    };

    XCheck() = delete;
    XCheck(config::XCheck *xc) {
        type = xcfg_xcheck_type(xc);
        switch (type) {
        case config::XCHECK_TYPE_FIXED:
            data_u64 = xcfg_xcheck_data_u64(xc);
            break;

        case config::XCHECK_TYPE_DJB2:
        case config::XCHECK_TYPE_AS_TYPE:
        case config::XCHECK_TYPE_CUSTOM:
            data_str = xcfg_xcheck_data_string(xc);
            break;
        }
    }

    XCheck(config::XCheckType ty, config::StringLenPtr s)
        : type(ty), data_str(s) {
    }
};

} // namespace crosschecks

#endif // CROSSCHECK_PLUGIN_CONFIG_H
