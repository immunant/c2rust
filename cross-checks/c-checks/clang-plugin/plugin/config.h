#ifndef CROSSCHECK_PLUGIN_CONFIG_H
#define CROSSCHECK_PLUGIN_CONFIG_H

#include "llvm/Support/YAMLTraits.h"

#include <string>
#include <variant>
#include <vector>

struct XCheck {
    enum Tag {
        UNKNOWN = 0,
        FUNCTION_ENTRY = 1,
        FUNCTION_EXIT = 2,
        FUNCTION_ARG = 3,
        FUNCTION_RETURN = 4,
    };

    enum Type {
        DEFAULT,
        DISABLED,
        FIXED,
        DJB2,
        CUSTOM,
    } type;
    std::variant<std::monostate, uint64_t, std::string> data;

    XCheck(Type ty = DEFAULT) : type(ty), data() {}
    XCheck(Type ty, const std::string &s) : type(ty), data(s) {}

    // Required by IO::mapOptional
    bool operator==(const XCheck &other) const {
        return type == other.type && data == other.data;
    }
};

LLVM_YAML_DECLARE_MAPPING_TRAITS(XCheck)
LLVM_YAML_DECLARE_MAPPING_TRAITS(std::optional<XCheck>)
LLVM_YAML_DECLARE_ENUM_TRAITS(XCheck::Tag)
LLVM_YAML_IS_STRING_MAP(XCheck)

namespace llvm {
namespace yaml {
template<>
inline void yamlize(IO &io, XCheck &xc, bool, EmptyContext &ctx) {
    if (io.outputting()) {
        // TODO: implement
        llvm_unreachable("Unimplemented");
    } else {
        io.beginEnumScalar();
#define CHECK_ENUM(_s, _e)               \
    if (io.matchEnumScalar(_s, false)) { \
        xc.type = XCheck::_e;            \
        return;                          \
    }
        CHECK_ENUM("default",  DEFAULT);
        CHECK_ENUM("none",     DISABLED);
        CHECK_ENUM("disabled", DISABLED);
#undef CHECK_ENUM
        // Skip calling io.endEnumScalar(),
        // since that just exits with an error
        // for all values not covered above

        io.beginMapping();
#define CHECK_DATA(_s, _dt, _e) do { \
        std::optional<_dt> data;     \
        io.mapOptional(_s, data);    \
        if (data) {                  \
            xc.type = XCheck::_e;    \
            xc.data = *data;         \
            return;                  \
        }                            \
    } while (0)
        CHECK_DATA("fixed",  uint64_t,    FIXED);
        CHECK_DATA("djb2",   std::string, DJB2);
        CHECK_DATA("custom", std::string, CUSTOM);
#undef CHECK_DATA
        io.endMapping();

        // We couldn't handle this input, so throw an error
        io.setError("Unknown cross-check type");
    }
}

template<>
inline void yamlize(IO &io, std::optional<XCheck> &oxc,
                    bool required, EmptyContext &ctx) {
    XCheck xc;
    if (io.outputting() && oxc) {
        xc = *oxc;
    }
    yamlize(io, xc, required, ctx);
    if (!io.outputting()) {
        oxc = xc;
    }
}
} // namespace yaml
} // namespace llvm

struct ExtraXCheck {
    XCheck::Tag tag;
    std::string custom;
};

LLVM_YAML_DECLARE_MAPPING_TRAITS(ExtraXCheck)
LLVM_YAML_IS_SEQUENCE_VECTOR(ExtraXCheck)

struct DefaultsConfig {
    std::optional<bool> disable_xchecks;
    std::optional<XCheck> entry;
    std::optional<XCheck> exit;
    std::optional<XCheck> all_args;
    std::optional<XCheck> ret;
    // TODO: do we want entry/exit_extra here???

    DefaultsConfig() = default;
    DefaultsConfig(llvm::yaml::IO &io) {
        io.mapOptional("disable_xchecks", disable_xchecks);
        io.mapOptional("entry",     entry);
        io.mapOptional("exit",      exit);
        io.mapOptional("all_args",  all_args);
        io.mapOptional("return",    ret);
    }

    // Update the optionals in this config with the contents
    // of another DefaultsConfig
    void update(const DefaultsConfig &other) {
#define UPDATE_FIELD(field) if (other.field) { this->field = *other.field; }
        UPDATE_FIELD(disable_xchecks);
        UPDATE_FIELD(entry);
        UPDATE_FIELD(exit);
        UPDATE_FIELD(all_args);
        UPDATE_FIELD(ret);
#undef UPDATE_FIELD
    }
};

struct FunctionConfig {
    std::string name;
    std::optional<bool> disable_xchecks;
    std::optional<XCheck> entry;
    std::optional<XCheck> exit;
    std::optional<XCheck> all_args;
    std::map<std::string, XCheck> args;
    std::optional<XCheck> ret;
    std::optional<std::string> ahasher;
    std::optional<std::string> shasher;
    // TODO: nested
    std::vector<ExtraXCheck> entry_extra;
    std::vector<ExtraXCheck> exit_extra;

    FunctionConfig(std::string_view n) : name(n) {}

    void read_config(llvm::yaml::IO &io) {
        io.mapOptional("disable_xchecks", disable_xchecks);
        io.mapOptional("entry",     entry);
        io.mapOptional("exit",      exit);
        io.mapOptional("all_args",  all_args);
        io.mapOptional("args",      args);
        io.mapOptional("return",    ret);
        io.mapOptional("ahasher",   ahasher);
        io.mapOptional("shasher",   shasher);
        io.mapOptional("entry_extra", entry_extra);
        io.mapOptional("exit_extra",  exit_extra);
    }

    void update(const FunctionConfig &other) {
        assert(this->name == other.name && "Mismatched names in FunctionConfig::update()");
#define UPDATE_FIELD(field) if (other.field) { this->field = *other.field; }
        UPDATE_FIELD(disable_xchecks);
        UPDATE_FIELD(entry);
        UPDATE_FIELD(exit);
        UPDATE_FIELD(all_args);
        UPDATE_FIELD(ret);
        UPDATE_FIELD(ahasher);
        UPDATE_FIELD(shasher);
#undef UPDATE_FIELD
        for (auto &it : other.args)
            args.insert_or_assign(it.first, it.second);
        // Append other.entry_extra to ours
        // FIXME: should replace the existing entries instead???
        entry_extra.insert(entry_extra.end(),
                           other.entry_extra.begin(),
                           other.entry_extra.end());
        exit_extra.insert(exit_extra.end(),
                          other.exit_extra.begin(),
                          other.exit_extra.end());
    }
};

struct StructConfig {
    std::string name;
    std::optional<bool> disable_xchecks;
    std::optional<std::string> field_hasher;
    std::optional<std::string> custom_hash;
    std::map<std::string, XCheck> fields;
    std::optional<std::string> ahasher;
    std::optional<std::string> shasher;

    StructConfig(std::string_view n) : name(n) {}

    void read_config(llvm::yaml::IO &io) {
        io.mapOptional("disable_xchecks", disable_xchecks);
        io.mapOptional("field_hasher",  field_hasher);
        io.mapOptional("custom_hash",   custom_hash);
        io.mapOptional("fields",        fields);
        io.mapOptional("ahasher",       ahasher);
        io.mapOptional("shasher",       shasher);
    }

    void update(const StructConfig &other) {
        assert(this->name == other.name && "Mismatched names in StructConfig::update()");
#define UPDATE_FIELD(field) if (other.field) { this->field = *other.field; }
        UPDATE_FIELD(disable_xchecks);
        UPDATE_FIELD(field_hasher);
        UPDATE_FIELD(custom_hash);
        UPDATE_FIELD(ahasher);
        UPDATE_FIELD(shasher);
#undef UPDATE_FIELD
        for (auto &it : other.fields)
            fields.insert_or_assign(it.first, it.second);
    }
};

typedef std::variant<std::monostate, DefaultsConfig,
        FunctionConfig, StructConfig> ItemConfig;
typedef std::vector<ItemConfig> FileConfig;
typedef std::map<std::string, FileConfig> Config;

LLVM_YAML_DECLARE_MAPPING_TRAITS(FunctionConfig)
LLVM_YAML_DECLARE_MAPPING_TRAITS(StructConfig)
LLVM_YAML_DECLARE_MAPPING_TRAITS(ItemConfig)
LLVM_YAML_IS_SEQUENCE_VECTOR(ItemConfig)
LLVM_YAML_IS_STRING_MAP(FileConfig)

namespace llvm {
namespace yaml {
// TODO: generalize to ScalarTraits<std::optional<T>>
template<>
struct ScalarTraits<std::optional<bool>> {
    typedef std::optional<bool> OptT;

    static void output(const OptT &val, void *ctx, raw_ostream &out) {
        ScalarTraits<bool>::output(val.value_or(false), ctx, out);
    }

    static llvm::StringRef input(llvm::StringRef scalar, void *ctx, OptT &val) {
        bool x;
        auto res = ScalarTraits<bool>::input(scalar, ctx, x);
        val = x;
        return res;
    }

    static llvm::yaml::QuotingType mustQuote(llvm::StringRef s) {
        return ScalarTraits<bool>::mustQuote(s);
    }
};

template<>
struct ScalarTraits<std::optional<uint64_t>> {
    typedef std::optional<uint64_t> OptT;

    static void output(const OptT &val, void *ctx, raw_ostream &out) {
        ScalarTraits<uint64_t>::output(val.value_or(0), ctx, out);
    }

    static llvm::StringRef input(llvm::StringRef scalar, void *ctx, OptT &val) {
        uint64_t x;
        auto res = ScalarTraits<uint64_t>::input(scalar, ctx, x);
        val = x;
        return res;
    }

    static llvm::yaml::QuotingType mustQuote(llvm::StringRef s) {
        return ScalarTraits<uint64_t>::mustQuote(s);
    }
};

template<>
struct ScalarTraits<std::optional<std::string>> {
    typedef std::optional<std::string> OptT;

    static void output(const OptT &val, void *ctx, raw_ostream &out) {
        std::string empty_str;
        ScalarTraits<std::string>::output(val.value_or(empty_str), ctx, out);
    }

    static llvm::StringRef input(llvm::StringRef scalar, void *ctx, OptT &val) {
        std::string x;
        auto res = ScalarTraits<std::string>::input(scalar, ctx, x);
        val = x;
        return res;
    }

    static llvm::yaml::QuotingType mustQuote(llvm::StringRef s) {
        return ScalarTraits<std::string>::mustQuote(s);
    }
};
} // namespace yaml
} // namespace llvm

#endif // CROSSCHECK_PLUGIN_CONFIG_H
