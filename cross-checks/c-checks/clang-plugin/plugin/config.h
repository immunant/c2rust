#ifndef CROSSCHECK_PLUGIN_CONFIG_H
#define CROSSCHECK_PLUGIN_CONFIG_H

#include "llvm/Support/YAMLTraits.h"

#include <string>
#include <variant>
#include <vector>

struct XCheck {
    enum Type {
        DEFAULT,
        DISABLED,
        FIXED,
        DJB2,
        CUSTOM,
    } type;
    std::variant<std::monostate, uint64_t, std::string> data;

    XCheck(Type ty = DEFAULT) : type(ty), data() {}

    // Required by IO::mapOptional
    bool operator==(const XCheck &other) const {
        return type == other.type && data == other.data;
    }
};

struct DefaultsConfig {
    llvm::Optional<bool> disable_xchecks;
    llvm::Optional<XCheck> entry;
    llvm::Optional<XCheck> exit;
    llvm::Optional<XCheck> all_args;
    llvm::Optional<XCheck> ret;

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
    llvm::Optional<bool> disable_xchecks;
    llvm::Optional<XCheck> entry;
    llvm::Optional<XCheck> exit;
    llvm::Optional<XCheck> all_args;
    std::map<std::string, XCheck> args;
    llvm::Optional<XCheck> ret;
    llvm::Optional<std::string> ahasher;
    llvm::Optional<std::string> shasher;
    // TODO: nested

    // Construct a FunctionConfig from a given IO
    FunctionConfig(llvm::yaml::IO &io) {
        io.mapRequired("name",      name);
        io.mapOptional("disable_xchecks", disable_xchecks);
        io.mapOptional("entry",     entry);
        io.mapOptional("exit",      exit);
        io.mapOptional("all_args",  all_args);
        io.mapOptional("args",      args);
        io.mapOptional("return",    ret);
        io.mapOptional("ahasher",   ahasher);
        io.mapOptional("shasher",   shasher);
    }
};

struct StructConfig {
    std::string name;
    llvm::Optional<bool> disable_xchecks;
    llvm::Optional<std::string> field_hasher;
    llvm::Optional<std::string> custom_hash;
    std::map<std::string, XCheck> fields;
    llvm::Optional<std::string> ahasher;
    llvm::Optional<std::string> shasher;

    StructConfig(llvm::yaml::IO &io) {
        io.mapRequired("name",          name);
        io.mapOptional("disable_xchecks", disable_xchecks);
        io.mapOptional("field_hasher",  field_hasher);
        io.mapOptional("custom_hash",   custom_hash);
        io.mapOptional("fields",        fields);
        io.mapOptional("ahasher",       ahasher);
        io.mapOptional("shasher",       shasher);

    }
};

typedef std::variant<std::monostate, DefaultsConfig,
        FunctionConfig, StructConfig> ItemConfig;
typedef std::vector<ItemConfig> FileConfig;
typedef std::map<std::string, FileConfig> Config;

LLVM_YAML_DECLARE_MAPPING_TRAITS(XCheck)
LLVM_YAML_IS_STRING_MAP(XCheck)
LLVM_YAML_DECLARE_MAPPING_TRAITS(ItemConfig)
LLVM_YAML_IS_SEQUENCE_VECTOR(ItemConfig)
LLVM_YAML_IS_STRING_MAP(FileConfig)

#endif // CROSSCHECK_PLUGIN_CONFIG_H
