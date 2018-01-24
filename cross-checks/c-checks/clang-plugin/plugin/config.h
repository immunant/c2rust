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

struct FunctionConfig {
    std::string name;
    bool disable_xchecks;
    XCheck entry;
    XCheck exit;
    XCheck all_args;
    std::map<std::string, XCheck> args;
    XCheck ret;
    std::string ahasher;
    std::string shasher;
    // TODO: nested

    // Construct a FunctionConfig from a given IO
    FunctionConfig(llvm::yaml::IO &io) {
        io.mapRequired("name",      name);
        io.mapOptional("disable_xchecks", disable_xchecks, false);
        io.mapOptional("entry",     entry,    XCheck(XCheck::DEFAULT));
        io.mapOptional("exit",      exit,     XCheck(XCheck::DEFAULT));
        io.mapOptional("all_args",  all_args, XCheck(XCheck::DISABLED));
        io.mapOptional("args",      args);
        io.mapOptional("return",    ret,      XCheck(XCheck::DISABLED));
        io.mapOptional("ahasher",   ahasher);
        io.mapOptional("shasher",   shasher);
    }
};

struct StructConfig {
    std::string name;
    // TODO
};

typedef std::variant<std::monostate, FunctionConfig, StructConfig> ItemConfig;
typedef std::vector<ItemConfig> FileConfig;
typedef std::map<std::string, FileConfig> Config;

LLVM_YAML_DECLARE_MAPPING_TRAITS(XCheck)
LLVM_YAML_IS_STRING_MAP(XCheck)
LLVM_YAML_DECLARE_MAPPING_TRAITS(ItemConfig)
LLVM_YAML_IS_SEQUENCE_VECTOR(ItemConfig)
LLVM_YAML_IS_STRING_MAP(FileConfig)

#endif // CROSSCHECK_PLUGIN_CONFIG_H
