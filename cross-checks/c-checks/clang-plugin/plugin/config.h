#ifndef CROSSCHECK_PLUGIN_CONFIG_H
#define CROSSCHECK_PLUGIN_CONFIG_H

#include "llvm/Support/YAMLTraits.h"

#include <string>
#include <variant>
#include <vector>

struct XCheckType {
    enum {
        DEFAULT,
        DISABLED,
        FIXED,
        DJB2,
        CUSTOM,
    } type;
    std::variant<uint64_t, std::string> data;

    XCheckType() : type(DEFAULT), data() {}
};

struct FunctionConfig {
    std::string name;
    bool disable_xchecks;
    XCheckType entry;
    XCheckType exit;
    XCheckType all_args;
    std::map<std::string, XCheckType> args;
    XCheckType ret;
    std::string ahasher;
    std::string shasher;
    // TODO: nested

    // Construct a FunctionConfig from a given IO
    FunctionConfig(llvm::yaml::IO &io) {
        io.mapRequired("name",      name);
        io.mapOptional("disable_xchecks", disable_xchecks, false);
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
    // TODO
};

typedef std::variant<std::monostate, FunctionConfig, StructConfig> ItemConfig;
typedef std::vector<ItemConfig> FileConfig;
typedef std::map<std::string, FileConfig> Config;

LLVM_YAML_DECLARE_MAPPING_TRAITS(XCheckType)
LLVM_YAML_IS_STRING_MAP(XCheckType)
LLVM_YAML_DECLARE_MAPPING_TRAITS(ItemConfig)
LLVM_YAML_IS_SEQUENCE_VECTOR(ItemConfig)
LLVM_YAML_IS_STRING_MAP(FileConfig)

#endif // CROSSCHECK_PLUGIN_CONFIG_H
