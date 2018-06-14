#include "llvm/Support/YAMLTraits.h"

#include "config.h"

namespace llvm {
namespace yaml {

void ScalarEnumerationTraits<XCheck::Tag>::enumeration(IO &io, XCheck::Tag &tag) {
    io.enumCase(tag, "UNKNOWN",         XCheck::Tag::UNKNOWN);
    io.enumCase(tag, "FUNCTION_ENTRY",  XCheck::Tag::FUNCTION_ENTRY);
    io.enumCase(tag, "FUNCTION_EXIT",   XCheck::Tag::FUNCTION_EXIT);
    io.enumCase(tag, "FUNCTION_ARG",    XCheck::Tag::FUNCTION_ARG);
    io.enumCase(tag, "FUNCTION_RETURN", XCheck::Tag::FUNCTION_RETURN);
}

void MappingTraits<ExtraXCheck>::mapping(IO &io, ExtraXCheck &ex) {
    io.mapOptional("tag", ex.tag, XCheck::Tag::UNKNOWN);
    io.mapRequired("custom", ex.custom);
}

void MappingTraits<FunctionConfig>::mapping(IO &io, FunctionConfig &cfg) {
    cfg.yamlize(io);
}

void MappingTraits<StructConfig>::mapping(IO &io, StructConfig &cfg) {
    cfg.yamlize(io);
}

void MappingTraits<ItemConfig>::mapping(IO &io, ItemConfig &cfg) {
    if (io.outputting()) {
        switch (cfg.index()) {
        case 1:
            // FunctionConfig
            // TODO: implement
            llvm_unreachable("Unimplemented");
            break;

        case 2:
            // StructConfig
            // TODO: implement
            llvm_unreachable("Unimplemented");
            break;

        default:
            llvm_unreachable("Invalid ItemConfig index");
        }
    } else {
        std::string item_type;
        io.mapRequired("item", item_type);
        if (item_type == "defaults") {
            cfg.emplace<DefaultsConfig>(io);
        } else if (item_type == "function") {
            std::string name;
            io.mapRequired("name", name);
            cfg.emplace<FunctionConfig>(name).yamlize(io);
        } else if (item_type == "struct") {
            std::string name;
            io.mapRequired("name", name);
            cfg.emplace<StructConfig>(name).yamlize(io);
        } else {
            io.setError(Twine("Unknown item type: ") + item_type);
        }
    }
}

}
}
