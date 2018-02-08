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

template<>
void yamlize(IO &io, XCheck &xc, bool, EmptyContext &ctx) {
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
#define CHECK_DATA(_s, _dt, _e) do {              \
        EmptyContext ctx;                         \
        Optional<_dt> data;                       \
        io.mapOptionalWithContext(_s, data, ctx); \
        if (data) {                               \
            xc.type = XCheck::_e;                 \
            xc.data = *data;                      \
            return;                               \
        }                                         \
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
            cfg = DefaultsConfig(io);
        } else if (item_type == "function") {
            cfg = FunctionConfig(io);
        } else if (item_type == "struct") {
            cfg = StructConfig(io);
        } else {
            io.setError(Twine("Unknown item type: ") + item_type);
        }
    }
}

}
}
