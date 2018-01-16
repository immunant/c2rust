include "llvm/Option/OptParser.td"

def config_files : JoinedOrSeparate<["-"], "C">, Flags<[RenderJoined]>,
    HelpText<"Read external configuration from file">;
