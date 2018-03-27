include "llvm/Option/OptParser.td"

def config_files : JoinedOrSeparate<["-"], "C">, Flags<[RenderJoined]>,
    HelpText<"Read external configuration from file">;
def disable_xchecks : Flag<["--"], "disable-xchecks">,
    HelpText<"Disable cross-checks by default">;
