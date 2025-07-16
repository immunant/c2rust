{
  lib,
  runCommand,
  callPackage,
  makeBinaryWrapper,
  llvmPackages,
}:
let
  c2rust = callPackage ./package.nix { };
in
runCommand "c2rust-${c2rust.version}"
  {
    inherit (c2rust) version meta;
    nativeBuildInputs = [
      makeBinaryWrapper
    ];

    passthru.unwrapped = c2rust;
  }
  ''
    makeWrapper ${lib.getExe c2rust} "$out/bin/c2rust" \
      --prefix PATH : ${
        lib.makeBinPath [
          c2rust.toolchain
          llvmPackages.libllvm
        ]
      }
  ''
