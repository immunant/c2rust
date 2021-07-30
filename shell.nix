let
  fetchFromGitHub = { owner, repo, rev, sha256 }: builtins.fetchTarball {
    name = "source";
    url = "https://api.github.com/repos/${owner}/${repo}/tarball/${rev}";
    inherit sha256;
  };
  pkgs = import <nixpkgs> {
    overlays = [
      (import (fetchFromGitHub {
        owner = "mozilla";
        repo = "nixpkgs-mozilla";
        rev = "4a07484cf0e49047f82d83fd119acffbad3b235f";
        sha256 = "1qg9gj6xlz2zsbdsijn5z2km29lc90qzwkcm9p1rpn4bcmkknq1r";
      } + "/rust-overlay.nix"))
      (final: prev: {
        rustChannelNightly = let rustChannel = final.rustChannelOf {
          rustToolchain = ./rust-toolchain;
          sha256 = "15219mpsg6bdgh137njrwzxvin9ipf20n62yykv34lk8j3z7h0xr";
        }; in rustChannel // { rust = rustChannel.rust.override (args: {
          extensions = args.extensions or [ ] ++ [ "rustc-dev" ];
        }); };
        rustChannel = final.rustChannelNightly;

        cargo = final.rustChannel.rust;
        rustc = final.rustChannel.rust;
      })
    ];
  };
  inherit (pkgs) lib llvmPackages;
  stdenv = pkgs.clangStdenv;
in
stdenv.mkDerivation {
  name = "c2rust";
  nativeBuildInputs = [
    pkgs.cmake
    pkgs.pkgconfig
    pkgs.rustChannel.rust
  ];
  buildInputs = [
    (lib.getDev llvmPackages.clang)
    (lib.getDev llvmPackages.llvm)
    pkgs.openssl
    pkgs.python3
    pkgs.zlib
  ];

  CLANG_CMAKE_DIR = "${lib.getDev llvmPackages.libclang}/lib/cmake/clang";
  LIBCLANG_PATH="${lib.getLib llvmPackages.libclang}/lib";
  LLVM_CONFIG_PATH = "${lib.getDev llvmPackages.llvm}/bin/llvm-config";
}
