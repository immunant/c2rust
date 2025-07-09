{
  description = "Flake for c2rust";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
    };
    oxalica = {
      url = "github:oxalica/rust-overlay";
    };
  };

  outputs = inputs@{ self, nixpkgs, utils, fenix, oxalica}:
    utils.lib.eachDefaultSystem (system:
    let
        fenixStable = fenix.packages.${system}.fromToolchainFile {
          file = ./rust-toolchain.toml;
          sha256 = "sha256-r/8YBFuFa4hpwgE3FnME7nQA2Uc1uqj0eCE1NWmI1u0";
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import oxalica)
          ];
          config = {
            allowUnfree = true;
          };
        };
        in {
          defaultPackage = self.devShell.${system};
          devShell = pkgs.mkShell.override { } {

            LIBCLANG_PATH = "${pkgs.llvmPackages_14.libclang.lib}/lib";
            CMAKE_LLVM_DIR = "${pkgs.llvmPackages_14.libllvm.dev}/lib/cmake/llvm";
            CMAKE_CLANG_DIR = "${pkgs.llvmPackages_14.libclang.dev}/lib/cmake/clang";
            shellHook = ''
              export CARGO_TARGET_DIR="$(git rev-parse --show-toplevel)/target_dirs/nix_rustc";
            '';
            RUST_SRC_PATH = pkgs.rustPlatform.rustLibSrc;
            buildInputs =
              with pkgs; [
                clangStdenv.cc
                llvmPackages_14.libclang
                pkg-config
                fenix.packages.${system}.rust-analyzer
                llvmPackages_14.clang
                cmake
                llvmPackages_14.llvm
                llvmPackages_14.libllvm
                openssl
                (python3.withPackages
                  (python-pkgs:
                    with python-pkgs;
                    [ "bencode-python3"
                      cbor
                      colorlog
                      mako
                      pip
                      plumbum
                      psutil
                      pygments
                      typing
                      "scan-build"
                      pyyaml
                      toml
                    ]
                  )
                )
                zlib
                (rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)
              ];
          };
    });
}
