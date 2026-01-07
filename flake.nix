{
  description = "Flake for c2rust";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      utils,
      fenix,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        fenixToolchain =
          let
            toml = with builtins; (fromTOML (readFile ./rust-toolchain.toml)).toolchain;
          in
          (fenix.packages.${system}.fromToolchainName {
            name = toml.channel;
            sha256 = "sha256-r/8YBFuFa4hpwgE3FnME7nQA2Uc1uqj0eCE1NWmI1u0";
          })."completeToolchain";

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ];
          config = {
            allowUnfree = true;
          };
        };

        myLLVM = pkgs.llvmPackages_18;
        myStdenv = pkgs.clang18Stdenv;

        rustPlatform = pkgs.makeRustPlatform {
          cargo = fenixToolchain;
          rustc = fenixToolchain;
        };
        env = with pkgs; {
          LIBCLANG_PATH = "${myLLVM.libclang.lib}/lib";
          CMAKE_LLVM_DIR = "${myLLVM.libllvm.dev}/lib/cmake/llvm";
          CMAKE_CLANG_DIR = "${myLLVM.libclang.dev}/lib/cmake/clang";
          LLVM_CONFIG_PATH = "${myLLVM.libllvm.dev}/bin/llvm-config";
          CLANG_PATH = "${myLLVM.clang}/bin/clang";
          TINYCBOR_DIR = "${pkgs.tinycbor}";
          NIX_ENFORCE_NO_NATIVE = 0; # Enable SSE instructions.
          # Enable nix in the c2rust test suite
          # This flag is used to tell the test scripts to look for
          # libraries under nix paths.
          C2RUST_USE_NIX = 1;
          RUST_SRC_PATH = "${fenixToolchain}/lib/rustlib/src/rust/library";
        };
      in
      rec {
        packages = {
          default = rustPlatform.buildRustPackage (
            with pkgs;
            env
            // {
              pname = "c2rust";
              version = "0.20.0";
              src = ./.;
              doCheck = false; # Can use checkFlags to disable specific tests

              patches = [ ./nix-tinycbor-cmake.patch ];

              nativeBuildInputs = with pkgs; [
                pkg-config
                cmake
                uv
                (python3.withPackages (
                  python-pkgs: with python-pkgs; [
                    "bencode-python3"
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
                ))
                myStdenv.cc
                myLLVM.libclang
                myLLVM.clang
                myLLVM.llvm
                myLLVM.libllvm
              ];

              buildInputs = with pkgs; [
                rustPlatform.bindgenHook
                myStdenv.cc
                myLLVM.libclang
                myLLVM.clang
                myLLVM.llvm
                myLLVM.libllvm
                tinycbor
                openssl
                zlib
                fenixToolchain
              ];

              cargoLock = {
                lockFile = ./Cargo.lock;
              };
            }
          );
        };
        defaultPackage = packages.default;

        devShells = {
          # Include a fixed version of clang in the development environment for testing.
          default = pkgs.mkShell (
            with pkgs;
            env
            // {
              strictDeps = true;
              inputsFrom = [ packages.default ];
              buildInputs = [ ];
            }
          );
        };

        devShell = devShells.default;
      }
    );
}
