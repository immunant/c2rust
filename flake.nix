{
  description = "github.com/mstone/c2rust";

  inputs.crane.url = "github:ipetkov/crane";
  inputs.crane.inputs.flake-utils.follows = "flake-utils";
  inputs.crane.inputs.nixpkgs.follows = "nixpkgs";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";

  inputs.rust-overlay.url = "github:oxalica/rust-overlay";
  inputs.rust-overlay.inputs.flake-utils.follows = "flake-utils";
  inputs.rust-overlay.inputs.nixpkgs.follows = "nixpkgs";

  outputs = {self, nixpkgs, crane, rust-overlay, flake-utils}:
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "c2rust";
      systems = flake-utils.lib.allSystems;
      preOverlays = [ rust-overlay.overlay ];
      overlay = final: prev: {
        c2rust = rec {
          c2rust = lib.c2rust { isShell = false; };
          devShell = lib.c2rust { isShell = true; };
          defaultPackage = c2rust;

          rust = with final; with pkgs; (rust-bin.nightly."2022-02-14".minimal.override {
            extensions = [ "rustfmt-preview" "rust-src" "rustc-dev" "llvm-tools-preview" ];
	  });

          lib.c2rust = { isShell, subpkg ? "c2rust", subdir ? "." }:
            let
              buildInputs = with final; with pkgs; [
                rust
                llvmPackages_latest.clang
                llvmPackages_latest.libclang
                cmake
                llvmPackages_latest.llvm
                openssl
                pkgconfig
                python3
                rustup
                zlib
                curl
              ] ++ final.lib.optionals isShell [
                entr
              ] ++ final.lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
                Security
              ]) ++ final.lib.optionals stdenv.isLinux ([
              ]);
            in with final; with pkgs; crane.lib.${final.system}.buildPackage {
            pname = "${subpkg}";
            version = "0.1";

            src = self;

            inherit buildInputs;
            dontUseCmakeConfigure = true;
            RUST_SYSROOT = "${rust}";
            cargoExtraArgs = "--features dynamic-instrumentation";

            preFixup = final.lib.optionalString stdenv.isDarwin ''
              if [ -f "$out/bin/c2rust-instrument" ]; then
                install_name_tool -add_rpath "${rust}/lib" "$out/bin/c2rust-instrument"
              fi
            '';

            doCheck = false;
          };
        };
      };
    };
}
