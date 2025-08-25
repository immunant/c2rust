{
  lib,
  # Requires rust-overlay for the pinned nightly
  # Specifically it requires an old snapshop.
  rust-bin,
  makeRustPlatform,
  rustc,
  clangStdenv,
  # binaries
  llvmPackages,
  cmake,
  pkg-config,
  # libraries
  tinycbor,
  # Check binaries
  python3,
}:
let
  # Something changed in nixpkgs since this snapshot,
  # so we need to stuff some additional attributes in it.
  toolchain =
    rust-bin.nightly."2022-08-08".minimal.override
      or (throw "Requires the snapshot/2024-08-01 tag of rust-overlay")
      {
        extensions = [
          "rustc-dev"
          "rust-src"
        ];
      }
    // {
      inherit (rustc) targetPlatforms badTargetPlatforms;
    };

  rustPlatform = makeRustPlatform {
    stdenv = clangStdenv;
    rustc = toolchain;
    cargo = toolchain;
  };

  fs = lib.fileset;

  cargoToml = builtins.fromTOML (builtins.readFile ../Cargo.toml);
in
rustPlatform.buildRustPackage (finalAttrs: {
  strictDeps = true;
  pname = "c2rust";
  inherit (cargoToml.workspace.package) version;

  src = fs.toSource {
    root = ../.;
    fileset = fs.difference (fs.gitTracked ../.) (
      fs.unions [
        ../.github
        ../.gitignore
        ../.gitmodules
        ../docs
        ../examples
        ../manual
        ../scripts
        ../.typos.toml
        ../README.md
        ../LICENSE
        ../book.toml
        ../flake.lock
        ../flake.nix
        ../shell.nix
      ]
    );
  };

  patches = [
    ./tinycbor.patch
  ];

  cargoLock.lockFile = ../Cargo.lock;
  cargoBuildFlags = [
    "-p"
    "c2rust"
  ];

  nativeBuildInputs = [
    rustPlatform.bindgenHook
    llvmPackages.libllvm
    cmake
    pkg-config
  ];

  buildInputs = [
    tinycbor
    llvmPackages.libclang
  ];

  env = {
    CMAKE_CLANG_DIR = "${llvmPackages.libclang.dev}/lib/cmake/clang";
    RUSTFLAGS = "-L native=${tinycbor}/lib";
  };

  # FIXME: uhhhhhhhhhhhhhhhhhhhhhhhhhhhh
  doCheck = false;
  nativeCheckInputs = [
    python3
  ];

  passthru.toolchain = toolchain;

  meta = {
    description = "helper for migrating C99 code to Rust";
    homepage = "https://c2rust.com/";
    changelog = "https://github.com/immunant/c2rust/releases/tag/v${finalAttrs.version}";
    downloadPage = "https://github.com/immunant/c2rust";
    mainProgram = "c2rust";
    license = with lib.licenses; [
      # c2rust
      bsd3
      # c2rust/c2rust-ast-pritner
      mit
      asl20
    ];
  };
})
