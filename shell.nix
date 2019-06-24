let
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs llvmPackages;
  stdenv = pkgs.clangStdenv;
in
stdenv.mkDerivation {
  name = "c2rust";
  buildInputs = [
    pkgs.clang
    pkgs.cmake
    pkgs.llvm
    pkgs.openssl
    pkgs.pkgconfig
    pkgs.python35
    pkgs.rustup
    pkgs.zlib
  ];
  LIBCLANG_PATH="${llvmPackages.libclang}/lib";
}
