let
  nixpkgs = import <nixpkgs> { };
  inherit (nixpkgs) pkgs llvmPackages;
  stdenv = pkgs.clangStdenv;
in
stdenv.mkDerivation {
  name = "c2rust";
  buildInputs = [
    pkgs.clang
    pkgs.cmake
    pkgs.llvm
    pkgs.libllvm
    pkgs.openssl
    pkgs.pkgconfig
    pkgs.python3
    pkgs.rustup
    pkgs.zlib
  ];
  LIBCLANG_PATH = "${pkgs.libclang.lib}/lib";
  CMAKE_LLVM_DIR = "${llvmPackages.libllvm.dev}/lib/cmake/llvm";
  CMAKE_CLANG_DIR = "${llvmPackages.libclang.dev}/lib/cmake/clang";
}
