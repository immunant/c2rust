{
  description = "Flake for c2rust";

  inputs = {
    nixpkgs.url = "https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay/snapshot/2024-08-01";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import rust-overlay)
            (import self)
          ];
        };
      in
      {
        packages = {
          default = pkgs.c2rust;
          unwrapped = pkgs.c2rust.unwrapped;
        };

        devShells.default = pkgs.callPackage ./shell.nix { };

        formatter = pkgs.nixfmt-tree;
        checks = {
          inherit (pkgs) c2rust;
          devShell = self.devShells.${system}.default;
        };
      }
    )
    // {
      overlays = import self;
    };
}
