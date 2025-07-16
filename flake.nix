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
          ];
        };
      in
      {
        packages = {
          default = pkgs.callPackage ./nix { };
          unwrapped = self.packages.${system}.default.unwrapped;
        };

        overlays =
          final: prev:
          let
            overlain = final.extend (import rust-overlay);
          in
          {
            c2rust = overlain.callPackage ./nix { };
          };

        devShells.default = pkgs.callPackage ./shell.nix { };

        formatter = pkgs.nixfmt-tree;
        checks = {
          c2rust = self.packages.${system}.default;
          devShell = self.devShells.${system}.default;
        };
      }
    );
}
